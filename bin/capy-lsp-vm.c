#include "../c/capy.h"
#include "gc_args.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct DispatchData
{
    const char *line;
    bool should_exit;
    int status;
};

static const char *skip_json_ws(const char *p)
{
    while (*p && isspace((unsigned char)*p))
    {
        p++;
    }
    return p;
}

static bool is_json_delim(char ch)
{
    return ch == '\0' || ch == ',' || ch == '}' || isspace((unsigned char)ch);
}

static bool json_field_string_equals(const char *json, const char *field, const char *value)
{
    char pattern[128];
    size_t field_len = strlen(field);
    size_t value_len = strlen(value);

    if (field_len + 3 > sizeof(pattern))
    {
        return false;
    }

    snprintf(pattern, sizeof(pattern), "\"%s\"", field);

    for (const char *p = json; (p = strstr(p, pattern)) != NULL; p += field_len + 2)
    {
        const char *q = skip_json_ws(p + field_len + 2);
        if (*q != ':')
        {
            continue;
        }

        q = skip_json_ws(q + 1);
        if (*q != '"')
        {
            continue;
        }

        q++;
        if (strncmp(q, value, value_len) == 0 && q[value_len] == '"')
        {
            return true;
        }
    }

    return false;
}

static bool json_field_bool_true(const char *json, const char *field)
{
    char pattern[128];
    size_t field_len = strlen(field);

    if (field_len + 3 > sizeof(pattern))
    {
        return false;
    }

    snprintf(pattern, sizeof(pattern), "\"%s\"", field);

    for (const char *p = json; (p = strstr(p, pattern)) != NULL; p += field_len + 2)
    {
        const char *q = skip_json_ws(p + field_len + 2);
        if (*q != ':')
        {
            continue;
        }

        q = skip_json_ws(q + 1);
        if (strncmp(q, "true", 4) == 0 && is_json_delim(q[4]))
        {
            return true;
        }
    }

    return false;
}

static bool is_shutdown_like_json(const char *json)
{
    return json_field_string_equals(json, "method", "shutdown") ||
           json_field_string_equals(json, "method", "exit") ||
           json_field_string_equals(json, "type", "shutdown") ||
           json_field_string_equals(json, "type", "exit") ||
           json_field_bool_true(json, "shutdown") ||
           json_field_bool_true(json, "exit");
}

static char *scheme_dispatch_expression(const char *line)
{
    const char *prefix = "(import (capy lsp worker))\n(dispatch-json \"";
    const char *suffix = "\")";
    size_t capacity = strlen(prefix) + strlen(suffix) + strlen(line) * 2 + 1;
    char *expr = malloc(capacity);
    char *out = expr;

    if (!expr)
    {
        return NULL;
    }

    memcpy(out, prefix, strlen(prefix));
    out += strlen(prefix);

    for (const char *p = line; *p; p++)
    {
        if (*p == '\\' || *p == '"')
        {
            *out++ = '\\';
        }
        *out++ = *p;
    }

    memcpy(out, suffix, strlen(suffix));
    out += strlen(suffix);
    *out = '\0';

    return expr;
}

static char *read_json_line(FILE *input, bool *ok)
{
    size_t capacity = 4096;
    size_t length = 0;
    char *line = malloc(capacity);
    int ch;

    *ok = true;

    if (!line)
    {
        *ok = false;
        return NULL;
    }

    while ((ch = fgetc(input)) != EOF)
    {
        if (ch == '\n')
        {
            break;
        }

        if (ch == '\r')
        {
            continue;
        }

        if (length + 1 >= capacity)
        {
            size_t next_capacity = capacity * 2;
            char *next = realloc(line, next_capacity);
            if (!next)
            {
                free(line);
                *ok = false;
                return NULL;
            }
            line = next;
            capacity = next_capacity;
        }

        line[length++] = (char)ch;
    }

    if (ferror(input))
    {
        free(line);
        *ok = false;
        return NULL;
    }

    if (ch == EOF && length == 0)
    {
        free(line);
        return NULL;
    }

    line[length] = '\0';
    return line;
}

static void prepare_dispatch(ContextRef ctx, Value *args, void *data)
{
    struct DispatchData *dispatch = (struct DispatchData *)data;
    Value request = scm_string(ctx, dispatch->line);
    *args = scm_cons(ctx, request, VALUE_NULL);
}

static int finish_dispatch(ContextRef ctx, bool success, Value result, void *data)
{
    struct DispatchData *dispatch = (struct DispatchData *)data;
    uintptr_t length;
    uintptr_t written = 0;
    char *response;

    if (!success)
    {
        uintptr_t error_length = scm_value_utf8_length(ctx, result);
        uintptr_t error_written = 0;
        char *error = malloc((size_t)error_length + 1);
        if (error &&
            scm_value_to_utf8(ctx, result, error, error_length + 1, &error_written))
        {
            fprintf(stderr, "capy-lsp-vm: dispatch-json failed: %s\n", error);
        }
        else
        {
            fputs("capy-lsp-vm: dispatch-json failed\n", stderr);
        }
        free(error);
        dispatch->status = 1;
        return 1;
    }

    if (!scm_is_string(ctx, result))
    {
        fputs("capy-lsp-vm: dispatch-json returned a non-string value\n", stderr);
        dispatch->status = 1;
        return 1;
    }

    length = scm_string_utf8_length(ctx, result);
    response = malloc((size_t)length + 1);
    if (!response)
    {
        fputs("capy-lsp-vm: failed to allocate response buffer\n", stderr);
        dispatch->status = 1;
        return 1;
    }

    if (!scm_string_to_utf8(ctx, result, response, length + 1, &written) || written != length)
    {
        fputs("capy-lsp-vm: failed to copy response string\n", stderr);
        free(response);
        dispatch->status = 1;
        return 1;
    }

    if (fwrite(response, 1, (size_t)written, stdout) != (size_t)written ||
        fputc('\n', stdout) == EOF ||
        fflush(stdout) == EOF)
    {
        fputs("capy-lsp-vm: failed to write response\n", stderr);
        free(response);
        dispatch->status = 1;
        return 1;
    }

    if (is_shutdown_like_json(response))
    {
        dispatch->should_exit = true;
    }

    free(response);
    return 0;
}

static int finish_bootstrap(ContextRef ctx, bool success, Value result, void *data)
{
    struct DispatchData *dispatch = (struct DispatchData *)data;

    (void)ctx;
    (void)result;

    if (!success)
    {
        fputs("capy-lsp-vm: failed to load (capy lsp worker)\n", stderr);
        dispatch->status = 1;
        return 1;
    }

    return 0;
}

int main(int argc, char **argv)
{
    ScmRef scm;
    int exit_code = 0;

    if (!capy_apply_gc_args(&argc, argv))
    {
        return 1;
    }

    scm = scm_new();

    {
        struct DispatchData bootstrap = {
            .line = "(import (capy lsp worker))",
            .should_exit = false,
            .status = 0,
        };
        int bootstrap_status = scm_call(scm,
                                        "boot cli",
                                        "eval-string",
                                        prepare_dispatch,
                                        &bootstrap,
                                        finish_bootstrap,
                                        &bootstrap);
        if (bootstrap_status != 0 || bootstrap.status != 0)
        {
            scm_free(scm);
            return bootstrap.status != 0 ? bootstrap.status : bootstrap_status;
        }
    }

    for (;;)
    {
        bool ok;
        char *line = read_json_line(stdin, &ok);
        struct DispatchData dispatch;
        char *expr;
        int call_status;

        if (!line)
        {
            exit_code = ok ? 0 : 1;
            break;
        }

        if (line[0] == '\0')
        {
            free(line);
            continue;
        }

        expr = scheme_dispatch_expression(line);
        if (!expr)
        {
            fputs("capy-lsp-vm: failed to allocate dispatch expression\n", stderr);
            free(line);
            exit_code = 1;
            break;
        }

        dispatch.line = expr;
        dispatch.should_exit = is_shutdown_like_json(line);
        dispatch.status = 0;

        call_status = scm_call(scm,
                               "boot cli",
                               "eval-string/value",
                               prepare_dispatch,
                               &dispatch,
                               finish_dispatch,
                               &dispatch);

        free(expr);
        free(line);

        if (call_status != 0 || dispatch.status != 0)
        {
            exit_code = dispatch.status != 0 ? dispatch.status : call_status;
            break;
        }

        if (dispatch.should_exit)
        {
            break;
        }
    }

    scm_free(scm);
    return exit_code;
}
