#include "../c/capy.h"
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

struct PrepareData
{
    int argc;
    char **argv;
};

void prepare_resume(ContextRef ctx, Value *args, void *data)
{
    struct PrepareData *pdata = (struct PrepareData *)data;
    scm_program_arguments_init(ctx, pdata->argc,
                               (const char *const *)pdata->argv);
    *args = scm_cons(ctx, scm_program_arguments(ctx), VALUE_NULL);
}

int finish_resume(ContextRef ctx, bool success, Value result, void *data)
{
    // No-op
    return success ? 0 : 1;
}

int main(int argc, char **argv)
{
    ScmRef scm = scm_new();

    struct PrepareData pdata = {argc, argv};

    scm_call(scm,
             "boot cli",
             "enter-compiler",
             prepare_resume,
             &pdata,
             finish_resume,
             NULL);
    scm_free(scm);
}