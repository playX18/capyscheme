#ifndef CAPY_BIN_GC_ARGS_H
#define CAPY_BIN_GC_ARGS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct CapyGcArg
{
  const char *flag;
  const char *env;
};

static const struct CapyGcArg CAPY_GC_ARGS[] = {
    {"--gc-plan", "MMTK_PLAN"},
    {"--gc-trigger", "MMTK_GC_TRIGGER"},
    {"--gc-max-heap", "CAPY_GC_MAX_HEAP"},
    {"--gc-heuristic", "CAPY_GC_HEURISTIC"},
    {"--gc-min-free-percent", "CAPY_GC_MIN_FREE_PERCENT"},
    {"--gc-init-free-percent", "CAPY_GC_INIT_FREE_PERCENT"},
    {"--gc-allocation-threshold-percent", "CAPY_GC_ALLOCATION_THRESHOLD_PERCENT"},
    {"--gc-alloc-spike-percent", "CAPY_GC_ALLOC_SPIKE_PERCENT"},
    {"--gc-learning-steps", "CAPY_GC_LEARNING_STEPS"},
    {"--gc-guaranteed-interval-ms", "CAPY_GC_GUARANTEED_INTERVAL_MS"},
};

static const struct CapyGcArg *capy_gc_arg_for(const char *arg, const char **value)
{
  size_t count = sizeof(CAPY_GC_ARGS) / sizeof(CAPY_GC_ARGS[0]);

  for (size_t i = 0; i < count; i++)
  {
    const char *flag = CAPY_GC_ARGS[i].flag;
    size_t flag_len = strlen(flag);

    if (strcmp(arg, flag) == 0)
    {
      *value = NULL;
      return &CAPY_GC_ARGS[i];
    }

    if (strncmp(arg, flag, flag_len) == 0 && arg[flag_len] == '=')
    {
      *value = arg + flag_len + 1;
      return &CAPY_GC_ARGS[i];
    }
  }

  return NULL;
}

static int capy_apply_gc_args(int *argc, char **argv)
{
  int write = 1;
  int pass_through = 0;

  for (int read = 1; read < *argc; read++)
  {
    const char *arg = argv[read];
    const char *value = NULL;
    const struct CapyGcArg *gc_arg = pass_through ? NULL : capy_gc_arg_for(arg, &value);

    if (gc_arg)
    {
      if (!value)
      {
        if (read + 1 >= *argc)
        {
          fprintf(stderr, "%s requires a value\n", gc_arg->flag);
          return 0;
        }

        value = argv[++read];
      }

      if (setenv(gc_arg->env, value, 1) != 0)
      {
        perror("setenv");
        return 0;
      }

      continue;
    }

    argv[write++] = argv[read];
    if (strcmp(arg, "--") == 0)
    {
      pass_through = 1;
    }
  }

  argv[write] = NULL;
  *argc = write;
  return 1;
}

#endif
