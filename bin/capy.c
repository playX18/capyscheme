#include "../c/capy.h"
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char *find_heap(int *argc, char ***argv) {
  char *heap_path = NULL;
  char exe_path[PATH_MAX];
  char *exe_dir;
  char cwd[PATH_MAX];

  // Check current working directory
  if (getcwd(cwd, sizeof(cwd))) {
    char *candidate = malloc(strlen(cwd) + strlen("/capy.heap") + 1);

    sprintf(candidate, "%s/capy.heap", cwd);
    if (access(candidate, F_OK) == 0) {
      return candidate;
    }
    free(candidate);
  }

// Check /proc/self/exe on Linux
#ifdef __linux__
  char proc_path[PATH_MAX];
  ssize_t len = readlink("/proc/self/exe", proc_path, sizeof(proc_path) - 1);
  if (len != -1) {
    proc_path[len] = '\0';
    char *proc_dir = dirname(proc_path);

    char *candidate = malloc(strlen(proc_dir) + strlen("/capy.heap") + 1);
    sprintf(candidate, "%s/capy.heap", proc_dir);
    if (access(candidate, F_OK) == 0) {
      return candidate;
    }
    free(candidate);
  }
#endif
// Check using _NSGetExecutablePath on macOS
#ifdef __APPLE__
  char mac_path[PATH_MAX];
  uint32_t size = sizeof(mac_path);
  if (_NSGetExecutablePath(mac_path, &size) == 0) {
    char *mac_dir = dirname(mac_path);

    char *candidate = malloc(strlen(mac_dir) + strlen("/capy.heap") + 1);
    sprintf(candidate, "%s/capy.heap", mac_dir);
    if (access(candidate, F_OK) == 0) {
      return candidate;
    }
    free(candidate);
  }
#endif
// Check using sysctl on FreeBSD
#ifdef __FreeBSD__
  int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1};
  char freebsd_path[PATH_MAX];
  size_t cb = sizeof(freebsd_path);
  if (sysctl(mib, 4, freebsd_path, &cb, NULL, 0) == 0) {
    char *freebsd_dir = dirname(freebsd_path);

    char *candidate = malloc(strlen(freebsd_dir) + strlen("/capy.heap") + 1);
    sprintf(candidate, "%s/capy.heap", freebsd_dir);
    if (access(candidate, F_OK) == 0) {
      return candidate;
    }
    free(candidate);
  }
#endif

  // Get executable directory from argv[0]
  if ((*argc) > 0 && (*argv)[0]) {
    strcpy(exe_path, (*argv)[0]);
    char resolved[PATH_MAX];

    if (realpath(exe_path, resolved)) {
      strcpy(exe_path, resolved);
    }
    exe_dir = dirname(exe_path);

    char *candidate = malloc(strlen(exe_dir) + strlen("/capy.heap") + 1);
    sprintf(candidate, "%s/capy.heap", exe_dir);
    if (access(candidate, F_OK) == 0) {
      return candidate;
    }

    free(candidate);
  }

  return NULL;
}

struct PrepareData {
  int argc;
  char **argv;
};

void prepare_resume(ContextRef ctx, Value *args, void *data) {
  struct PrepareData *pdata = (struct PrepareData *)data;
  scm_program_arguments_init(ctx, pdata->argc,
                             (const char *const *)pdata->argv);
  *args = scm_cons(ctx, scm_program_arguments(ctx), VALUE_NULL);
}

int finish_resume(ContextRef ctx, bool success, Value result, void *data) {
  // No-op
  return success ? 0 : 1;
}

int main(int argc, char **argv) {
  char *heap = find_heap(&argc, &argv);
  if (heap) {
    FILE *file = fopen(heap, "r");
    if (!file) {
      printf(";; failed to open heap file: %s\n", heap);
      return 1;
    }

    uint8_t *buffer = NULL;
    size_t size = 0;
    fseek(file, 0, SEEK_END);
    size = ftell(file);
    fseek(file, 0, SEEK_SET);
    buffer = malloc(size);
    fread(buffer, 1, size, file);
    fclose(file);

    ScmRef scm = scm_from_image(buffer, size);
    free(buffer);

    struct PrepareData pdata = {argc, argv};

    int res = scm_resume_accumulator(scm, prepare_resume, &pdata, finish_resume,
                                     NULL);
    scm_free(scm);
    return res;
  } else {
    printf(";; heap not found\n");
    return 1;
  }
}