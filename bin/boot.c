/*
 * CapyScheme bootloader
 *
 *
 * Builts the entire runtime & stdlib and then dumps heap image.
 */

 #include "../c/capy.h"

int main() {
    ScmRef scm = scm_new();
    scm_load_file(scm, "./lib/entrypoint.scm");
    scm_free(scm);
}
