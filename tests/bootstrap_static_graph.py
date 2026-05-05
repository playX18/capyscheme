import pathlib
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]


def read(path):
    return (ROOT / path).read_text(encoding="utf-8")


class StaticGraphBootstrapTests(unittest.TestCase):
    def test_module_autoload_implementation_is_removed(self):
        modules = read("lib/boot/modules.scm")

        self.assertNotIn("try-module-autoload", modules)
        self.assertNotIn("autoloads-in-progress", modules)
        self.assertNotIn("autoloads-done", modules)
        self.assertNotIn("clear-module-autoload!", modules)
        self.assertIn("module is not present in the static module graph", modules)

    def test_cli_no_longer_enables_fresh_auto_compile(self):
        cli = read("lib/boot/cli.scm")
        cli_slim = read("lib/boot/cli-slim.scm")

        self.assertNotIn('"fresh-auto-compile"', cli)
        self.assertNotIn("-fresh-auto-compile", cli_slim)

    def test_stage_zero_uses_explicit_bootstrap_batch(self):
        makefile = read("Makefile")

        self.assertNotIn("--fresh-auto-compile", makefile)
        self.assertNotIn("COMPILE_PSYNTAX", makefile)
        self.assertIn("lib/boot/bootstrap-batch.scm bootstrap/bootstrap-min.scm", makefile)

    def test_bootstrap_manifest_contains_seed_imports(self):
        manifest = read("bootstrap/bootstrap-min.scm")

        self.assertIn("(import (rnrs))", manifest)
        self.assertIn("(import (scheme base))", manifest)
        self.assertIn("(import (srfi 1))", manifest)
        self.assertIn("(import (srfi 13))", manifest)


if __name__ == "__main__":
    unittest.main()
