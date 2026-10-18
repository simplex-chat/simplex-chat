import importlib.util
import os
import pathlib
import sys
import time
import unittest


spec = importlib.util.spec_from_file_location(
    "select_intel_setup", pathlib.Path(__file__).with_name("select-intel-setup.py")
)
selector = importlib.util.module_from_spec(spec)
spec.loader.exec_module(selector)

OLD = "/nix/store/vghjgkr64qjigddh3gnf3c103wbvpxmz-ghc-boot-lib-ghc-boot-9.14.drv"
NEW = "/nix/store/" + "0" * 32 + "-ghc-boot-lib-ghc-boot-9.14.drv"


class FailedComponentSelection(unittest.TestCase):
    def test_current_and_rebuilt_component(self):
        for path in [OLD, NEW]:
            with self.subTest(path=path):
                self.assertEqual(
                    selector.select_failed_component(f"error: Cannot build '{path}'."),
                    path,
                )

    def test_old_nix_error_and_repeated_path(self):
        log = f"error: builder for '{NEW}' failed with exit code 134\n"
        log += f"error: Cannot build '{NEW}'.\n"
        self.assertEqual(selector.select_failed_component(log), NEW)

    def test_unrelated_or_progress_paths_are_ignored(self):
        for log in [
            f"building '{NEW}'...",
            f"error: Cannot build '{NEW.replace('-9.14.drv', '-9.12.drv')}'.",
            f"error: Cannot build '{NEW.replace('-lib-', '-exe-')}'.",
            f"error: Cannot build '{NEW.replace('/nix/store/', '/tmp/')}'.",
            f"error: Cannot build '{NEW.replace('0' * 32, 'e' * 32)}'.",
        ]:
            with self.subTest(log=log):
                self.assertIsNone(selector.select_failed_component(log))

    def test_multiple_components_are_rejected(self):
        with self.assertRaisesRegex(ValueError, "multiple"):
            selector.select_failed_component(
                f"error: Cannot build '{OLD}'.\nerror: Cannot build '{NEW}'."
            )


class AppleToolDiscovery(unittest.TestCase):
    @unittest.skipUnless(sys.platform == "darwin" and os.environ.get("GITHUB_ACTIONS") == "true",
                         "real Apple metadata check runs on hosted Darwin CI")
    def test_selected_tools_before_production_build(self):
        # This suite already runs in the Intel policy step before the long
        # build. Exercise the actual resolver early, without invoking GHC.
        spec = importlib.util.spec_from_file_location(
            "intel_trace", pathlib.Path(__file__).with_name("trace-intel-ghc.py")
        )
        trace = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(trace)
        deadline = time.monotonic() + 20
        compiler, sdk = trace.apple_tool_paths(deadline)
        self.assertTrue(compiler.is_file())
        self.assertTrue(sdk.is_dir())
        version = trace.run_step([compiler, "--version"], deadline, 10, True, phase="check-apple-clang")
        self.assertIn("Apple clang", version)


if __name__ == "__main__":
    unittest.main()
