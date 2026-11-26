import pathlib
import tempfile
import unittest

from .testing import Recorder, Step, build_script, patch_all


class ContainerTest(unittest.TestCase):
    def test_lifecycle_and_steps(self):
        recorder = Recorder(answers={"printenv": "/home/builder\n"})
        src = pathlib.Path("/src")
        with (
            patch_all("run", recorder),
            build_script.Container(src, {src: "/project"}, {"TAG": "22.04"}, ["--cap-add", "SYS_ADMIN"]) as c,
        ):
            home = c.run(Step(("printenv", "HOME")), capture=True)
            c.run(Step(("tee", "/f"), workdir="/w", env={"A": "1"}, user="root", input="x"))
        build, start, printenv, tee, rm, rmi = recorder.commands
        self.assertEqual(build[:4], ["docker", "build", "--no-cache", "-f"])
        self.assertIn("TAG=22.04", build)
        self.assertEqual(build[-1], "/src")
        self.assertEqual(start[:6], ["docker", "run", "-t", "-d", "--name", c.name])
        self.assertIn("/src:/project", start)
        self.assertEqual(printenv, ["docker", "exec", "-w", "/project", c.name, "printenv", "HOME"])
        self.assertEqual(home, "/home/builder\n")
        self.assertEqual(tee, ["docker", "exec", "-w", "/w", "-i", "-u", "root", "-e", "A=1", c.name, "tee", "/f"])
        self.assertEqual(recorder.inputs[3], "x")
        self.assertEqual((rm[:3], rmi[:3]), (["docker", "rm", "--force"], ["docker", "image", "rm"]))

    def test_image_is_removed_when_the_container_fails_to_start(self):
        recorder = Recorder(fail_once=["docker run"])
        with (
            patch_all("run", recorder),
            self.assertRaises(build_script.BuildError),
            build_script.Container(pathlib.Path("/src"), {}),
        ):
            pass
        self.assertEqual(recorder.commands[-1][:3], ["docker", "image", "rm"])

    def test_built_image_is_saved_to_its_archive_and_removed_from_docker(self):
        recorder = Recorder()
        with tempfile.TemporaryDirectory() as tmp:
            archive = pathlib.Path(tmp) / "image/image.tar"
            with patch_all("run", recorder), build_script.Container(pathlib.Path("/src"), {}, image=archive) as c:
                pass
            self.assertTrue(archive.parent.is_dir())
        build, save, start, rm, rmi = recorder.commands
        self.assertEqual((build[5:7], build[-1]), (["-t", build_script.ARCHIVED_IMAGE], "/src"))
        self.assertEqual(save, ["docker", "save", "-o", str(archive), build_script.ARCHIVED_IMAGE])
        self.assertEqual(start[-1], build_script.ARCHIVED_IMAGE)
        self.assertEqual(rm, ["docker", "rm", "--force", c.name])
        self.assertEqual(rmi, ["docker", "image", "rm", build_script.ARCHIVED_IMAGE])

    def test_archived_image_is_loaded_in_place_of_a_build(self):
        recorder = Recorder()
        with tempfile.TemporaryDirectory() as tmp:
            archive = pathlib.Path(tmp) / "image.tar"
            archive.write_bytes(b"tar")
            with patch_all("run", recorder), build_script.Container(pathlib.Path("/src"), {}, image=archive):
                pass
        load, start, _, rmi = recorder.commands
        self.assertEqual(load, ["docker", "load", "-i", str(archive)])
        self.assertEqual(start[-1], build_script.ARCHIVED_IMAGE)
        self.assertEqual(rmi[-1], build_script.ARCHIVED_IMAGE)

    def test_copy_out_follows_symlinks(self):
        recorder = Recorder()
        with patch_all("run", recorder):
            build_script.Container(pathlib.Path("/src"), {}).copy_out("/project/result/x.zip", pathlib.Path("/out"))
        self.assertEqual(recorder.commands[0][:3], ["docker", "cp", "-L"])
