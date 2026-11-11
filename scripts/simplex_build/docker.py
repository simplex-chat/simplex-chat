"""Commands run in a Docker container of Dockerfile.build, or on this machine."""

import os
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path, PurePosixPath
from types import TracebackType
from typing import Protocol

from .common import BuildError, run, warn

PROJECT = "/project"


OUT = "/out"


@dataclass(frozen=True, slots=True)
class Step:
    """A command run in a container, without a shell."""

    argv: tuple[str, ...]
    workdir: str = PROJECT
    env: Mapping[str, str] = field(default_factory=dict)
    user: str | None = None
    input: str | None = None


class Container:
    """A container of an image built from Dockerfile.build, removed with the image on exit."""

    def __init__(
        self,
        source: Path,
        mounts: Mapping[Path, str],
        build_args: Mapping[str, str] | None = None,
        run_args: Sequence[str] = (),
    ) -> None:
        self.name = f"simplex-reproduce-{os.getpid()}-{id(self)}"
        self.owned = True
        self.source = source
        self.mounts = dict(mounts)
        self.build_args = {"USER_UID": str(os.getuid()), "USER_GID": str(os.getgid()), **(build_args or {})}
        self.run_args = list(run_args)

    @classmethod
    def running(cls, name: str, source: Path) -> "Container":
        """A container that runs already, such as CI's builder, with source at /project; kept on exit."""
        container = cls(source, {source: PROJECT})
        container.name = name
        container.owned = False
        return container

    def __enter__(self) -> "Container":
        if not self.owned:
            return self
        build: list[str | Path] = ["docker", "build", "--no-cache", "-f", self.source / "Dockerfile.build", "-t", self.name]
        for key, value in self.build_args.items():
            build += ["--build-arg", f"{key}={value}"]
        run([*build, self.source])
        start: list[str | Path] = ["docker", "run", "-t", "-d", "--name", self.name, *self.run_args]
        for host, container in self.mounts.items():
            start += ["-v", f"{host}:{container}"]
        try:
            run([*start, self.name], capture=True)
        except BaseException:
            self.remove()
            raise
        return self

    def __exit__(
        self, exc_type: type[BaseException] | None, exc: BaseException | None, traceback: TracebackType | None
    ) -> None:
        if self.owned:
            self.remove()

    def remove(self) -> None:
        run(["docker", "rm", "--force", self.name], check=False, capture=True)
        run(["docker", "image", "rm", self.name], check=False, capture=True)

    def run(self, step: Step, capture: bool = False, secrets: Mapping[str, str] | None = None) -> str:
        """Secrets are environment variables passed to docker by name, so that their values are in no command line."""
        args = ["docker", "exec", "-w", step.workdir]
        if step.input is not None:
            args.append("-i")
        if step.user:
            args += ["-u", step.user]
        for key, value in step.env.items():
            args += ["-e", f"{key}={value}"]
        for key in secrets or {}:
            args += ["-e", key]
        title = " ".join(step.argv)
        return run([*args, self.name, *step.argv], env=secrets, input=step.input, capture=capture, title=title).stdout or ""

    def run_all(self, steps: Sequence[Step]) -> None:
        for step in steps:
            self.run(step)

    def copy_out(self, path: str, target: Path) -> None:
        # -L copies the targets of symlinks, such as Nix's out-links into the container's store
        run(["docker", "cp", "-L", f"{self.name}:{path}", target])

    def path(self, host: Path) -> str:
        """Where a host path under one of the mounts is in the container."""
        for mount, target in self.mounts.items():
            if host.is_relative_to(mount):
                return str(PurePosixPath(target.removesuffix(":ro")) / host.relative_to(mount).as_posix())
        raise BuildError(f"{host} is in no mount of the container")


class HostRunner:
    """Runs steps on this machine, with the build tools installed on it."""

    def run(self, step: Step, capture: bool = False, secrets: Mapping[str, str] | None = None) -> str:
        env = {**step.env, **(secrets or {})}
        return run(step.argv, cwd=Path(step.workdir), env=env, input=step.input, capture=capture, title=" ".join(step.argv)).stdout or ""

    def path(self, host: Path) -> str:
        return str(host)


class Runner(Protocol):
    def run(self, step: Step, capture: bool = False, secrets: Mapping[str, str] | None = None) -> str: ...

    def path(self, host: Path) -> str: ...


def retry_once(action: Callable[[], object]) -> None:
    # Gradle's downloads fail now and then
    try:
        action()
    except BuildError as e:
        warn(f"{e}; running it again")
        action()
