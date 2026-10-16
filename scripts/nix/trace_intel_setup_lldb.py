"""LLDB command for the unchanged compiler-produced ghc-boot Setup binary."""

import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess
import time

import lldb


def permission_denied_before_launch(error, process):
    # Never repeat a process that LLDB launched, even if it already exited.
    denied = "this is a non-interactive debug session, cannot get permission to debug processes."
    pid = process.GetProcessID() if process.IsValid() else 0
    return error.Fail() and denied in str(error) and pid == 0


def abort_fallback(executable, argv, environment, tracer, deadline, direct=False):
    keys = ("SIMPLEX_SETUP_ABORT_DYLD_PRESENT", "SIMPLEX_SETUP_ABORT_DYLD_VALUE")
    child_environment = dict(item.split("=", 1) for item in environment)
    if any(key in child_environment for key in keys):
        raise ValueError("abort tracer private environment key already exists")
    original_dyld = child_environment.get("DYLD_INSERT_LIBRARIES")
    child_environment[keys[0]] = "0" if original_dyld is None else "1"
    if original_dyld is not None:
        child_environment[keys[1]] = original_dyld
    child_environment["DYLD_INSERT_LIBRARIES"] = str(tracer) + (
        ":" + original_dyld if original_dyld else "")
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        raise TimeoutError("Intel Setup overall diagnostic deadline expired")
    digest = hashlib.sha256(executable.read_bytes()).hexdigest()
    label = "direct-abort" if direct else "fallback"
    reason = ("explicit capture mode; no debugger launch attempted" if direct else
              "original LLDB permission denial retained")
    print("Intel Setup " + label + ": " + reason + "; "
          "diagnostic-only abort dylib inserted into unchanged Setup; "
          "constructor restores child loader environment; no core output", flush=True)
    result = subprocess.run(argv, executable=str(executable), env=child_environment,
                            timeout=remaining, check=False)
    if hashlib.sha256(executable.read_bytes()).hexdigest() != digest:
        raise RuntimeError("compiler-produced Setup bytes changed during fallback")
    code = 128 - result.returncode if result.returncode < 0 else result.returncode
    print("Intel Setup " + label + " result: " + json.dumps({
        "returncode": result.returncode, "shell_status": code,
        "executable_sha256": digest, "unchanged": True,
    }), flush=True)
    return code


def nul_entries(path):
    data = path.read_bytes()
    if not data.endswith(b"\0"):
        raise ValueError("incomplete NUL-delimited capture")
    return [os.fsdecode(item) for item in data[:-1].split(b"\0")]


def drain(process):
    for reader in (process.GetSTDOUT, process.GetSTDERR):
        while True:
            text = reader(65536)
            if not text:
                break
            print(text, end="", flush=True)


def describe_stop(process):
    stops = []
    for thread in process:
        data = [thread.GetStopReasonDataAtIndex(i)
                for i in range(thread.GetStopReasonDataCount())]
        stops.append({"thread": thread.GetThreadID(),
                      "reason": thread.GetStopReason(), "data": data,
                      "description": thread.GetStopDescription(1024)})
    print("Intel Setup stop: " + json.dumps(stops), flush=True)
    return stops


def dump_stop(target, process):
    # Use SB objects directly: a Python command's nested CLI execution context
    # can still refer to the pre-launch process even after SBTarget.Launch.
    for thread in process:
        print("Thread " + str(thread.GetThreadID()), flush=True)
        for index in range(min(thread.GetNumFrames(), 80)):
            print(str(thread.GetFrameAtIndex(index)), flush=True)
        if thread.GetNumFrames() > 80:
            print("Backtrace truncated after 80 frames", flush=True)
    stopped = next((thread for thread in process
                    if thread.GetStopReason() not in (lldb.eStopReasonNone,
                                                      lldb.eStopReasonInvalid)), None)
    if stopped is not None and stopped.GetNumFrames():
        frame = stopped.GetFrameAtIndex(0)
        for group in frame.GetRegisters():
            print("Registers: " + str(group.GetName()), flush=True)
            for register in group:
                print(str(register.GetName()) + " = " + str(register.GetValue()), flush=True)
        print("Instructions at stopped PC:", flush=True)
        for instruction in target.ReadInstructions(frame.GetPCAddress(), 12):
            print(str(instruction), flush=True)
    print("Loaded images:", flush=True)
    for index in range(target.GetNumModules()):
        module = target.GetModuleAtIndex(index)
        print(str(module.GetFileSpec()) + " UUID=" + str(module.GetUUIDString()), flush=True)


def stopped_signals(process):
    reasons = [(thread.GetStopReason(),
                thread.GetStopReasonDataAtIndex(0) if thread.GetStopReasonDataCount() else None)
               for thread in process
               if thread.GetStopReason() not in (lldb.eStopReasonNone, lldb.eStopReasonInvalid)]
    return [number for reason, number in reasons
            if reason == lldb.eStopReasonSignal and number is not None], reasons


def run(debugger, command, _result, _internal_dict):
    process = None
    code = 1
    deadline = time.monotonic() + 300
    try:
        arguments = shlex.split(command)
        if len(arguments) not in (1, 2, 3):
            raise ValueError("expected argv-capture pathname, optional abort dylib and capture mode")
        capture = Path(arguments[0])
        tracer = Path(arguments[1]) if len(arguments) >= 2 else None
        capture_mode = arguments[2] if len(arguments) == 3 else "lldb"
        if capture_mode not in ("lldb", "direct-abort"):
            raise ValueError("unknown Intel Setup capture mode: " + capture_mode)
        argv = nul_entries(capture)
        environment_file = Path(str(capture) + ".env")
        environment_bytes = environment_file.read_bytes()
        environment = nul_entries(environment_file)
        environment_file.unlink()
        target = debugger.GetSelectedTarget()
        executable = target.GetExecutable()
        executable_path = Path(executable.fullpath)
        print("Intel Setup launch: " + json.dumps({
            "executable": str(executable_path), "argv": argv,
            "capture_mode": capture_mode,
            "cwd": os.getcwd(),
            "executable_sha256": hashlib.sha256(executable_path.read_bytes()).hexdigest(),
            "environment_sha256": hashlib.sha256(environment_bytes).hexdigest(),
            "environment_entries": len(environment),
        }), flush=True)

        # Hosted Intel denies debugger attachment after assigning a PID. Use
        # an explicit ordinary launch there, rather than infer whether its
        # failed debugger launch executed any Setup code and risk repeating it.
        # LLDB only hosts this Python command; no debug process is launched.
        if capture_mode == "direct-abort":
            code = abort_fallback(executable_path, argv, environment, tracer,
                                  deadline, direct=True)
            return

        # Supplying the executable separately leaves argv[0] equal to Cabal's
        # original Setup pathname, rather than the retained sibling's name.
        launch = lldb.SBLaunchInfo(argv)
        launch.SetExecutableFile(executable, False)
        launch.SetArguments(argv, False)
        launch.SetWorkingDirectory(os.getcwd())
        launch.SetEnvironmentEntries(environment, False)
        launch.SetLaunchFlags(launch.GetLaunchFlags()
                             & ~lldb.eLaunchFlagDisableASLR
                             | lldb.eLaunchFlagStopAtEntry)
        # Synchronous launch waits past LLDB's transient initial loader stop.
        # The shell wrapper bounds this call and the debugger at 300 seconds.
        debugger.SetAsync(False)
        error = lldb.SBError()
        process = target.Launch(launch, error)
        if error.Fail() or not process.IsValid():
            print("Intel Setup LLDB launch failure: " + json.dumps({
                "error": str(error), "valid": process.IsValid(),
                "pid": process.GetProcessID() if process.IsValid() else 0,
                "state": process.GetState() if process.IsValid() else None,
            }), flush=True)
            if tracer is not None and permission_denied_before_launch(error, process):
                code = abort_fallback(executable_path, argv, environment, tracer, deadline)
                return
            raise RuntimeError("LLDB launch failed: " + str(error))

        unix_signals = process.GetUnixSignals()
        benign = set()
        for name in ("SIGPIPE", "SIGALRM", "SIGVTALRM", "SIGPROF"):
            number = unix_signals.GetSignalNumberFromName(name)
            if number < 0 or not all((unix_signals.SetShouldStop(number, False),
                                     unix_signals.SetShouldNotify(number, False),
                                     unix_signals.SetShouldSuppress(number, False))):
                raise RuntimeError("cannot configure LLDB signal policy for " + name)
            benign.add(number)
        print("Intel Setup signals pass/nostop/nonotify: SIGPIPE SIGALRM SIGVTALRM SIGPROF", flush=True)
        for name in ("SIGABRT", "SIGSEGV"):
            number = unix_signals.GetSignalNumberFromName(name)
            if number < 0 or not all((unix_signals.SetShouldStop(number, True),
                                     unix_signals.SetShouldNotify(number, True),
                                     unix_signals.SetShouldSuppress(number, False))):
                raise RuntimeError("cannot preserve LLDB fault policy for " + name)
        _, first_reasons = stopped_signals(process)
        print("Intel Setup entry stop: " + json.dumps(first_reasons), flush=True)
        initial_stop = unix_signals.GetSignalNumberFromName("SIGSTOP")
        entry_reasons = (lldb.eStopReasonExec, lldb.eStopReasonTrace,
                         lldb.eStopReasonBreakpoint, lldb.eStopReasonPlanComplete)
        # Continue the deliberate entry stop, but preserve any genuine fault
        # occurring before entry. Apply policy before GHC starts its ticker.
        if process.GetState() == lldb.eStateStopped and first_reasons and all(
                reason in entry_reasons
                or (reason == lldb.eStopReasonSignal and (number in benign or number == initial_stop))
                for reason, number in first_reasons):
            error = process.Continue()
            if error.Fail():
                raise RuntimeError("LLDB continue failed: " + str(error))
        # A benign signal may already have been queued when policy changed.
        while process.GetState() == lldb.eStateStopped:
            numbers, reasons = stopped_signals(process)
            if not numbers or len(numbers) != len(reasons) or not set(numbers) <= benign:
                break
            print("Intel Setup resuming already-stopped benign signal: " + str(numbers), flush=True)
            error = process.Continue()
            if error.Fail():
                raise RuntimeError("LLDB continue failed: " + str(error))

        drain(process)
        state = process.GetState()
        if state == lldb.eStateExited:
            code = process.GetExitStatus()
            print("Intel Setup exited: " + str(code), flush=True)
        elif state in (lldb.eStateStopped, lldb.eStateCrashed, lldb.eStateSuspended):
            stops = describe_stop(process)
            dump_stop(target, process)
            signals = [stop["data"][0] for stop in stops
                       if stop["reason"] == lldb.eStopReasonSignal and stop["data"]]
            code = 128 + int(signals[0]) if signals else 1
        else:
            raise RuntimeError("unexpected LLDB process state: " + str(state))
    except Exception as error:
        code = 1
        print("Intel Setup diagnostic error: " + str(error), flush=True)
    finally:
        if process is not None and process.IsValid():
            if process.GetState() not in (lldb.eStateExited, lldb.eStateDetached):
                process.Kill()
            drain(process)
        # LLDB's own successful batch execution must not mask a failed child.
        debugger.HandleCommand("quit " + str(code if 0 <= code <= 255 else 1))


def __lldb_init_module(debugger, _internal_dict):
    debugger.HandleCommand("command script add -f trace_intel_setup_lldb.run intel-setup-run")
