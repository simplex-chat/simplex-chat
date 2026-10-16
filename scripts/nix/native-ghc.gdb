# Keep the compiler's normal address layout and literal argv. Its shell wrapper
# execs the actual ELF compiler; do not follow utility subprocesses instead.
set confirm off
set pagination off
set startup-with-shell off
set disable-randomization off
set follow-exec-mode same
set follow-fork-mode parent
set detach-on-fork on
set auto-load off
set debuginfod enabled off
set print elements 32
handle SIGPIPE SIGALRM SIGVTALRM SIGPROF nostop noprint pass
run

# Normal compiler exit codes (including nonzero) remain unchanged. A stopped
# fault has no exit code: capture bounded diagnostics and fail the build.
if $_isvoid($_exitcode)
  echo === GHC fault: C stack, registers, PC and mappings ===\n
  thread apply all bt 80
  info registers
  info symbol $pc
  info sharedlibrary
  info proc mappings
  x/32i $pc-32
  quit 1
end
quit $_exitcode
