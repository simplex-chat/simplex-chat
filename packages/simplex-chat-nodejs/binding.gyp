{
  "targets": [
    {
      "target_name": "simplex_shim",
      "sources": [ "cpp/simplex.cc" ],
      "libraries": [
        "-L<(module_root_dir)/libs",
        "-lsimplex"
      ],
      "ldflags": [
        "-Wl,-rpath,'$$ORIGIN'/../../libs"
      ]
    }
  ]
}
