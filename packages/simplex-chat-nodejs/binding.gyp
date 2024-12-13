{
  "targets": [
    {
      "target_name": "addon",
      "sources": [ "cpp/simplex.cc" ],
      "libraries": [
        "-Wl,-rpath,libs",
        "-L<(module_root_dir)/build/Release/",
        "-L<(module_root_dir)/libs",
        "-lsimplex"
      ]
    }
  ]
}