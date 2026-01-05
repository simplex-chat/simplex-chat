{
  "targets": [
    {
      "target_name": "simplex_shim",
      "sources": [ "cpp/simplex.cc" ],
      "libraries": [
        "-L<(module_root_dir)/libs",
        "-lsimplex"
      ],
      "conditions": [
        ["OS=='mac'", {
          "xcode_settings": {
            "OTHER_LDFLAGS": [
              "-Wl,-rpath,@loader_path/../../libs"
            ]
          }
        }],
        ["OS=='linux'", {
          "ldflags": [
            "-Wl,-rpath,'$$ORIGIN'/../../libs"
          ]
        }]
      ]
    }
  ]
}
