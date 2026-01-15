{
  "targets": [
    {
      "target_name": "simplex",
      "sources": [ "cpp/simplex.cc" ],
      "include_dirs": [
        "<!@(node -p \"require('node-addon-api').include\")"
      ],
      "dependencies": [
        "<!(node -p \"require('node-addon-api').gyp\")"
      ],
      "libraries": [
        "-L<(module_root_dir)/libs",
        "-lsimplex"
      ],
      "cflags!": [ "-fno-exceptions" ],
      "cflags_cc!": [ "-fno-exceptions" ],
      "defines": [ "NAPI_DISABLE_CPP_EXCEPTIONS" ],
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
