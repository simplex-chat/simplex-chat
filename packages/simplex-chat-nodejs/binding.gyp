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
      "cflags!": [ "-fno-exceptions" ],
      "cflags_cc!": [ "-fno-exceptions" ],
      "defines": [ "NAPI_DISABLE_CPP_EXCEPTIONS" ],
      "conditions": [
        ["OS=='mac'", {
          "libraries": [
            "-L<(module_root_dir)/libs",
            "-lsimplex"
          ],
          "xcode_settings": {
            "OTHER_LDFLAGS": [
              "-Wl,-rpath,@loader_path/../../libs"
            ]
          }
        }],
        ["OS=='linux'", {
          "libraries": [
            "-L<(module_root_dir)/libs",
            "-lsimplex"
          ],
          "ldflags": [
            "-Wl,-rpath,'$$ORIGIN'/../../libs"
          ]
        }],
        ["OS=='win'", {
          "libraries": [
            "<(module_root_dir)/libs/libsimplex.lib"
          ],
          "copies": [{
            "destination": "<(PRODUCT_DIR)",
            "files": [
              "<(module_root_dir)/libs/*"
            ]
          }]
        }]
      ]
    }
  ]
}
