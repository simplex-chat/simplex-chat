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
      "xcode_settings": { "GCC_ENABLE_CPP_EXCEPTIONS": "YES" },
      "msvs_settings": { "VCCLCompilerTool": { "ExceptionHandling": 1 } },
      "defines": [ "NAPI_DISABLE_CPP_EXCEPTIONS" ],
      "conditions": [
        ["OS=='linux'", {
          "libraries": [ "-ldl" ]
        }]
      ]
    }
  ]
}
