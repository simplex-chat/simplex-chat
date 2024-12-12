{
  "targets": [
    {
      "target_name": "addon",
      "sources": [ "cpp/simplex.cc" ],
      "libraries": [
        "-L<(module_root_dir)/libs",
        "-lHSsimplex-chat-6.2.0.5-inplace-ghc9.6.3",
        "-L<(module_root_dir)/build/Release/"
      ]
    }
  ]
}