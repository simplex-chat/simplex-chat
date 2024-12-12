#include <sstream>
#include <string>

#include <node.h>
#include "simplex.h"

namespace simplex
{

  using v8::Array;
  using v8::ArrayBuffer;
  using v8::Context;
  using v8::FunctionCallbackInfo;
  using v8::Isolate;
  using v8::Local;
  using v8::Maybe;
  using v8::Number;
  using v8::Object;
  using v8::String;
  using v8::Value;

  void haskell_init()
  {
    int argc = 5;
    char *argv[] = {
        "simplex",
        "+RTS",  // requires `hs_init_with_rtsopts`
        "-A64m", // chunk size for new allocations
        "-H64m", // initial heap size
        "-xn",   // non-moving GC
        0};
    char **pargv = argv;
    hs_init_with_rtsopts(&argc, &pargv);
  }

  void ChatMigrateInit(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    std::string path(*(v8::String::Utf8Value(isolate, args[0])));
    std::string key(*(v8::String::Utf8Value(isolate, args[1])));
    std::string confirm(*(v8::String::Utf8Value(isolate, args[2])));

    long *ctrl(0);

    std::string res = chat_migrate_init(path.c_str(), key.c_str(), confirm.c_str(), &ctrl);
    std::stringstream ss;
    ss << ctrl
       << "\n"
       << res;
    // printf("ChatCtrl after init %d", cChatCtrl);
    //   v8::Local<v8::String> ctrl = v8::String::NewFromUtf8(isolate, cppChatCtrl.c_str(), v8::String::kNormalString);

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, ss.str().c_str())
            .ToLocalChecked());
  }

  void ChatCloseStore(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    long *ctrl = (long *)((long)args[0].As<Number>()->Value());

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, chat_close_store(ctrl))
            .ToLocalChecked());
  }

  void ChatSendCmd(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    long *ctrl = (long *)((long)args[0].As<Number>()->Value());
    std::string cmd(*(v8::String::Utf8Value(isolate, args[1])));

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, chat_send_cmd(ctrl, cmd.c_str()))
            .ToLocalChecked());
  }

  void ChatRecvMsgWait(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    long *ctrl = (long *)((long)args[0].As<Number>()->Value());
    long wait = ((long)args[1].As<Number>()->Value());

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, chat_recv_msg_wait(ctrl, wait))
            .ToLocalChecked());
  }

  void ChatWriteFile(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    long *ctrl = (long *)((long)args[0].As<Number>()->Value());
    std::string path(*(v8::String::Utf8Value(isolate, args[1])));

    Local<v8::ArrayBuffer> arr = args[2].As<ArrayBuffer>();
    char *buffer = (char *)arr->Data();
    int len(arr->ByteLength());
    // std::array address(*arr);

    // v8::Array a = *buffer;
    // std::string data(*((isolate, args[1])));

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, chat_write_file(ctrl, path.c_str(), buffer, len))
            .ToLocalChecked());
  }

  void ChatReadFile(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    long *ctrl = (long *)((long)args[0].As<Number>()->Value());
    std::string path(*(v8::String::Utf8Value(isolate, args[1])));

    Local<v8::ArrayBuffer> arr = args[2].As<ArrayBuffer>();
    char *buffer = (char *)arr->Data();
    int len(arr->ByteLength());

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, chat_write_file(ctrl, path.c_str(), buffer, len))
            .ToLocalChecked());
  }

  void ChatEncryptFile(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    long *ctrl = (long *)((long)args[0].As<Number>()->Value());
    std::string fromPath(*(v8::String::Utf8Value(isolate, args[1])));
    std::string toPath(*(v8::String::Utf8Value(isolate, args[2])));

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, chat_encrypt_file(ctrl, fromPath.c_str(), toPath.c_str()))
            .ToLocalChecked());
  }

  void ChatDecryptFile(const FunctionCallbackInfo<Value> &args)
  {
    Isolate *isolate = args.GetIsolate();
    Local<Context> context = isolate->GetCurrentContext();

    std::string fromPath(*(v8::String::Utf8Value(isolate, args[0])));
    std::string key(*(v8::String::Utf8Value(isolate, args[1])));
    std::string nonce(*(v8::String::Utf8Value(isolate, args[2])));
    std::string toPath(*(v8::String::Utf8Value(isolate, args[3])));

    args.GetReturnValue().Set(
        String::NewFromUtf8(
            isolate, chat_decrypt_file(fromPath.c_str(), key.c_str(), nonce.c_str(), toPath.c_str()))
            .ToLocalChecked());
  }

  void Initialize(Local<Object> exports)
  {
    haskell_init();
    NODE_SET_METHOD(exports, "chat_migrate_init", ChatMigrateInit);
    NODE_SET_METHOD(exports, "chat_close_store", ChatCloseStore);
    NODE_SET_METHOD(exports, "chat_send_cmd", ChatSendCmd);
    NODE_SET_METHOD(exports, "chat_recv_msg_wait", ChatRecvMsgWait);
    NODE_SET_METHOD(exports, "chat_write_file", ChatWriteFile);
    NODE_SET_METHOD(exports, "chat_read_file", ChatReadFile);
    NODE_SET_METHOD(exports, "chat_encrypt_file", ChatEncryptFile);
    NODE_SET_METHOD(exports, "chat_decrypt_file", ChatDecryptFile);
  }

  NODE_MODULE(NODE_GYP_MODULE_NAME, Initialize)

} // namespace simplex