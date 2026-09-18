#include <napi.h>
#include <sstream>
#include <string>
#include <functional>
#include <cstdlib>
#include <climits>
#include <memory>
#include <thread>
#include "simplex.h"

namespace simplex {

using namespace Napi;

void haskell_init() {
#ifdef _WIN32
  // non-moving GC is broken on windows with GHC 9.4-9.6.3
  int argc = 5;
  const char *argv[] = {
      "simplex",
      "+RTS",  // requires `hs_init_with_rtsopts`
      "-A64m", // chunk size for new allocations
      "-H64m", // initial heap size
      "--install-signal-handlers=no",
      nullptr};
#else
  int argc = 6;
  const char *argv[] = {
      "simplex",
      "+RTS",  // requires `hs_init_with_rtsopts`
      "-A64m", // chunk size for new allocations
      "-H64m", // initial heap size
      "-xn",   // non-moving GC
      "--install-signal-handlers=no",
      nullptr};
#endif
  char **pargv = const_cast<char **>(argv);
  hs_init_with_rtsopts(&argc, &pargv);
}

class ResultAsyncWorker : public AsyncWorker {
 public:
  using ExecuteFn = std::function<void(ResultAsyncWorker*)>;
  using ResultProcessor = std::function<void(ResultAsyncWorker*, Napi::Env)>;

  ResultAsyncWorker(Function& callback, ExecuteFn execute_fn, ResultProcessor result_processor = nullptr)
      : AsyncWorker(callback), execute_fn_(std::move(execute_fn)), result_processor_(std::move(result_processor)) {}

  void Execute() override {
    execute_fn_(this);
  }

  void OnOK() override {
    HandleScope scope(Env());
    if (result_processor_) {
      result_processor_(this, Env());
    } else {
      Callback().Call({Env().Null(), String::New(Env(), result_)});
    }
  }

  void OnError(const Error& e) override {
    HandleScope scope(Env());
    Callback().Call({e.Value(), Env().Undefined()});
  }

  void SetResult(std::string result) {
    result_ = std::move(result);
  }

  void SetWorkerError(const std::string& msg) {
    SetError(msg);
  }

  const std::string& GetStringResult() const {
    return result_;
  }

  void SetCtrl(uintptr_t ctrl) {
    ctrl_ = ctrl;
  }

  uintptr_t GetCtrl() const {
    return ctrl_;
  }

  // the worker thread reads this object's memory until the worker completes
  void KeepAlive(Object obj) {
    keep_alive_ = Persistent(obj);
  }

 protected:
  std::string result_;
  uintptr_t ctrl_ = 0;

 private:
  ExecuteFn execute_fn_;
  ResultProcessor result_processor_;
  ObjectReference keep_alive_;
};

class BinaryAsyncWorker : public AsyncWorker {
 public:
  using ExecuteFn = std::function<void(BinaryAsyncWorker*)>;

  BinaryAsyncWorker(Function& callback, ExecuteFn execute_fn)
      : AsyncWorker(callback), execute_fn_(std::move(execute_fn)) {}

  ~BinaryAsyncWorker() {
    free(original_buf);
  }

  void Execute() override {
    execute_fn_(this);
  }

  void OnOK() override {
    HandleScope scope(Env());
    char* buf = original_buf;
    original_buf = nullptr;
    if (binary_len == 0) {
      free(buf);
      Callback().Call({Env().Null(), Buffer<char>::New(Env(), 0)});
      return;
    }
    // Copies when the runtime forbids external buffers (Electron); the finalizer then runs immediately.
    Buffer<char> buffer = Buffer<char>::NewOrCopy(Env(), buf + 5, binary_len, [](Napi::Env, char*, char* orig) { free(orig); }, buf);
    Callback().Call({Env().Null(), buffer});
  }

  void OnError(const Error& e) override {
    HandleScope scope(Env());
    Callback().Call({e.Value(), Env().Undefined()});
  }

  void SetWorkerError(const std::string& msg) {
    SetError(msg);
  }

  char* original_buf = nullptr;
  size_t binary_len = 0;

 private:
  ExecuteFn execute_fn_;
};

// Helper for converting chat_ctrl pointer to BigInt
Napi::BigInt ToChatCtrlBigInt(Napi::Env env, uintptr_t ctrl) {
  return Napi::BigInt::New(env, static_cast<uint64_t>(ctrl));
}

// Helper for converting BigInt to chat_ctrl pointer
chat_ctrl FromChatCtrlBigInt(const Napi::Value& value) {
  Napi::Env env = value.Env();
  if (!value.IsBigInt()) {
    Napi::TypeError::New(env, "Expected BigInt for ctrl").ThrowAsJavaScriptException();
    return nullptr;
  }
  Napi::BigInt big = value.As<Napi::BigInt>();
  bool lossless;
  uint64_t val = big.Uint64Value(&lossless);
  if (!lossless) {
    Napi::TypeError::New(env, "BigInt too large for ctrl").ThrowAsJavaScriptException();
    return nullptr;
  }
  return reinterpret_cast<chat_ctrl>(val);
}

// Helper for handling common C result patterns (no empty check)
void HandleCResult(ResultAsyncWorker* worker, char* c_res, const std::string& func_name) {
  if (c_res == nullptr) {
    worker->SetWorkerError(func_name + " failed");
    return;
  }
  std::string res = c_res;
  free(c_res);
  worker->SetResult(res);
}

Napi::Promise CreatePromiseAndCallback(Env env, Function& cb_out) {
  Promise::Deferred deferred = Promise::Deferred::New(env);
  cb_out = Function::New(env, [deferred](const CallbackInfo& args) {
    if (!args[0].IsNull() && !args[0].IsUndefined()) {
      deferred.Reject(args[0]);
    } else {
      deferred.Resolve(args[1]);
    }
  });
  return deferred.Promise();
}

// Common result processors
ResultAsyncWorker::ResultProcessor MigrateResultProcessor() {
  return [](ResultAsyncWorker* worker, Napi::Env env) {
    Napi::Array arr = Napi::Array::New(env, 2);
    arr.Set(0u, ToChatCtrlBigInt(env, worker->GetCtrl()));
    arr.Set(1u, Napi::String::New(env, worker->GetStringResult()));
    worker->Callback().Call({env.Null(), arr});
  };
}

// Refactored functions using common patterns

Value ChatMigrateInit(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 3 || !args[0].IsString() || !args[1].IsString() || !args[2].IsString()) {
    TypeError::New(env, "Expected three string arguments").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  std::string path = args[0].As<String>().Utf8Value();
  std::string key = args[1].As<String>().Utf8Value();
  std::string confirm = args[2].As<String>().Utf8Value();

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [path, key, confirm](ResultAsyncWorker* worker) {
    chat_ctrl ctrl = nullptr;
    char* c_res = chat_migrate_init(path.c_str(), key.c_str(), confirm.c_str(), &ctrl);
    worker->SetCtrl(reinterpret_cast<uintptr_t>(ctrl));
    HandleCResult(worker, c_res, "chat_migrate_init");
  };

  ResultAsyncWorker* worker = new ResultAsyncWorker(cb, std::move(execute_fn), MigrateResultProcessor());
  worker->Queue();

  return promise;
}

Value ChatMigrateInitQueue(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 4 || !args[0].IsString() || !args[1].IsString() || !args[2].IsString() || !args[3].IsNumber()) {
    TypeError::New(env, "Expected three string arguments and number").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  std::string path = args[0].As<String>().Utf8Value();
  std::string key = args[1].As<String>().Utf8Value();
  std::string confirm = args[2].As<String>().Utf8Value();
  Number queue_size_arg = args[3].As<Number>();
  int queue_size = queue_size_arg.Int32Value();
  if (static_cast<double>(queue_size) != queue_size_arg.DoubleValue()) {
    RangeError::New(env, "Expected 32-bit integer queue size").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [path, key, confirm, queue_size](ResultAsyncWorker* worker) {
    chat_ctrl ctrl = nullptr;
    char* c_res = chat_migrate_init_queue(path.c_str(), key.c_str(), confirm.c_str(), queue_size, &ctrl);
    worker->SetCtrl(reinterpret_cast<uintptr_t>(ctrl));
    HandleCResult(worker, c_res, "chat_migrate_init_queue");
  };

  ResultAsyncWorker* worker = new ResultAsyncWorker(cb, std::move(execute_fn), MigrateResultProcessor());
  worker->Queue();

  return promise;
}

Value ChatCloseStore(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 1 || !args[0].IsBigInt()) {
    TypeError::New(env, "Expected bigint (ctrl)").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  chat_ctrl ctrl = FromChatCtrlBigInt(args[0]);

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [ctrl](ResultAsyncWorker* worker) {
    char* c_res = chat_close_store(ctrl);
    HandleCResult(worker, c_res, "chat_close_store");
  };

  ResultAsyncWorker* worker = new ResultAsyncWorker(cb, std::move(execute_fn));
  worker->Queue();

  return promise;
}

Value ChatSendCmd(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 2 || !args[0].IsBigInt() || !args[1].IsString()) {
    TypeError::New(env, "Expected bigint (ctrl) and string (cmd)").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  chat_ctrl ctrl = FromChatCtrlBigInt(args[0]);
  std::string cmd = args[1].As<String>().Utf8Value();

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [ctrl, cmd](ResultAsyncWorker* worker) {
    char* c_res = chat_send_cmd(ctrl, cmd.c_str());
    HandleCResult(worker, c_res, "chat_send_cmd");
  };

  ResultAsyncWorker* worker = new ResultAsyncWorker(cb, std::move(execute_fn));
  worker->Queue();

  return promise;
}

Value ChatRecvMsgWait(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 2 || !args[0].IsBigInt() || !args[1].IsNumber()) {
    TypeError::New(env, "Expected bigint (ctrl), number (wait)").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  chat_ctrl ctrl = FromChatCtrlBigInt(args[0]);
  int wait = static_cast<int>(args[1].As<Number>().Int32Value());

  auto deferred = std::make_shared<Promise::Deferred>(Promise::Deferred::New(env));
  auto tsfn = ThreadSafeFunction::New(env, Function::New(env, [](const CallbackInfo&) {}), "chat_recv_msg_wait", 0, 1);
  // A blocking receive would hold a libuv pool thread for up to `wait`, stalling fs, dns and crypto.
  std::thread([tsfn, deferred, ctrl, wait]() mutable {
    char* c_res = chat_recv_msg_wait(ctrl, wait);
    tsfn.BlockingCall([deferred, c_res](Napi::Env env, Function) {
      if (c_res == nullptr) {
        deferred->Reject(Error::New(env, "chat_recv_msg_wait failed").Value());
      } else {
        deferred->Resolve(String::New(env, c_res));
        free(c_res);
      }
    });
    tsfn.Release();
  }).detach();

  return deferred->Promise();
}

Value ChatWriteFile(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 3 || !args[0].IsBigInt() || !args[1].IsString() || !(args[2].IsArrayBuffer() || args[2].IsTypedArray())) {
    TypeError::New(env, "Expected bigint (ctrl), string (path), ArrayBuffer or Uint8Array").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  chat_ctrl ctrl = FromChatCtrlBigInt(args[0]);
  std::string path = args[1].As<String>().Utf8Value();
  char* data;
  size_t len;
  if (args[2].IsArrayBuffer()) {
    ArrayBuffer ab = args[2].As<ArrayBuffer>();
    data = static_cast<char*>(ab.Data());
    len = ab.ByteLength();
  } else {
    TypedArray view = args[2].As<TypedArray>();
    data = static_cast<char*>(view.ArrayBuffer().Data()) + view.ByteOffset();
    len = view.ByteLength();
  }
  if (len > static_cast<size_t>(INT_MAX)) {
    RangeError::New(env, "Buffer is too large").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [ctrl, path, data, len](ResultAsyncWorker* worker) {
    char* c_res = chat_write_file(ctrl, path.c_str(), data, static_cast<int>(len));
    HandleCResult(worker, c_res, "chat_write_file");
  };

  ResultAsyncWorker* worker = new ResultAsyncWorker(cb, std::move(execute_fn));
  worker->KeepAlive(args[2].As<Object>());
  worker->Queue();

  return promise;
}

Value ChatReadFile(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 3 || !args[0].IsString() || !args[1].IsString() || !args[2].IsString()) {
    TypeError::New(env, "Expected three strings (path, key, nonce)").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  std::string path = args[0].As<String>().Utf8Value();
  std::string key = args[1].As<String>().Utf8Value();
  std::string nonce = args[2].As<String>().Utf8Value();

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [path, key, nonce](BinaryAsyncWorker* worker) {
    char* buf = chat_read_file(path.c_str(), key.c_str(), nonce.c_str());
    if (buf == nullptr) {
      worker->SetWorkerError("chat_read_file failed");
      return;
    }
    char status = buf[0];
    if (status == 1) {
      std::string err = buf + 1;
      free(buf);
      worker->SetWorkerError(err);
      return;
    } else if (status == 0) {
      uint32_t len = *(uint32_t*)(buf + 1);
      worker->original_buf = buf;
      worker->binary_len = len;
    } else {
      free(buf);
      worker->SetWorkerError("Unexpected status from chat_read_file");
      return;
    }
  };

  BinaryAsyncWorker* worker = new BinaryAsyncWorker(cb, std::move(execute_fn));
  worker->Queue();

  return promise;
}

Value ChatEncryptFile(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 3 || !args[0].IsBigInt() || !args[1].IsString() || !args[2].IsString()) {
    TypeError::New(env, "Expected bigint (ctrl), two strings (fromPath, toPath)").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  chat_ctrl ctrl = FromChatCtrlBigInt(args[0]);
  std::string fromPath = args[1].As<String>().Utf8Value();
  std::string toPath = args[2].As<String>().Utf8Value();

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [ctrl, fromPath, toPath](ResultAsyncWorker* worker) {
    char* c_res = chat_encrypt_file(ctrl, fromPath.c_str(), toPath.c_str());
    HandleCResult(worker, c_res, "chat_encrypt_file");
  };

  ResultAsyncWorker* worker = new ResultAsyncWorker(cb, std::move(execute_fn));
  worker->Queue();

  return promise;
}

Value ChatDecryptFile(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 4 || !args[0].IsString() || !args[1].IsString() || !args[2].IsString() || !args[3].IsString()) {
    TypeError::New(env, "Expected four strings (fromPath, key, nonce, toPath)").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  std::string fromPath = args[0].As<String>().Utf8Value();
  std::string key = args[1].As<String>().Utf8Value();
  std::string nonce = args[2].As<String>().Utf8Value();
  std::string toPath = args[3].As<String>().Utf8Value();

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [fromPath, key, nonce, toPath](ResultAsyncWorker* worker) {
    char* c_res = chat_decrypt_file(fromPath.c_str(), key.c_str(), nonce.c_str(), toPath.c_str());
    HandleCResult(worker, c_res, "chat_decrypt_file");
  };

  ResultAsyncWorker* worker = new ResultAsyncWorker(cb, std::move(execute_fn));
  worker->Queue();

  return promise;
}

Object Init(Env env, Object exports) {
  haskell_init();
  exports.Set("chat_migrate_init", Function::New(env, ChatMigrateInit));
  exports.Set("chat_migrate_init_queue", Function::New(env, ChatMigrateInitQueue));
  exports.Set("chat_close_store", Function::New(env, ChatCloseStore));
  exports.Set("chat_send_cmd", Function::New(env, ChatSendCmd));
  exports.Set("chat_recv_msg_wait", Function::New(env, ChatRecvMsgWait));
  exports.Set("chat_write_file", Function::New(env, ChatWriteFile));
  exports.Set("chat_read_file", Function::New(env, ChatReadFile));
  exports.Set("chat_encrypt_file", Function::New(env, ChatEncryptFile));
  exports.Set("chat_decrypt_file", Function::New(env, ChatDecryptFile));
  return exports;
}

NODE_API_MODULE(simplex, Init)

}
