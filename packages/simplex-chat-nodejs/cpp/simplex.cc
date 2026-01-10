#include <napi.h>
#include <sstream>
#include <string>
#include <functional>
#include <cstdlib>
#include "simplex.h"

namespace simplex {

using namespace Napi;

void haskell_init() {
  int argc = 5;
  const char *argv[] = {
      "simplex",
      "+RTS",  // requires `hs_init_with_rtsopts`
      "-A64m", // chunk size for new allocations
      "-H64m", // initial heap size
      "-xn",   // non-moving GC
      nullptr};
  char **pargv = const_cast<char **>(argv);
  hs_init_with_rtsopts(&argc, &pargv);
}

Napi::Value ParseJson(Env env, const std::string& json_str) {
  Object global = env.Global();
  Object json = global.Get("JSON").As<Object>();
  Function parse = json.Get("parse").As<Function>();
  return parse.Call(json, {String::New(env, json_str)});
}

class JsonAsyncWorker : public AsyncWorker {
 public:
  using ExecuteFn = std::function<void(JsonAsyncWorker*)>;
  using ResultProcessor = std::function<void(JsonAsyncWorker*, Napi::Env)>;

  JsonAsyncWorker(Function& callback, ExecuteFn execute_fn, ResultProcessor result_processor = nullptr)
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

  Value ParseAndHandleJson(Napi::Env env, bool allow_empty = false) {
    if (result_.empty() && allow_empty) return env.Undefined();
    if (result_.empty()) {
      Callback().Call({Error::New(env, "Empty result").Value(), env.Undefined()});
      return env.Undefined();
    }
    Value parsed = ParseJson(env, result_);
    if (env.IsExceptionPending()) {
      Error exception = env.GetAndClearPendingException();
      Callback().Call({exception.Value(), env.Undefined()});
      return env.Undefined();
    }
    return parsed;
  }

  void SetCtrl(uintptr_t ctrl) {
    ctrl_ = ctrl;
  }

  uintptr_t GetCtrl() const {
    return ctrl_;
  }

 protected:
  std::string result_;
  uintptr_t ctrl_ = 0;

 private:
  ExecuteFn execute_fn_;
  ResultProcessor result_processor_;
};

class BinaryAsyncWorker : public AsyncWorker {
 public:
  using ExecuteFn = std::function<void(BinaryAsyncWorker*)>;

  BinaryAsyncWorker(Function& callback, ExecuteFn execute_fn)
      : AsyncWorker(callback), execute_fn_(std::move(execute_fn)) {}

  void Execute() override {
    execute_fn_(this);
  }

  void OnOK() override {
    HandleScope scope(Env());
    if (original_buf == nullptr || binary_len == 0) {
      Callback().Call({Env().Null(), Env().Undefined()});
      return;
    }
    char* data_ptr = original_buf + 5;
    auto finalizer = [](Napi::Env env, char* finalize_data, char* orig) {
      free(orig);
    };
    Napi::Buffer<char> buffer = Napi::Buffer<char>::New(Env(), data_ptr, binary_len, finalizer, original_buf);
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

// Helper for handling common C result patterns (where empty res is error)
void HandleCResult(JsonAsyncWorker* worker, char* c_res, const std::string& func_name) {
  if (c_res == nullptr) {
    worker->SetWorkerError(func_name + " failed");
    return;
  }
  std::string res = c_res;
  free(c_res);
  if (res.empty()) {
    worker->SetWorkerError(func_name + " failed");
    return;
  }
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
JsonAsyncWorker::ResultProcessor JsonResultProcessor() {
  return [](JsonAsyncWorker* worker, Napi::Env env) {
    Value parsed = worker->ParseAndHandleJson(env);
    if (parsed.IsUndefined()) return;
    worker->Callback().Call({env.Null(), parsed});
  };
}

JsonAsyncWorker::ResultProcessor RecvResultProcessor() {
  return [](JsonAsyncWorker* worker, Napi::Env env) {
    Value parsed = worker->ParseAndHandleJson(env, true);  // Allow empty
    if (parsed.IsUndefined() && !worker->GetStringResult().empty()) return;
    worker->Callback().Call({env.Null(), parsed});
  };
}

JsonAsyncWorker::ResultProcessor WriteResultProcessor() {
  return [](JsonAsyncWorker* worker, Napi::Env env) {
    Value parsed = worker->ParseAndHandleJson(env);
    if (parsed.IsUndefined()) return;
    Object parsed_obj = parsed.As<Object>();
    Value type_val = parsed_obj.Get("type");
    if (!type_val.IsString()) {
      Error err = Error::New(env, "Invalid response type");
      worker->Callback().Call({err.Value(), env.Undefined()});
      return;
    }
    std::string type = type_val.As<String>().Utf8Value();
    if (type == "error") {
      Value err_val = parsed_obj.Get("writeError");
      std::string err_msg = err_val.IsString() ? err_val.As<String>().Utf8Value() : "Unknown error";
      Error err = Error::New(env, err_msg);
      worker->Callback().Call({err.Value(), env.Undefined()});
    } else {
      Value cryptoArgs = parsed_obj.Get("cryptoArgs");
      if (cryptoArgs.IsUndefined()) {
        Error err = Error::New(env, "Missing cryptoArgs");
        worker->Callback().Call({err.Value(), env.Undefined()});
        return;
      }
      worker->Callback().Call({env.Null(), cryptoArgs});
    }
  };
}

JsonAsyncWorker::ResultProcessor MigrateResultProcessor() {
  return [](JsonAsyncWorker* worker, Napi::Env env) {
    Value parsed = worker->ParseAndHandleJson(env);
    if (parsed.IsUndefined()) return;
    Object parsed_obj = parsed.As<Object>();
    Value type_val = parsed_obj.Get("type");
    if (type_val.IsString() && type_val.As<String>().Utf8Value() == "ok") {
      worker->Callback().Call({env.Null(), ToChatCtrlBigInt(env, worker->GetCtrl())});
    } else {
      Error err = Error::New(env, "Database or migration error (see dbMigrationError property)");
      err.Set("dbMigrationError", parsed_obj);
      worker->Callback().Call({err.Value(), env.Undefined()});
    }
  };
}

JsonAsyncWorker::ResultProcessor ErrorResultProcessor() {
  return [](JsonAsyncWorker* worker, Napi::Env env) {
    if (worker->GetStringResult().empty()) {
      worker->Callback().Call({env.Null(), env.Undefined()});
    } else {
      Error err = Error::New(env, worker->GetStringResult());
      worker->Callback().Call({err.Value(), env.Undefined()});
    }
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

  auto execute_fn = [path, key, confirm](JsonAsyncWorker* worker) {
    chat_ctrl ctrl = nullptr;
    char* c_res = chat_migrate_init(path.c_str(), key.c_str(), confirm.c_str(), &ctrl);
    if (c_res == nullptr) {
      worker->SetWorkerError("chat_migrate_init failed");
      return;
    }
    std::string res = c_res;
    free(c_res);
    worker->SetCtrl(reinterpret_cast<uintptr_t>(ctrl));
    worker->SetResult(res);
  };

  JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_fn), MigrateResultProcessor());
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

  auto execute_fn = [ctrl](JsonAsyncWorker* worker) {
    char* c_res = chat_close_store(ctrl);
    if (c_res == nullptr) {
      worker->SetWorkerError("chat_close_store failed");
      return;
    }
    std::string res = c_res;
    free(c_res);
    worker->SetResult(res);
  };

  JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_fn), ErrorResultProcessor());
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

  auto execute_fn = [ctrl, cmd](JsonAsyncWorker* worker) {
    char* c_res = chat_send_cmd(ctrl, cmd.c_str());
    HandleCResult(worker, c_res, "chat_send_cmd");
  };

  JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_fn), JsonResultProcessor());
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

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [ctrl, wait](JsonAsyncWorker* worker) {
    char* c_res = chat_recv_msg_wait(ctrl, wait);
    if (c_res == nullptr) {
      worker->SetWorkerError("chat_recv_msg_wait failed");
      return;
    }
    std::string res = c_res;
    free(c_res);
    worker->SetResult(res);
  };

  JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_fn), RecvResultProcessor());
  worker->Queue();

  return promise;
}

Value ChatWriteFile(const CallbackInfo& args) {
  Env env = args.Env();
  if (args.Length() < 3 || !args[0].IsBigInt() || !args[1].IsString() || !args[2].IsArrayBuffer()) {
    TypeError::New(env, "Expected bigint (ctrl), string (path), ArrayBuffer").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  chat_ctrl ctrl = FromChatCtrlBigInt(args[0]);
  std::string path = args[1].As<String>().Utf8Value();
  ArrayBuffer ab = args[2].As<ArrayBuffer>();
  char* data = static_cast<char*>(ab.Data());
  int len = static_cast<int>(ab.ByteLength());

  Function cb;
  Promise promise = CreatePromiseAndCallback(env, cb);

  auto execute_fn = [ctrl, path, data, len](JsonAsyncWorker* worker) {
    char* c_res = chat_write_file(ctrl, path.c_str(), data, len);
    HandleCResult(worker, c_res, "chat_write_file");
  };

  // Note: To keep ab alive, we can use a Reference, but since data is used in lambda capture by value, it's fine as long as the worker lives.
  // If needed, add Reference<ArrayBuffer> to JsonAsyncWorker for this case.
  JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_fn), WriteResultProcessor());
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

  auto execute_fn = [ctrl, fromPath, toPath](JsonAsyncWorker* worker) {
    char* c_res = chat_encrypt_file(ctrl, fromPath.c_str(), toPath.c_str());
    HandleCResult(worker, c_res, "chat_encrypt_file");
  };

  JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_fn), WriteResultProcessor());
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

  auto execute_fn = [fromPath, key, nonce, toPath](JsonAsyncWorker* worker) {
    char* c_res = chat_decrypt_file(fromPath.c_str(), key.c_str(), nonce.c_str(), toPath.c_str());
    if (c_res == nullptr) {
      worker->SetWorkerError("chat_decrypt_file failed");
      return;
    }
    std::string res = c_res;
    free(c_res);
    worker->SetResult(res);
  };

  JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_fn), ErrorResultProcessor());
  worker->Queue();

  return promise;
}

Object Init(Env env, Object exports) {
  haskell_init();
  exports.Set("chatMigrateInit", Function::New(env, ChatMigrateInit));
  exports.Set("chatCloseStore", Function::New(env, ChatCloseStore));
  exports.Set("chatSendCmd", Function::New(env, ChatSendCmd));
  exports.Set("chatRecvMsgWait", Function::New(env, ChatRecvMsgWait));
  exports.Set("chatWriteFile", Function::New(env, ChatWriteFile));
  exports.Set("chatReadFile", Function::New(env, ChatReadFile));
  exports.Set("chatEncryptFile", Function::New(env, ChatEncryptFile));
  exports.Set("chatDecryptFile", Function::New(env, ChatDecryptFile));
  return exports;
}

NODE_API_MODULE(simplex, Init)

}
