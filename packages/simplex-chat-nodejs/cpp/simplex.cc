#include <napi.h>
#include <sstream>
#include <string>
#include <functional>
#include <cstdlib>
#include "simplex.h"

namespace simplex
{

  using namespace Napi;

  void haskell_init()
  {
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

  class GenericAsyncWorker : public AsyncWorker
  {
    public:
      GenericAsyncWorker(Function& callback, std::function<void(GenericAsyncWorker*)> execute_lambda)
        : AsyncWorker(callback), execute_lambda_(std::move(execute_lambda)) {}

      void Execute() override {
        execute_lambda_(this);
      }

      void OnOK() override {
        HandleScope scope(Env());
        std::vector<napi_value> args = { Env().Null() };
        auto res = GetResult(Env());
        args.insert(args.end(), res.begin(), res.end());
        Callback().Call(args);
      }

      void OnError(const Error& e) override {
        HandleScope scope(Env());
        Callback().Call({e.Value(), Env().Undefined()});
      }

      virtual std::vector<napi_value> GetResult(Napi::Env env) override {
        return { String::New(env, result_) };
      }

      void SetResult(std::string result) {
        result_ = std::move(result);
      }

      void SetWorkerError(const std::string& msg) {
        SetError(msg);
      }

    protected:
      std::string result_;

    private:
      std::function<void(GenericAsyncWorker*)> execute_lambda_;
  };

  class JsonAsyncWorker : public GenericAsyncWorker
  {
    public:
      JsonAsyncWorker(Function& callback, std::function<void(GenericAsyncWorker*)> execute_lambda)
        : GenericAsyncWorker(callback, std::move(execute_lambda)) {}

      void OnOK() override {
        HandleScope scope(Env());
        Value parsed = ParseJson(Env(), result_);
        if (Env().IsExceptionPending()) {
          Error exception = Env().GetAndClearPendingException();
          Callback().Call({exception.Value(), Env().Undefined()});
          return;
        }
        Callback().Call({Env().Null(), parsed});
      }
  };

  class ResultUnwrapAsyncWorker : public GenericAsyncWorker
  {
    public:
      ResultUnwrapAsyncWorker(Function& callback, std::function<void(GenericAsyncWorker*)> execute_lambda)
        : GenericAsyncWorker(callback, std::move(execute_lambda)) {}

      void OnOK() override {
        HandleScope scope(Env());
        Value parsed = ParseJson(Env(), result_);
        if (Env().IsExceptionPending()) {
          Error exception = Env().GetAndClearPendingException();
          Callback().Call({exception.Value(), Env().Undefined()});
          return;
        }
        Object parsed_obj = parsed.As<Object>();
        Value type_val = parsed_obj.Get("type");
        if (!type_val.IsString()) {
          Error err = Error::New(Env(), "Invalid response type");
          Callback().Call({err.Value(), Env().Undefined()});
          return;
        }
        std::string type = type_val.As<String>().Utf8Value();
        if (type == "error") {
          Value err_val = parsed_obj.Get("writeError");
          std::string err_msg = err_val.IsString() ? err_val.As<String>().Utf8Value() : "Unknown error";
          Error err = Error::New(Env(), err_msg);
          Callback().Call({err.Value(), Env().Undefined()});
        } else {
          Value cryptoArgs = parsed_obj.Get("cryptoArgs");
          if (cryptoArgs.IsUndefined()) {
            Error err = Error::New(Env(), "Missing cryptoArgs");
            Callback().Call({err.Value(), Env().Undefined()});
            return;
          }
          Callback().Call({Env().Null(), cryptoArgs});
        }
      }
  };

  class MigrateAsyncWorker : public GenericAsyncWorker
  {
    public:
      MigrateAsyncWorker(Function& callback, std::function<void(GenericAsyncWorker*)> execute_lambda)
        : GenericAsyncWorker(callback, execute_lambda) {}

      void SetCtrl(uintptr_t ctrl) {
        ctrl_ = ctrl;
      }

      void OnOK() override {
        HandleScope scope(Env());
        Value parsed = ParseJson(Env(), result_);
        if (Env().IsExceptionPending()) {
          Error exception = Env().GetAndClearPendingException();
          Callback().Call({exception.Value(), Env().Undefined()});
          return;
        }
        Object parsed_obj = parsed.As<Object>();
        Value type_val = parsed_obj.Get("type");
        if (type_val.IsString() && type_val.As<String>().Utf8Value() == "ok") {
          Callback().Call({Env().Null(), BigInt::New(Env(), static_cast<uint64_t>(ctrl_))});
        } else {
          Error err = Error::New(Env(), "Database or migration error (see dbMigrationError property)");
          err.Set("dbMigrationError", parsed_obj);
          Callback().Call({err.Value(), Env().Undefined()});
        }
      }

    private:
      uintptr_t ctrl_ = 0;
  };

  class BinaryAsyncWorker : public AsyncWorker
  {
    public:
      BinaryAsyncWorker(Function& callback, std::function<void(BinaryAsyncWorker*)> execute_lambda)
        : AsyncWorker(callback), execute_lambda_(std::move(execute_lambda)) {}

      void Execute() override {
        execute_lambda_(this);
      }

      void OnOK() override {
        HandleScope scope(Env());
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

    public:
      char* original_buf = nullptr;
      size_t binary_len = 0;

    private:
      std::function<void(BinaryAsyncWorker*)> execute_lambda_;
  };

  class WriteAsyncWorker : public ResultUnwrapAsyncWorker
  {
    public:
      WriteAsyncWorker(Function& callback, std::function<void(GenericAsyncWorker*)> execute_lambda, ArrayBuffer ab)
        : ResultUnwrapAsyncWorker(callback, std::move(execute_lambda)), ab_ref(Reference<ArrayBuffer>::New(ab, 1)) {}

      Reference<ArrayBuffer> ab_ref;
  };

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

  Value ChatMigrateInit(const CallbackInfo& args)
  {
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

    auto execute_lambda = [path, key, confirm](GenericAsyncWorker* worker) {
      chat_ctrl ctrl = nullptr;
      char* c_res = chat_migrate_init(path.c_str(), key.c_str(), confirm.c_str(), &ctrl);
      if (c_res == nullptr) {
        worker->SetWorkerError("chat_migrate_init failed");
        return;
      }
      std::string res = c_res;
      free(c_res);
      auto mworker = static_cast<MigrateAsyncWorker*>(worker);
      mworker->SetCtrl(reinterpret_cast<uintptr_t>(ctrl));
      worker->SetResult(res);
    };

    MigrateAsyncWorker* worker = new MigrateAsyncWorker(cb, std::move(execute_lambda));
    worker->Queue();

    return promise;
  }

  class CloseStoreAsyncWorker : public GenericAsyncWorker
  {
    public:
      CloseStoreAsyncWorker(Function& callback, std::function<void(GenericAsyncWorker*)> execute_lambda)
        : GenericAsyncWorker(callback, std::move(execute_lambda)) {}

      void OnOK() override {
        HandleScope scope(Env());
        if (result_.empty()) {
          Callback().Call({Env().Null(), Env().Undefined()});
        } else {
          Error err = Error::New(Env(), result_);
          Callback().Call({err.Value(), Env().Undefined()});
        }
      }
  };

  Value ChatCloseStore(const CallbackInfo& args)
  {
    Env env = args.Env();
    if (args.Length() < 1 || !args[0].IsBigInt()) {
      TypeError::New(env, "Expected bigint (ctrl)").ThrowAsJavaScriptException();
      return env.Undefined();
    }

    bool lossless;
    chat_ctrl ctrl = reinterpret_cast<chat_ctrl>(args[0].As<BigInt>().Int64Value(&lossless));

    if (!lossless) {
      TypeError::New(env, "BigInt too large for ctrl").ThrowAsJavaScriptException();
      return env.Undefined();
    }

    Function cb;
    Promise promise = CreatePromiseAndCallback(env, cb);

    auto execute_lambda = [ctrl](GenericAsyncWorker* worker) {
      char* c_res = chat_close_store(ctrl);
      if (c_res == nullptr) {
        worker->SetWorkerError("chat_close_store failed");
        return;
      }
      std::string res = c_res;
      free(c_res);
      worker->SetResult(res);
    };

    CloseStoreAsyncWorker* worker = new CloseStoreAsyncWorker(cb, std::move(execute_lambda));
    worker->Queue();

    return promise;
  }

  Value ChatSendCmd(const CallbackInfo& args)
  {
    Env env = args.Env();
    if (args.Length() < 2 || !args[0].IsBigInt() || !args[1].IsString()) {
      TypeError::New(env, "Expected bigint (ctrl) and string (cmd)").ThrowAsJavaScriptException();
      return env.Undefined();
    }

    bool lossless;
    chat_ctrl ctrl = reinterpret_cast<chat_ctrl>(args[0].As<BigInt>().Int64Value(&lossless));
    if (!lossless) {
      TypeError::New(env, "BigInt too large for ctrl").ThrowAsJavaScriptException();
      return env.Undefined();
    }
    std::string cmd = args[1].As<String>().Utf8Value();

    Function cb;
    Promise promise = CreatePromiseAndCallback(env, cb);

    auto execute_lambda = [ctrl, cmd](GenericAsyncWorker* worker) {
      char* c_res = chat_send_cmd(ctrl, cmd.c_str());
      if (c_res == nullptr) {
        worker->SetWorkerError("chat_send_cmd failed");
        return;
      }
      std::string res = c_res;
      free(c_res);
      if (res.empty()) {
        worker->SetWorkerError("chat_send_cmd failed");
        return;
      }
      worker->SetResult(res);
    };

    JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_lambda));
    worker->Queue();

    return promise;
  }

  Value ChatRecvMsgWait(const CallbackInfo& args)
  {
    Env env = args.Env();
    if (args.Length() < 2 || !args[0].IsBigInt() || !args[1].IsNumber()) {
      TypeError::New(env, "Expected bigint (ctrl), number (wait)").ThrowAsJavaScriptException();
      return env.Undefined();
    }
  
    bool lossless;
    chat_ctrl ctrl = reinterpret_cast<chat_ctrl>(args[0].As<BigInt>().Int64Value(&lossless));
    if (!lossless) {
      TypeError::New(env, "BigInt too large for ctrl").ThrowAsJavaScriptException();
      return env.Undefined();
    }
    int wait = static_cast<int>(args[1].As<Number>().Int32Value());

    Function cb;
    Promise promise = CreatePromiseAndCallback(env, cb);

    auto execute_lambda = [ctrl, wait](GenericAsyncWorker* worker) {
      char* c_res = chat_recv_msg_wait(ctrl, wait);
      if (c_res == nullptr) {
        worker->SetWorkerError("chat_recv_msg_wait failed");
        return;
      }
      std::string res = c_res;
      free(c_res);
      if (res.empty()) {
        worker->SetWorkerError("chat_recv_msg_wait failed");
        return;
      }
      worker->SetResult(res);
    };

    JsonAsyncWorker* worker = new JsonAsyncWorker(cb, std::move(execute_lambda));
    worker->Queue();

    return promise;
  }

  Value ChatWriteFile(const CallbackInfo& args)
  {
    Env env = args.Env();
    if (args.Length() < 3 || !args[0].IsBigInt() || !args[1].IsString() || !args[2].IsArrayBuffer()) {
      TypeError::New(env, "Expected bigint (ctrl), string (path), ArrayBuffer").ThrowAsJavaScriptException();
      return env.Undefined();
    }

    bool lossless;
    chat_ctrl ctrl = reinterpret_cast<chat_ctrl>(args[0].As<BigInt>().Int64Value(&lossless));
    if (!lossless) {
      TypeError::New(env, "BigInt too large for ctrl").ThrowAsJavaScriptException();
      return env.Undefined();
    }
    std::string path = args[1].As<String>().Utf8Value();
    ArrayBuffer ab = args[2].As<ArrayBuffer>();
    char* data = static_cast<char*>(ab.Data());
    int len = static_cast<int>(ab.ByteLength());

    Function cb;
    Promise promise = CreatePromiseAndCallback(env, cb);

    auto execute_lambda = [ctrl, path, data, len](GenericAsyncWorker* worker) {
      char* c_res = chat_write_file(ctrl, path.c_str(), data, len);
      if (c_res == nullptr) {
        worker->SetWorkerError("chat_write_file failed");
        return;
      }
      std::string res = c_res;
      free(c_res);
      if (res.empty()) {
        worker->SetWorkerError("chat_write_file failed");
        return;
      }
      worker->SetResult(res);
    };

    WriteAsyncWorker* worker = new WriteAsyncWorker(cb, std::move(execute_lambda), ab);
    worker->Queue();

    return promise;
  }

  Value ChatReadFile(const CallbackInfo& args)
  {
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

    auto execute_lambda = [path, key, nonce](BinaryAsyncWorker* worker) {
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

    BinaryAsyncWorker* worker = new BinaryAsyncWorker(cb, std::move(execute_lambda));
    worker->Queue();

    return promise;
  }

  Value ChatEncryptFile(const CallbackInfo& args)
  {
    Env env = args.Env();
    if (args.Length() < 3 || !args[0].IsBigInt() || !args[1].IsString() || !args[2].IsString()) {
      TypeError::New(env, "Expected bigint (ctrl), two strings (fromPath, toPath)").ThrowAsJavaScriptException();
      return env.Undefined();
    }

    bool lossless;
    chat_ctrl ctrl = reinterpret_cast<chat_ctrl>(args[0].As<BigInt>().Int64Value(&lossless));
    if (!lossless) {
      TypeError::New(env, "BigInt too large for ctrl").ThrowAsJavaScriptException();
      return env.Undefined();
    }
    std::string fromPath = args[1].As<String>().Utf8Value();
    std::string toPath = args[2].As<String>().Utf8Value();

    Function cb;
    Promise promise = CreatePromiseAndCallback(env, cb);

    auto execute_lambda = [ctrl, fromPath, toPath](GenericAsyncWorker* worker) {
      char* c_res = chat_encrypt_file(ctrl, fromPath.c_str(), toPath.c_str());
      if (c_res == nullptr) {
        worker->SetWorkerError("chat_encrypt_file failed");
        return;
      }
      std::string res = c_res;
      free(c_res);
      if (res.empty()) {
        worker->SetWorkerError("chat_encrypt_file failed");
        return;
      }
      worker->SetResult(res);
    };

    ResultUnwrapAsyncWorker* worker = new ResultUnwrapAsyncWorker(cb, std::move(execute_lambda));
    worker->Queue();

    return promise;
  }

  Value ChatDecryptFile(const CallbackInfo& args)
  {
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

    auto execute_lambda = [fromPath, key, nonce, toPath](GenericAsyncWorker* worker) {
      char* c_res = chat_decrypt_file(fromPath.c_str(), key.c_str(), nonce.c_str(), toPath.c_str());
      std::string res = c_res ? c_res : "";
      free(c_res);
      if (!res.empty()) {
        worker->SetWorkerError(res);
        return;
      }
      worker->SetResult("ok");
    };

    GenericAsyncWorker* worker = new GenericAsyncWorker(cb, std::move(execute_lambda));
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
