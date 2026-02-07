// output/Data.Boolean/index.js
var otherwise = true;

// output/Node.WorkerBees/foreign.js
import workerThreads from "worker_threads";
function mainImpl(ctor) {
  return function() {
    if (workerThreads.isMainThread) {
      throw new Error("Worker running on main thread.");
    }
    ctor({
      exit: function() {
        process.exit();
      },
      receive: function(cb) {
        return function() {
          workerThreads.parentPort.on("message", function(value) {
            cb(value)();
          });
        };
      },
      reply: function(value) {
        return function() {
          workerThreads.parentPort.postMessage(value);
        };
      },
      threadId: workerThreads.threadId,
      workerData: workerThreads.workerData
    })();
  };
}
// output/Data.Bounded/foreign.js
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;
// output/Effect.Uncurried/foreign.js
var runEffectFn2 = function runEffectFn22(fn) {
  return function(a) {
    return function(b) {
      return function() {
        return fn(a, b);
      };
    };
  };
};
var runEffectFn3 = function runEffectFn32(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function() {
          return fn(a, b, c);
        };
      };
    };
  };
};
// output/Node.WorkerBees/index.js
var unwrap2 = function(v) {
  return v;
};
var makeAsMain = function() {
  return mainImpl;
};

// output/Yoga.Om.WorkerBees.Atomics/foreign.js
var loadImpl = (arr, idx) => Atomics.load(arr, idx);
var addImpl = (arr, idx, val) => Atomics.add(arr, idx, val);

// output/Yoga.Om.WorkerBees.Atomics/index.js
var load = /* @__PURE__ */ runEffectFn2(loadImpl);
var add2 = /* @__PURE__ */ runEffectFn3(addImpl);

// output/Yoga.Om.WorkerBees.SharedInt/index.js
var read2 = function(v) {
  return load(v)(0);
};
var fromSendable = unwrap2;
var add3 = function(v) {
  return function(val) {
    return add2(v)(0)(val);
  };
};

// output/Test.Workers.CounterWorker/index.js
var fibonacci = function(n) {
  if (n <= 1) {
    return n;
  }
  if (otherwise) {
    return fibonacci(n - 1 | 0) + fibonacci(n - 2 | 0) | 0;
  }
  throw new Error("Failed pattern match at Test.Workers.CounterWorker (line 13, column 1 - line 13, column 24): " + [n.constructor.name]);
};
var worker = function(ctx) {
  var counter = fromSendable(ctx.workerData);
  return ctx.receive(function(v) {
    var result = fibonacci(v.n);
    return function __do() {
      add3(counter)(1)();
      var count = read2(counter)();
      return ctx.reply({
        result,
        count
      })();
    };
  });
};
var main = /* @__PURE__ */ makeAsMain()(worker);
export {
  worker,
  main,
  fibonacci
};
