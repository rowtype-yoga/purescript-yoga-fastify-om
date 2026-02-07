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
// output/Node.WorkerBees/index.js
var makeAsMain = function() {
  return mainImpl;
};

// output/Test.Workers.FibonacciWorker/index.js
var fibonacci = function(n) {
  if (n <= 1) {
    return n;
  }
  if (otherwise) {
    return fibonacci(n - 1 | 0) + fibonacci(n - 2 | 0) | 0;
  }
  throw new Error("Failed pattern match at Test.Workers.FibonacciWorker (line 15, column 1 - line 15, column 24): " + [n.constructor.name]);
};
var worker = function(ctx) {
  return ctx.receive(function(v) {
    var result = fibonacci(v.n);
    return ctx.reply({
      result,
      thread: 0
    });
  });
};
var main = /* @__PURE__ */ makeAsMain()(worker);
export {
  worker,
  main,
  fibonacci
};
