// module Control.Promise

exports.promise = function (f) {
  return function () {
    return new Promise(function (success, error) {
      var succF = function (s) { return function() { return success(s); } };
      var failF = function (s) { return function() { return error(s); } };

      // This indicates the aff was wrong?
      try { f(succF)(failF)(); }
      catch (e) {
        error(e);
      }
    });
  };
};

exports.thenImpl = function(promise) {
  return function(errCB) {
    return function(succCB) {
      return function() {
        promise.then(succCB, errCB);
      };
    };
  };
};

exports.isError = function(x) { return x instanceof Error; }

exports.safeToString = function(x) {
  return x === null ? "null" : typeof x === "undefined" ? "undefined" : x.toString();
}
