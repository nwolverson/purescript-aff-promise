exports.helloPromise = Promise.resolve("Hello");
exports.goodbyePromise = Promise.reject("Goodbye");
exports.errPromise = Promise.reject(new Error('err'));
exports.errDescendantPromise = Promise.reject(new RangeError('Range error'));
exports.toStringableErrPromise = Promise.reject({ toString: () => "toString err" });
exports.nullErrPromise = Promise.reject(null);
exports.undefinedErrPromise = Promise.reject(undefined);
exports.customErrPromise = Promise.reject({ code: "err" });
