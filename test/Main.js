exports.helloPromise = Promise.resolve("Hello");
exports.goodbyePromise = Promise.reject("Goodbye");
exports.errPromise = Promise.reject(new Error('err'));
exports.errDescendantPromise = Promise.reject(new RangeError('DOM exception'));
exports.customErrPromise = Promise.reject({ code: "err" });
