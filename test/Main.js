exports.helloPromise = Promise.resolve("Hello");
exports.goodbyePromise = Promise.reject("Goodbye");
exports.errPromise = Promise.reject(new Error('err'));
exports.customErrPromise = Promise.reject({ code: "err" });
