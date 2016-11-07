exports.helloPromise = Promise.resolve("Hello");
exports.goodbyePromise = Promise.reject("Goodbye");
exports.errPromise = Promise.reject(new Error('err'));
