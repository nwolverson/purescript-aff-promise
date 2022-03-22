export var helloPromise = Promise.resolve("Hello");
export var goodbyePromise = Promise.reject("Goodbye");
export var errPromise = Promise.reject(new Error('err'));
export var customErrPromise = Promise.reject({ code: "err" });
