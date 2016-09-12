# purescript-aff-promise

Simple library for interop between Aff and JavaScript promises.

No typeclass instances etc are provided to use Promises directly - the intention is that your PureScript code uses Aff
internally, and is only wrapped as a Promise to present an API for consumption in JavaScript code.
