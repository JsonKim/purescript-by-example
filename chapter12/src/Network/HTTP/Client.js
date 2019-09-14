"use strict";

exports.getImpl = (uri, done, fail) => () => {
  require('request')(uri, (err, _, body) => {
    err ? fail(err)() : done(body)();
  });
};
