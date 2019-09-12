"use strict"

exports.setTimeoutImpl = (ms, onSuccess) =>
  () => {
    setTimeout(() => onSuccess(), ms);
  };
