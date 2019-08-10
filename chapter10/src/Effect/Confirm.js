"use strict"

exports.confirm = function(msg) {
  return function() {
    return window.confirm(msg);
  }
}
