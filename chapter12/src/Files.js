"use strict"

exports.readFileImpl = (path, onSuccess, onFailure) =>
  () => {
    require('fs').readFile(path, { encoding: 'utf-8' }, (error, data) => {
      if (error) {
        onFailure(error.code)();
      }
      else {
        onSuccess(data)();
      }
    });
  };

exports.writeFileImpl = (path, data, onSuccess, onFailure) =>
  () => {
    require('fs').writeFile(path, data, { encoding: 'utf-8' }, (error, data) => {
      if (error) {
        onFailure(error.code)();
      }
      else {
        onSuccess()();
      }
    });
  };
