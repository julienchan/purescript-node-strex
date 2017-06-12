'use strict';

exports._toBuffer = function (ofs) {
  return function (len) {
    return function (abuf) {
      return function () {
        return Buffer.from(abuf, ofs, len)
      }
    }
  }
}