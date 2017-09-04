require('isomorphic-fetch');

exports.fetchImpl = function(options) {
  // console.log("options", options);
  return function () {
    return fetch(options.uri, options)
  };
};

exports.textImpl = function (response) {
  // console.log("text", response);
  return function() {
    return response.text();
  };
}

exports.jsonImpl = function (response) {
  // console.log("json", response);
  return function() {
    return response.json();
  };
}
