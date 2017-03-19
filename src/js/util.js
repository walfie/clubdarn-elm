module.exports = {
  loadScript: loadScript
};

var d = document;

function loadScript(url, cb) {
  var s = d.createElement('script');
  s.async = 1;
  s.onload = cb;
  s.src = url;
  d.body.appendChild(s);
};

