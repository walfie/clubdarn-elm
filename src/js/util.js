module.exports = {
  loadScript: loadScript,
  loadCss: loadCss,
  loadMultipleCss: loadMultipleCss
};

var d = document;

function loadScript(url, cb) {
  var s = d.createElement('script');
  s.async = 1;
  s.onload = cb;
  s.src = url;
  d.body.appendChild(s);
};

function loadCss(url, cb) {
  var l = d.createElement('link');
  l.onload = function() { l.media = 'all'; cb(); };
  l.rel = 'stylesheet';
  l.media = 'none';
  l.href = url;
  d.body.appendChild(l);

  return l;
};

function loadMultipleCss(urls, cb) {
  var nLoaded = 0;
  urls.forEach(function(url) {
    loadCss(url, function() { if (++nLoaded == urls.length) cb(); });
  });
}

