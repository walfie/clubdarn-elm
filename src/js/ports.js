var Util = require('./util');

module.exports = {
  selectFile: selectFile,
  saveSettings: saveSettings
};

function saveSettings(settings) {
  window.localStorage.setItem('settings', JSON.stringify(settings));
}

var jsmediatagsUrl = 'https://rawgit.com/aadsm/jsmediatags/master/dist/jsmediatags.min.js';

// Only load jsmediatags script if the user selects files
function selectFile(port, elementId) {
  function handle() {
    handleFileSelection(port, window.jsmediatags, elementId);
  }

  window.jsmediatags ? handle() : Util.loadScript(jsmediatagsUrl, handle);
}

// Use jsmediatags to read the titles/artists from the ID3 tags
function handleFileSelection(port, jsmediatags, elementId) {
  var input = document.getElementById(elementId);
  var totalItems = input.files.length;

  function handledFile(maybeMetadata) {
    port.send({
      result: maybeMetadata,
      total: totalItems
    });
  }

  for (var i = 0; i < totalItems; i++) {
    var file = input.files[i];
    jsmediatags.read(file, {
      onSuccess: function(result) {
        var tags = result.tags || {};
        var metadata = {
          title: tags.title || '',
          artist: tags.artist || ''
        };

        handledFile(metadata);
      },
      onError: function(e) {
        console.error(e);
        handledFile(null);
      }
    });
  }

  if (totalItems <= 0) {
    totalItems = 1;
    handledFile(null);
  }
}

