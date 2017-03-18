require('../css/style.css');

var Elm = require('../elm/ClubDarn/Main.elm');
var Ports = require('./ports')
var Util = require('./util');

// TODO: Make this configurable
var apiBaseUrl = 'http://clubdarn.aikats.us/api';

Util.loadMultipleCss([
  'https://fonts.googleapis.com/icon?family=Material+Icons',
  'https://code.getmdl.io/1.3.0/material.teal-red.min.css'
], function() { document.body.style.visibility = 'visible'; });

var ClubDarn = (function() {
  // Initialize settings from localStorage
  var settingsJson = localStorage.getItem(settingsKey);
  var settings = null;
  try { settings = JSON.parse(settingsJson) } catch (e) {};

  // Start the app
  var flags = { apiBaseUrl: apiBaseUrl, settings: settings };
  var app = Elm.ClubDarn.Main.fullscreen(flags);

  // Update localStorage when we receive new settings
  app.ports.saveSettings.subscribe(Ports.saveSettings);

  app.ports.selectFile.subscribe(function(elementId) {
    return Ports.selectFile(app.ports.fileMetadata, elementId);
  });

  return app;
}());

