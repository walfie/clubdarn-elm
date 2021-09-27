require('../css/style.css');

var Elm = require('../elm/ClubDarn/Main.elm');
var Ports = require('./ports')
var Util = require('./util');

// TODO: Make this configurable
var apiBaseUrl = 'https://clubdarn.aikats.us/api';

var ClubDarn = (function() {
  // Initialize settings from localStorage
  var settingsJson = localStorage.getItem(Ports.settingsKey);
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

