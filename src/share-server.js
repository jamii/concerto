#!/usr/bin/env node

require('coffee-script');
var express = require('express'),
  sharejs = require('../../ShareJS/src'),
  hat = require('hat').rack(32, 36);

var argv = require('optimist').
  usage("Usage: $0 [-p portnum]").
  default('p', 8000).
  alias('p', 'port').
  argv;

var server = express();
server.use(express.static("../../"));

var options = {
  db: {type: 'none'},
  browserChannel: {cors: '*'},
  auth: function(client, action) {
    // This auth handler rejects any ops bound for docs starting with 'readonly'.
    if (action.name === 'submit op' && action.docName.match(/^readonly/)) {
      action.reject();
    } else {
      action.accept();
    }
  }
};

console.log("ShareJS example server v" + sharejs.version);
console.log("Options: ", options);

var port = 8005;

// Attach the sharejs REST and Socket.io interfaces to the server
sharejs.server.attach(server, options);

server.get('/?', function(req, res, next) {
  res.writeHead(302, {location: 'concerto/src/index.html'});
  res.end();
});

server.listen(port);
console.log("Demos running at http://localhost:" + port);

process.title = 'sharejs';
process.on('uncaughtException', function (err) {
  console.error('An error has occurred. Please file a ticket here: https://github.com/josephg/ShareJS/issues');
  console.error('Version ' + sharejs.version + ': ' + err.stack);
});
