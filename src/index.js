'use strict';

require('./index.html');
require('./scss/app.scss');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');
var app = Elm.Main.embed(mountNode);
// ELM PORT TO PLAY AUDIO
app.ports.playSound.subscribe(
    function(id) {
        var node = document.querySelector( "#sound" + id.toString() );
        node.play()
});