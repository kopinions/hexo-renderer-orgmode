'use strict';
var renderer = require('./lib/renderer');
var transformer = require('./lib/transformer');

hexo.on('ready', function() {
    console.log("ready");
});

// If use `hexo server`, the hexo will first enter `.on(exit)` event then start the server.
hexo.on('exit', function(err) {
    console.log("exit");
});

hexo.extend.filter.register('before_post_render', transformer);
hexo.extend.renderer.register('org', 'html', renderer.bind(hexo), false);
