'use strict';
var renderer = require('./lib/renderer');
var {filter} = require('./lib/transformer');

hexo.on('ready', function() {
    console.log("ready");
});
// If use `hexo server`, the hexo will first enter `.on(exit)` event then start the server.
hexo.on('exit', function(err) {
    console.log("exit");
});

hexo.extend.filter.register('before_post_render', filter);
hexo.extend.renderer.register('org', 'html', renderer.bind(hexo), false);
const { pattern, process } = require('./lib/processor')(hexo)
hexo.extend.processor.register(pattern, process);
