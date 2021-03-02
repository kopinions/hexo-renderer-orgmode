'use strict';
var renderer = require('./lib/renderer');
const {server} = require('./lib/emacs')
var {filter} = require('./lib/transformer');
const emacsed = () => {
    const process = require('process')
    var server_mode = process.argv.indexOf('server') > 0 || process.argv.indexOf('s') > 0 || process.argv.indexOf('--watch') > 0 || process.argv.indexOf('-w') > 0;
    if (server_mode || process.argv.indexOf('render') > 0 || process.argv.indexOf('generate') > 0 || process.argv.indexOf('g') > 0) {
	return true
    }
    return false
}
hexo.on('ready', function() {
    if (emacsed()) {
	server.start(hexo)
    }
});
// If use `hexo server`, the hexo will first enter `.on(exit)` event then start the server.
hexo.on('exit', function(err) {
    if (emacsed()) {
	server.stop(hexo)
    }

    if (err) {
	console.error(err)
    }
});

hexo.extend.filter.register('before_post_render', filter(hexo));
hexo.extend.renderer.register('org', 'html', renderer.bind(hexo), false);
const {pattern, process} = require('./lib/processor')(hexo)
hexo.extend.processor.register(pattern, process);
