'use strict';

const {spawn} = require('child_process');
const fs = require('fs');
const tmp = require('tmp');
const path = require('path');


function emacs_process(hexo, data, callback)
{
    const config = hexo.config;
    
    const emacs = config.org.emacs;


    const bootstrap = path.join(process.cwd(), "node_modules", "hexo-renderer-orgmode", "lisp", "bootstrap.el" );

    var exported = tmp.fileSync();
    
    var lisp = `(progn
(load "${bootstrap}")
(hexo-render (list :file "${data.path}"
              :output-file "${exported.name}"))
  (kill-emacs))`;

    if (config.org.debug) {
        console.log("emacs: ", emacs);
        console.log("lisp: \n", lisp);
    }

    var proc = spawn(emacs, ['--batch', '--eval', lisp, '2>&1']);

    proc.stdout.on('data', (d) => {
	console.log(d.toString());
    });

    proc.stderr.on('data', (d) => {
	console.log(d.toString());
    });
    
    proc.on('exit', (code, signal) => {
        if (config.org.debug) {
            console.log("DONE: ", data.path);
	}
	
        callback(fs.readFileSync(exported.name, 'utf8'));
    });
}

module.exports = {
    process: function(hexo, data, callback) {
        return emacs_process(hexo, data, callback);
    }
};
