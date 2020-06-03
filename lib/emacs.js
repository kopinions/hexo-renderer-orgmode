'use strict';

const {spawn} = require('child_process');
const fs = require('fs');
const tmp = require('tmp');
const path = require('path');
const { v4: uuidv4 } = require('uuid');
const server_name = uuidv4()

const debug = (message) => {
    if (hexo.config.org.debug) {
	console.log(message)
    }
}

function emacs_server_start(hexo) {
    const config = hexo.config;
    
    const emacs = config.org.emacs;
    const bootstrap = path.join(process.cwd(), "node_modules", "hexo-renderer-orgmode", "lisp", "bootstrap.el" );

    var lisp = `(progn
(load "${bootstrap}"))`;

    var proc = spawn(emacs, ['-Q', '--daemon='+server_name, "--eval", lisp], {stdio:'inherit'});
    proc.on('exit', function(code) {
        try {
	    if (code !=0 ) {
		console.log("Unable to start emacs server")
		hexo.exit(code);
		return;
	    }
	    debug("emacs start success");
        }
        catch(e) {
        }
    });

    return proc;
}

function emacs_server_stop(hexo, count) {
    return new Promise((resolve,reject) => {
	var lefted = (typeof count !== 'undefined') ?  count : 30*60
        const config = hexo.config;
	const emacsclient = config.org.emacsclient;
        
        var proc = spawn(emacsclient, ['-s', server_name, '-e', '(kill-emacs)']);

        proc.on('exit', function(code) {
            if (code != 0) {
		if (lefted <= 0) {
		    console.error("Can not waiting the emacs server to stop, please kill it manually")
		    reject(code);
		    return;
		}
                setTimeout(() => {
                    if (config.org.debug)
                        console.log("Wait for emacs daemon exit!!");
                    resolve(emacs_server_stop(hexo, lefted -1));
                }, 1000);
                return;
            }
            resolve();
        });
    });
}

function emacs_server_wait(hexo, count) {
    return new Promise((resolve,reject) => {
	var lefted = (typeof count !== 'undefined') ?  count : 30*60
	const config = hexo.config;
	const emacsclient = config.org.emacsclient;

        var proc = spawn(emacsclient, ['-s', server_name, '-e', '(message "ping")']);

        proc.on('exit', function(code) {
            if (code != 0) {
		if (lefted <= 0) {
		    console.error("Can not waiting the emacs server to started, please check the emacs server")
		    reject(code);
		    return;
		}
		
                setTimeout(() => {
                    if (config.org.debug) {
                        console.log("Waiting for emacs...");
		    }
                    resolve(emacs_server_wait(hexo, lefted-1));
                }, 1000);
                return;
            }
            resolve();
        });
    });
}

function emacs_process(hexo, data, count) {
    return new Promise((resolve, reject) => {
	var lefted = (typeof count !== 'undefined') ?  count : 5*60
	
	const config = hexo.config;
	
	const emacsclient = config.org.emacsclient;
	
	var exported = tmp.fileSync();
	
	var lisp = `(progn
(hexo-render (list :file "${data.path}"
              :output-file "${exported.name}")))`;

	if (config.org.debug) {
            console.log("lisp: \n", lisp);
	}

	var proc = spawn(emacsclient, ['-nq', '-s', server_name, '-e', lisp]);

	var stdout = '', stderr = '';
	
	proc.stdout.on('data', data => {
	    stdout = `${stdout} ${data}`;
	})

	proc.stderr.on('data', data => {
	    stderr = `${stderr} ${data}`;
	})

	proc.on('exit', (code, signal) => {
	    if (code != 0) {
		if (lefted <= 0) {
		    console.error("Can not waiting the emacs server to render, please check the emacs server or the file: ", data.path)
		    reject(code);
		    return;
		}
		
		setTimeout(() => {
                    if (config.org.debug) {
                        console.log("Waiting for emacs...");
		    }
                    resolve(emacs_process(hexo, data, lefted-1));
                }, 1000);
		return;
	    }

	    if (config.org.debug) {
		console.log("DONE: ", data.path);
	    }
            resolve(fs.readFileSync(exported.name, 'utf8'));
	});
    })
    
}

module.exports = {
    server: {
	start: emacs_server_start,
	stop: emacs_server_stop
    },
    process: function(hexo, data) {
	return emacs_server_wait(hexo)
	    .then(ignore => {
		return emacs_process(hexo, data);
	    });
    }
};
