'use strict';

const {spawn} = require('child_process');
const fs = require('fs');
const tmp = require('tmp');
const path = require('path');
const { v4: uuidv4 } = require('uuid');
const server_name = uuidv4()

const Logger = hexo => {
    return {
	debug: message => {
	    if (hexo.config.org.debug) {
		console.log(message);
	    }
	},
	error: message => {
	    console.error(message);
	}
    }
}


function emacs_server_start(hexo) {
    const logger = Logger(hexo);
    const config = hexo.config;
    
    const emacs = config.org.emacs;
    const bootstrap = path.join(process.cwd(), "node_modules", "hexo-renderer-orgmode", "lisp", "bootstrap.el" );

    var lisp = `
    (progn
      (setq hexo-renderer-org-cachedir      "${path.resolve(process.cwd(), config.org.cachedir || ".cache.d")}")
      (setq hexo-renderer-org-emacs-offline "${path.resolve(process.cwd(), config.org.offlinedir || ".offline.d")}")
      (setq hexo-renderer-org-theme         "${config.org.theme || ""}")
      (load "${bootstrap}"))
    `;

    logger.debug(`setup code: ${lisp}`);

    var proc = spawn(emacs, ['-Q', '--daemon='+server_name, "--eval", lisp], {stdio:'inherit'});
    proc.on('exit', function(code) {
        try {
	    if (code !=0 ) {
		logger.error("Can not start emacs server")
		hexo.exit(code);
		return;
	    }
	    logger.debug("emacs start success");
        }
        catch(e) {
        }
    });

    return proc;
}

function emacs_server_stop(hexo, count) {
    const logger = Logger(hexo)
    // wait for 1.5 min for emacs to quite
    var lefted = (typeof count !== 'undefined') ?  count : 3
    
    return new Promise((resolve,reject) => {
        if (lefted <= 0) {
	    logger.error("Can not waiting the emacs server to stop, please kill it manually")
	    reject(-1);
	    return;
        } else {
            const config = hexo.config;
	    const emacsclient = config.org.emacsclient;
            
            var proc = spawn(emacsclient, ['-s', server_name, '-e', '(kill-emacs)']);

            proc.on('exit', function(code) {
                if (code != 0) {
                    setTimeout(() => {
		        logger.debug("Wait for emacs daemon exit!!")
                        resolve(emacs_server_stop(hexo, lefted -1));
                    }, 30000);
                    return;
                }
                resolve();
            });
        }
        
    });
}

function emacs_server_wait(hexo, count) {
    const logger = Logger(hexo)

    return new Promise((resolve,reject) => {
        // wait for 5 minutes
        var lefted = (typeof count !== 'undefined') ?  count - 1 : 10
        if (lefted <= 0) {
            reject(-1);
        } else {
	    const config = hexo.config;
	    const emacsclient = config.org.emacsclient;

            var proc = spawn(emacsclient, ['-s', server_name, '-e', '(message "ping")']);

            proc.on('exit', function(code) {
                if (code != 0) {
                    setTimeout(() => {
		        logger.debug("Waiting emacs starting...")
                        resolve(emacs_server_wait(hexo, lefted-1));
                    }, 30000);
                    return;
                }
                
                resolve();
            });
        }
    });
}



function emacs_process(hexo, data, count) {
    const logger = Logger(hexo)

    // try 3 times with 10 seconds delay
    var lefted = (typeof count !== 'undefined') ?  count - 1 : 3

    return new Promise((resolve, reject) => {
        if (lefted <= 0) {
            reject(-1);
        } else {
	    const config = hexo.config;
	    
	    const emacsclient = config.org.emacsclient;
	    
	    var exported = tmp.fileSync();
	    
	    var lisp = `(hexo-render (list :file "${data.path}"
                      :output-file "${exported.name}"))`;

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
		    logger.error(`${data.path} has error happend: ${stdout}`);
		    logger.error(`${data.path} has error happend: ${stderr}`);
		    
		    setTimeout(() => {
		        logger.debug("Emac process error, try again...")
                        resolve(emacs_process(hexo, data, lefted-1));
                    }, 10000);
		    return;
	        }

	        logger.debug(`DONE: ${data.path}`)
                resolve(fs.readFileSync(exported.name, 'utf8'));
	    });
        }
    })
}

module.exports = {
    server: {
	start: emacs_server_start,
	stop: emacs_server_stop
    },
    process: function(hexo, data) {
	return emacs_server_wait(hexo, 120)
	    .then(ignore => {
		return emacs_process(hexo, data);
	    });
    }
};
