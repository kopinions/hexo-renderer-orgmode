var emacs = require('./emacs');
const cheerio = require('cheerio');
const {highlight} = require('hexo-util');

function render_html(html, config) {
    return new Promise((reslove, reject) => {

        // for use 'htmlize' to syntax highlight code block
        if (config.org.htmlize)
            reslove(html);

        // for use 'highlight.js' to syntax highlight code block (default)
        config.highlight = config.highlight || {};
        var $ = cheerio.load(html, {
            ignoreWhitespace: false,
            xmlMode: false,
            lowerCaseTags: false,
            decodeEntities: true
        });

        $('pre.src').each(function() {
            var text; // await highlight code text
            var lang = 'unknown';
            var code = $(this);
            var class_str = code.attr('class');
            if (class_str.startsWith('src src-')) {
                lang = class_str.substring('src src-'.length);
            }
	    
	    var locs = code.attr('data-locs')
	    
            var firstLine = parseInt(locs);

            var gutter;
            if (config.org.line_number)
                gutter = true;
            else {
                gutter = (firstLine == 0) ? false : true;
            }

            // USE hexo.utils to render highlight.js code block
            $(this).replaceWith( highlight(code.text(), {
                gutter: gutter,
                autoDetect: config.highlight.auto_detect,
                firstLine: firstLine,
                lang: lang
            }));
        });

        //reslove(get_content($));
        reslove($.html());
    });
}

function render(data) {
    var that = this;
    return new Promise((resolve, reject) => {
	emacs.process(that, data, resolve);
    }).then((html) => {
	return render_html(html, that.config)
    })
}

module.exports = render;
