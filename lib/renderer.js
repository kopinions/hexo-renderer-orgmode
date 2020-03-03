var emacs = require('./emacs');
const cheerio = require('cheerio');
const {basename} = require('path')
const { resolve } = require('url');
const {highlight, encodeURL, htmlTag} = require('hexo-util');

function render_html(html, context, data, config) {
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

	$('img').each(function() {
	    var img =$(this);
	    var src = img.attr('src');
	    const post_slug = basename(data.path, '.org')
	    const Post = context.model('Post')
	    const post = Post.findOne({slug: post_slug});

	    const PostAsset = context.model('PostAsset');
	    const asset = PostAsset.findOne({post: post._id, slug: src});
	    const attrs = {
		src: encodeURL(resolve('/', asset.path))
	    }
	    $(this).replaceWith(htmlTag('img', attrs));
	});

        reslove($.html());
    });
}

function render(data) {
    var that = this;
    
    return new Promise((resolve, reject) => {
	return emacs.process(that, data, resolve);
    }).then((html) => {
	return render_html(html, that, data, that.config)
    })
}

module.exports = render;
