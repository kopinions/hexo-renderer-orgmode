const Promise = require('bluebird');
const { extname, join } = require('path');
const { slugize, Permalink } = require('hexo-util');
var {meta} = require('./transformer');
const {isMatch, isTmpFile, isHiddenFile, isExcludedFile} = require('./utils');

let permalink;
function parseFilename(config, path) {
    config = config.substring(0, config.length - extname(config).length);
    path = path.substring(0, path.length - extname(path).length);

    if (!permalink || permalink.rule !== config) {
	permalink = new Permalink(config, {
	    segments: {
		year: /(\d{4})/,
		month: /(\d{2})/,
		day: /(\d{2})/,
		i_month: /(\d{1,2})/,
		i_day: /(\d{1,2})/
	    }
	});
    }

    const data = permalink.parse(path);

    if (data) {
	return data;
    }

    return {
	title: slugize(path)
    };
}

const preservedKeys = {
    title: true,
    year: true,
    month: true,
    day: true,
    i_month: true,
    i_day: true
};

const postDir = '_posts/';
const draftDir = '_drafts/';


module.exports = ctx => {
    const processPost = file => {
	var Post = ctx.model('Post')
	const doc = Post.findOne({source: file.path});
	const { path } = file.params;
	const { config } = ctx;
	const { timezone: timezoneCfg, use_date_for_updated } = config;
	let categories, tags;

	if (file.type === 'skip' && doc) {
	    return;
	}

	if (file.type === 'delete') {
	    if (doc) {
		return doc.remove();
	    }

	    return;
	}

	return Promise.all([
	    file.stat(),
	    file.read()
	]).spread((stats, content) => {
	    const data = meta(content);
	    const info = parseFilename(config.new_post_name, path);
	    const keys = Object.keys(info);

	    data.source = file.path;
	    data.raw = content;
	    data.slug = info.title;

	    if (file.params.published) {
		if (!Object.prototype.hasOwnProperty.call(data, 'published')) data.published = true;
	    } else {
		data.published = false;
	    }

	    for (let i = 0, len = keys.length; i < len; i++) {
		const key = keys[i];
		if (!preservedKeys[key]) data[key] = info[key];
	    }
	    
	    if (doc) {
		return doc.replace(data);
	    }
	    
	    return Post.insert(data);
	});

    };
    
    const processAsset = file => {
	const PostAsset = ctx.model('PostAsset');
	const Post = ctx.model('Post');
	const id = file.source.substring(ctx.base_dir.length).replace(/\\/g, '/');
	const doc = PostAsset.findById(id);

	if (file.type === 'delete') {
	    if (doc) {
		return doc.remove();
	    }

	    return;
	}

	// TODO: Better post searching
	const post = Post.toArray().find(post => file.source.startsWith(post.asset_dir));

	if (post != null && post.published) {
	    return PostAsset.save({
		_id: id,
		slug: file.source.substring(post.asset_dir.length),
		post: post._id,
		modified: file.type !== 'skip',
		renderable: file.params.renderable
	    });
	}

	if (doc) {
	    return doc.remove();
	}
	
    }
    
    return {
	pattern: path => {
	    if (isTmpFile(path)) return;

	    if (!path.endsWith('.org')) return;

	    let result;

	    if (path.startsWith(postDir)) {
		result = {
		    published: true,
		    path: path.substring(postDir.length)
		};
	    } else if (path.startsWith(draftDir)) {
		result = {
		    published: false,
		    path: path.substring(draftDir.length)
		};
	    }

	    if (!result || isHiddenFile(result.path)) {
		return;
	    }

	    result.renderable = ctx.render.isRenderable(path) && !isMatch(path, ctx.config.skip_render);
	    return result;
	},
	process: file => {
	    if (file.params.renderable) {
		return processPost(file);
	    } else {
		return postAsset(file);
	    }
	}
    };
    
}
