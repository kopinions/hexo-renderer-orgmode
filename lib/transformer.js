'use strict';

var moment = require('moment');

var transformers = {
    TITLE: {
	type: 'string',
	to: 'title',
	'default': 'missing title'
    },
    ALIAS: {
	type: 'array',
	to: 'alias',
	'default': []
    },
    DATE: {
	type: 'date',
	to: 'date',
	'default': moment()
    },
    UPDATED: {
	type: 'date',
	to: 'updated',
	'default': moment()
    },
    TAGS: {
	type: 'array',
	to: 'tags',
	fn: (tags) => tags.split(',').map((item) => item.trim()),
	'default': []
    },
    CATEGORIES: {
	type: 'array',
	to: 'categories',
	fn: (categories) => categories.split(',').map((item) => item.trim()),
	'default': []
    },
    LAYOUT: {
	type: 'string',
	to: 'layout',
	'default': 'post'
    },
    COMMENTS: {
	type: 'boolean',
	to: 'comments',
	'default': 'false'
    },
    PERMALINK: {
	type: 'string',
	to: 'slug',
	'default': ''
    },
    DESCRIPTION: {
	type: 'string',
	to: 'description',
	'default': 'missing description'
    },
    EXCERPT: {
	type: 'string',
	to: 'excerpt',
	'default': 'missing excerpt'
    }
    
};


function option(str) {
    var kv = str.match(/#\+([a-zA-Z]*):(.*)/);
    if (kv && kv.length == 3) {
	return {
	    key: kv[1],
	    value: kv[2].trim()
	}
    }
    
    return {
    }
}

function filter(hexo, data) {
    if (!/.*\.org/.test(data.source || data.path)) {
	// skip if is not a org file
	return data;
    }
    const filepath = data.source || data.path;
    var raw = data.raw;
    const extracted = meta(filepath, raw)
    const prototype = hexo.model('Post').Document.prototype;
    const setTagsFunc = prototype.setTags;
    const setCategoriesFunc = prototype.setCategories;
    for(const p in extracted) {
	if (p == 'tags') {
	    setTagsFunc.call(data, extracted[p]);
	} else if (p== 'categories') {
	    setCategoriesFunc.call(data, extracted[p]);
	} else {
	    data[p] = extracted[p];
	}
    }
}

function meta(filepath, content) {
    var r = content.match(/#\+[a-zA-Z]*:.*\n/g);
    const metas_in_file = r?
	  r.reduce((acc, item)=>{
	      const o = option(item);
	      if (o) {
		  acc[o.key]=o.value;
	      }
	      return acc;}, {}): {};
    return Object.entries(transformers).reduce((acc, item) => {
	const [key, {type, to, fn}] = item;
	const meta_value = metas_in_file[key]? metas_in_file[key]: item[1]['default'];
	switch(type) {
	case "string":
	    if (fn) {
		acc[to] = fn(meta_value)
	    } else {
		acc[to] = meta_value;
	    }
	    break;
	case "date":
	    if (typeof meta_value === 'string') {
		acc[to] = moment(Date.parse(meta_value.replace(/[^0-9:-]/g, ' ')));
	    } else {
		acc[to] = meta_value;
	    }
	    break;
	case "array":
	    if (meta_value.constructor === Array) {
		acc[to] = meta_value;
		break;
	    }
	    
	    if (fn) {
		acc[to] = fn(meta_value)
	    } else if (acc[to]) {
		acc[to] = acc[to].concat(meta_value);
	    } else {
		acc[to] = [meta_value];
	    }
	    break;
	case "boolean":
	    acc[to] = meta_value == 'no'? false : true;
	    break;
	}
	return acc;
    }, {});
}

module.exports = {
    meta: meta,
    filter: hexo => data => {
	return filter(hexo, data);
    }
};
