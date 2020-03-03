'use strict';

var moment = require('moment');

var transformers = {
    TITLE: {
	type: 'string',
	to: 'title'
    },
    ALIAS: {
	type: 'array',
	to: 'alias'
    },
    DATE: {
	type: 'date',
	to: 'date'
    },
    UPDATED: {
	type: 'date',
	to: 'updated'
    },
    TAGS: {
	type: 'array',
	to: 'tags',
	fn: (tags) => tags.split(',').map((item) => item.trim())
    },
    CATEGORIES: {
	type: 'array',
	to: 'categories',
	fn: (categories) => categories.split(',').map((item) => item.trim())
    },
    LAYOUT: {
	type: 'string',
	to: 'layout'
    },
    COMMENTS: {
	type: 'boolean',
	to: 'comments'
    },
    PERMALINK: {
	type: 'string',
	to: 'slug'
    },
    DESCRIPTION: {
	type: 'string',
	to: 'description'
    },
    EXCERPT: {
	type: 'string',
	to: 'excerpt'
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

function filter(data) {
    if (!/.*\.org/.test(data.source || data.path)) {
	// skip if is not a org file
	return data;
    }
    var raw = data.text || data.content;
    const extracted = meta(raw)
    for(const p in extracted) {
	if (p == 'tags') {
	    data.setTags(extracted[p]);
	} else if (p== 'categories') {
	    data.setCategories(extracted[p]);
	} else {
	    data[p] = extracted[p];
	    
	}
    }
}

function meta(content) {
    var data = {};
    var r = content.match(/#\+[a-zA-Z]*:.*\n/g);
    if (r) {
	for (var i = 0; i < r.length; i++) {
	    var o = option(r[i]);
	    var op = transformers[o.key];
	    if (op) {
		switch(op.type) {
		case "string":
		    if (op.fn) {
			data[op.to] = op.fn(o.value)
		    } else {
			data[op.to] = o.value;
		    }
		    break;
		case "date":
		    data[op.to] = moment(Date.parse(o.value.replace(/[^0-9:-]/g, ' ')));
		    break;
		case "array":
		    if (op.fn) {
			data[op.to] = op.fn(o.value)
		    } else if (data[op.to]) {
			data[op.to] = data[op.to].concat(o.value);
		    } else {
			data[op.to] = [o.value];
		    }
		    break;
		case "boolean":
		    data[op.to] = o.value == 'no'? false : true;
		    break;
		}
	    }
	}
    }    
    return data;
}

module.exports = {
    meta,
    filter
};
