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
	type: 'string',
	fn: (tags) => this.setTags(tags.split(',').map((item) => item.trim()))
    },
    CATEGORIES: {
	type: 'string'
	fn: (categories) => this.setTags(categories.split(',').map((item) => item.trim()))
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
    }
};


function option(str) {
    var kv = str.match(/#\+([a-zA-Z]*):(.*)/g);
    if (kv && kv.length == 3) {
	return {
	    key: kv[1],
	    value: kv[2]
	}
    }
    
    return {
    }
}

function transform(data) {
    if (!/.*\.org/.test(data.source)) {
	// skip if is not a org file
	return data;
    }
    var r = data.content.match(/#\+[a-zA-Z]*:.*\n/g);
    if (r) {
	for (var i = 0; i < r.length; i++) {
	    var o = option(r[i]);
	    var op = transformers[o.key];
	    if (op) {
		switch(op.type) {
		case "string":
		    if (op.fn) {
			op.fn.bind(data).call(o.value);
		    } else {
			data[op.to] = o.value;
		    }
		    break;
		case "date":
		    data[op.to] = moment(Date.parse(o.value.replace(/[^0-9:-]/g, ' ')));
		    break;
		case "array":
		    if (data[op.to]) {
			data[op.to] = data[op.to];
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
}

module.exports = transform;
