(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList === 'function' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done(elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done(elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done(elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? elm$http$Http$GoodStatus_ : elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return elm$core$Dict$empty;
	}

	var headers = elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? elm$core$Maybe$Just(event.total) : elm$core$Maybe$Nothing
		}))));
	});
}



// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}var author$project$Main$ChangedUrl = function (a) {
	return {$: 'ChangedUrl', a: a};
};
var author$project$Main$ClickedLink = function (a) {
	return {$: 'ClickedLink', a: a};
};
var author$project$Main$NavMsg = function (a) {
	return {$: 'NavMsg', a: a};
};
var author$project$Main$PageCtrl = function (a) {
	return {$: 'PageCtrl', a: a};
};
var author$project$Main$Redirect = function (a) {
	return {$: 'Redirect', a: a};
};
var author$project$Main$About = function (a) {
	return {$: 'About', a: a};
};
var author$project$Main$GotAboutMsg = function (a) {
	return {$: 'GotAboutMsg', a: a};
};
var author$project$Main$GotHomeMsg = function (a) {
	return {$: 'GotHomeMsg', a: a};
};
var author$project$Main$GotLiverMsg = function (a) {
	return {$: 'GotLiverMsg', a: a};
};
var author$project$Main$GotSnapshotMsg = function (a) {
	return {$: 'GotSnapshotMsg', a: a};
};
var author$project$Main$GotTimelineMsg = function (a) {
	return {$: 'GotTimelineMsg', a: a};
};
var author$project$Main$Home = function (a) {
	return {$: 'Home', a: a};
};
var author$project$Main$Liver = function (a) {
	return {$: 'Liver', a: a};
};
var author$project$Main$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var author$project$Main$Snapshot = function (a) {
	return {$: 'Snapshot', a: a};
};
var author$project$Main$Timeline = function (a) {
	return {$: 'Timeline', a: a};
};
var author$project$Main$toSession = function (model) {
	switch (model.$) {
		case 'NotFound':
			var session = model.a;
			return session;
		case 'Redirect':
			var session = model.a;
			return session;
		case 'Home':
			var subModel = model.a;
			return subModel.session;
		case 'About':
			var subModel = model.a;
			return subModel.session;
		case 'Snapshot':
			var subModel = model.a;
			return subModel.session;
		case 'Timeline':
			var subModel = model.a;
			return subModel.session;
		default:
			var subModel = model.a;
			return subModel.session;
	}
};
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$core$Platform$Cmd$map = _Platform_map;
var author$project$Main$updateWith = F4(
	function (toModel, toMsg, model, _n0) {
		var subModel = _n0.a;
		var subCmd = _n0.b;
		return _Utils_Tuple2(
			toModel(subModel),
			A2(elm$core$Platform$Cmd$map, toMsg, subCmd));
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Page$About$init = function (session) {
	return _Utils_Tuple2(
		{counter: 0, session: session},
		elm$core$Platform$Cmd$none);
};
var author$project$Page$Home$Model = function (session) {
	return {session: session};
};
var author$project$Page$Home$init = function (session) {
	return _Utils_Tuple2(
		author$project$Page$Home$Model(session),
		elm$core$Platform$Cmd$none);
};
var author$project$Page$Liver$Model = F3(
	function (session, data, mliver) {
		return {data: data, mliver: mliver, session: session};
	});
var author$project$Nijisanji$UpdateInfo = F3(
	function (twitterName, updateTime, tweetId) {
		return {tweetId: tweetId, twitterName: twitterName, updateTime: updateTime};
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$map3 = _Json_map3;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Nijisanji$decodeUpdateInfo = A4(
	elm$json$Json$Decode$map3,
	author$project$Nijisanji$UpdateInfo,
	A2(elm$json$Json$Decode$field, 'twitterName', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'updateTime', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'tweetId', elm$json$Json$Decode$string));
var author$project$Page$Liver$GotData = function (a) {
	return {$: 'GotData', a: a};
};
var author$project$Util$apiUrl = 'https://name-update-2434.herokuapp.com/';
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$index = _Json_decodeIndex;
var elm$json$Json$Decode$succeed = _Json_succeed;
var author$project$Util$decodePair = F2(
	function (a, b) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (aVal) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (bVal) {
						return elm$json$Json$Decode$succeed(
							_Utils_Tuple2(aVal, bVal));
					},
					A2(elm$json$Json$Decode$index, 1, b));
			},
			A2(elm$json$Json$Decode$index, 0, a));
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Debug$log = _Debug_log;
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (_n0.$ === 'Just') {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var elm$http$Http$Timeout_ = {$: 'Timeout_'};
var elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			elm$core$Basics$identity,
			A2(elm$core$Basics$composeR, toResult, toMsg));
	});
var elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var elm$http$Http$NetworkError = {$: 'NetworkError'};
var elm$http$Http$Timeout = {$: 'Timeout'};
var elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return elm$core$Result$Err(elm$http$Http$Timeout);
			case 'NetworkError_':
				return elm$core$Result$Err(elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					elm$core$Result$mapError,
					elm$http$Http$BadBody,
					toResult(body));
		}
	});
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			elm$http$Http$expectStringResponse,
			toMsg,
			elm$http$Http$resolve(
				function (string) {
					return A2(
						elm$core$Result$mapError,
						elm$json$Json$Decode$errorToString,
						A2(elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var elm$http$Http$emptyBody = _Http_emptyBody;
var elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var elm$http$Http$init = elm$core$Task$succeed(
	A2(elm$http$Http$State, elm$core$Dict$empty, _List_Nil));
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _n2 = A2(elm$core$Dict$get, tracker, reqs);
					if (_n2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _n2.a;
						return A2(
							elm$core$Task$andThen,
							function (_n3) {
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2(elm$core$Dict$remove, tracker, reqs));
							},
							elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						elm$core$Task$andThen,
						function (pid) {
							var _n4 = req.tracker;
							if (_n4.$ === 'Nothing') {
								return A3(elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _n4.a;
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3(elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			elm$core$Task$andThen,
			function (reqs) {
				return elm$core$Task$succeed(
					A2(elm$http$Http$State, reqs, subs));
			},
			A3(elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _n0) {
		var actualTracker = _n0.a;
		var toMsg = _n0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? elm$core$Maybe$Just(
			A2(
				elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : elm$core$Maybe$Nothing;
	});
var elm$http$Http$onSelfMsg = F3(
	function (router, _n0, state) {
		var tracker = _n0.a;
		var progress = _n0.b;
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$filterMap,
					A3(elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var elm$http$Http$subMap = F2(
	function (func, _n0) {
		var tracker = _n0.a;
		var toMsg = _n0.b;
		return A2(
			elm$http$Http$MySub,
			tracker,
			A2(elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager(elm$http$Http$init, elm$http$Http$onEffects, elm$http$Http$onSelfMsg, elm$http$Http$cmdMap, elm$http$Http$subMap);
var elm$http$Http$command = _Platform_leaf('Http');
var elm$http$Http$subscription = _Platform_leaf('Http');
var elm$http$Http$request = function (r) {
	return elm$http$Http$command(
		elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var elm$http$Http$get = function (r) {
	return elm$http$Http$request(
		{body: elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: elm$core$Maybe$Nothing, tracker: elm$core$Maybe$Nothing, url: r.url});
};
var elm$json$Json$Decode$list = _Json_decodeList;
var author$project$Page$Liver$getData = F2(
	function (mliver, mMaxTweetId) {
		if (mliver.$ === 'Nothing') {
			return elm$core$Platform$Cmd$none;
		} else {
			var liver = mliver.a;
			var params = A2(
				elm$core$List$filterMap,
				elm$core$Basics$identity,
				_List_fromArray(
					[
						elm$core$Maybe$Just('liver=' + liver),
						A2(
						elm$core$Maybe$map,
						function (maxTweetId) {
							return 'maxTweetId=' + maxTweetId;
						},
						mMaxTweetId)
					]));
			return elm$http$Http$get(
				{
					expect: A2(
						elm$http$Http$expectJson,
						author$project$Page$Liver$GotData,
						elm$json$Json$Decode$list(
							A2(author$project$Util$decodePair, elm$json$Json$Decode$string, author$project$Nijisanji$decodeUpdateInfo))),
					url: A2(
						elm$core$Debug$log,
						'here',
						author$project$Util$apiUrl + ('timeline/?' + A2(elm$core$String$join, '&', params)))
				});
		}
	});
var author$project$Page$Liver$init = function (_n0) {
	var session = _n0.a;
	var liver = _n0.b;
	return _Utils_Tuple2(
		A3(
			author$project$Page$Liver$Model,
			session,
			elm$core$Result$Ok(_List_Nil),
			liver),
		A2(author$project$Page$Liver$getData, liver, elm$core$Maybe$Nothing));
};
var author$project$Nijisanji$Gamers = {$: 'Gamers'};
var author$project$Nijisanji$Nijisanji = {$: 'Nijisanji'};
var author$project$Nijisanji$SEEDs = {$: 'SEEDs'};
var author$project$Page$Snapshot$Model = F4(
	function (session, data, clock, filter) {
		return {clock: clock, data: data, filter: filter, session: session};
	});
var author$project$Page$Snapshot$GotData = function (a) {
	return {$: 'GotData', a: a};
};
var author$project$Page$Snapshot$datetimeToString = function (time) {
	return time.date + ('T' + function () {
		var _n0 = time.time;
		if (_n0 === '') {
			return '00:00';
		} else {
			return time.time;
		}
	}());
};
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		elm$json$Json$Decode$map,
		elm$core$Dict$fromList,
		elm$json$Json$Decode$keyValuePairs(decoder));
};
var author$project$Page$Snapshot$getData = function (mTime) {
	var _n0 = A2(elm$core$Debug$log, 'getData', mTime);
	var param = function () {
		if (mTime.$ === 'Nothing') {
			return '';
		} else {
			var datetime = mTime.a;
			return 'date=' + author$project$Page$Snapshot$datetimeToString(datetime);
		}
	}();
	return elm$http$Http$get(
		{
			expect: A2(
				elm$http$Http$expectJson,
				author$project$Page$Snapshot$GotData,
				elm$json$Json$Decode$dict(author$project$Nijisanji$decodeUpdateInfo)),
			url: author$project$Util$apiUrl + ('snapshot/?' + param)
		});
};
var author$project$Page$Snapshot$init = function (session) {
	return _Utils_Tuple2(
		A4(
			author$project$Page$Snapshot$Model,
			session,
			elm$core$Result$Err('Loading...'),
			elm$core$Maybe$Nothing,
			_List_fromArray(
				[author$project$Nijisanji$Nijisanji, author$project$Nijisanji$Gamers, author$project$Nijisanji$SEEDs])),
		author$project$Page$Snapshot$getData(elm$core$Maybe$Nothing));
};
var author$project$Page$Timeline$Model = F4(
	function (session, data, filter, remainNoData) {
		return {data: data, filter: filter, remainNoData: remainNoData, session: session};
	});
var author$project$Page$Timeline$GotData = function (a) {
	return {$: 'GotData', a: a};
};
var elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Set$isEmpty = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$isEmpty(dict);
};
var author$project$Page$Timeline$getData = F2(
	function (filter, mMaxTweetId) {
		var params = A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					elm$core$Set$isEmpty(filter) ? elm$core$Maybe$Just('liver=nil') : elm$core$Maybe$Just(
					'liver=' + A2(
						elm$core$String$join,
						',',
						elm$core$Set$toList(filter))),
					A2(
					elm$core$Maybe$map,
					function (maxTweetId) {
						return 'maxTweetId=' + maxTweetId;
					},
					mMaxTweetId)
				]));
		return elm$http$Http$get(
			{
				expect: A2(
					elm$http$Http$expectJson,
					author$project$Page$Timeline$GotData,
					elm$json$Json$Decode$list(
						A2(author$project$Util$decodePair, elm$json$Json$Decode$string, author$project$Nijisanji$decodeUpdateInfo))),
				url: author$project$Util$apiUrl + ('timeline/?' + A2(elm$core$String$join, '&', params))
			});
	});
var author$project$Page$Timeline$init = function (_n0) {
	var session = _n0.a;
	var filter = _n0.b;
	return _Utils_Tuple2(
		A4(
			author$project$Page$Timeline$Model,
			session,
			elm$core$Result$Err('Loading...'),
			filter,
			false),
		A2(author$project$Page$Timeline$getData, filter, elm$core$Maybe$Nothing));
};
var author$project$Route$Home = {$: 'Home'};
var author$project$Nijisanji$filterToQueryUrl = function (filter) {
	return elm$core$Set$isEmpty(filter) ? '' : ('?filter=' + A2(
		elm$core$String$join,
		',',
		elm$core$Set$toList(filter)));
};
var author$project$Route$routeToString = function (page) {
	var pieces = function () {
		switch (page.$) {
			case 'Root':
				return _List_Nil;
			case 'Home':
				return _List_Nil;
			case 'About':
				return _List_fromArray(
					['about']);
			case 'Snapshot':
				return _List_fromArray(
					['snapshot']);
			case 'Timeline':
				var filter = page.a;
				return _List_fromArray(
					[
						'timeline',
						author$project$Nijisanji$filterToQueryUrl(filter)
					]);
			default:
				if (page.a.$ === 'Nothing') {
					var _n1 = page.a;
					return _List_fromArray(
						['liver']);
				} else {
					var liver = page.a.a;
					return _List_fromArray(
						['liver', liver]);
				}
		}
	}();
	return '/#/' + A2(elm$core$String$join, '/', pieces);
};
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Navigation$replaceUrl = _Browser_replaceUrl;
var author$project$Route$replaceUrl = F2(
	function (key, route) {
		return A2(
			elm$browser$Browser$Navigation$replaceUrl,
			key,
			author$project$Route$routeToString(route));
	});
var author$project$Session$navKey = function (session) {
	return session.navKey;
};
var author$project$Main$changeRouteTo = F2(
	function (maybeRoute, model) {
		var session = author$project$Main$toSession(model);
		if (maybeRoute.$ === 'Nothing') {
			return _Utils_Tuple2(
				author$project$Main$NotFound(session),
				elm$core$Platform$Cmd$none);
		} else {
			switch (maybeRoute.a.$) {
				case 'Root':
					var _n1 = maybeRoute.a;
					return _Utils_Tuple2(
						model,
						A2(
							author$project$Route$replaceUrl,
							author$project$Session$navKey(session),
							author$project$Route$Home));
				case 'Home':
					var _n2 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Home,
						author$project$Main$GotHomeMsg,
						model,
						author$project$Page$Home$init(session));
				case 'About':
					var _n3 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$About,
						author$project$Main$GotAboutMsg,
						model,
						author$project$Page$About$init(session));
				case 'Snapshot':
					var _n4 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Snapshot,
						author$project$Main$GotSnapshotMsg,
						model,
						author$project$Page$Snapshot$init(session));
				case 'Timeline':
					var filter = maybeRoute.a.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Timeline,
						author$project$Main$GotTimelineMsg,
						model,
						author$project$Page$Timeline$init(
							_Utils_Tuple2(session, filter)));
				default:
					var liver = maybeRoute.a.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Liver,
						author$project$Main$GotLiverMsg,
						model,
						author$project$Page$Liver$init(
							_Utils_Tuple2(session, liver)));
			}
		}
	});
var author$project$Nijisanji$Member = F4(
	function (name, exGroup, screenName, userId) {
		return {exGroup: exGroup, name: name, screenName: screenName, userId: userId};
	});
var author$project$Nijisanji$achikitaChinami = A4(author$project$Nijisanji$Member, '遠北千南', author$project$Nijisanji$SEEDs, 'ac1kt', 1019541420728713216);
var author$project$Nijisanji$aduchiMomo = A4(author$project$Nijisanji$Member, '安土桃', author$project$Nijisanji$SEEDs, 'momo_aduchi', 1002075894880452609);
var author$project$Nijisanji$akabaneYouko = A4(author$project$Nijisanji$Member, '赤羽葉子', author$project$Nijisanji$Gamers, 'Youko_Akabane', 988489581367513088);
var author$project$Nijisanji$amemoriSayo = A4(author$project$Nijisanji$Member, '雨森小夜', author$project$Nijisanji$SEEDs, 'Sayo_Amemori', 1019625123114987521);
var author$project$Nijisanji$asukaHina = A4(author$project$Nijisanji$Member, '飛鳥ひな', author$project$Nijisanji$SEEDs, 'hina__asuka', 1011952508317548550);
var author$project$Nijisanji$belmondBanderas = A4(author$project$Nijisanji$Member, 'ベルモンド・バンデラス', author$project$Nijisanji$SEEDs, 'belmond_b_2434', 1007214007827091456);
var author$project$Nijisanji$debidebiDebiru = A4(author$project$Nijisanji$Member, 'でびでび・でびる', author$project$Nijisanji$SEEDs, 'debidebiru_sama', 1034137905000636417);
var author$project$Nijisanji$dola = A4(author$project$Nijisanji$Member, 'ドーラ', author$project$Nijisanji$SEEDs, '___Dola', 1002073779848151041);
var author$project$Nijisanji$elu = A4(author$project$Nijisanji$Member, 'える', author$project$Nijisanji$Nijisanji, 'Elu_World', 958726740108484608);
var author$project$Nijisanji$fuminoTamaki = A4(author$project$Nijisanji$Member, '文野環', author$project$Nijisanji$Nijisanji, 'nekokan_chu', 971925378913611776);
var author$project$Nijisanji$fushimiGaku = A4(author$project$Nijisanji$Member, '伏見ガク', author$project$Nijisanji$Nijisanji, 'gaku_fushimi', 970645330956963840);
var author$project$Nijisanji$gilzarenIII = A4(author$project$Nijisanji$Member, 'ギルザレンⅢ世', author$project$Nijisanji$Nijisanji, 'Gilzaren_III', 971032696393789440);
var author$project$Nijisanji$hanabatakeChaika = A4(author$project$Nijisanji$Member, '花畑チャイカ', author$project$Nijisanji$SEEDs, 'ZulmIhP1nlMOT5y', 999942020238995456);
var author$project$Nijisanji$harusakiAir = A4(author$project$Nijisanji$Member, '春崎エアル', author$project$Nijisanji$SEEDs, 'harusakiair2434', 1004740613814681601);
var author$project$Nijisanji$hassakuYuzu = A4(author$project$Nijisanji$Member, '八朔ゆず', author$project$Nijisanji$SEEDs, '839yuzu', 1002057794764197888);
var author$project$Nijisanji$higuchiKaede = A4(author$project$Nijisanji$Member, '樋口楓', author$project$Nijisanji$Nijisanji, 'HiguchiKaede', 958646957190217728);
var author$project$Nijisanji$honmaHimawari = A4(author$project$Nijisanji$Member, '本間ひまわり', author$project$Nijisanji$Gamers, 'honmahimawari', 1011167857596493824);
var author$project$Nijisanji$ienagaMugi = A4(author$project$Nijisanji$Member, '家長むぎ', author$project$Nijisanji$Nijisanji, 'ienaga_mugi23', 970618643120664576);
var author$project$Nijisanji$izumoKasumi = A4(author$project$Nijisanji$Member, '出雲霞', author$project$Nijisanji$SEEDs, 'ikasumi_zzz', 1002059898929082370);
var author$project$Nijisanji$joeRikiichi = A4(author$project$Nijisanji$Member, 'ジョー・力一', author$project$Nijisanji$SEEDs, 'JoeRikiichi', 1034345063432650752);
var author$project$Nijisanji$kanae = A4(author$project$Nijisanji$Member, '叶', author$project$Nijisanji$Gamers, 'Kanae_2434', 988101299106267138);
var author$project$Nijisanji$kandaShoichi = A4(author$project$Nijisanji$Member, '神田笑一', author$project$Nijisanji$SEEDs, 'Kanda_Shoichi', 1015204476242677761);
var author$project$Nijisanji$kenmochiToya = A4(author$project$Nijisanji$Member, '剣持刀也', author$project$Nijisanji$Nijisanji, 'rei_Toya_rei', 970692564096499712);
var author$project$Nijisanji$kuroiShiba = A4(author$project$Nijisanji$Member, '黒井しば', author$project$Nijisanji$SEEDs, 'BlackShiba_chan', 1043029918014009345);
var author$project$Nijisanji$kuzuha = A4(author$project$Nijisanji$Member, '葛葉', author$project$Nijisanji$Gamers, 'Vamp_Kuzu', 965760241169088512);
var author$project$Nijisanji$machitaChima = A4(author$project$Nijisanji$Member, '町田ちま', author$project$Nijisanji$SEEDs, 'chima_machita23', 1028458841871007744);
var author$project$Nijisanji$maimotoKeisuke = A4(author$project$Nijisanji$Member, '舞元啓介', author$project$Nijisanji$SEEDs, 'maimoto_k', 1027050557465223169);
var author$project$Nijisanji$makainoRirimu = A4(author$project$Nijisanji$Member, '魔界ノりりむ', author$project$Nijisanji$Gamers, 'makaino_ririmu', 1022782812775100416);
var author$project$Nijisanji$moira = A4(author$project$Nijisanji$Member, 'モイラ', author$project$Nijisanji$Nijisanji, 'Moiramoimoimoi', 958632495196459009);
var author$project$Nijisanji$mononobeAlice = A4(author$project$Nijisanji$Member, '物述有栖', author$project$Nijisanji$Nijisanji, 'AliceMononobe', 970660632834920449);
var author$project$Nijisanji$morinakaKazaki = A4(author$project$Nijisanji$Member, '森中花咲', author$project$Nijisanji$Nijisanji, 'KazakiMorinaka', 973784758688927745);
var author$project$Nijisanji$nakaoAzuma = A4(author$project$Nijisanji$Member, '名伽尾アズマ', author$project$Nijisanji$SEEDs, 'azuma_dazo', 1002069549221478400);
var author$project$Nijisanji$naruseNaru = A4(author$project$Nijisanji$Member, '成瀬鳴', author$project$Nijisanji$SEEDs, 'narusenaru_2434', 1003299893404815360);
var author$project$Nijisanji$narutoKogane = A4(author$project$Nijisanji$Member, '鳴門こがね', author$project$Nijisanji$SEEDs, 'nArUtO_kOgAnE', 1025290579767250944);
var author$project$Nijisanji$rindouMikoto = A4(author$project$Nijisanji$Member, '竜胆尊', author$project$Nijisanji$SEEDs, 'RindouMikoto', 1009751809966010368);
var author$project$Nijisanji$ryushen = A4(author$project$Nijisanji$Member, '緑仙', author$project$Nijisanji$SEEDs, 'midori_2434', 1001291760272736257);
var author$project$Nijisanji$sakuraRitsuki = A4(author$project$Nijisanji$Member, '桜凛月', author$project$Nijisanji$SEEDs, 'SAKURA_RITSUKI', 964340295914569728);
var author$project$Nijisanji$sasakiSaku = A4(author$project$Nijisanji$Member, '笹木咲', author$project$Nijisanji$Gamers, 'saku_sasaki', 1012211447160455170);
var author$project$Nijisanji$setsuna = A4(author$project$Nijisanji$Member, '雪汝', author$project$Nijisanji$Gamers, 'setsuna2434', 1023138752850321408);
var author$project$Nijisanji$shibuyaHajime = A4(author$project$Nijisanji$Member, '渋谷ハジメ', author$project$Nijisanji$Nijisanji, 'sibuya_hajime', 958695135008628737);
var author$project$Nijisanji$shiinaYuika = A4(author$project$Nijisanji$Member, '椎名唯華', author$project$Nijisanji$Gamers, 'yuika_siina', 1022844567735824384);
var author$project$Nijisanji$shizukaRin = A4(author$project$Nijisanji$Member, '静凛', author$project$Nijisanji$Nijisanji, 'ShizuRin23', 958629229330968580);
var author$project$Nijisanji$sisterCleaire = A4(author$project$Nijisanji$Member, 'シスター・クレア', author$project$Nijisanji$SEEDs, 'SisterCleaire', 1000324666697728000);
var author$project$Nijisanji$suzukaUtako = A4(author$project$Nijisanji$Member, '鈴鹿詩子', author$project$Nijisanji$Nijisanji, 'suzukautako', 970940146680868864);
var author$project$Nijisanji$suzukiMasaru = A4(author$project$Nijisanji$Member, '鈴木勝', author$project$Nijisanji$SEEDs, 'Darkness_Eater', 994984072647593984);
var author$project$Nijisanji$suzuyaAki = A4(author$project$Nijisanji$Member, '鈴谷アキ', author$project$Nijisanji$Nijisanji, 'aki_suzuya', 958675678689243137);
var author$project$Nijisanji$takamiyaRion = A4(author$project$Nijisanji$Member, '鷹宮リオン', author$project$Nijisanji$SEEDs, 'TakamiyaRion', 1009734224369221632);
var author$project$Nijisanji$todorokiKyoko = A4(author$project$Nijisanji$Member, '轟京子', author$project$Nijisanji$SEEDs, 'KT_seeds', 1002050003559317504);
var author$project$Nijisanji$tsukimiShizuku = A4(author$project$Nijisanji$Member, '月見しずく', author$project$Nijisanji$SEEDs, 'tukimi_sizuku', 1026998487701893120);
var author$project$Nijisanji$tsukinoMito = A4(author$project$Nijisanji$Member, '月ノ美兎', author$project$Nijisanji$Nijisanji, 'MitoTsukino', 958737597336928256);
var author$project$Nijisanji$udukiKou = A4(author$project$Nijisanji$Member, '卯月コウ', author$project$Nijisanji$SEEDs, 'udukikohh', 1002077473360658432);
var author$project$Nijisanji$umiyashanokami = A4(author$project$Nijisanji$Member, '海夜叉神', author$project$Nijisanji$SEEDs, 'god_yaksa23', 1000032460741033985);
var author$project$Nijisanji$ushimiIchigo = A4(author$project$Nijisanji$Member, '宇志海いちご', author$project$Nijisanji$Nijisanji, 'ushimi_ichigo', 971316705363464192);
var author$project$Nijisanji$yagurumaRine = A4(author$project$Nijisanji$Member, '矢車りね', author$project$Nijisanji$SEEDs, 'Rine_Yaguruma', 1043543347430797312);
var author$project$Nijisanji$yamiyonoMoruru = A4(author$project$Nijisanji$Member, '闇夜乃モルル', author$project$Nijisanji$Gamers, '_rnrrdark', 1000747836365942784);
var author$project$Nijisanji$yashiroKizuku = A4(author$project$Nijisanji$Member, '社築', author$project$Nijisanji$SEEDs, '846kizuQ', 1002342365695078401);
var author$project$Nijisanji$yuhiRiri = A4(author$project$Nijisanji$Member, '夕陽リリ', author$project$Nijisanji$Nijisanji, 'Yuuhi_Riri', 970645643965317126);
var author$project$Nijisanji$yukiChihiro = A4(author$project$Nijisanji$Member, '勇気ちひろ', author$project$Nijisanji$Nijisanji, 'Chihiro_yuki23', 958767484902957056);
var author$project$Nijisanji$yumeoiKakeru = A4(author$project$Nijisanji$Member, '夢追翔', author$project$Nijisanji$SEEDs, 'kakeru_yumeoi', 1019066007447470080);
var author$project$Nijisanji$allMembers = _List_fromArray(
	[author$project$Nijisanji$tsukinoMito, author$project$Nijisanji$higuchiKaede, author$project$Nijisanji$shizukaRin, author$project$Nijisanji$suzuyaAki, author$project$Nijisanji$moira, author$project$Nijisanji$elu, author$project$Nijisanji$shibuyaHajime, author$project$Nijisanji$yukiChihiro, author$project$Nijisanji$mononobeAlice, author$project$Nijisanji$gilzarenIII, author$project$Nijisanji$yuhiRiri, author$project$Nijisanji$ienagaMugi, author$project$Nijisanji$kenmochiToya, author$project$Nijisanji$fushimiGaku, author$project$Nijisanji$ushimiIchigo, author$project$Nijisanji$fuminoTamaki, author$project$Nijisanji$suzukaUtako, author$project$Nijisanji$morinakaKazaki, author$project$Nijisanji$suzukiMasaru, author$project$Nijisanji$akabaneYouko, author$project$Nijisanji$kanae, author$project$Nijisanji$dola, author$project$Nijisanji$sisterCleaire, author$project$Nijisanji$hassakuYuzu, author$project$Nijisanji$nakaoAzuma, author$project$Nijisanji$izumoKasumi, author$project$Nijisanji$udukiKou, author$project$Nijisanji$umiyashanokami, author$project$Nijisanji$yashiroKizuku, author$project$Nijisanji$aduchiMomo, author$project$Nijisanji$todorokiKyoko, author$project$Nijisanji$hanabatakeChaika, author$project$Nijisanji$ryushen, author$project$Nijisanji$harusakiAir, author$project$Nijisanji$naruseNaru, author$project$Nijisanji$honmaHimawari, author$project$Nijisanji$yamiyonoMoruru, author$project$Nijisanji$makainoRirimu, author$project$Nijisanji$setsuna, author$project$Nijisanji$takamiyaRion, author$project$Nijisanji$kuzuha, author$project$Nijisanji$shiinaYuika, author$project$Nijisanji$kandaShoichi, author$project$Nijisanji$narutoKogane, author$project$Nijisanji$asukaHina, author$project$Nijisanji$amemoriSayo, author$project$Nijisanji$maimotoKeisuke, author$project$Nijisanji$debidebiDebiru, author$project$Nijisanji$rindouMikoto, author$project$Nijisanji$joeRikiichi, author$project$Nijisanji$machitaChima, author$project$Nijisanji$tsukimiShizuku, author$project$Nijisanji$sakuraRitsuki, author$project$Nijisanji$achikitaChinami, author$project$Nijisanji$belmondBanderas, author$project$Nijisanji$yagurumaRine, author$project$Nijisanji$yumeoiKakeru, author$project$Nijisanji$kuroiShiba, author$project$Nijisanji$sasakiSaku]);
var elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var elm$core$Set$empty = elm$core$Set$Set_elm_builtin(elm$core$Dict$empty);
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A3(elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var elm$core$Set$fromList = function (list) {
	return A3(elm$core$List$foldl, elm$core$Set$insert, elm$core$Set$empty, list);
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$url$Url$Parser$Internal$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var elm$url$Url$Parser$Query$custom = F2(
	function (key, func) {
		return elm$url$Url$Parser$Internal$Parser(
			function (dict) {
				return func(
					A2(
						elm$core$Maybe$withDefault,
						_List_Nil,
						A2(elm$core$Dict$get, key, dict)));
			});
	});
var author$project$Nijisanji$queryFilter = A2(elm$url$Url$Parser$Query$custom, 'filter', elm$core$Set$fromList);
var author$project$Route$About = {$: 'About'};
var author$project$Route$Liver = function (a) {
	return {$: 'Liver', a: a};
};
var author$project$Route$Snapshot = {$: 'Snapshot'};
var author$project$Route$Timeline = function (a) {
	return {$: 'Timeline', a: a};
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$url$Url$percentDecode = _Url_percentDecode;
var elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var elm$url$Url$Parser$mapState = F2(
	function (func, _n0) {
		var visited = _n0.visited;
		var unvisited = _n0.unvisited;
		var params = _n0.params;
		var frag = _n0.frag;
		var value = _n0.value;
		return A5(
			elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var elm$url$Url$Parser$map = F2(
	function (subValue, _n0) {
		var parseArg = _n0.a;
		return elm$url$Url$Parser$Parser(
			function (_n1) {
				var visited = _n1.visited;
				var unvisited = _n1.unvisited;
				var params = _n1.params;
				var frag = _n1.frag;
				var value = _n1.value;
				return A2(
					elm$core$List$map,
					elm$url$Url$Parser$mapState(value),
					parseArg(
						A5(elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$url$Url$Parser$oneOf = function (parsers) {
	return elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				elm$core$List$concatMap,
				function (_n0) {
					var parser = _n0.a;
					return parser(state);
				},
				parsers);
		});
};
var elm$url$Url$Parser$query = function (_n0) {
	var queryParser = _n0.a;
	return elm$url$Url$Parser$Parser(
		function (_n1) {
			var visited = _n1.visited;
			var unvisited = _n1.unvisited;
			var params = _n1.params;
			var frag = _n1.frag;
			var value = _n1.value;
			return _List_fromArray(
				[
					A5(
					elm$url$Url$Parser$State,
					visited,
					unvisited,
					params,
					frag,
					value(
						queryParser(params)))
				]);
		});
};
var elm$url$Url$Parser$slash = F2(
	function (_n0, _n1) {
		var parseBefore = _n0.a;
		var parseAfter = _n1.a;
		return elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var elm$url$Url$Parser$questionMark = F2(
	function (parser, queryParser) {
		return A2(
			elm$url$Url$Parser$slash,
			parser,
			elm$url$Url$Parser$query(queryParser));
	});
var elm$url$Url$Parser$s = function (str) {
	return elm$url$Url$Parser$Parser(
		function (_n0) {
			var visited = _n0.visited;
			var unvisited = _n0.unvisited;
			var params = _n0.params;
			var frag = _n0.frag;
			var value = _n0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						elm$url$Url$Parser$State,
						A2(elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return elm$url$Url$Parser$Parser(
			function (_n0) {
				var visited = _n0.visited;
				var unvisited = _n0.unvisited;
				var params = _n0.params;
				var frag = _n0.frag;
				var value = _n0.value;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _n2 = stringToSomething(next);
					if (_n2.$ === 'Just') {
						var nextValue = _n2.a;
						return _List_fromArray(
							[
								A5(
								elm$url$Url$Parser$State,
								A2(elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var elm$url$Url$Parser$string = A2(elm$url$Url$Parser$custom, 'STRING', elm$core$Maybe$Just);
var elm$url$Url$Parser$top = elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var author$project$Route$parser = function () {
	var timeline = function (filter) {
		return author$project$Route$Timeline(
			elm$core$Set$isEmpty(filter) ? elm$core$Set$fromList(
				A2(
					elm$core$List$map,
					function (m) {
						return m.name;
					},
					author$project$Nijisanji$allMembers)) : filter);
	};
	return elm$url$Url$Parser$oneOf(
		_List_fromArray(
			[
				A2(elm$url$Url$Parser$map, author$project$Route$Home, elm$url$Url$Parser$top),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Home,
				elm$url$Url$Parser$s('home')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$About,
				elm$url$Url$Parser$s('about')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Snapshot,
				elm$url$Url$Parser$s('snapshot')),
				A2(
				elm$url$Url$Parser$map,
				timeline,
				A2(
					elm$url$Url$Parser$questionMark,
					elm$url$Url$Parser$s('timeline'),
					author$project$Nijisanji$queryFilter)),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Liver(elm$core$Maybe$Nothing),
				elm$url$Url$Parser$s('liver')),
				A2(
				elm$url$Url$Parser$map,
				A2(elm$core$Basics$composeL, author$project$Route$Liver, elm$url$Url$percentDecode),
				A2(
					elm$url$Url$Parser$slash,
					elm$url$Url$Parser$s('liver'),
					elm$url$Url$Parser$string))
			]));
}();
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + elm$core$String$fromInt(port_));
		}
	});
var elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var elm$url$Url$toString = function (url) {
	var http = function () {
		var _n0 = url.protocol;
		if (_n0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _n1 = state.unvisited;
			if (!_n1.b) {
				return elm$core$Maybe$Just(state.value);
			} else {
				if ((_n1.a === '') && (!_n1.b.b)) {
					return elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				elm$core$List$cons,
				segment,
				elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var elm$url$Url$Parser$preparePath = function (path) {
	var _n0 = A2(elm$core$String$split, '/', path);
	if (_n0.b && (_n0.a === '')) {
		var segments = _n0.b;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _n0;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return elm$core$Maybe$Just(
				A2(elm$core$List$cons, value, list));
		}
	});
var elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _n0 = A2(elm$core$String$split, '=', segment);
		if ((_n0.b && _n0.b.b) && (!_n0.b.b.b)) {
			var rawKey = _n0.a;
			var _n1 = _n0.b;
			var rawValue = _n1.a;
			var _n2 = elm$url$Url$percentDecode(rawKey);
			if (_n2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _n2.a;
				var _n3 = elm$url$Url$percentDecode(rawValue);
				if (_n3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _n3.a;
					return A3(
						elm$core$Dict$update,
						key,
						elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			elm$core$List$foldr,
			elm$url$Url$Parser$addParam,
			elm$core$Dict$empty,
			A2(elm$core$String$split, '&', qry));
	}
};
var elm$url$Url$Parser$parse = F2(
	function (_n0, url) {
		var parser = _n0.a;
		return elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					elm$url$Url$Parser$State,
					_List_Nil,
					elm$url$Url$Parser$preparePath(url.path),
					elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					elm$core$Basics$identity)));
	});
var author$project$Route$fromUrl = function (url) {
	var modifiedUrl = _Utils_update(
		url,
		{
			fragment: elm$core$Maybe$Nothing,
			path: A2(elm$core$Maybe$withDefault, '', url.fragment)
		});
	var strUrl = elm$url$Url$toString(modifiedUrl);
	var modifiedUrl2 = elm$url$Url$fromString(strUrl);
	var result = A2(
		elm$core$Maybe$andThen,
		elm$url$Url$Parser$parse(author$project$Route$parser),
		modifiedUrl2);
	return result;
};
var author$project$Session$Session = F2(
	function (navKey, navState) {
		return {navKey: navKey, navState: navState};
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$Hidden = {$: 'Hidden'};
var rundis$elm_bootstrap$Bootstrap$Navbar$State = function (a) {
	return {$: 'State', a: a};
};
var elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var rundis$elm_bootstrap$Bootstrap$Navbar$mapState = F2(
	function (mapper, _n0) {
		var state = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Navbar$State(
			mapper(state));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$initWindowSize = F2(
	function (toMsg, state) {
		return A2(
			elm$core$Task$perform,
			function (vp) {
				return toMsg(
					A2(
						rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
						function (s) {
							return _Utils_update(
								s,
								{
									windowWidth: elm$core$Maybe$Just(vp.viewport.width)
								});
						},
						state));
			},
			elm$browser$Browser$Dom$getViewport);
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$initialState = function (toMsg) {
	var state = rundis$elm_bootstrap$Bootstrap$Navbar$State(
		{dropdowns: elm$core$Dict$empty, height: elm$core$Maybe$Nothing, visibility: rundis$elm_bootstrap$Bootstrap$Navbar$Hidden, windowWidth: elm$core$Maybe$Nothing});
	return _Utils_Tuple2(
		state,
		A2(rundis$elm_bootstrap$Bootstrap$Navbar$initWindowSize, toMsg, state));
};
var author$project$Main$init = F3(
	function (_n0, url, navKey) {
		var _n1 = rundis$elm_bootstrap$Bootstrap$Navbar$initialState(
			A2(elm$core$Basics$composeL, author$project$Main$PageCtrl, author$project$Main$NavMsg));
		var navState = _n1.a;
		var navCmd = _n1.b;
		var _n2 = A2(
			author$project$Main$changeRouteTo,
			author$project$Route$fromUrl(url),
			author$project$Main$Redirect(
				A2(author$project$Session$Session, navKey, navState)));
		var model = _n2.a;
		var cmd = _n2.b;
		return _Utils_Tuple2(
			model,
			elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[navCmd, cmd])));
	});
var author$project$Main$ScrollBottom = function (a) {
	return {$: 'ScrollBottom', a: a};
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Page$About$subscriptions = function (_n0) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Home$subscriptions = function (_n0) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Liver$ScrollBottom = function (a) {
	return {$: 'ScrollBottom', a: a};
};
var elm$json$Json$Decode$value = _Json_decodeValue;
var author$project$Ports$scrollBottom = _Platform_incomingPort('scrollBottom', elm$json$Json$Decode$value);
var author$project$Page$Liver$subscriptions = function (_n0) {
	return author$project$Ports$scrollBottom(author$project$Page$Liver$ScrollBottom);
};
var author$project$Page$Snapshot$subscriptions = function (_n0) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Timeline$LoadMore = {$: 'LoadMore'};
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var author$project$Page$Timeline$subscriptions = function (_n0) {
	return author$project$Ports$scrollBottom(
		elm$core$Basics$always(author$project$Page$Timeline$LoadMore));
};
var elm$core$Platform$Sub$map = _Platform_map;
var elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var elm$browser$Browser$AnimationManager$init = elm$core$Task$succeed(
	A3(elm$browser$Browser$AnimationManager$State, _List_Nil, elm$core$Maybe$Nothing, 0));
var elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _n0) {
		var request = _n0.request;
		var oldTime = _n0.oldTime;
		var _n1 = _Utils_Tuple2(request, subs);
		if (_n1.a.$ === 'Nothing') {
			if (!_n1.b.b) {
				var _n2 = _n1.a;
				return elm$browser$Browser$AnimationManager$init;
			} else {
				var _n4 = _n1.a;
				return A2(
					elm$core$Task$andThen,
					function (pid) {
						return A2(
							elm$core$Task$andThen,
							function (time) {
								return elm$core$Task$succeed(
									A3(
										elm$browser$Browser$AnimationManager$State,
										subs,
										elm$core$Maybe$Just(pid),
										time));
							},
							elm$browser$Browser$AnimationManager$now);
					},
					elm$core$Process$spawn(
						A2(
							elm$core$Task$andThen,
							elm$core$Platform$sendToSelf(router),
							elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_n1.b.b) {
				var pid = _n1.a.a;
				return A2(
					elm$core$Task$andThen,
					function (_n3) {
						return elm$browser$Browser$AnimationManager$init;
					},
					elm$core$Process$kill(pid));
			} else {
				return elm$core$Task$succeed(
					A3(elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var elm$time$Time$millisToPosix = elm$time$Time$Posix;
var elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _n0) {
		var subs = _n0.subs;
		var oldTime = _n0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(
						elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			elm$core$Task$andThen,
			function (pid) {
				return A2(
					elm$core$Task$andThen,
					function (_n1) {
						return elm$core$Task$succeed(
							A3(
								elm$browser$Browser$AnimationManager$State,
								subs,
								elm$core$Maybe$Just(pid),
								newTime));
					},
					elm$core$Task$sequence(
						A2(elm$core$List$map, send, subs)));
			},
			elm$core$Process$spawn(
				A2(
					elm$core$Task$andThen,
					elm$core$Platform$sendToSelf(router),
					elm$browser$Browser$AnimationManager$rAF)));
	});
var elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Time(
				A2(elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Delta(
				A2(elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager(elm$browser$Browser$AnimationManager$init, elm$browser$Browser$AnimationManager$onEffects, elm$browser$Browser$AnimationManager$onSelfMsg, 0, elm$browser$Browser$AnimationManager$subMap);
var elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return elm$browser$Browser$AnimationManager$subscription(
		elm$browser$Browser$AnimationManager$Time(tagger));
};
var elm$browser$Browser$Events$onAnimationFrame = elm$browser$Browser$AnimationManager$onAnimationFrame;
var elm$browser$Browser$Events$Window = {$: 'Window'};
var elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var elm$browser$Browser$Events$init = elm$core$Task$succeed(
	A2(elm$browser$Browser$Events$State, _List_Nil, elm$core$Dict$empty));
var elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var elm$browser$Browser$Events$spawn = F3(
	function (router, key, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						elm$core$Platform$sendToSelf,
						router,
						A2(elm$browser$Browser$Events$Event, key, event));
				}));
	});
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _n6) {
				var deads = _n6.a;
				var lives = _n6.b;
				var news = _n6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						elm$core$List$cons,
						A3(elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_n4, pid, _n5) {
				var deads = _n5.a;
				var lives = _n5.b;
				var news = _n5.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _n2, _n3) {
				var deads = _n3.a;
				var lives = _n3.b;
				var news = _n3.c;
				return _Utils_Tuple3(
					deads,
					A3(elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2(elm$core$List$map, elm$browser$Browser$Events$addKey, subs);
		var _n0 = A6(
			elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, elm$core$Dict$empty, _List_Nil));
		var deadPids = _n0.a;
		var livePids = _n0.b;
		var makeNewPids = _n0.c;
		return A2(
			elm$core$Task$andThen,
			function (pids) {
				return elm$core$Task$succeed(
					A2(
						elm$browser$Browser$Events$State,
						newSubs,
						A2(
							elm$core$Dict$union,
							livePids,
							elm$core$Dict$fromList(pids))));
			},
			A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$sequence(makeNewPids);
				},
				elm$core$Task$sequence(
					A2(elm$core$List$map, elm$core$Process$kill, deadPids))));
	});
var elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _n0, state) {
		var key = _n0.key;
		var event = _n0.event;
		var toMessage = function (_n2) {
			var subKey = _n2.a;
			var _n3 = _n2.b;
			var node = _n3.a;
			var name = _n3.b;
			var decoder = _n3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : elm$core$Maybe$Nothing;
		};
		var messages = A2(elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Platform$sendToApp(router),
					messages)));
	});
var elm$browser$Browser$Events$subMap = F2(
	function (func, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var decoder = _n0.c;
		return A3(
			elm$browser$Browser$Events$MySub,
			node,
			name,
			A2(elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager(elm$browser$Browser$Events$init, elm$browser$Browser$Events$onEffects, elm$browser$Browser$Events$onSelfMsg, 0, elm$browser$Browser$Events$subMap);
var elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return elm$browser$Browser$Events$subscription(
			A3(elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		elm$browser$Browser$Events$on,
		elm$browser$Browser$Events$Window,
		'resize',
		A2(
			elm$json$Json$Decode$field,
			'target',
			A3(
				elm$json$Json$Decode$map2,
				func,
				A2(elm$json$Json$Decode$field, 'innerWidth', elm$json$Json$Decode$int),
				A2(elm$json$Json$Decode$field, 'innerHeight', elm$json$Json$Decode$int))));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingDown = {$: 'AnimatingDown'};
var rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingUp = {$: 'AnimatingUp'};
var elm$browser$Browser$Events$Document = {$: 'Document'};
var elm$browser$Browser$Events$onClick = A2(elm$browser$Browser$Events$on, elm$browser$Browser$Events$Document, 'click');
var elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2(elm$core$Dict$map, func, left),
				A2(elm$core$Dict$map, func, right));
		}
	});
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$Closed = {$: 'Closed'};
var rundis$elm_bootstrap$Bootstrap$Navbar$ListenClicks = {$: 'ListenClicks'};
var rundis$elm_bootstrap$Bootstrap$Navbar$Open = {$: 'Open'};
var rundis$elm_bootstrap$Bootstrap$Navbar$dropdownSubscriptions = F2(
	function (state, toMsg) {
		var dropdowns = state.a.dropdowns;
		var updDropdowns = A2(
			elm$core$Dict$map,
			F2(
				function (_n2, status) {
					switch (status.$) {
						case 'Open':
							return rundis$elm_bootstrap$Bootstrap$Navbar$ListenClicks;
						case 'ListenClicks':
							return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
						default:
							return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
					}
				}),
			dropdowns);
		var updState = A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
			function (s) {
				return _Utils_update(
					s,
					{dropdowns: updDropdowns});
			},
			state);
		var needsSub = function (s) {
			return A2(
				elm$core$List$any,
				function (_n1) {
					var status = _n1.b;
					return _Utils_eq(status, s);
				},
				elm$core$Dict$toList(dropdowns));
		};
		return elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					needsSub(rundis$elm_bootstrap$Bootstrap$Navbar$Open) ? elm$browser$Browser$Events$onAnimationFrame(
					function (_n0) {
						return toMsg(updState);
					}) : elm$core$Platform$Sub$none,
					needsSub(rundis$elm_bootstrap$Bootstrap$Navbar$ListenClicks) ? elm$browser$Browser$Events$onClick(
					elm$json$Json$Decode$succeed(
						toMsg(updState))) : elm$core$Platform$Sub$none
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$subscriptions = F2(
	function (state, toMsg) {
		var visibility = state.a.visibility;
		var updState = function (v) {
			return A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
				function (s) {
					return _Utils_update(
						s,
						{visibility: v});
				},
				state);
		};
		return elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					function () {
					switch (visibility.$) {
						case 'StartDown':
							return elm$browser$Browser$Events$onAnimationFrame(
								function (_n1) {
									return toMsg(
										updState(rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingDown));
								});
						case 'StartUp':
							return elm$browser$Browser$Events$onAnimationFrame(
								function (_n2) {
									return toMsg(
										updState(rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingUp));
								});
						default:
							return elm$core$Platform$Sub$none;
					}
				}(),
					elm$browser$Browser$Events$onResize(
					F2(
						function (x, _n3) {
							return toMsg(
								A2(
									rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
									function (s) {
										return _Utils_update(
											s,
											{
												windowWidth: elm$core$Maybe$Just(x)
											});
									},
									state));
						})),
					A2(rundis$elm_bootstrap$Bootstrap$Navbar$dropdownSubscriptions, state, toMsg)
				]));
	});
var author$project$Main$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$subscriptions,
				author$project$Main$toSession(model).navState,
				A2(elm$core$Basics$composeL, author$project$Main$PageCtrl, author$project$Main$NavMsg)),
				author$project$Ports$scrollBottom(author$project$Main$ScrollBottom),
				function () {
				switch (model.$) {
					case 'NotFound':
						return elm$core$Platform$Sub$none;
					case 'Redirect':
						return elm$core$Platform$Sub$none;
					case 'Home':
						var subModel = model.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Main$GotHomeMsg,
							author$project$Page$Home$subscriptions(subModel));
					case 'About':
						var subModel = model.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Main$GotAboutMsg,
							author$project$Page$About$subscriptions(subModel));
					case 'Snapshot':
						var subModel = model.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Main$GotSnapshotMsg,
							author$project$Page$Snapshot$subscriptions(subModel));
					case 'Timeline':
						var subModel = model.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Main$GotTimelineMsg,
							author$project$Page$Timeline$subscriptions(subModel));
					default:
						var subModel = model.a;
						return A2(
							elm$core$Platform$Sub$map,
							author$project$Main$GotLiverMsg,
							author$project$Page$Liver$subscriptions(subModel));
				}
			}()
			]));
};
var author$project$Main$updateSession = F2(
	function (model, newSession) {
		switch (model.$) {
			case 'NotFound':
				var oldSession = model.a;
				return author$project$Main$NotFound(newSession);
			case 'Redirect':
				var oldSession = model.a;
				return author$project$Main$Redirect(newSession);
			case 'Home':
				var subModel = model.a;
				return author$project$Main$Home(
					_Utils_update(
						subModel,
						{session: newSession}));
			case 'About':
				var subModel = model.a;
				return author$project$Main$About(
					_Utils_update(
						subModel,
						{session: newSession}));
			case 'Snapshot':
				var subModel = model.a;
				return author$project$Main$Snapshot(
					_Utils_update(
						subModel,
						{session: newSession}));
			case 'Timeline':
				var subModel = model.a;
				return author$project$Main$Timeline(
					_Utils_update(
						subModel,
						{session: newSession}));
			default:
				var subModel = model.a;
				return author$project$Main$Liver(
					_Utils_update(
						subModel,
						{session: newSession}));
		}
	});
var author$project$Page$About$update = F2(
	function (msg, model) {
		if (msg.$ === 'Increment') {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{counter: model.counter + 1}),
				elm$core$Platform$Cmd$none);
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{counter: model.counter - 1}),
				elm$core$Platform$Cmd$none);
		}
	});
var author$project$Page$Home$update = F2(
	function (_n0, model) {
		return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var author$project$Page$Liver$update = F2(
	function (msg, model) {
		if (msg.$ === 'GotData') {
			var result = msg.a;
			if (result.$ === 'Ok') {
				var data = result.a;
				var newData = function () {
					var _n3 = model.data;
					if (_n3.$ === 'Ok') {
						var oldData = _n3.a;
						return _Utils_ap(oldData, data);
					} else {
						return data;
					}
				}();
				var _n2 = A2(
					elm$core$Debug$log,
					'GotData',
					_Utils_Tuple2(
						elm$core$List$length(data),
						elm$core$List$length(newData)));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							data: elm$core$Result$Ok(newData)
						}),
					elm$core$Platform$Cmd$none);
			} else {
				var err = result.a;
				var errMsg = function () {
					switch (err.$) {
						case 'BadUrl':
							var s = err.a;
							return 'Bad URL: ' + s;
						case 'Timeout':
							return 'Timeout';
						case 'NetworkError':
							return 'Network error';
						case 'BadStatus':
							var n = err.a;
							return 'Bad Status: ' + elm$core$String$fromInt(n);
						default:
							var s = err.a;
							return 'Bad body: ' + s;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							data: elm$core$Result$Err(errMsg)
						}),
					elm$core$Platform$Cmd$none);
			}
		} else {
			var _n5 = model.data;
			if (_n5.$ === 'Ok') {
				var data = _n5.a;
				var _n6 = elm$core$List$head(
					elm$core$List$reverse(data));
				if (_n6.$ === 'Nothing') {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				} else {
					var last = _n6.a;
					var maxTweetId = A2(elm$core$Debug$log, 'maxTweetId', last.b.tweetId);
					return _Utils_Tuple2(
						model,
						A2(
							author$project$Page$Liver$getData,
							model.mliver,
							elm$core$Maybe$Just(maxTweetId)));
				}
			} else {
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			}
		}
	});
var author$project$Page$Snapshot$updateDate = F2(
	function (date, mDateTime) {
		if (mDateTime.$ === 'Nothing') {
			return elm$core$Maybe$Just(
				{date: date, time: '00:00'});
		} else {
			var datetime = mDateTime.a;
			return elm$core$Maybe$Just(
				_Utils_update(
					datetime,
					{date: date}));
		}
	});
var author$project$Page$Snapshot$updateTime = F2(
	function (time, mDateTime) {
		if (mDateTime.$ === 'Nothing') {
			return elm$core$Maybe$Nothing;
		} else {
			var datetime = mDateTime.a;
			return elm$core$Maybe$Just(
				_Utils_update(
					datetime,
					{time: time}));
		}
	});
var author$project$Page$Snapshot$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'GotData':
				var result = msg.a;
				if (result.$ === 'Ok') {
					var data = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								data: elm$core$Result$Ok(data)
							}),
						elm$core$Platform$Cmd$none);
				} else {
					var err = result.a;
					var errMsg = function () {
						switch (err.$) {
							case 'BadUrl':
								var s = err.a;
								return 'Bad URL: ' + s;
							case 'Timeout':
								return 'Timeout';
							case 'NetworkError':
								return 'Network error';
							case 'BadStatus':
								var n = err.a;
								return 'Bad Status: ' + elm$core$String$fromInt(n);
							default:
								var s = err.a;
								return 'Bad body: ' + s;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								data: elm$core$Result$Err(errMsg)
							}),
						elm$core$Platform$Cmd$none);
				}
			case 'FilterUpdate':
				var filter = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{filter: filter}),
					elm$core$Platform$Cmd$none);
			case 'TimeUpdate':
				var time = msg.a;
				var _n3 = A2(elm$core$Debug$log, 'time', time);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							clock: A2(author$project$Page$Snapshot$updateTime, time, model.clock)
						}),
					author$project$Page$Snapshot$getData(
						A2(author$project$Page$Snapshot$updateTime, time, model.clock)));
			default:
				var date = msg.a;
				var _n4 = A2(elm$core$Debug$log, 'date', date);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							clock: A2(author$project$Page$Snapshot$updateDate, date, model.clock)
						}),
					author$project$Page$Snapshot$getData(
						A2(author$project$Page$Snapshot$updateDate, date, model.clock)));
		}
	});
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var author$project$Page$Timeline$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'GotData':
				var result = msg.a;
				if (result.$ === 'Ok') {
					var data = result.a;
					var remainNoData = elm$core$List$isEmpty(data);
					var newData = function () {
						var _n3 = model.data;
						if (_n3.$ === 'Ok') {
							var oldData = _n3.a;
							return _Utils_ap(oldData, data);
						} else {
							return data;
						}
					}();
					var _n2 = A2(
						elm$core$Debug$log,
						'GotData',
						_Utils_Tuple2(
							elm$core$List$length(data),
							elm$core$List$length(newData)));
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								data: elm$core$Result$Ok(newData),
								remainNoData: remainNoData
							}),
						elm$core$Platform$Cmd$none);
				} else {
					var err = result.a;
					var errMsg = function () {
						switch (err.$) {
							case 'BadUrl':
								var s = err.a;
								return 'Bad URL: ' + s;
							case 'Timeout':
								return 'Timeout';
							case 'NetworkError':
								return 'Network error';
							case 'BadStatus':
								var n = err.a;
								return 'Bad Status: ' + elm$core$String$fromInt(n);
							default:
								var s = err.a;
								return 'Bad body: ' + s;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								data: elm$core$Result$Err(errMsg)
							}),
						elm$core$Platform$Cmd$none);
				}
			case 'FilterUpdate':
				var filter = msg.a;
				var remainNoData = elm$core$Set$isEmpty(filter);
				var _n5 = A2(elm$core$Debug$log, 'filter=', filter);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							data: elm$core$Result$Ok(_List_Nil),
							filter: filter,
							remainNoData: remainNoData
						}),
					A2(author$project$Page$Timeline$getData, filter, elm$core$Maybe$Nothing));
			default:
				var _n6 = model.data;
				if (_n6.$ === 'Ok') {
					var data = _n6.a;
					var _n7 = elm$core$List$head(
						elm$core$List$reverse(data));
					if (_n7.$ === 'Nothing') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{remainNoData: true}),
							elm$core$Platform$Cmd$none);
					} else {
						var last = _n7.a;
						var maxTweetId = A2(elm$core$Debug$log, 'maxTweetId', last.b.tweetId);
						return _Utils_Tuple2(
							model,
							A2(
								author$project$Page$Timeline$getData,
								model.filter,
								elm$core$Maybe$Just(maxTweetId)));
					}
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
		}
	});
var elm$browser$Browser$Navigation$load = _Browser_load;
var elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var author$project$Main$update = F2(
	function (msg, model) {
		var _n0 = _Utils_Tuple2(msg, model);
		_n0$11:
		while (true) {
			switch (_n0.a.$) {
				case 'Ignored':
					var _n1 = _n0.a;
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				case 'ClickedLink':
					var urlRequest = _n0.a.a;
					if (urlRequest.$ === 'Internal') {
						var url = urlRequest.a;
						var _n3 = url.fragment;
						if (_n3.$ === 'Nothing') {
							return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(
								model,
								A2(
									elm$browser$Browser$Navigation$pushUrl,
									author$project$Session$navKey(
										author$project$Main$toSession(model)),
									elm$url$Url$toString(url)));
						}
					} else {
						var href = urlRequest.a;
						return _Utils_Tuple2(
							model,
							elm$browser$Browser$Navigation$load(href));
					}
				case 'ChangedUrl':
					var url = _n0.a.a;
					return A2(
						author$project$Main$changeRouteTo,
						author$project$Route$fromUrl(url),
						model);
				case 'GotHomeMsg':
					if (_n0.b.$ === 'Home') {
						var subMsg = _n0.a.a;
						var home = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Home,
							author$project$Main$GotHomeMsg,
							model,
							A2(author$project$Page$Home$update, subMsg, home));
					} else {
						break _n0$11;
					}
				case 'GotAboutMsg':
					if (_n0.b.$ === 'About') {
						var subMsg = _n0.a.a;
						var about = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$About,
							author$project$Main$GotAboutMsg,
							model,
							A2(author$project$Page$About$update, subMsg, about));
					} else {
						break _n0$11;
					}
				case 'GotSnapshotMsg':
					if (_n0.b.$ === 'Snapshot') {
						var subMsg = _n0.a.a;
						var subModel = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Snapshot,
							author$project$Main$GotSnapshotMsg,
							model,
							A2(author$project$Page$Snapshot$update, subMsg, subModel));
					} else {
						break _n0$11;
					}
				case 'GotTimelineMsg':
					if (_n0.b.$ === 'Timeline') {
						var subMsg = _n0.a.a;
						var subModel = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Timeline,
							author$project$Main$GotTimelineMsg,
							model,
							A2(author$project$Page$Timeline$update, subMsg, subModel));
					} else {
						break _n0$11;
					}
				case 'GotLiverMsg':
					if (_n0.b.$ === 'Liver') {
						var subMsg = _n0.a.a;
						var subModel = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Liver,
							author$project$Main$GotLiverMsg,
							model,
							A2(author$project$Page$Liver$update, subMsg, subModel));
					} else {
						break _n0$11;
					}
				case 'ChangedRoute':
					var route = _n0.a.a;
					return A2(author$project$Main$changeRouteTo, route, model);
				case 'PageCtrl':
					var pageCtrl = _n0.a.a;
					var oldSession = author$project$Main$toSession(model);
					var navState = pageCtrl.a;
					return _Utils_Tuple2(
						A2(
							author$project$Main$updateSession,
							model,
							_Utils_update(
								oldSession,
								{navState: navState})),
						elm$core$Platform$Cmd$none);
				default:
					var val = _n0.a.a;
					var _n5 = A2(elm$core$Debug$log, 'ScrollBottom!', _Utils_Tuple0);
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			}
		}
		var e = _n0;
		var _n6 = A2(elm$core$Debug$log, 'update: uncaught msg', e);
		return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
	});
var author$project$Main$Ignored = {$: 'Ignored'};
var author$project$Page$About = {$: 'About'};
var author$project$Page$Home = {$: 'Home'};
var author$project$Page$Other = {$: 'Other'};
var author$project$Page$Snapshot = {$: 'Snapshot'};
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$json$Json$Encode$string = _Json_wrap;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var elm$html$Html$a = _VirtualDom_node('a');
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var rundis$elm_bootstrap$Bootstrap$Navbar$Brand = function (a) {
	return {$: 'Brand', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$Config = function (a) {
	return {$: 'Config', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig = F2(
	function (mapper, _n0) {
		var conf = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Navbar$Config(
			mapper(conf));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$brand = F3(
	function (attributes, children, config_) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig,
			function (conf) {
				return _Utils_update(
					conf,
					{
						brand: elm$core$Maybe$Just(
							rundis$elm_bootstrap$Bootstrap$Navbar$Brand(
								A2(
									elm$html$Html$a,
									_Utils_ap(
										_List_fromArray(
											[
												elm$html$Html$Attributes$class('navbar-brand')
											]),
										attributes),
									children)))
					});
			},
			config_);
	});
var rundis$elm_bootstrap$Bootstrap$General$Internal$XS = {$: 'XS'};
var rundis$elm_bootstrap$Bootstrap$Internal$Role$Light = {$: 'Light'};
var rundis$elm_bootstrap$Bootstrap$Navbar$Light = {$: 'Light'};
var rundis$elm_bootstrap$Bootstrap$Navbar$Roled = function (a) {
	return {$: 'Roled', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$config = function (toMsg) {
	return rundis$elm_bootstrap$Bootstrap$Navbar$Config(
		{
			brand: elm$core$Maybe$Nothing,
			customItems: _List_Nil,
			items: _List_Nil,
			options: {
				attributes: _List_Nil,
				fix: elm$core$Maybe$Nothing,
				isContainer: false,
				scheme: elm$core$Maybe$Just(
					{
						bgColor: rundis$elm_bootstrap$Bootstrap$Navbar$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Role$Light),
						modifier: rundis$elm_bootstrap$Bootstrap$Navbar$Light
					}),
				toggleAt: rundis$elm_bootstrap$Bootstrap$General$Internal$XS
			},
			toMsg: toMsg,
			withAnimation: false
		});
};
var rundis$elm_bootstrap$Bootstrap$Navbar$updateOptions = F2(
	function (mapper, _n0) {
		var conf = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Navbar$Config(
			_Utils_update(
				conf,
				{
					options: mapper(conf.options)
				}));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$container = function (conf) {
	return A2(
		rundis$elm_bootstrap$Bootstrap$Navbar$updateOptions,
		function (opts) {
			return _Utils_update(
				opts,
				{isContainer: true});
		},
		conf);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$Item = function (a) {
	return {$: 'Item', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$itemLink = F2(
	function (attributes, children) {
		return rundis$elm_bootstrap$Bootstrap$Navbar$Item(
			{attributes: attributes, children: children});
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$items = F2(
	function (items_, config_) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig,
			function (conf) {
				return _Utils_update(
					conf,
					{items: items_});
			},
			config_);
	});
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$nav = _VirtualDom_node('nav');
var elm$html$Html$span = _VirtualDom_node('span');
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var rundis$elm_bootstrap$Bootstrap$Navbar$maybeBrand = function (brand_) {
	if (brand_.$ === 'Just') {
		var b = brand_.a.a;
		return _List_fromArray(
			[b]);
	} else {
		return _List_Nil;
	}
};
var elm$core$Basics$not = _Basics_not;
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$sizeToComparable = function (size) {
	switch (size.$) {
		case 'XS':
			return 1;
		case 'SM':
			return 2;
		case 'MD':
			return 3;
		case 'LG':
			return 4;
		default:
			return 5;
	}
};
var rundis$elm_bootstrap$Bootstrap$General$Internal$LG = {$: 'LG'};
var rundis$elm_bootstrap$Bootstrap$General$Internal$MD = {$: 'MD'};
var rundis$elm_bootstrap$Bootstrap$General$Internal$SM = {$: 'SM'};
var rundis$elm_bootstrap$Bootstrap$General$Internal$XL = {$: 'XL'};
var rundis$elm_bootstrap$Bootstrap$Navbar$toScreenSize = function (windowWidth) {
	return (windowWidth <= 576) ? rundis$elm_bootstrap$Bootstrap$General$Internal$XS : ((windowWidth <= 768) ? rundis$elm_bootstrap$Bootstrap$General$Internal$SM : ((windowWidth <= 992) ? rundis$elm_bootstrap$Bootstrap$General$Internal$MD : ((windowWidth <= 1200) ? rundis$elm_bootstrap$Bootstrap$General$Internal$LG : rundis$elm_bootstrap$Bootstrap$General$Internal$XL)));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu = F2(
	function (_n0, _n1) {
		var windowWidth = _n0.a.windowWidth;
		var options = _n1.options;
		var winMedia = function () {
			if (windowWidth.$ === 'Just') {
				var s = windowWidth.a;
				return rundis$elm_bootstrap$Bootstrap$Navbar$toScreenSize(s);
			} else {
				return rundis$elm_bootstrap$Bootstrap$General$Internal$XS;
			}
		}();
		return _Utils_cmp(
			rundis$elm_bootstrap$Bootstrap$Navbar$sizeToComparable(winMedia),
			rundis$elm_bootstrap$Bootstrap$Navbar$sizeToComparable(options.toggleAt)) > 0;
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$Shown = {$: 'Shown'};
var rundis$elm_bootstrap$Bootstrap$Navbar$StartDown = {$: 'StartDown'};
var rundis$elm_bootstrap$Bootstrap$Navbar$StartUp = {$: 'StartUp'};
var rundis$elm_bootstrap$Bootstrap$Navbar$visibilityTransition = F2(
	function (withAnimation_, visibility) {
		var _n0 = _Utils_Tuple2(withAnimation_, visibility);
		if (_n0.a) {
			switch (_n0.b.$) {
				case 'Hidden':
					var _n1 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$StartDown;
				case 'StartDown':
					var _n2 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingDown;
				case 'AnimatingDown':
					var _n3 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Shown;
				case 'Shown':
					var _n4 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$StartUp;
				case 'StartUp':
					var _n5 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingUp;
				default:
					var _n6 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Hidden;
			}
		} else {
			switch (_n0.b.$) {
				case 'Hidden':
					var _n7 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Shown;
				case 'Shown':
					var _n8 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Hidden;
				default:
					return rundis$elm_bootstrap$Bootstrap$Navbar$Hidden;
			}
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$transitionHandler = F2(
	function (state, configRec) {
		return elm$json$Json$Decode$succeed(
			configRec.toMsg(
				A2(
					rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
					function (s) {
						return _Utils_update(
							s,
							{
								visibility: A2(rundis$elm_bootstrap$Bootstrap$Navbar$visibilityTransition, configRec.withAnimation, s.visibility)
							});
					},
					state)));
	});
var elm$core$String$fromFloat = _String_fromNumber;
var rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle = function (maybeHeight) {
	var pixelHeight = A2(
		elm$core$Maybe$withDefault,
		'0',
		A2(
			elm$core$Maybe$map,
			function (v) {
				return elm$core$String$fromFloat(v) + 'px';
			},
			maybeHeight));
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'position', 'relative'),
			A2(elm$html$Html$Attributes$style, 'height', pixelHeight),
			A2(elm$html$Html$Attributes$style, 'width', '100%'),
			A2(elm$html$Html$Attributes$style, 'overflow', 'hidden'),
			A2(elm$html$Html$Attributes$style, '-webkit-transition-timing-function', 'ease'),
			A2(elm$html$Html$Attributes$style, '-o-transition-timing-function', 'ease'),
			A2(elm$html$Html$Attributes$style, 'transition-timing-function', 'ease'),
			A2(elm$html$Html$Attributes$style, '-webkit-transition-duration', '0.35s'),
			A2(elm$html$Html$Attributes$style, '-o-transition-duration', '0.35s'),
			A2(elm$html$Html$Attributes$style, 'transition-duration', '0.35s'),
			A2(elm$html$Html$Attributes$style, '-webkit-transition-property', 'height'),
			A2(elm$html$Html$Attributes$style, '-o-transition-property', 'height'),
			A2(elm$html$Html$Attributes$style, 'transition-property', 'height')
		]);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$menuAttributes = F2(
	function (state, configRec) {
		var visibility = state.a.visibility;
		var height = state.a.height;
		var defaults = _List_fromArray(
			[
				elm$html$Html$Attributes$class('collapse navbar-collapse')
			]);
		switch (visibility.$) {
			case 'Hidden':
				if (height.$ === 'Nothing') {
					return ((!configRec.withAnimation) || A2(rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu, state, configRec)) ? defaults : _List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'display', 'block'),
							A2(elm$html$Html$Attributes$style, 'height', '0'),
							A2(elm$html$Html$Attributes$style, 'overflow', 'hidden'),
							A2(elm$html$Html$Attributes$style, 'width', '100%')
						]);
				} else {
					return defaults;
				}
			case 'StartDown':
				return rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(elm$core$Maybe$Nothing);
			case 'AnimatingDown':
				return _Utils_ap(
					rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(height),
					_List_fromArray(
						[
							A2(
							elm$html$Html$Events$on,
							'transitionend',
							A2(rundis$elm_bootstrap$Bootstrap$Navbar$transitionHandler, state, configRec))
						]));
			case 'AnimatingUp':
				return _Utils_ap(
					rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(elm$core$Maybe$Nothing),
					_List_fromArray(
						[
							A2(
							elm$html$Html$Events$on,
							'transitionend',
							A2(rundis$elm_bootstrap$Bootstrap$Navbar$transitionHandler, state, configRec))
						]));
			case 'StartUp':
				return rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(height);
			default:
				return _Utils_ap(
					defaults,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('show')
						]));
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$menuWrapperAttributes = F2(
	function (state, confRec) {
		var visibility = state.a.visibility;
		var height = state.a.height;
		var styleBlock = _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'display', 'block'),
				A2(elm$html$Html$Attributes$style, 'width', '100%')
			]);
		var display = function () {
			if (height.$ === 'Nothing') {
				return ((!confRec.withAnimation) || A2(rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu, state, confRec)) ? 'flex' : 'block';
			} else {
				return 'flex';
			}
		}();
		switch (visibility.$) {
			case 'Hidden':
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', display),
						A2(elm$html$Html$Attributes$style, 'width', '100%')
					]);
			case 'StartDown':
				return styleBlock;
			case 'AnimatingDown':
				return styleBlock;
			case 'AnimatingUp':
				return styleBlock;
			case 'StartUp':
				return styleBlock;
			default:
				return ((!confRec.withAnimation) || A2(rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu, state, confRec)) ? _List_fromArray(
					[
						elm$html$Html$Attributes$class('collapse navbar-collapse show')
					]) : _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'block')
					]);
		}
	});
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$html$Html$Attributes$classList = function (classes) {
	return elm$html$Html$Attributes$class(
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$map,
				elm$core$Tuple$first,
				A2(elm$core$List$filter, elm$core$Tuple$second, classes))));
};
var rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption = function (size) {
	switch (size.$) {
		case 'XS':
			return elm$core$Maybe$Nothing;
		case 'SM':
			return elm$core$Maybe$Just('sm');
		case 'MD':
			return elm$core$Maybe$Just('md');
		case 'LG':
			return elm$core$Maybe$Just('lg');
		default:
			return elm$core$Maybe$Just('xl');
	}
};
var rundis$elm_bootstrap$Bootstrap$Navbar$expandOption = function (size) {
	var toClass = function (sz) {
		return elm$html$Html$Attributes$class(
			'navbar-expand' + A2(
				elm$core$Maybe$withDefault,
				'',
				A2(
					elm$core$Maybe$map,
					function (s) {
						return '-' + s;
					},
					rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(sz))));
	};
	switch (size.$) {
		case 'XS':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$SM)
				]);
		case 'SM':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$MD)
				]);
		case 'MD':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$LG)
				]);
		case 'LG':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$XL)
				]);
		default:
			return _List_Nil;
	}
};
var rundis$elm_bootstrap$Bootstrap$Navbar$fixOption = function (fix) {
	if (fix.$ === 'Top') {
		return 'fixed-top';
	} else {
		return 'fixed-bottom';
	}
};
var elm$core$Basics$round = _Basics_round;
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var avh4$elm_color$Color$toCssString = function (_n0) {
	var r = _n0.a;
	var g = _n0.b;
	var b = _n0.c;
	var a = _n0.d;
	var roundTo = function (x) {
		return elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return elm$core$Basics$round(x * 10000) / 100;
	};
	return elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				elm$core$String$fromFloat(
				pct(r)),
				'%,',
				elm$core$String$fromFloat(
				pct(g)),
				'%,',
				elm$core$String$fromFloat(
				pct(b)),
				'%,',
				elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass = F2(
	function (prefix, role) {
		return elm$html$Html$Attributes$class(
			prefix + ('-' + function () {
				switch (role.$) {
					case 'Primary':
						return 'primary';
					case 'Secondary':
						return 'secondary';
					case 'Success':
						return 'success';
					case 'Info':
						return 'info';
					case 'Warning':
						return 'warning';
					case 'Danger':
						return 'danger';
					case 'Light':
						return 'light';
					default:
						return 'dark';
				}
			}()));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$backgroundColorOption = function (bgClass) {
	switch (bgClass.$) {
		case 'Roled':
			var role = bgClass.a;
			return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role);
		case 'Custom':
			var color = bgClass.a;
			return A2(
				elm$html$Html$Attributes$style,
				'background-color',
				avh4$elm_color$Color$toCssString(color));
		default:
			var classString = bgClass.a;
			return elm$html$Html$Attributes$class(classString);
	}
};
var rundis$elm_bootstrap$Bootstrap$Navbar$linkModifierClass = function (modifier) {
	return elm$html$Html$Attributes$class(
		function () {
			if (modifier.$ === 'Dark') {
				return 'navbar-dark';
			} else {
				return 'navbar-light';
			}
		}());
};
var rundis$elm_bootstrap$Bootstrap$Navbar$schemeAttributes = function (_n0) {
	var modifier = _n0.modifier;
	var bgColor = _n0.bgColor;
	return _List_fromArray(
		[
			rundis$elm_bootstrap$Bootstrap$Navbar$linkModifierClass(modifier),
			rundis$elm_bootstrap$Bootstrap$Navbar$backgroundColorOption(bgColor)
		]);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$navbarAttributes = function (options) {
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('navbar', true),
						_Utils_Tuple2('container', options.isContainer)
					]))
			]),
		_Utils_ap(
			rundis$elm_bootstrap$Bootstrap$Navbar$expandOption(options.toggleAt),
			_Utils_ap(
				function () {
					var _n0 = options.scheme;
					if (_n0.$ === 'Just') {
						var scheme_ = _n0.a;
						return rundis$elm_bootstrap$Bootstrap$Navbar$schemeAttributes(scheme_);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _n1 = options.fix;
						if (_n1.$ === 'Just') {
							var fix = _n1.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									rundis$elm_bootstrap$Bootstrap$Navbar$fixOption(fix))
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.attributes))));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$renderCustom = function (items_) {
	return A2(
		elm$core$List$map,
		function (_n0) {
			var item = _n0.a;
			return item;
		},
		items_);
};
var elm$html$Html$ul = _VirtualDom_node('ul');
var elm$core$Basics$neq = _Utils_notEqual;
var elm$html$Html$li = _VirtualDom_node('li');
var rundis$elm_bootstrap$Bootstrap$Navbar$getOrInitDropdownStatus = F2(
	function (id, _n0) {
		var dropdowns = _n0.a.dropdowns;
		return A2(
			elm$core$Maybe$withDefault,
			rundis$elm_bootstrap$Bootstrap$Navbar$Closed,
			A2(elm$core$Dict$get, id, dropdowns));
	});
var elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$toggleOpen = F3(
	function (state, id, _n0) {
		var toMsg = _n0.toMsg;
		var currStatus = A2(rundis$elm_bootstrap$Bootstrap$Navbar$getOrInitDropdownStatus, id, state);
		var newStatus = function () {
			switch (currStatus.$) {
				case 'Open':
					return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
				case 'ListenClicks':
					return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
				default:
					return rundis$elm_bootstrap$Bootstrap$Navbar$Open;
			}
		}();
		return toMsg(
			A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
				function (s) {
					return _Utils_update(
						s,
						{
							dropdowns: A3(elm$core$Dict$insert, id, newStatus, s.dropdowns)
						});
				},
				state));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdownToggle = F4(
	function (state, id, configRec, _n0) {
		var attributes = _n0.a.attributes;
		var children = _n0.a.children;
		return A2(
			elm$html$Html$a,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('nav-link dropdown-toggle'),
						elm$html$Html$Attributes$href('#'),
						A2(
						elm$html$Html$Events$custom,
						'click',
						elm$json$Json$Decode$succeed(
							{
								message: A3(rundis$elm_bootstrap$Bootstrap$Navbar$toggleOpen, state, id, configRec),
								preventDefault: true,
								stopPropagation: false
							}))
					]),
				attributes),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdown = F3(
	function (state, configRec, _n0) {
		var ddRec = _n0.a;
		var needsDropup = A2(
			elm$core$Maybe$withDefault,
			false,
			A2(
				elm$core$Maybe$map,
				function (fix) {
					if (fix.$ === 'Bottom') {
						return true;
					} else {
						return false;
					}
				},
				configRec.options.fix));
		var isShown = !_Utils_eq(
			A2(rundis$elm_bootstrap$Bootstrap$Navbar$getOrInitDropdownStatus, ddRec.id, state),
			rundis$elm_bootstrap$Bootstrap$Navbar$Closed);
		return A2(
			elm$html$Html$li,
			_List_fromArray(
				[
					elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('nav-item', true),
							_Utils_Tuple2('dropdown', true),
							_Utils_Tuple2('shown', isShown),
							_Utils_Tuple2('dropup', needsDropup)
						]))
				]),
			_List_fromArray(
				[
					A4(rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdownToggle, state, ddRec.id, configRec, ddRec.toggle),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('dropdown-menu', true),
									_Utils_Tuple2('show', isShown)
								]))
						]),
					A2(
						elm$core$List$map,
						function (_n1) {
							var item = _n1.a;
							return item;
						},
						ddRec.items))
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$renderItemLink = function (_n0) {
	var attributes = _n0.attributes;
	var children = _n0.children;
	return A2(
		elm$html$Html$li,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('nav-item')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_Utils_ap(
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('nav-link')
						]),
					attributes),
				children)
			]));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$renderNav = F3(
	function (state, configRec, navItems) {
		return A2(
			elm$html$Html$ul,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('navbar-nav mr-auto')
				]),
			A2(
				elm$core$List$map,
				function (item) {
					if (item.$ === 'Item') {
						var item_ = item.a;
						return rundis$elm_bootstrap$Bootstrap$Navbar$renderItemLink(item_);
					} else {
						var dropdown_ = item.a;
						return A3(rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdown, state, configRec, dropdown_);
					}
				},
				navItems));
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$json$Json$Decode$decodeValue = _Json_run;
var elm$json$Json$Decode$fail = _Json_fail;
var elm$json$Json$Decode$float = _Json_decodeFloat;
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'parentElement', decoder);
};
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'target', decoder);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$heightDecoder = function () {
	var tagDecoder = A3(
		elm$json$Json$Decode$map2,
		F2(
			function (tag, val) {
				return _Utils_Tuple2(tag, val);
			}),
		A2(elm$json$Json$Decode$field, 'tagName', elm$json$Json$Decode$string),
		elm$json$Json$Decode$value);
	var resToDec = function (res) {
		if (res.$ === 'Ok') {
			var v = res.a;
			return elm$json$Json$Decode$succeed(v);
		} else {
			var err = res.a;
			return elm$json$Json$Decode$fail(
				elm$json$Json$Decode$errorToString(err));
		}
	};
	var fromNavDec = elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				elm$json$Json$Decode$at,
				_List_fromArray(
					['childNodes', '2', 'childNodes', '0', 'offsetHeight']),
				elm$json$Json$Decode$float),
				A2(
				elm$json$Json$Decode$at,
				_List_fromArray(
					['childNodes', '1', 'childNodes', '0', 'offsetHeight']),
				elm$json$Json$Decode$float)
			]));
	var fromButtonDec = rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement(fromNavDec);
	return A2(
		elm$json$Json$Decode$andThen,
		function (_n0) {
			var tag = _n0.a;
			var val = _n0.b;
			switch (tag) {
				case 'NAV':
					return resToDec(
						A2(elm$json$Json$Decode$decodeValue, fromNavDec, val));
				case 'BUTTON':
					return resToDec(
						A2(elm$json$Json$Decode$decodeValue, fromButtonDec, val));
				default:
					return elm$json$Json$Decode$succeed(0);
			}
		},
		rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target(
			rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement(tagDecoder)));
}();
var rundis$elm_bootstrap$Bootstrap$Navbar$toggleHandler = F2(
	function (state, configRec) {
		var height = state.a.height;
		var updState = function (h) {
			return A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
				function (s) {
					return _Utils_update(
						s,
						{
							height: elm$core$Maybe$Just(h),
							visibility: A2(rundis$elm_bootstrap$Bootstrap$Navbar$visibilityTransition, configRec.withAnimation, s.visibility)
						});
				},
				state);
		};
		return A2(
			elm$html$Html$Events$on,
			'click',
			A2(
				elm$json$Json$Decode$andThen,
				function (v) {
					return elm$json$Json$Decode$succeed(
						configRec.toMsg(
							(v > 0) ? updState(v) : updState(
								A2(elm$core$Maybe$withDefault, 0, height))));
				},
				rundis$elm_bootstrap$Bootstrap$Navbar$heightDecoder));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$view = F2(
	function (state, conf) {
		var configRec = conf.a;
		return A2(
			elm$html$Html$nav,
			rundis$elm_bootstrap$Bootstrap$Navbar$navbarAttributes(configRec.options),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Navbar$maybeBrand(configRec.brand),
				_Utils_ap(
					_List_fromArray(
						[
							A2(
							elm$html$Html$button,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'navbar-toggler' + A2(
										elm$core$Maybe$withDefault,
										'',
										A2(
											elm$core$Maybe$map,
											function (_n0) {
												return ' navbar-toggler-right';
											},
											configRec.brand))),
									elm$html$Html$Attributes$type_('button'),
									A2(rundis$elm_bootstrap$Bootstrap$Navbar$toggleHandler, state, configRec)
								]),
							_List_fromArray(
								[
									A2(
									elm$html$Html$span,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('navbar-toggler-icon')
										]),
									_List_Nil)
								]))
						]),
					_List_fromArray(
						[
							A2(
							elm$html$Html$div,
							A2(rundis$elm_bootstrap$Bootstrap$Navbar$menuAttributes, state, configRec),
							_List_fromArray(
								[
									A2(
									elm$html$Html$div,
									A2(rundis$elm_bootstrap$Bootstrap$Navbar$menuWrapperAttributes, state, configRec),
									_Utils_ap(
										_List_fromArray(
											[
												A3(rundis$elm_bootstrap$Bootstrap$Navbar$renderNav, state, configRec, configRec.items)
											]),
										rundis$elm_bootstrap$Bootstrap$Navbar$renderCustom(configRec.customItems)))
								]))
						]))));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$withAnimation = function (config_) {
	return A2(
		rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig,
		function (conf) {
			return _Utils_update(
				conf,
				{withAnimation: true});
		},
		config_);
};
var author$project$Page$viewMenu = F2(
	function (handler, session) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$view,
			session.navState,
			A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$items,
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Navbar$itemLink,
						_List_fromArray(
							[
								elm$html$Html$Attributes$href('#/timeline')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Timeline')
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Navbar$itemLink,
						_List_fromArray(
							[
								elm$html$Html$Attributes$href('#/liver')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Liver')
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Navbar$itemLink,
						_List_fromArray(
							[
								elm$html$Html$Attributes$href('#/snapshot')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Snapshot')
							]))
					]),
				A3(
					rundis$elm_bootstrap$Bootstrap$Navbar$brand,
					_List_fromArray(
						[
							elm$html$Html$Attributes$href('#')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('にじさんじ名前更新bot')
						]),
					rundis$elm_bootstrap$Bootstrap$Navbar$container(
						rundis$elm_bootstrap$Bootstrap$Navbar$withAnimation(
							rundis$elm_bootstrap$Bootstrap$Navbar$config(handler.navbarHandler))))));
	});
var author$project$Page$view = F4(
	function (session, page, handler, _n0) {
		var title = _n0.title;
		var content = _n0.content;
		return {
			body: _List_fromArray(
				[
					A2(author$project$Page$viewMenu, handler, session),
					content
				]),
			title: title + ' - にじさんじ名前更新bot'
		};
	});
var author$project$Page$About$Decrement = {$: 'Decrement'};
var author$project$Page$About$Increment = {$: 'Increment'};
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$html$Html$main_ = _VirtualDom_node('main');
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Page$About$view = function (model) {
	return {
		content: A2(
			elm$html$Html$main_,
			_List_fromArray(
				[
					elm$html$Html$Attributes$id('content'),
					elm$html$Html$Attributes$class('container'),
					elm$html$Html$Attributes$tabindex(-1)
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$button,
					_List_fromArray(
						[
							elm$html$Html$Events$onClick(author$project$Page$About$Decrement)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('-')
						])),
					A2(
					elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							elm$html$Html$text(
							elm$core$String$fromInt(model.counter))
						])),
					A2(
					elm$html$Html$button,
					_List_fromArray(
						[
							elm$html$Html$Events$onClick(author$project$Page$About$Increment)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('+')
						]))
				])),
		title: 'About'
	};
};
var author$project$Page$Blank$view = {
	content: elm$html$Html$text(''),
	title: ''
};
var author$project$Page$Home$viewLink = function (path) {
	return A2(
		elm$html$Html$li,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(path)
					]),
				_List_fromArray(
					[
						elm$html$Html$text(path)
					]))
			]));
};
var author$project$Page$Home$view = function (model) {
	return {
		content: A2(
			elm$html$Html$main_,
			_List_fromArray(
				[
					elm$html$Html$Attributes$id('content'),
					elm$html$Html$Attributes$class('container'),
					elm$html$Html$Attributes$tabindex(-1)
				]),
			_List_fromArray(
				[
					elm$html$Html$text('Homeによよようこここそそそそそ'),
					A2(
					elm$html$Html$ul,
					_List_Nil,
					_List_fromArray(
						[
							author$project$Page$Home$viewLink('/#/home'),
							author$project$Page$Home$viewLink('/#/about'),
							author$project$Page$Home$viewLink('/#/snapshot'),
							author$project$Page$Home$viewLink('/#/timeline'),
							author$project$Page$Home$viewLink('/#/liver'),
							author$project$Page$Home$viewLink('http://abehiroshi.la.coocan.jp')
						]))
				])),
		title: 'Home'
	};
};
var author$project$Nijisanji$allMembersMap = elm$core$Dict$fromList(
	A2(
		elm$core$List$map,
		function (member) {
			return _Utils_Tuple2(member.name, member);
		},
		author$project$Nijisanji$allMembers));
var author$project$Nijisanji$getScreenName = function (name) {
	return A2(
		elm$core$Maybe$map,
		function (m) {
			return m.screenName;
		},
		A2(elm$core$Dict$get, name, author$project$Nijisanji$allMembersMap));
};
var author$project$Page$Liver$dataToNameInfo = function (data) {
	return A2(
		elm$core$List$map,
		function (_n0) {
			var name = _n0.a;
			var updateInfo = _n0.b;
			return {
				liverName: name,
				liverTwitterScreenName: function () {
					var _n1 = author$project$Nijisanji$getScreenName(name);
					if (_n1.$ === 'Nothing') {
						return A2(elm$core$Debug$log, 'ほんまかー!', '');
					} else {
						var screenName = _n1.a;
						return screenName;
					}
				}(),
				liverUpdateInfo: elm$core$Maybe$Just(updateInfo)
			};
		},
		data);
};
var author$project$Nijisanji$gamersMembers = _List_fromArray(
	[author$project$Nijisanji$setsuna, author$project$Nijisanji$shiinaYuika, author$project$Nijisanji$makainoRirimu, author$project$Nijisanji$sasakiSaku, author$project$Nijisanji$honmaHimawari, author$project$Nijisanji$yamiyonoMoruru, author$project$Nijisanji$akabaneYouko, author$project$Nijisanji$kanae, author$project$Nijisanji$kuzuha]);
var author$project$Nijisanji$nijisanjiMembers = _List_fromArray(
	[author$project$Nijisanji$tsukinoMito, author$project$Nijisanji$higuchiKaede, author$project$Nijisanji$shizukaRin, author$project$Nijisanji$suzuyaAki, author$project$Nijisanji$moira, author$project$Nijisanji$elu, author$project$Nijisanji$shibuyaHajime, author$project$Nijisanji$yukiChihiro, author$project$Nijisanji$mononobeAlice, author$project$Nijisanji$gilzarenIII, author$project$Nijisanji$yuhiRiri, author$project$Nijisanji$ienagaMugi, author$project$Nijisanji$kenmochiToya, author$project$Nijisanji$fushimiGaku, author$project$Nijisanji$ushimiIchigo, author$project$Nijisanji$fuminoTamaki, author$project$Nijisanji$suzukaUtako, author$project$Nijisanji$morinakaKazaki]);
var author$project$Nijisanji$kudouChitose = A4(author$project$Nijisanji$Member, '久遠千歳', author$project$Nijisanji$SEEDs, 'kudou_chitose', 1081927064033185794);
var author$project$Nijisanji$warabedaMeiji = A4(author$project$Nijisanji$Member, '童田明治', author$project$Nijisanji$SEEDs, 'warabeda_meiji', 1082065005061652480);
var author$project$Nijisanji$seedsMembers = _List_fromArray(
	[author$project$Nijisanji$dola, author$project$Nijisanji$umiyashanokami, author$project$Nijisanji$nakaoAzuma, author$project$Nijisanji$izumoKasumi, author$project$Nijisanji$todorokiKyoko, author$project$Nijisanji$sisterCleaire, author$project$Nijisanji$hanabatakeChaika, author$project$Nijisanji$yashiroKizuku, author$project$Nijisanji$hassakuYuzu, author$project$Nijisanji$aduchiMomo, author$project$Nijisanji$udukiKou, author$project$Nijisanji$suzukiMasaru, author$project$Nijisanji$ryushen, author$project$Nijisanji$harusakiAir, author$project$Nijisanji$kandaShoichi, author$project$Nijisanji$amemoriSayo, author$project$Nijisanji$narutoKogane, author$project$Nijisanji$takamiyaRion, author$project$Nijisanji$asukaHina, author$project$Nijisanji$maimotoKeisuke, author$project$Nijisanji$debidebiDebiru, author$project$Nijisanji$rindouMikoto, author$project$Nijisanji$joeRikiichi, author$project$Nijisanji$machitaChima, author$project$Nijisanji$tsukimiShizuku, author$project$Nijisanji$sakuraRitsuki, author$project$Nijisanji$achikitaChinami, author$project$Nijisanji$yumeoiKakeru, author$project$Nijisanji$belmondBanderas, author$project$Nijisanji$naruseNaru, author$project$Nijisanji$yagurumaRine, author$project$Nijisanji$kuroiShiba, author$project$Nijisanji$warabedaMeiji, author$project$Nijisanji$kudouChitose]);
var author$project$Nijisanji$groupMembers = function (g) {
	switch (g.$) {
		case 'Nijisanji':
			return author$project$Nijisanji$nijisanjiMembers;
		case 'Gamers':
			return author$project$Nijisanji$gamersMembers;
		default:
			return author$project$Nijisanji$seedsMembers;
	}
};
var author$project$Route$href = function (targetRoute) {
	return elm$html$Html$Attributes$href(
		author$project$Route$routeToString(targetRoute));
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs = function (a) {
	return {$: 'Attrs', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Button$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs(attrs_);
};
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size = modifier.a;
				return _Utils_update(
					options,
					{
						size: elm$core$Maybe$Just(size)
					});
			case 'Coloring':
				var coloring = modifier.a;
				return _Utils_update(
					options,
					{
						coloring: elm$core$Maybe$Just(coloring)
					});
			case 'Block':
				return _Utils_update(
					options,
					{block: true});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			default:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions = {attributes: _List_Nil, block: false, coloring: elm$core$Maybe$Nothing, disabled: false, size: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass = function (role) {
	switch (role.$) {
		case 'Primary':
			return 'primary';
		case 'Secondary':
			return 'secondary';
		case 'Success':
			return 'success';
		case 'Info':
			return 'info';
		case 'Warning':
			return 'warning';
		case 'Danger':
			return 'danger';
		case 'Dark':
			return 'dark';
		case 'Light':
			return 'light';
		default:
			return 'link';
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier, rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('btn', true),
						_Utils_Tuple2('btn-block', options.block),
						_Utils_Tuple2('disabled', options.disabled)
					])),
				elm$html$Html$Attributes$disabled(options.disabled)
			]),
		_Utils_ap(
			function () {
				var _n0 = A2(elm$core$Maybe$andThen, rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption, options.size);
				if (_n0.$ === 'Just') {
					var s = _n0.a;
					return _List_fromArray(
						[
							elm$html$Html$Attributes$class('btn-' + s)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _n1 = options.coloring;
					if (_n1.$ === 'Just') {
						if (_n1.a.$ === 'Roled') {
							var role = _n1.a.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'btn-' + rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						} else {
							var role = _n1.a.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'btn-outline-' + rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						}
					} else {
						return _List_Nil;
					}
				}(),
				options.attributes)));
};
var rundis$elm_bootstrap$Bootstrap$Button$linkButton = F2(
	function (options, children) {
		return A2(
			elm$html$Html$a,
			A2(
				elm$core$List$cons,
				A2(elm$html$Html$Attributes$attribute, 'role', 'button'),
				rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options)),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring = function (a) {
	return {$: 'Coloring', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled = function (a) {
	return {$: 'Roled', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Secondary = {$: 'Secondary'};
var rundis$elm_bootstrap$Bootstrap$Button$secondary = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Button$Secondary));
var rundis$elm_bootstrap$Bootstrap$Card$Config = function (a) {
	return {$: 'Config', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$CardBlock = function (a) {
	return {$: 'CardBlock', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$applyBlockModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 'AlignedBlock':
				var align = option.a;
				return _Utils_update(
					options,
					{
						aligned: elm$core$Maybe$Just(align)
					});
			case 'BlockColoring':
				var role = option.a;
				return _Utils_update(
					options,
					{
						coloring: elm$core$Maybe$Just(role)
					});
			case 'BlockTextColoring':
				var color = option.a;
				return _Utils_update(
					options,
					{
						textColoring: elm$core$Maybe$Just(color)
					});
			default:
				var attrs = option.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultBlockOptions = {aligned: elm$core$Maybe$Nothing, attributes: _List_Nil, coloring: elm$core$Maybe$Nothing, textColoring: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption = function (dir) {
	switch (dir.$) {
		case 'Center':
			return 'center';
		case 'Left':
			return 'left';
		default:
			return 'right';
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass = function (_n0) {
	var dir = _n0.dir;
	var size = _n0.size;
	return elm$html$Html$Attributes$class(
		'text' + (A2(
			elm$core$Maybe$withDefault,
			'-',
			A2(
				elm$core$Maybe$map,
				function (s) {
					return '-' + (s + '-');
				},
				rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size))) + rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption(dir)));
};
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textColorClass = function (color) {
	if (color.$ === 'White') {
		return elm$html$Html$Attributes$class('text-white');
	} else {
		var role = color.a;
		return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'text', role);
	}
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$blockAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Card$Internal$applyBlockModifier, rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultBlockOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('card-body')
			]),
		_Utils_ap(
			function () {
				var _n0 = options.aligned;
				if (_n0.$ === 'Just') {
					var align = _n0.a;
					return _List_fromArray(
						[
							rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(align)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _n1 = options.coloring;
					if (_n1.$ === 'Just') {
						var role = _n1.a;
						return _List_fromArray(
							[
								A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _n2 = options.textColoring;
						if (_n2.$ === 'Just') {
							var color = _n2.a;
							return _List_fromArray(
								[
									rundis$elm_bootstrap$Bootstrap$Internal$Text$textColorClass(color)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.attributes))));
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$block = F2(
	function (options, items) {
		return rundis$elm_bootstrap$Bootstrap$Card$Internal$CardBlock(
			A2(
				elm$html$Html$div,
				rundis$elm_bootstrap$Bootstrap$Card$Internal$blockAttributes(options),
				A2(
					elm$core$List$map,
					function (_n0) {
						var e = _n0.a;
						return e;
					},
					items)));
	});
var rundis$elm_bootstrap$Bootstrap$Card$block = F3(
	function (options, items, _n0) {
		var conf = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Card$Config(
			_Utils_update(
				conf,
				{
					blocks: _Utils_ap(
						conf.blocks,
						_List_fromArray(
							[
								A2(rundis$elm_bootstrap$Bootstrap$Card$Internal$block, options, items)
							]))
				}));
	});
var rundis$elm_bootstrap$Bootstrap$Card$config = function (options) {
	return rundis$elm_bootstrap$Bootstrap$Card$Config(
		{blocks: _List_Nil, footer: elm$core$Maybe$Nothing, header: elm$core$Maybe$Nothing, imgBottom: elm$core$Maybe$Nothing, imgTop: elm$core$Maybe$Nothing, options: options});
};
var elm$html$Html$h4 = _VirtualDom_node('h4');
var rundis$elm_bootstrap$Bootstrap$Card$Header = function (a) {
	return {$: 'Header', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Card$headerPrivate = F4(
	function (elemFn, attributes, children, _n0) {
		var conf = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Card$Config(
			_Utils_update(
				conf,
				{
					header: elm$core$Maybe$Just(
						rundis$elm_bootstrap$Bootstrap$Card$Header(
							A2(
								elemFn,
								A2(
									elm$core$List$cons,
									elm$html$Html$Attributes$class('card-header'),
									attributes),
								children)))
				}));
	});
var rundis$elm_bootstrap$Bootstrap$Card$headerH4 = rundis$elm_bootstrap$Bootstrap$Card$headerPrivate(elm$html$Html$h4);
var rundis$elm_bootstrap$Bootstrap$Card$Internal$applyModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 'Aligned':
				var align = option.a;
				return _Utils_update(
					options,
					{
						aligned: elm$core$Maybe$Just(align)
					});
			case 'Coloring':
				var coloring = option.a;
				return _Utils_update(
					options,
					{
						coloring: elm$core$Maybe$Just(coloring)
					});
			case 'TextColoring':
				var coloring = option.a;
				return _Utils_update(
					options,
					{
						textColoring: elm$core$Maybe$Just(coloring)
					});
			default:
				var attrs = option.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultOptions = {aligned: elm$core$Maybe$Nothing, attributes: _List_Nil, coloring: elm$core$Maybe$Nothing, textColoring: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$cardAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Card$Internal$applyModifier, rundis$elm_bootstrap$Bootstrap$Card$Internal$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('card')
			]),
		_Utils_ap(
			function () {
				var _n0 = options.coloring;
				if (_n0.$ === 'Just') {
					if (_n0.a.$ === 'Roled') {
						var role = _n0.a.a;
						return _List_fromArray(
							[
								A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role)
							]);
					} else {
						var role = _n0.a.a;
						return _List_fromArray(
							[
								A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'border', role)
							]);
					}
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _n1 = options.textColoring;
					if (_n1.$ === 'Just') {
						var color = _n1.a;
						return _List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$Internal$Text$textColorClass(color)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _n2 = options.aligned;
						if (_n2.$ === 'Just') {
							var align = _n2.a;
							return _List_fromArray(
								[
									rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(align)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.attributes))));
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$renderBlocks = function (blocks) {
	return A2(
		elm$core$List$map,
		function (block_) {
			if (block_.$ === 'CardBlock') {
				var e = block_.a;
				return e;
			} else {
				var e = block_.a;
				return e;
			}
		},
		blocks);
};
var rundis$elm_bootstrap$Bootstrap$Card$view = function (_n0) {
	var conf = _n0.a;
	return A2(
		elm$html$Html$div,
		rundis$elm_bootstrap$Bootstrap$Card$Internal$cardAttributes(conf.options),
		_Utils_ap(
			A2(
				elm$core$List$filterMap,
				elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						elm$core$Maybe$map,
						function (_n1) {
							var e = _n1.a;
							return e;
						},
						conf.header),
						A2(
						elm$core$Maybe$map,
						function (_n2) {
							var e = _n2.a;
							return e;
						},
						conf.imgTop)
					])),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Card$Internal$renderBlocks(conf.blocks),
				A2(
					elm$core$List$filterMap,
					elm$core$Basics$identity,
					_List_fromArray(
						[
							A2(
							elm$core$Maybe$map,
							function (_n3) {
								var e = _n3.a;
								return e;
							},
							conf.footer),
							A2(
							elm$core$Maybe$map,
							function (_n4) {
								var e = _n4.a;
								return e;
							},
							conf.imgBottom)
						])))));
};
var rundis$elm_bootstrap$Bootstrap$Card$Internal$BlockItem = function (a) {
	return {$: 'BlockItem', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Card$Block$custom = function (element) {
	return rundis$elm_bootstrap$Bootstrap$Card$Internal$BlockItem(element);
};
var author$project$Page$Liver$viewLiverList = function () {
	var mkGroup = F2(
		function (g, name) {
			var mkSubBottun = function (liverName) {
				return A2(
					rundis$elm_bootstrap$Bootstrap$Button$linkButton,
					_List_fromArray(
						[
							rundis$elm_bootstrap$Bootstrap$Button$secondary,
							rundis$elm_bootstrap$Bootstrap$Button$attrs(
							_List_fromArray(
								[
									author$project$Route$href(
									author$project$Route$Liver(
										elm$core$Maybe$Just(liverName)))
								]))
						]),
					_List_fromArray(
						[
							elm$html$Html$text(liverName)
						]));
			};
			var groupMembersName = A2(
				elm$core$List$map,
				function (m) {
					return m.name;
				},
				author$project$Nijisanji$groupMembers(g));
			return A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Card$view(
						A3(
							rundis$elm_bootstrap$Bootstrap$Card$block,
							_List_Nil,
							_List_fromArray(
								[
									rundis$elm_bootstrap$Bootstrap$Card$Block$custom(
									A2(
										elm$html$Html$div,
										_List_Nil,
										A2(elm$core$List$map, mkSubBottun, groupMembersName)))
								]),
							A3(
								rundis$elm_bootstrap$Bootstrap$Card$headerH4,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(name)
									]),
								rundis$elm_bootstrap$Bootstrap$Card$config(_List_Nil))))
					]));
		});
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('contents-filter')
			]),
		_List_fromArray(
			[
				A2(mkGroup, author$project$Nijisanji$Nijisanji, '一期生二期生出身'),
				A2(mkGroup, author$project$Nijisanji$Gamers, 'ゲーマーズ出身'),
				A2(mkGroup, author$project$Nijisanji$SEEDs, 'SEEDs出身+α')
			]));
}();
var elm$html$Html$img = _VirtualDom_node('img');
var elm$html$Html$Attributes$src = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var stephenreddek$elm_emoji$Emoji$twemojiBaseUrl = 'https://twemoji.maxcdn.com/2/72x72/';
var elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						elm$core$List$cons,
						sep,
						A2(elm$core$List$cons, x, rest));
				});
			var spersed = A3(elm$core$List$foldr, step, _List_Nil, tl);
			return A2(elm$core$List$cons, hd, spersed);
		}
	});
var stephenreddek$elm_emoji$Emoji$urlWithBase = F2(
	function (base, codepts) {
		return base + (A2(
			elm$core$String$join,
			'',
			A2(elm$core$List$intersperse, '-', codepts)) + '.png');
	});
var stephenreddek$elm_emoji$Emoji$replaceWithTwemoji = function (codepts) {
	return A2(
		elm$html$Html$img,
		_List_fromArray(
			[
				elm$html$Html$Attributes$src(
				A2(stephenreddek$elm_emoji$Emoji$urlWithBase, stephenreddek$elm_emoji$Emoji$twemojiBaseUrl, codepts)),
				elm$html$Html$Attributes$class('elm-emoji-img elm-emoji-twem')
			]),
		_List_Nil);
};
var stephenreddek$elm_emoji$Emoji$Internal$Parse$String_ = function (a) {
	return {$: 'String_', a: a};
};
var elm$core$String$cons = _String_cons;
var elm$core$String$reverse = _String_reverse;
var stephenreddek$elm_emoji$Emoji$Internal$Parse$CodeChunk = function (a) {
	return {$: 'CodeChunk', a: a};
};
var stephenreddek$elm_emoji$Emoji$Internal$Parse$StringChunk = function (a) {
	return {$: 'StringChunk', a: a};
};
var stephenreddek$elm_emoji$Emoji$Internal$Parse$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A2(
			elm$core$Maybe$withDefault,
			string,
			A2(
				elm$core$Maybe$map,
				A2(
					elm$core$Basics$composeR,
					elm$core$Tuple$second,
					stephenreddek$elm_emoji$Emoji$Internal$Parse$dropLeft(n - 1)),
				elm$core$String$uncons(string)));
	});
var stephenreddek$elm_emoji$Emoji$Internal$Valid$pairs = _Utils_ap(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'😀',
			_List_fromArray(
				['1f600'])),
			_Utils_Tuple2(
			'😁',
			_List_fromArray(
				['1f601'])),
			_Utils_Tuple2(
			'😂',
			_List_fromArray(
				['1f602'])),
			_Utils_Tuple2(
			'\ud83e\udd23',
			_List_fromArray(
				['1f923'])),
			_Utils_Tuple2(
			'😃',
			_List_fromArray(
				['1f603'])),
			_Utils_Tuple2(
			'😄',
			_List_fromArray(
				['1f604'])),
			_Utils_Tuple2(
			'😅',
			_List_fromArray(
				['1f605'])),
			_Utils_Tuple2(
			'😆',
			_List_fromArray(
				['1f606'])),
			_Utils_Tuple2(
			'😉',
			_List_fromArray(
				['1f609'])),
			_Utils_Tuple2(
			'😊',
			_List_fromArray(
				['1f60a'])),
			_Utils_Tuple2(
			'😋',
			_List_fromArray(
				['1f60b'])),
			_Utils_Tuple2(
			'😎',
			_List_fromArray(
				['1f60e'])),
			_Utils_Tuple2(
			'😍',
			_List_fromArray(
				['1f60d'])),
			_Utils_Tuple2(
			'😘',
			_List_fromArray(
				['1f618'])),
			_Utils_Tuple2(
			'😗',
			_List_fromArray(
				['1f617'])),
			_Utils_Tuple2(
			'😙',
			_List_fromArray(
				['1f619'])),
			_Utils_Tuple2(
			'😚',
			_List_fromArray(
				['1f61a'])),
			_Utils_Tuple2(
			'☺',
			_List_fromArray(
				['263a'])),
			_Utils_Tuple2(
			'🙂',
			_List_fromArray(
				['1f642'])),
			_Utils_Tuple2(
			'\ud83e\udd17',
			_List_fromArray(
				['1f917'])),
			_Utils_Tuple2(
			'\ud83e\udd14',
			_List_fromArray(
				['1f914'])),
			_Utils_Tuple2(
			'😐',
			_List_fromArray(
				['1f610'])),
			_Utils_Tuple2(
			'😑',
			_List_fromArray(
				['1f611'])),
			_Utils_Tuple2(
			'😶',
			_List_fromArray(
				['1f636'])),
			_Utils_Tuple2(
			'\ud83d\ude44',
			_List_fromArray(
				['1f644'])),
			_Utils_Tuple2(
			'😏',
			_List_fromArray(
				['1f60f'])),
			_Utils_Tuple2(
			'😣',
			_List_fromArray(
				['1f623'])),
			_Utils_Tuple2(
			'😥',
			_List_fromArray(
				['1f625'])),
			_Utils_Tuple2(
			'😮',
			_List_fromArray(
				['1f62e'])),
			_Utils_Tuple2(
			'\ud83e\udd10',
			_List_fromArray(
				['1f910'])),
			_Utils_Tuple2(
			'😯',
			_List_fromArray(
				['1f62f'])),
			_Utils_Tuple2(
			'😪',
			_List_fromArray(
				['1f62a'])),
			_Utils_Tuple2(
			'😫',
			_List_fromArray(
				['1f62b'])),
			_Utils_Tuple2(
			'😴',
			_List_fromArray(
				['1f634'])),
			_Utils_Tuple2(
			'😌',
			_List_fromArray(
				['1f60c'])),
			_Utils_Tuple2(
			'\ud83e\udd13',
			_List_fromArray(
				['1f913'])),
			_Utils_Tuple2(
			'😛',
			_List_fromArray(
				['1f61b'])),
			_Utils_Tuple2(
			'😜',
			_List_fromArray(
				['1f61c'])),
			_Utils_Tuple2(
			'😝',
			_List_fromArray(
				['1f61d'])),
			_Utils_Tuple2(
			'\ud83e\udd24',
			_List_fromArray(
				['1f924'])),
			_Utils_Tuple2(
			'😒',
			_List_fromArray(
				['1f612'])),
			_Utils_Tuple2(
			'😓',
			_List_fromArray(
				['1f613'])),
			_Utils_Tuple2(
			'😔',
			_List_fromArray(
				['1f614'])),
			_Utils_Tuple2(
			'😕',
			_List_fromArray(
				['1f615'])),
			_Utils_Tuple2(
			'\ud83d\ude43',
			_List_fromArray(
				['1f643'])),
			_Utils_Tuple2(
			'\ud83e\udd11',
			_List_fromArray(
				['1f911'])),
			_Utils_Tuple2(
			'😲',
			_List_fromArray(
				['1f632'])),
			_Utils_Tuple2(
			'☹',
			_List_fromArray(
				['2639'])),
			_Utils_Tuple2(
			'🙁',
			_List_fromArray(
				['1f641'])),
			_Utils_Tuple2(
			'😖',
			_List_fromArray(
				['1f616'])),
			_Utils_Tuple2(
			'😞',
			_List_fromArray(
				['1f61e'])),
			_Utils_Tuple2(
			'😟',
			_List_fromArray(
				['1f61f'])),
			_Utils_Tuple2(
			'😤',
			_List_fromArray(
				['1f624'])),
			_Utils_Tuple2(
			'😢',
			_List_fromArray(
				['1f622'])),
			_Utils_Tuple2(
			'😭',
			_List_fromArray(
				['1f62d'])),
			_Utils_Tuple2(
			'😦',
			_List_fromArray(
				['1f626'])),
			_Utils_Tuple2(
			'😧',
			_List_fromArray(
				['1f627'])),
			_Utils_Tuple2(
			'😨',
			_List_fromArray(
				['1f628'])),
			_Utils_Tuple2(
			'😩',
			_List_fromArray(
				['1f629'])),
			_Utils_Tuple2(
			'😬',
			_List_fromArray(
				['1f62c'])),
			_Utils_Tuple2(
			'😰',
			_List_fromArray(
				['1f630'])),
			_Utils_Tuple2(
			'😱',
			_List_fromArray(
				['1f631'])),
			_Utils_Tuple2(
			'😳',
			_List_fromArray(
				['1f633'])),
			_Utils_Tuple2(
			'😵',
			_List_fromArray(
				['1f635'])),
			_Utils_Tuple2(
			'😡',
			_List_fromArray(
				['1f621'])),
			_Utils_Tuple2(
			'😠',
			_List_fromArray(
				['1f620'])),
			_Utils_Tuple2(
			'😇',
			_List_fromArray(
				['1f607'])),
			_Utils_Tuple2(
			'\ud83e\udd20',
			_List_fromArray(
				['1f920'])),
			_Utils_Tuple2(
			'\ud83e\udd21',
			_List_fromArray(
				['1f921'])),
			_Utils_Tuple2(
			'\ud83e\udd25',
			_List_fromArray(
				['1f925'])),
			_Utils_Tuple2(
			'😷',
			_List_fromArray(
				['1f637'])),
			_Utils_Tuple2(
			'\ud83e\udd12',
			_List_fromArray(
				['1f912'])),
			_Utils_Tuple2(
			'\ud83e\udd15',
			_List_fromArray(
				['1f915'])),
			_Utils_Tuple2(
			'\ud83e\udd22',
			_List_fromArray(
				['1f922'])),
			_Utils_Tuple2(
			'\ud83e\udd27',
			_List_fromArray(
				['1f927'])),
			_Utils_Tuple2(
			'😈',
			_List_fromArray(
				['1f608'])),
			_Utils_Tuple2(
			'👿',
			_List_fromArray(
				['1f47f'])),
			_Utils_Tuple2(
			'👹',
			_List_fromArray(
				['1f479'])),
			_Utils_Tuple2(
			'👺',
			_List_fromArray(
				['1f47a'])),
			_Utils_Tuple2(
			'💀',
			_List_fromArray(
				['1f480'])),
			_Utils_Tuple2(
			'☠',
			_List_fromArray(
				['2620'])),
			_Utils_Tuple2(
			'👻',
			_List_fromArray(
				['1f47b'])),
			_Utils_Tuple2(
			'👽',
			_List_fromArray(
				['1f47d'])),
			_Utils_Tuple2(
			'👾',
			_List_fromArray(
				['1f47e'])),
			_Utils_Tuple2(
			'\ud83e\udd16',
			_List_fromArray(
				['1f916'])),
			_Utils_Tuple2(
			'💩',
			_List_fromArray(
				['1f4a9'])),
			_Utils_Tuple2(
			'😺',
			_List_fromArray(
				['1f63a'])),
			_Utils_Tuple2(
			'😸',
			_List_fromArray(
				['1f638'])),
			_Utils_Tuple2(
			'😹',
			_List_fromArray(
				['1f639'])),
			_Utils_Tuple2(
			'😻',
			_List_fromArray(
				['1f63b'])),
			_Utils_Tuple2(
			'😼',
			_List_fromArray(
				['1f63c'])),
			_Utils_Tuple2(
			'😽',
			_List_fromArray(
				['1f63d'])),
			_Utils_Tuple2(
			'🙀',
			_List_fromArray(
				['1f640'])),
			_Utils_Tuple2(
			'😿',
			_List_fromArray(
				['1f63f'])),
			_Utils_Tuple2(
			'😾',
			_List_fromArray(
				['1f63e'])),
			_Utils_Tuple2(
			'🙈',
			_List_fromArray(
				['1f648'])),
			_Utils_Tuple2(
			'🙉',
			_List_fromArray(
				['1f649'])),
			_Utils_Tuple2(
			'🙊',
			_List_fromArray(
				['1f64a'])),
			_Utils_Tuple2(
			'👦',
			_List_fromArray(
				['1f466']))
		]),
	_Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'👦\ud83c\udffb',
				_List_fromArray(
					['1f466', '1f3fb'])),
				_Utils_Tuple2(
				'👦\ud83c\udffc',
				_List_fromArray(
					['1f466', '1f3fc'])),
				_Utils_Tuple2(
				'👦\ud83c\udffd',
				_List_fromArray(
					['1f466', '1f3fd'])),
				_Utils_Tuple2(
				'👦\ud83c\udffe',
				_List_fromArray(
					['1f466', '1f3fe'])),
				_Utils_Tuple2(
				'👦\ud83c\udfff',
				_List_fromArray(
					['1f466', '1f3ff'])),
				_Utils_Tuple2(
				'👧',
				_List_fromArray(
					['1f467'])),
				_Utils_Tuple2(
				'👧\ud83c\udffb',
				_List_fromArray(
					['1f467', '1f3fb'])),
				_Utils_Tuple2(
				'👧\ud83c\udffc',
				_List_fromArray(
					['1f467', '1f3fc'])),
				_Utils_Tuple2(
				'👧\ud83c\udffd',
				_List_fromArray(
					['1f467', '1f3fd'])),
				_Utils_Tuple2(
				'👧\ud83c\udffe',
				_List_fromArray(
					['1f467', '1f3fe'])),
				_Utils_Tuple2(
				'👧\ud83c\udfff',
				_List_fromArray(
					['1f467', '1f3ff'])),
				_Utils_Tuple2(
				'👨',
				_List_fromArray(
					['1f468'])),
				_Utils_Tuple2(
				'👨\ud83c\udffb',
				_List_fromArray(
					['1f468', '1f3fb'])),
				_Utils_Tuple2(
				'👨\ud83c\udffc',
				_List_fromArray(
					['1f468', '1f3fc'])),
				_Utils_Tuple2(
				'👨\ud83c\udffd',
				_List_fromArray(
					['1f468', '1f3fd'])),
				_Utils_Tuple2(
				'👨\ud83c\udffe',
				_List_fromArray(
					['1f468', '1f3fe'])),
				_Utils_Tuple2(
				'👨\ud83c\udfff',
				_List_fromArray(
					['1f468', '1f3ff'])),
				_Utils_Tuple2(
				'👩',
				_List_fromArray(
					['1f469'])),
				_Utils_Tuple2(
				'👩\ud83c\udffb',
				_List_fromArray(
					['1f469', '1f3fb'])),
				_Utils_Tuple2(
				'👩\ud83c\udffc',
				_List_fromArray(
					['1f469', '1f3fc'])),
				_Utils_Tuple2(
				'👩\ud83c\udffd',
				_List_fromArray(
					['1f469', '1f3fd'])),
				_Utils_Tuple2(
				'👩\ud83c\udffe',
				_List_fromArray(
					['1f469', '1f3fe'])),
				_Utils_Tuple2(
				'👩\ud83c\udfff',
				_List_fromArray(
					['1f469', '1f3ff'])),
				_Utils_Tuple2(
				'👴',
				_List_fromArray(
					['1f474'])),
				_Utils_Tuple2(
				'👴\ud83c\udffb',
				_List_fromArray(
					['1f474', '1f3fb'])),
				_Utils_Tuple2(
				'👴\ud83c\udffc',
				_List_fromArray(
					['1f474', '1f3fc'])),
				_Utils_Tuple2(
				'👴\ud83c\udffd',
				_List_fromArray(
					['1f474', '1f3fd'])),
				_Utils_Tuple2(
				'👴\ud83c\udffe',
				_List_fromArray(
					['1f474', '1f3fe'])),
				_Utils_Tuple2(
				'👴\ud83c\udfff',
				_List_fromArray(
					['1f474', '1f3ff'])),
				_Utils_Tuple2(
				'👵',
				_List_fromArray(
					['1f475'])),
				_Utils_Tuple2(
				'👵\ud83c\udffb',
				_List_fromArray(
					['1f475', '1f3fb'])),
				_Utils_Tuple2(
				'👵\ud83c\udffc',
				_List_fromArray(
					['1f475', '1f3fc'])),
				_Utils_Tuple2(
				'👵\ud83c\udffd',
				_List_fromArray(
					['1f475', '1f3fd'])),
				_Utils_Tuple2(
				'👵\ud83c\udffe',
				_List_fromArray(
					['1f475', '1f3fe'])),
				_Utils_Tuple2(
				'👵\ud83c\udfff',
				_List_fromArray(
					['1f475', '1f3ff'])),
				_Utils_Tuple2(
				'👶',
				_List_fromArray(
					['1f476'])),
				_Utils_Tuple2(
				'👶\ud83c\udffb',
				_List_fromArray(
					['1f476', '1f3fb'])),
				_Utils_Tuple2(
				'👶\ud83c\udffc',
				_List_fromArray(
					['1f476', '1f3fc'])),
				_Utils_Tuple2(
				'👶\ud83c\udffd',
				_List_fromArray(
					['1f476', '1f3fd'])),
				_Utils_Tuple2(
				'👶\ud83c\udffe',
				_List_fromArray(
					['1f476', '1f3fe'])),
				_Utils_Tuple2(
				'👶\ud83c\udfff',
				_List_fromArray(
					['1f476', '1f3ff'])),
				_Utils_Tuple2(
				'👼',
				_List_fromArray(
					['1f47c'])),
				_Utils_Tuple2(
				'👼\ud83c\udffb',
				_List_fromArray(
					['1f47c', '1f3fb'])),
				_Utils_Tuple2(
				'👼\ud83c\udffc',
				_List_fromArray(
					['1f47c', '1f3fc'])),
				_Utils_Tuple2(
				'👼\ud83c\udffd',
				_List_fromArray(
					['1f47c', '1f3fd'])),
				_Utils_Tuple2(
				'👼\ud83c\udffe',
				_List_fromArray(
					['1f47c', '1f3fe'])),
				_Utils_Tuple2(
				'👼\ud83c\udfff',
				_List_fromArray(
					['1f47c', '1f3ff'])),
				_Utils_Tuple2(
				'👮',
				_List_fromArray(
					['1f46e'])),
				_Utils_Tuple2(
				'👮\ud83c\udffb',
				_List_fromArray(
					['1f46e', '1f3fb'])),
				_Utils_Tuple2(
				'👮\ud83c\udffc',
				_List_fromArray(
					['1f46e', '1f3fc'])),
				_Utils_Tuple2(
				'👮\ud83c\udffd',
				_List_fromArray(
					['1f46e', '1f3fd'])),
				_Utils_Tuple2(
				'👮\ud83c\udffe',
				_List_fromArray(
					['1f46e', '1f3fe'])),
				_Utils_Tuple2(
				'👮\ud83c\udfff',
				_List_fromArray(
					['1f46e', '1f3ff'])),
				_Utils_Tuple2(
				'🕵',
				_List_fromArray(
					['1f575'])),
				_Utils_Tuple2(
				'🕵\ud83c\udffb',
				_List_fromArray(
					['1f575', '1f3fb'])),
				_Utils_Tuple2(
				'🕵\ud83c\udffc',
				_List_fromArray(
					['1f575', '1f3fc'])),
				_Utils_Tuple2(
				'🕵\ud83c\udffd',
				_List_fromArray(
					['1f575', '1f3fd'])),
				_Utils_Tuple2(
				'🕵\ud83c\udffe',
				_List_fromArray(
					['1f575', '1f3fe'])),
				_Utils_Tuple2(
				'🕵\ud83c\udfff',
				_List_fromArray(
					['1f575', '1f3ff'])),
				_Utils_Tuple2(
				'💂',
				_List_fromArray(
					['1f482'])),
				_Utils_Tuple2(
				'💂\ud83c\udffb',
				_List_fromArray(
					['1f482', '1f3fb'])),
				_Utils_Tuple2(
				'💂\ud83c\udffc',
				_List_fromArray(
					['1f482', '1f3fc'])),
				_Utils_Tuple2(
				'💂\ud83c\udffd',
				_List_fromArray(
					['1f482', '1f3fd'])),
				_Utils_Tuple2(
				'💂\ud83c\udffe',
				_List_fromArray(
					['1f482', '1f3fe'])),
				_Utils_Tuple2(
				'💂\ud83c\udfff',
				_List_fromArray(
					['1f482', '1f3ff'])),
				_Utils_Tuple2(
				'👷',
				_List_fromArray(
					['1f477'])),
				_Utils_Tuple2(
				'👷\ud83c\udffb',
				_List_fromArray(
					['1f477', '1f3fb'])),
				_Utils_Tuple2(
				'👷\ud83c\udffc',
				_List_fromArray(
					['1f477', '1f3fc'])),
				_Utils_Tuple2(
				'👷\ud83c\udffd',
				_List_fromArray(
					['1f477', '1f3fd'])),
				_Utils_Tuple2(
				'👷\ud83c\udffe',
				_List_fromArray(
					['1f477', '1f3fe'])),
				_Utils_Tuple2(
				'👷\ud83c\udfff',
				_List_fromArray(
					['1f477', '1f3ff'])),
				_Utils_Tuple2(
				'👳',
				_List_fromArray(
					['1f473'])),
				_Utils_Tuple2(
				'👳\ud83c\udffb',
				_List_fromArray(
					['1f473', '1f3fb'])),
				_Utils_Tuple2(
				'👳\ud83c\udffc',
				_List_fromArray(
					['1f473', '1f3fc'])),
				_Utils_Tuple2(
				'👳\ud83c\udffd',
				_List_fromArray(
					['1f473', '1f3fd'])),
				_Utils_Tuple2(
				'👳\ud83c\udffe',
				_List_fromArray(
					['1f473', '1f3fe'])),
				_Utils_Tuple2(
				'👳\ud83c\udfff',
				_List_fromArray(
					['1f473', '1f3ff'])),
				_Utils_Tuple2(
				'👱',
				_List_fromArray(
					['1f471'])),
				_Utils_Tuple2(
				'👱\ud83c\udffb',
				_List_fromArray(
					['1f471', '1f3fb'])),
				_Utils_Tuple2(
				'👱\ud83c\udffc',
				_List_fromArray(
					['1f471', '1f3fc'])),
				_Utils_Tuple2(
				'👱\ud83c\udffd',
				_List_fromArray(
					['1f471', '1f3fd'])),
				_Utils_Tuple2(
				'👱\ud83c\udffe',
				_List_fromArray(
					['1f471', '1f3fe'])),
				_Utils_Tuple2(
				'👱\ud83c\udfff',
				_List_fromArray(
					['1f471', '1f3ff'])),
				_Utils_Tuple2(
				'🎅',
				_List_fromArray(
					['1f385'])),
				_Utils_Tuple2(
				'🎅\ud83c\udffb',
				_List_fromArray(
					['1f385', '1f3fb'])),
				_Utils_Tuple2(
				'🎅\ud83c\udffc',
				_List_fromArray(
					['1f385', '1f3fc'])),
				_Utils_Tuple2(
				'🎅\ud83c\udffd',
				_List_fromArray(
					['1f385', '1f3fd'])),
				_Utils_Tuple2(
				'🎅\ud83c\udffe',
				_List_fromArray(
					['1f385', '1f3fe'])),
				_Utils_Tuple2(
				'🎅\ud83c\udfff',
				_List_fromArray(
					['1f385', '1f3ff'])),
				_Utils_Tuple2(
				'\ud83e\udd36',
				_List_fromArray(
					['1f936'])),
				_Utils_Tuple2(
				'\ud83e\udd36\ud83c\udffb',
				_List_fromArray(
					['1f936', '1f3fb'])),
				_Utils_Tuple2(
				'\ud83e\udd36\ud83c\udffc',
				_List_fromArray(
					['1f936', '1f3fc'])),
				_Utils_Tuple2(
				'\ud83e\udd36\ud83c\udffd',
				_List_fromArray(
					['1f936', '1f3fd'])),
				_Utils_Tuple2(
				'\ud83e\udd36\ud83c\udffe',
				_List_fromArray(
					['1f936', '1f3fe'])),
				_Utils_Tuple2(
				'\ud83e\udd36\ud83c\udfff',
				_List_fromArray(
					['1f936', '1f3ff'])),
				_Utils_Tuple2(
				'👸',
				_List_fromArray(
					['1f478'])),
				_Utils_Tuple2(
				'👸\ud83c\udffb',
				_List_fromArray(
					['1f478', '1f3fb'])),
				_Utils_Tuple2(
				'👸\ud83c\udffc',
				_List_fromArray(
					['1f478', '1f3fc'])),
				_Utils_Tuple2(
				'👸\ud83c\udffd',
				_List_fromArray(
					['1f478', '1f3fd']))
			]),
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'👸\ud83c\udffe',
					_List_fromArray(
						['1f478', '1f3fe'])),
					_Utils_Tuple2(
					'👸\ud83c\udfff',
					_List_fromArray(
						['1f478', '1f3ff'])),
					_Utils_Tuple2(
					'\ud83e\udd34',
					_List_fromArray(
						['1f934'])),
					_Utils_Tuple2(
					'\ud83e\udd34\ud83c\udffb',
					_List_fromArray(
						['1f934', '1f3fb'])),
					_Utils_Tuple2(
					'\ud83e\udd34\ud83c\udffc',
					_List_fromArray(
						['1f934', '1f3fc'])),
					_Utils_Tuple2(
					'\ud83e\udd34\ud83c\udffd',
					_List_fromArray(
						['1f934', '1f3fd'])),
					_Utils_Tuple2(
					'\ud83e\udd34\ud83c\udffe',
					_List_fromArray(
						['1f934', '1f3fe'])),
					_Utils_Tuple2(
					'\ud83e\udd34\ud83c\udfff',
					_List_fromArray(
						['1f934', '1f3ff'])),
					_Utils_Tuple2(
					'👰',
					_List_fromArray(
						['1f470'])),
					_Utils_Tuple2(
					'👰\ud83c\udffb',
					_List_fromArray(
						['1f470', '1f3fb'])),
					_Utils_Tuple2(
					'👰\ud83c\udffc',
					_List_fromArray(
						['1f470', '1f3fc'])),
					_Utils_Tuple2(
					'👰\ud83c\udffd',
					_List_fromArray(
						['1f470', '1f3fd'])),
					_Utils_Tuple2(
					'👰\ud83c\udffe',
					_List_fromArray(
						['1f470', '1f3fe'])),
					_Utils_Tuple2(
					'👰\ud83c\udfff',
					_List_fromArray(
						['1f470', '1f3ff'])),
					_Utils_Tuple2(
					'\ud83e\udd35',
					_List_fromArray(
						['1f935'])),
					_Utils_Tuple2(
					'\ud83e\udd35\ud83c\udffb',
					_List_fromArray(
						['1f935', '1f3fb'])),
					_Utils_Tuple2(
					'\ud83e\udd35\ud83c\udffc',
					_List_fromArray(
						['1f935', '1f3fc'])),
					_Utils_Tuple2(
					'\ud83e\udd35\ud83c\udffd',
					_List_fromArray(
						['1f935', '1f3fd'])),
					_Utils_Tuple2(
					'\ud83e\udd35\ud83c\udffe',
					_List_fromArray(
						['1f935', '1f3fe'])),
					_Utils_Tuple2(
					'\ud83e\udd35\ud83c\udfff',
					_List_fromArray(
						['1f935', '1f3ff'])),
					_Utils_Tuple2(
					'\ud83e\udd30',
					_List_fromArray(
						['1f930'])),
					_Utils_Tuple2(
					'\ud83e\udd30\ud83c\udffb',
					_List_fromArray(
						['1f930', '1f3fb'])),
					_Utils_Tuple2(
					'\ud83e\udd30\ud83c\udffc',
					_List_fromArray(
						['1f930', '1f3fc'])),
					_Utils_Tuple2(
					'\ud83e\udd30\ud83c\udffd',
					_List_fromArray(
						['1f930', '1f3fd'])),
					_Utils_Tuple2(
					'\ud83e\udd30\ud83c\udffe',
					_List_fromArray(
						['1f930', '1f3fe'])),
					_Utils_Tuple2(
					'\ud83e\udd30\ud83c\udfff',
					_List_fromArray(
						['1f930', '1f3ff'])),
					_Utils_Tuple2(
					'👲',
					_List_fromArray(
						['1f472'])),
					_Utils_Tuple2(
					'👲\ud83c\udffb',
					_List_fromArray(
						['1f472', '1f3fb'])),
					_Utils_Tuple2(
					'👲\ud83c\udffc',
					_List_fromArray(
						['1f472', '1f3fc'])),
					_Utils_Tuple2(
					'👲\ud83c\udffd',
					_List_fromArray(
						['1f472', '1f3fd'])),
					_Utils_Tuple2(
					'👲\ud83c\udffe',
					_List_fromArray(
						['1f472', '1f3fe'])),
					_Utils_Tuple2(
					'👲\ud83c\udfff',
					_List_fromArray(
						['1f472', '1f3ff'])),
					_Utils_Tuple2(
					'🙍',
					_List_fromArray(
						['1f64d'])),
					_Utils_Tuple2(
					'🙍\ud83c\udffb',
					_List_fromArray(
						['1f64d', '1f3fb'])),
					_Utils_Tuple2(
					'🙍\ud83c\udffc',
					_List_fromArray(
						['1f64d', '1f3fc'])),
					_Utils_Tuple2(
					'🙍\ud83c\udffd',
					_List_fromArray(
						['1f64d', '1f3fd'])),
					_Utils_Tuple2(
					'🙍\ud83c\udffe',
					_List_fromArray(
						['1f64d', '1f3fe'])),
					_Utils_Tuple2(
					'🙍\ud83c\udfff',
					_List_fromArray(
						['1f64d', '1f3ff'])),
					_Utils_Tuple2(
					'🙎',
					_List_fromArray(
						['1f64e'])),
					_Utils_Tuple2(
					'🙎\ud83c\udffb',
					_List_fromArray(
						['1f64e', '1f3fb'])),
					_Utils_Tuple2(
					'🙎\ud83c\udffc',
					_List_fromArray(
						['1f64e', '1f3fc'])),
					_Utils_Tuple2(
					'🙎\ud83c\udffd',
					_List_fromArray(
						['1f64e', '1f3fd'])),
					_Utils_Tuple2(
					'🙎\ud83c\udffe',
					_List_fromArray(
						['1f64e', '1f3fe'])),
					_Utils_Tuple2(
					'🙎\ud83c\udfff',
					_List_fromArray(
						['1f64e', '1f3ff'])),
					_Utils_Tuple2(
					'🙅',
					_List_fromArray(
						['1f645'])),
					_Utils_Tuple2(
					'🙅\ud83c\udffb',
					_List_fromArray(
						['1f645', '1f3fb'])),
					_Utils_Tuple2(
					'🙅\ud83c\udffc',
					_List_fromArray(
						['1f645', '1f3fc'])),
					_Utils_Tuple2(
					'🙅\ud83c\udffd',
					_List_fromArray(
						['1f645', '1f3fd'])),
					_Utils_Tuple2(
					'🙅\ud83c\udffe',
					_List_fromArray(
						['1f645', '1f3fe'])),
					_Utils_Tuple2(
					'🙅\ud83c\udfff',
					_List_fromArray(
						['1f645', '1f3ff'])),
					_Utils_Tuple2(
					'🙆',
					_List_fromArray(
						['1f646'])),
					_Utils_Tuple2(
					'🙆\ud83c\udffb',
					_List_fromArray(
						['1f646', '1f3fb'])),
					_Utils_Tuple2(
					'🙆\ud83c\udffc',
					_List_fromArray(
						['1f646', '1f3fc'])),
					_Utils_Tuple2(
					'🙆\ud83c\udffd',
					_List_fromArray(
						['1f646', '1f3fd'])),
					_Utils_Tuple2(
					'🙆\ud83c\udffe',
					_List_fromArray(
						['1f646', '1f3fe'])),
					_Utils_Tuple2(
					'🙆\ud83c\udfff',
					_List_fromArray(
						['1f646', '1f3ff'])),
					_Utils_Tuple2(
					'💁',
					_List_fromArray(
						['1f481'])),
					_Utils_Tuple2(
					'💁\ud83c\udffb',
					_List_fromArray(
						['1f481', '1f3fb'])),
					_Utils_Tuple2(
					'💁\ud83c\udffc',
					_List_fromArray(
						['1f481', '1f3fc'])),
					_Utils_Tuple2(
					'💁\ud83c\udffd',
					_List_fromArray(
						['1f481', '1f3fd'])),
					_Utils_Tuple2(
					'💁\ud83c\udffe',
					_List_fromArray(
						['1f481', '1f3fe'])),
					_Utils_Tuple2(
					'💁\ud83c\udfff',
					_List_fromArray(
						['1f481', '1f3ff'])),
					_Utils_Tuple2(
					'🙋',
					_List_fromArray(
						['1f64b'])),
					_Utils_Tuple2(
					'🙋\ud83c\udffb',
					_List_fromArray(
						['1f64b', '1f3fb'])),
					_Utils_Tuple2(
					'🙋\ud83c\udffc',
					_List_fromArray(
						['1f64b', '1f3fc'])),
					_Utils_Tuple2(
					'🙋\ud83c\udffd',
					_List_fromArray(
						['1f64b', '1f3fd'])),
					_Utils_Tuple2(
					'🙋\ud83c\udffe',
					_List_fromArray(
						['1f64b', '1f3fe'])),
					_Utils_Tuple2(
					'🙋\ud83c\udfff',
					_List_fromArray(
						['1f64b', '1f3ff'])),
					_Utils_Tuple2(
					'🙇',
					_List_fromArray(
						['1f647'])),
					_Utils_Tuple2(
					'🙇\ud83c\udffb',
					_List_fromArray(
						['1f647', '1f3fb'])),
					_Utils_Tuple2(
					'🙇\ud83c\udffc',
					_List_fromArray(
						['1f647', '1f3fc'])),
					_Utils_Tuple2(
					'🙇\ud83c\udffd',
					_List_fromArray(
						['1f647', '1f3fd'])),
					_Utils_Tuple2(
					'🙇\ud83c\udffe',
					_List_fromArray(
						['1f647', '1f3fe'])),
					_Utils_Tuple2(
					'🙇\ud83c\udfff',
					_List_fromArray(
						['1f647', '1f3ff'])),
					_Utils_Tuple2(
					'\ud83e\udd26',
					_List_fromArray(
						['1f926'])),
					_Utils_Tuple2(
					'\ud83e\udd26\ud83c\udffb',
					_List_fromArray(
						['1f926', '1f3fb'])),
					_Utils_Tuple2(
					'\ud83e\udd26\ud83c\udffc',
					_List_fromArray(
						['1f926', '1f3fc'])),
					_Utils_Tuple2(
					'\ud83e\udd26\ud83c\udffd',
					_List_fromArray(
						['1f926', '1f3fd'])),
					_Utils_Tuple2(
					'\ud83e\udd26\ud83c\udffe',
					_List_fromArray(
						['1f926', '1f3fe'])),
					_Utils_Tuple2(
					'\ud83e\udd26\ud83c\udfff',
					_List_fromArray(
						['1f926', '1f3ff'])),
					_Utils_Tuple2(
					'\ud83e\udd37',
					_List_fromArray(
						['1f937'])),
					_Utils_Tuple2(
					'\ud83e\udd37\ud83c\udffb',
					_List_fromArray(
						['1f937', '1f3fb'])),
					_Utils_Tuple2(
					'\ud83e\udd37\ud83c\udffc',
					_List_fromArray(
						['1f937', '1f3fc'])),
					_Utils_Tuple2(
					'\ud83e\udd37\ud83c\udffd',
					_List_fromArray(
						['1f937', '1f3fd'])),
					_Utils_Tuple2(
					'\ud83e\udd37\ud83c\udffe',
					_List_fromArray(
						['1f937', '1f3fe'])),
					_Utils_Tuple2(
					'\ud83e\udd37\ud83c\udfff',
					_List_fromArray(
						['1f937', '1f3ff'])),
					_Utils_Tuple2(
					'💆',
					_List_fromArray(
						['1f486'])),
					_Utils_Tuple2(
					'💆\ud83c\udffb',
					_List_fromArray(
						['1f486', '1f3fb'])),
					_Utils_Tuple2(
					'💆\ud83c\udffc',
					_List_fromArray(
						['1f486', '1f3fc'])),
					_Utils_Tuple2(
					'💆\ud83c\udffd',
					_List_fromArray(
						['1f486', '1f3fd'])),
					_Utils_Tuple2(
					'💆\ud83c\udffe',
					_List_fromArray(
						['1f486', '1f3fe'])),
					_Utils_Tuple2(
					'💆\ud83c\udfff',
					_List_fromArray(
						['1f486', '1f3ff'])),
					_Utils_Tuple2(
					'💇',
					_List_fromArray(
						['1f487'])),
					_Utils_Tuple2(
					'💇\ud83c\udffb',
					_List_fromArray(
						['1f487', '1f3fb'])),
					_Utils_Tuple2(
					'💇\ud83c\udffc',
					_List_fromArray(
						['1f487', '1f3fc'])),
					_Utils_Tuple2(
					'💇\ud83c\udffd',
					_List_fromArray(
						['1f487', '1f3fd'])),
					_Utils_Tuple2(
					'💇\ud83c\udffe',
					_List_fromArray(
						['1f487', '1f3fe'])),
					_Utils_Tuple2(
					'💇\ud83c\udfff',
					_List_fromArray(
						['1f487', '1f3ff'])),
					_Utils_Tuple2(
					'🚶',
					_List_fromArray(
						['1f6b6']))
				]),
			_Utils_ap(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'🚶\ud83c\udffb',
						_List_fromArray(
							['1f6b6', '1f3fb'])),
						_Utils_Tuple2(
						'🚶\ud83c\udffc',
						_List_fromArray(
							['1f6b6', '1f3fc'])),
						_Utils_Tuple2(
						'🚶\ud83c\udffd',
						_List_fromArray(
							['1f6b6', '1f3fd'])),
						_Utils_Tuple2(
						'🚶\ud83c\udffe',
						_List_fromArray(
							['1f6b6', '1f3fe'])),
						_Utils_Tuple2(
						'🚶\ud83c\udfff',
						_List_fromArray(
							['1f6b6', '1f3ff'])),
						_Utils_Tuple2(
						'🏃',
						_List_fromArray(
							['1f3c3'])),
						_Utils_Tuple2(
						'🏃\ud83c\udffb',
						_List_fromArray(
							['1f3c3', '1f3fb'])),
						_Utils_Tuple2(
						'🏃\ud83c\udffc',
						_List_fromArray(
							['1f3c3', '1f3fc'])),
						_Utils_Tuple2(
						'🏃\ud83c\udffd',
						_List_fromArray(
							['1f3c3', '1f3fd'])),
						_Utils_Tuple2(
						'🏃\ud83c\udffe',
						_List_fromArray(
							['1f3c3', '1f3fe'])),
						_Utils_Tuple2(
						'🏃\ud83c\udfff',
						_List_fromArray(
							['1f3c3', '1f3ff'])),
						_Utils_Tuple2(
						'💃',
						_List_fromArray(
							['1f483'])),
						_Utils_Tuple2(
						'💃\ud83c\udffb',
						_List_fromArray(
							['1f483', '1f3fb'])),
						_Utils_Tuple2(
						'💃\ud83c\udffc',
						_List_fromArray(
							['1f483', '1f3fc'])),
						_Utils_Tuple2(
						'💃\ud83c\udffd',
						_List_fromArray(
							['1f483', '1f3fd'])),
						_Utils_Tuple2(
						'💃\ud83c\udffe',
						_List_fromArray(
							['1f483', '1f3fe'])),
						_Utils_Tuple2(
						'💃\ud83c\udfff',
						_List_fromArray(
							['1f483', '1f3ff'])),
						_Utils_Tuple2(
						'\ud83d\udd7a',
						_List_fromArray(
							['1f57a'])),
						_Utils_Tuple2(
						'\ud83d\udd7a\ud83c\udffb',
						_List_fromArray(
							['1f57a', '1f3fb'])),
						_Utils_Tuple2(
						'\ud83d\udd7a\ud83c\udffc',
						_List_fromArray(
							['1f57a', '1f3fc'])),
						_Utils_Tuple2(
						'\ud83d\udd7a\ud83c\udffd',
						_List_fromArray(
							['1f57a', '1f3fd'])),
						_Utils_Tuple2(
						'\ud83d\udd7a\ud83c\udffe',
						_List_fromArray(
							['1f57a', '1f3fe'])),
						_Utils_Tuple2(
						'\ud83d\udd7a\ud83c\udfff',
						_List_fromArray(
							['1f57a', '1f3ff'])),
						_Utils_Tuple2(
						'👯',
						_List_fromArray(
							['1f46f'])),
						_Utils_Tuple2(
						'🕴',
						_List_fromArray(
							['1f574'])),
						_Utils_Tuple2(
						'🗣',
						_List_fromArray(
							['1f5e3'])),
						_Utils_Tuple2(
						'👤',
						_List_fromArray(
							['1f464'])),
						_Utils_Tuple2(
						'👥',
						_List_fromArray(
							['1f465'])),
						_Utils_Tuple2(
						'\ud83e\udd3a',
						_List_fromArray(
							['1f93a'])),
						_Utils_Tuple2(
						'🏇',
						_List_fromArray(
							['1f3c7'])),
						_Utils_Tuple2(
						'⛷',
						_List_fromArray(
							['26f7'])),
						_Utils_Tuple2(
						'🏂',
						_List_fromArray(
							['1f3c2'])),
						_Utils_Tuple2(
						'🏌',
						_List_fromArray(
							['1f3cc'])),
						_Utils_Tuple2(
						'🏄',
						_List_fromArray(
							['1f3c4'])),
						_Utils_Tuple2(
						'🏄\ud83c\udffb',
						_List_fromArray(
							['1f3c4', '1f3fb'])),
						_Utils_Tuple2(
						'🏄\ud83c\udffc',
						_List_fromArray(
							['1f3c4', '1f3fc'])),
						_Utils_Tuple2(
						'🏄\ud83c\udffd',
						_List_fromArray(
							['1f3c4', '1f3fd'])),
						_Utils_Tuple2(
						'🏄\ud83c\udffe',
						_List_fromArray(
							['1f3c4', '1f3fe'])),
						_Utils_Tuple2(
						'🏄\ud83c\udfff',
						_List_fromArray(
							['1f3c4', '1f3ff'])),
						_Utils_Tuple2(
						'🚣',
						_List_fromArray(
							['1f6a3'])),
						_Utils_Tuple2(
						'🚣\ud83c\udffb',
						_List_fromArray(
							['1f6a3', '1f3fb'])),
						_Utils_Tuple2(
						'🚣\ud83c\udffc',
						_List_fromArray(
							['1f6a3', '1f3fc'])),
						_Utils_Tuple2(
						'🚣\ud83c\udffd',
						_List_fromArray(
							['1f6a3', '1f3fd'])),
						_Utils_Tuple2(
						'🚣\ud83c\udffe',
						_List_fromArray(
							['1f6a3', '1f3fe'])),
						_Utils_Tuple2(
						'🚣\ud83c\udfff',
						_List_fromArray(
							['1f6a3', '1f3ff'])),
						_Utils_Tuple2(
						'🏊',
						_List_fromArray(
							['1f3ca'])),
						_Utils_Tuple2(
						'🏊\ud83c\udffb',
						_List_fromArray(
							['1f3ca', '1f3fb'])),
						_Utils_Tuple2(
						'🏊\ud83c\udffc',
						_List_fromArray(
							['1f3ca', '1f3fc'])),
						_Utils_Tuple2(
						'🏊\ud83c\udffd',
						_List_fromArray(
							['1f3ca', '1f3fd'])),
						_Utils_Tuple2(
						'🏊\ud83c\udffe',
						_List_fromArray(
							['1f3ca', '1f3fe'])),
						_Utils_Tuple2(
						'🏊\ud83c\udfff',
						_List_fromArray(
							['1f3ca', '1f3ff'])),
						_Utils_Tuple2(
						'⛹',
						_List_fromArray(
							['26f9'])),
						_Utils_Tuple2(
						'⛹\ud83c\udffb',
						_List_fromArray(
							['26f9', '1f3fb'])),
						_Utils_Tuple2(
						'⛹\ud83c\udffc',
						_List_fromArray(
							['26f9', '1f3fc'])),
						_Utils_Tuple2(
						'⛹\ud83c\udffd',
						_List_fromArray(
							['26f9', '1f3fd'])),
						_Utils_Tuple2(
						'⛹\ud83c\udffe',
						_List_fromArray(
							['26f9', '1f3fe'])),
						_Utils_Tuple2(
						'⛹\ud83c\udfff',
						_List_fromArray(
							['26f9', '1f3ff'])),
						_Utils_Tuple2(
						'🏋',
						_List_fromArray(
							['1f3cb'])),
						_Utils_Tuple2(
						'🏋\ud83c\udffb',
						_List_fromArray(
							['1f3cb', '1f3fb'])),
						_Utils_Tuple2(
						'🏋\ud83c\udffc',
						_List_fromArray(
							['1f3cb', '1f3fc'])),
						_Utils_Tuple2(
						'🏋\ud83c\udffd',
						_List_fromArray(
							['1f3cb', '1f3fd'])),
						_Utils_Tuple2(
						'🏋\ud83c\udffe',
						_List_fromArray(
							['1f3cb', '1f3fe'])),
						_Utils_Tuple2(
						'🏋\ud83c\udfff',
						_List_fromArray(
							['1f3cb', '1f3ff'])),
						_Utils_Tuple2(
						'🚴',
						_List_fromArray(
							['1f6b4'])),
						_Utils_Tuple2(
						'🚴\ud83c\udffb',
						_List_fromArray(
							['1f6b4', '1f3fb'])),
						_Utils_Tuple2(
						'🚴\ud83c\udffc',
						_List_fromArray(
							['1f6b4', '1f3fc'])),
						_Utils_Tuple2(
						'🚴\ud83c\udffd',
						_List_fromArray(
							['1f6b4', '1f3fd'])),
						_Utils_Tuple2(
						'🚴\ud83c\udffe',
						_List_fromArray(
							['1f6b4', '1f3fe'])),
						_Utils_Tuple2(
						'🚴\ud83c\udfff',
						_List_fromArray(
							['1f6b4', '1f3ff'])),
						_Utils_Tuple2(
						'🚵',
						_List_fromArray(
							['1f6b5'])),
						_Utils_Tuple2(
						'🚵\ud83c\udffb',
						_List_fromArray(
							['1f6b5', '1f3fb'])),
						_Utils_Tuple2(
						'🚵\ud83c\udffc',
						_List_fromArray(
							['1f6b5', '1f3fc'])),
						_Utils_Tuple2(
						'🚵\ud83c\udffd',
						_List_fromArray(
							['1f6b5', '1f3fd'])),
						_Utils_Tuple2(
						'🚵\ud83c\udffe',
						_List_fromArray(
							['1f6b5', '1f3fe'])),
						_Utils_Tuple2(
						'🚵\ud83c\udfff',
						_List_fromArray(
							['1f6b5', '1f3ff'])),
						_Utils_Tuple2(
						'🏎',
						_List_fromArray(
							['1f3ce'])),
						_Utils_Tuple2(
						'🏍',
						_List_fromArray(
							['1f3cd'])),
						_Utils_Tuple2(
						'\ud83e\udd38',
						_List_fromArray(
							['1f938'])),
						_Utils_Tuple2(
						'\ud83e\udd38\ud83c\udffb',
						_List_fromArray(
							['1f938', '1f3fb'])),
						_Utils_Tuple2(
						'\ud83e\udd38\ud83c\udffc',
						_List_fromArray(
							['1f938', '1f3fc'])),
						_Utils_Tuple2(
						'\ud83e\udd38\ud83c\udffd',
						_List_fromArray(
							['1f938', '1f3fd'])),
						_Utils_Tuple2(
						'\ud83e\udd38\ud83c\udffe',
						_List_fromArray(
							['1f938', '1f3fe'])),
						_Utils_Tuple2(
						'\ud83e\udd38\ud83c\udfff',
						_List_fromArray(
							['1f938', '1f3ff'])),
						_Utils_Tuple2(
						'\ud83e\udd3c',
						_List_fromArray(
							['1f93c'])),
						_Utils_Tuple2(
						'\ud83e\udd3c\ud83c\udffb',
						_List_fromArray(
							['1f93c', '1f3fb'])),
						_Utils_Tuple2(
						'\ud83e\udd3c\ud83c\udffc',
						_List_fromArray(
							['1f93c', '1f3fc'])),
						_Utils_Tuple2(
						'\ud83e\udd3c\ud83c\udffd',
						_List_fromArray(
							['1f93c', '1f3fd'])),
						_Utils_Tuple2(
						'\ud83e\udd3c\ud83c\udffe',
						_List_fromArray(
							['1f93c', '1f3fe'])),
						_Utils_Tuple2(
						'\ud83e\udd3c\ud83c\udfff',
						_List_fromArray(
							['1f93c', '1f3ff'])),
						_Utils_Tuple2(
						'\ud83e\udd3d',
						_List_fromArray(
							['1f93d'])),
						_Utils_Tuple2(
						'\ud83e\udd3d\ud83c\udffb',
						_List_fromArray(
							['1f93d', '1f3fb'])),
						_Utils_Tuple2(
						'\ud83e\udd3d\ud83c\udffc',
						_List_fromArray(
							['1f93d', '1f3fc'])),
						_Utils_Tuple2(
						'\ud83e\udd3d\ud83c\udffd',
						_List_fromArray(
							['1f93d', '1f3fd'])),
						_Utils_Tuple2(
						'\ud83e\udd3d\ud83c\udffe',
						_List_fromArray(
							['1f93d', '1f3fe'])),
						_Utils_Tuple2(
						'\ud83e\udd3d\ud83c\udfff',
						_List_fromArray(
							['1f93d', '1f3ff'])),
						_Utils_Tuple2(
						'\ud83e\udd3e',
						_List_fromArray(
							['1f93e'])),
						_Utils_Tuple2(
						'\ud83e\udd3e\ud83c\udffb',
						_List_fromArray(
							['1f93e', '1f3fb'])),
						_Utils_Tuple2(
						'\ud83e\udd3e\ud83c\udffc',
						_List_fromArray(
							['1f93e', '1f3fc'])),
						_Utils_Tuple2(
						'\ud83e\udd3e\ud83c\udffd',
						_List_fromArray(
							['1f93e', '1f3fd']))
					]),
				_Utils_ap(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'\ud83e\udd3e\ud83c\udffe',
							_List_fromArray(
								['1f93e', '1f3fe'])),
							_Utils_Tuple2(
							'\ud83e\udd3e\ud83c\udfff',
							_List_fromArray(
								['1f93e', '1f3ff'])),
							_Utils_Tuple2(
							'\ud83e\udd39',
							_List_fromArray(
								['1f939'])),
							_Utils_Tuple2(
							'\ud83e\udd39\ud83c\udffb',
							_List_fromArray(
								['1f939', '1f3fb'])),
							_Utils_Tuple2(
							'\ud83e\udd39\ud83c\udffc',
							_List_fromArray(
								['1f939', '1f3fc'])),
							_Utils_Tuple2(
							'\ud83e\udd39\ud83c\udffd',
							_List_fromArray(
								['1f939', '1f3fd'])),
							_Utils_Tuple2(
							'\ud83e\udd39\ud83c\udffe',
							_List_fromArray(
								['1f939', '1f3fe'])),
							_Utils_Tuple2(
							'\ud83e\udd39\ud83c\udfff',
							_List_fromArray(
								['1f939', '1f3ff'])),
							_Utils_Tuple2(
							'👫',
							_List_fromArray(
								['1f46b'])),
							_Utils_Tuple2(
							'👬',
							_List_fromArray(
								['1f46c'])),
							_Utils_Tuple2(
							'👭',
							_List_fromArray(
								['1f46d'])),
							_Utils_Tuple2(
							'💏',
							_List_fromArray(
								['1f48f'])),
							_Utils_Tuple2(
							'👩\u200d❤️\u200d💋\u200d👨',
							_List_fromArray(
								['1f469', '200d', '2764', 'fe0f', '200d', '1f48b', '200d', '1f468'])),
							_Utils_Tuple2(
							'👨\u200d❤️\u200d💋\u200d👨',
							_List_fromArray(
								['1f468', '200d', '2764', 'fe0f', '200d', '1f48b', '200d', '1f468'])),
							_Utils_Tuple2(
							'👩\u200d❤️\u200d💋\u200d👩',
							_List_fromArray(
								['1f469', '200d', '2764', 'fe0f', '200d', '1f48b', '200d', '1f469'])),
							_Utils_Tuple2(
							'💑',
							_List_fromArray(
								['1f491'])),
							_Utils_Tuple2(
							'👩\u200d❤️\u200d👨',
							_List_fromArray(
								['1f469', '200d', '2764', 'fe0f', '200d', '1f468'])),
							_Utils_Tuple2(
							'👨\u200d❤️\u200d👨',
							_List_fromArray(
								['1f468', '200d', '2764', 'fe0f', '200d', '1f468'])),
							_Utils_Tuple2(
							'👩\u200d❤️\u200d👩',
							_List_fromArray(
								['1f469', '200d', '2764', 'fe0f', '200d', '1f469'])),
							_Utils_Tuple2(
							'👪',
							_List_fromArray(
								['1f46a'])),
							_Utils_Tuple2(
							'👨\u200d👩\u200d👦',
							_List_fromArray(
								['1f468', '200d', '1f469', '200d', '1f466'])),
							_Utils_Tuple2(
							'👨\u200d👩\u200d👧',
							_List_fromArray(
								['1f468', '200d', '1f469', '200d', '1f467'])),
							_Utils_Tuple2(
							'👨\u200d👩\u200d👧\u200d👦',
							_List_fromArray(
								['1f468', '200d', '1f469', '200d', '1f467', '200d', '1f466'])),
							_Utils_Tuple2(
							'👨\u200d👩\u200d👦\u200d👦',
							_List_fromArray(
								['1f468', '200d', '1f469', '200d', '1f466', '200d', '1f466'])),
							_Utils_Tuple2(
							'👨\u200d👩\u200d👧\u200d👧',
							_List_fromArray(
								['1f468', '200d', '1f469', '200d', '1f467', '200d', '1f467'])),
							_Utils_Tuple2(
							'👨\u200d👨\u200d👦',
							_List_fromArray(
								['1f468', '200d', '1f468', '200d', '1f466'])),
							_Utils_Tuple2(
							'👨\u200d👨\u200d👧',
							_List_fromArray(
								['1f468', '200d', '1f468', '200d', '1f467'])),
							_Utils_Tuple2(
							'👨\u200d👨\u200d👧\u200d👦',
							_List_fromArray(
								['1f468', '200d', '1f468', '200d', '1f467', '200d', '1f466'])),
							_Utils_Tuple2(
							'👨\u200d👨\u200d👦\u200d👦',
							_List_fromArray(
								['1f468', '200d', '1f468', '200d', '1f466', '200d', '1f466'])),
							_Utils_Tuple2(
							'👨\u200d👨\u200d👧\u200d👧',
							_List_fromArray(
								['1f468', '200d', '1f468', '200d', '1f467', '200d', '1f467'])),
							_Utils_Tuple2(
							'👩\u200d👩\u200d👦',
							_List_fromArray(
								['1f469', '200d', '1f469', '200d', '1f466'])),
							_Utils_Tuple2(
							'👩\u200d👩\u200d👧',
							_List_fromArray(
								['1f469', '200d', '1f469', '200d', '1f467'])),
							_Utils_Tuple2(
							'👩\u200d👩\u200d👧\u200d👦',
							_List_fromArray(
								['1f469', '200d', '1f469', '200d', '1f467', '200d', '1f466'])),
							_Utils_Tuple2(
							'👩\u200d👩\u200d👦\u200d👦',
							_List_fromArray(
								['1f469', '200d', '1f469', '200d', '1f466', '200d', '1f466'])),
							_Utils_Tuple2(
							'👩\u200d👩\u200d👧\u200d👧',
							_List_fromArray(
								['1f469', '200d', '1f469', '200d', '1f467', '200d', '1f467'])),
							_Utils_Tuple2(
							'\ud83c\udffb',
							_List_fromArray(
								['1f3fb'])),
							_Utils_Tuple2(
							'\ud83c\udffc',
							_List_fromArray(
								['1f3fc'])),
							_Utils_Tuple2(
							'\ud83c\udffd',
							_List_fromArray(
								['1f3fd'])),
							_Utils_Tuple2(
							'\ud83c\udffe',
							_List_fromArray(
								['1f3fe'])),
							_Utils_Tuple2(
							'\ud83c\udfff',
							_List_fromArray(
								['1f3ff'])),
							_Utils_Tuple2(
							'💪',
							_List_fromArray(
								['1f4aa'])),
							_Utils_Tuple2(
							'💪\ud83c\udffb',
							_List_fromArray(
								['1f4aa', '1f3fb'])),
							_Utils_Tuple2(
							'💪\ud83c\udffc',
							_List_fromArray(
								['1f4aa', '1f3fc'])),
							_Utils_Tuple2(
							'💪\ud83c\udffd',
							_List_fromArray(
								['1f4aa', '1f3fd'])),
							_Utils_Tuple2(
							'💪\ud83c\udffe',
							_List_fromArray(
								['1f4aa', '1f3fe'])),
							_Utils_Tuple2(
							'💪\ud83c\udfff',
							_List_fromArray(
								['1f4aa', '1f3ff'])),
							_Utils_Tuple2(
							'\ud83e\udd33',
							_List_fromArray(
								['1f933'])),
							_Utils_Tuple2(
							'\ud83e\udd33\ud83c\udffb',
							_List_fromArray(
								['1f933', '1f3fb'])),
							_Utils_Tuple2(
							'\ud83e\udd33\ud83c\udffc',
							_List_fromArray(
								['1f933', '1f3fc'])),
							_Utils_Tuple2(
							'\ud83e\udd33\ud83c\udffd',
							_List_fromArray(
								['1f933', '1f3fd'])),
							_Utils_Tuple2(
							'\ud83e\udd33\ud83c\udffe',
							_List_fromArray(
								['1f933', '1f3fe'])),
							_Utils_Tuple2(
							'\ud83e\udd33\ud83c\udfff',
							_List_fromArray(
								['1f933', '1f3ff'])),
							_Utils_Tuple2(
							'👈',
							_List_fromArray(
								['1f448'])),
							_Utils_Tuple2(
							'👈\ud83c\udffb',
							_List_fromArray(
								['1f448', '1f3fb'])),
							_Utils_Tuple2(
							'👈\ud83c\udffc',
							_List_fromArray(
								['1f448', '1f3fc'])),
							_Utils_Tuple2(
							'👈\ud83c\udffd',
							_List_fromArray(
								['1f448', '1f3fd'])),
							_Utils_Tuple2(
							'👈\ud83c\udffe',
							_List_fromArray(
								['1f448', '1f3fe'])),
							_Utils_Tuple2(
							'👈\ud83c\udfff',
							_List_fromArray(
								['1f448', '1f3ff'])),
							_Utils_Tuple2(
							'👉',
							_List_fromArray(
								['1f449'])),
							_Utils_Tuple2(
							'👉\ud83c\udffb',
							_List_fromArray(
								['1f449', '1f3fb'])),
							_Utils_Tuple2(
							'👉\ud83c\udffc',
							_List_fromArray(
								['1f449', '1f3fc'])),
							_Utils_Tuple2(
							'👉\ud83c\udffd',
							_List_fromArray(
								['1f449', '1f3fd'])),
							_Utils_Tuple2(
							'👉\ud83c\udffe',
							_List_fromArray(
								['1f449', '1f3fe'])),
							_Utils_Tuple2(
							'👉\ud83c\udfff',
							_List_fromArray(
								['1f449', '1f3ff'])),
							_Utils_Tuple2(
							'☝',
							_List_fromArray(
								['261d'])),
							_Utils_Tuple2(
							'☝\ud83c\udffb',
							_List_fromArray(
								['261d', '1f3fb'])),
							_Utils_Tuple2(
							'☝\ud83c\udffc',
							_List_fromArray(
								['261d', '1f3fc'])),
							_Utils_Tuple2(
							'☝\ud83c\udffd',
							_List_fromArray(
								['261d', '1f3fd'])),
							_Utils_Tuple2(
							'☝\ud83c\udffe',
							_List_fromArray(
								['261d', '1f3fe'])),
							_Utils_Tuple2(
							'☝\ud83c\udfff',
							_List_fromArray(
								['261d', '1f3ff'])),
							_Utils_Tuple2(
							'👆',
							_List_fromArray(
								['1f446'])),
							_Utils_Tuple2(
							'👆\ud83c\udffb',
							_List_fromArray(
								['1f446', '1f3fb'])),
							_Utils_Tuple2(
							'👆\ud83c\udffc',
							_List_fromArray(
								['1f446', '1f3fc'])),
							_Utils_Tuple2(
							'👆\ud83c\udffd',
							_List_fromArray(
								['1f446', '1f3fd'])),
							_Utils_Tuple2(
							'👆\ud83c\udffe',
							_List_fromArray(
								['1f446', '1f3fe'])),
							_Utils_Tuple2(
							'👆\ud83c\udfff',
							_List_fromArray(
								['1f446', '1f3ff'])),
							_Utils_Tuple2(
							'🖕',
							_List_fromArray(
								['1f595'])),
							_Utils_Tuple2(
							'🖕\ud83c\udffb',
							_List_fromArray(
								['1f595', '1f3fb'])),
							_Utils_Tuple2(
							'🖕\ud83c\udffc',
							_List_fromArray(
								['1f595', '1f3fc'])),
							_Utils_Tuple2(
							'🖕\ud83c\udffd',
							_List_fromArray(
								['1f595', '1f3fd'])),
							_Utils_Tuple2(
							'🖕\ud83c\udffe',
							_List_fromArray(
								['1f595', '1f3fe'])),
							_Utils_Tuple2(
							'🖕\ud83c\udfff',
							_List_fromArray(
								['1f595', '1f3ff'])),
							_Utils_Tuple2(
							'👇',
							_List_fromArray(
								['1f447'])),
							_Utils_Tuple2(
							'👇\ud83c\udffb',
							_List_fromArray(
								['1f447', '1f3fb'])),
							_Utils_Tuple2(
							'👇\ud83c\udffc',
							_List_fromArray(
								['1f447', '1f3fc'])),
							_Utils_Tuple2(
							'👇\ud83c\udffd',
							_List_fromArray(
								['1f447', '1f3fd'])),
							_Utils_Tuple2(
							'👇\ud83c\udffe',
							_List_fromArray(
								['1f447', '1f3fe'])),
							_Utils_Tuple2(
							'👇\ud83c\udfff',
							_List_fromArray(
								['1f447', '1f3ff'])),
							_Utils_Tuple2(
							'✌',
							_List_fromArray(
								['270c'])),
							_Utils_Tuple2(
							'✌\ud83c\udffb',
							_List_fromArray(
								['270c', '1f3fb'])),
							_Utils_Tuple2(
							'✌\ud83c\udffc',
							_List_fromArray(
								['270c', '1f3fc'])),
							_Utils_Tuple2(
							'✌\ud83c\udffd',
							_List_fromArray(
								['270c', '1f3fd'])),
							_Utils_Tuple2(
							'✌\ud83c\udffe',
							_List_fromArray(
								['270c', '1f3fe'])),
							_Utils_Tuple2(
							'✌\ud83c\udfff',
							_List_fromArray(
								['270c', '1f3ff'])),
							_Utils_Tuple2(
							'\ud83e\udd1e',
							_List_fromArray(
								['1f91e'])),
							_Utils_Tuple2(
							'\ud83e\udd1e\ud83c\udffb',
							_List_fromArray(
								['1f91e', '1f3fb'])),
							_Utils_Tuple2(
							'\ud83e\udd1e\ud83c\udffc',
							_List_fromArray(
								['1f91e', '1f3fc'])),
							_Utils_Tuple2(
							'\ud83e\udd1e\ud83c\udffd',
							_List_fromArray(
								['1f91e', '1f3fd'])),
							_Utils_Tuple2(
							'\ud83e\udd1e\ud83c\udffe',
							_List_fromArray(
								['1f91e', '1f3fe']))
						]),
					_Utils_ap(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'\ud83e\udd1e\ud83c\udfff',
								_List_fromArray(
									['1f91e', '1f3ff'])),
								_Utils_Tuple2(
								'🖖',
								_List_fromArray(
									['1f596'])),
								_Utils_Tuple2(
								'🖖\ud83c\udffb',
								_List_fromArray(
									['1f596', '1f3fb'])),
								_Utils_Tuple2(
								'🖖\ud83c\udffc',
								_List_fromArray(
									['1f596', '1f3fc'])),
								_Utils_Tuple2(
								'🖖\ud83c\udffd',
								_List_fromArray(
									['1f596', '1f3fd'])),
								_Utils_Tuple2(
								'🖖\ud83c\udffe',
								_List_fromArray(
									['1f596', '1f3fe'])),
								_Utils_Tuple2(
								'🖖\ud83c\udfff',
								_List_fromArray(
									['1f596', '1f3ff'])),
								_Utils_Tuple2(
								'\ud83e\udd18',
								_List_fromArray(
									['1f918'])),
								_Utils_Tuple2(
								'\ud83e\udd18\ud83c\udffb',
								_List_fromArray(
									['1f918', '1f3fb'])),
								_Utils_Tuple2(
								'\ud83e\udd18\ud83c\udffc',
								_List_fromArray(
									['1f918', '1f3fc'])),
								_Utils_Tuple2(
								'\ud83e\udd18\ud83c\udffd',
								_List_fromArray(
									['1f918', '1f3fd'])),
								_Utils_Tuple2(
								'\ud83e\udd18\ud83c\udffe',
								_List_fromArray(
									['1f918', '1f3fe'])),
								_Utils_Tuple2(
								'\ud83e\udd18\ud83c\udfff',
								_List_fromArray(
									['1f918', '1f3ff'])),
								_Utils_Tuple2(
								'\ud83e\udd19',
								_List_fromArray(
									['1f919'])),
								_Utils_Tuple2(
								'\ud83e\udd19\ud83c\udffb',
								_List_fromArray(
									['1f919', '1f3fb'])),
								_Utils_Tuple2(
								'\ud83e\udd19\ud83c\udffc',
								_List_fromArray(
									['1f919', '1f3fc'])),
								_Utils_Tuple2(
								'\ud83e\udd19\ud83c\udffd',
								_List_fromArray(
									['1f919', '1f3fd'])),
								_Utils_Tuple2(
								'\ud83e\udd19\ud83c\udffe',
								_List_fromArray(
									['1f919', '1f3fe'])),
								_Utils_Tuple2(
								'\ud83e\udd19\ud83c\udfff',
								_List_fromArray(
									['1f919', '1f3ff'])),
								_Utils_Tuple2(
								'🖐',
								_List_fromArray(
									['1f590'])),
								_Utils_Tuple2(
								'🖐\ud83c\udffb',
								_List_fromArray(
									['1f590', '1f3fb'])),
								_Utils_Tuple2(
								'🖐\ud83c\udffc',
								_List_fromArray(
									['1f590', '1f3fc'])),
								_Utils_Tuple2(
								'🖐\ud83c\udffd',
								_List_fromArray(
									['1f590', '1f3fd'])),
								_Utils_Tuple2(
								'🖐\ud83c\udffe',
								_List_fromArray(
									['1f590', '1f3fe'])),
								_Utils_Tuple2(
								'🖐\ud83c\udfff',
								_List_fromArray(
									['1f590', '1f3ff'])),
								_Utils_Tuple2(
								'✋',
								_List_fromArray(
									['270b'])),
								_Utils_Tuple2(
								'✋\ud83c\udffb',
								_List_fromArray(
									['270b', '1f3fb'])),
								_Utils_Tuple2(
								'✋\ud83c\udffc',
								_List_fromArray(
									['270b', '1f3fc'])),
								_Utils_Tuple2(
								'✋\ud83c\udffd',
								_List_fromArray(
									['270b', '1f3fd'])),
								_Utils_Tuple2(
								'✋\ud83c\udffe',
								_List_fromArray(
									['270b', '1f3fe'])),
								_Utils_Tuple2(
								'✋\ud83c\udfff',
								_List_fromArray(
									['270b', '1f3ff'])),
								_Utils_Tuple2(
								'👌',
								_List_fromArray(
									['1f44c'])),
								_Utils_Tuple2(
								'👌\ud83c\udffb',
								_List_fromArray(
									['1f44c', '1f3fb'])),
								_Utils_Tuple2(
								'👌\ud83c\udffc',
								_List_fromArray(
									['1f44c', '1f3fc'])),
								_Utils_Tuple2(
								'👌\ud83c\udffd',
								_List_fromArray(
									['1f44c', '1f3fd'])),
								_Utils_Tuple2(
								'👌\ud83c\udffe',
								_List_fromArray(
									['1f44c', '1f3fe'])),
								_Utils_Tuple2(
								'👌\ud83c\udfff',
								_List_fromArray(
									['1f44c', '1f3ff'])),
								_Utils_Tuple2(
								'👍',
								_List_fromArray(
									['1f44d'])),
								_Utils_Tuple2(
								'👍\ud83c\udffb',
								_List_fromArray(
									['1f44d', '1f3fb'])),
								_Utils_Tuple2(
								'👍\ud83c\udffc',
								_List_fromArray(
									['1f44d', '1f3fc'])),
								_Utils_Tuple2(
								'👍\ud83c\udffd',
								_List_fromArray(
									['1f44d', '1f3fd'])),
								_Utils_Tuple2(
								'👍\ud83c\udffe',
								_List_fromArray(
									['1f44d', '1f3fe'])),
								_Utils_Tuple2(
								'👍\ud83c\udfff',
								_List_fromArray(
									['1f44d', '1f3ff'])),
								_Utils_Tuple2(
								'👎',
								_List_fromArray(
									['1f44e'])),
								_Utils_Tuple2(
								'👎\ud83c\udffb',
								_List_fromArray(
									['1f44e', '1f3fb'])),
								_Utils_Tuple2(
								'👎\ud83c\udffc',
								_List_fromArray(
									['1f44e', '1f3fc'])),
								_Utils_Tuple2(
								'👎\ud83c\udffd',
								_List_fromArray(
									['1f44e', '1f3fd'])),
								_Utils_Tuple2(
								'👎\ud83c\udffe',
								_List_fromArray(
									['1f44e', '1f3fe'])),
								_Utils_Tuple2(
								'👎\ud83c\udfff',
								_List_fromArray(
									['1f44e', '1f3ff'])),
								_Utils_Tuple2(
								'✊',
								_List_fromArray(
									['270a'])),
								_Utils_Tuple2(
								'✊\ud83c\udffb',
								_List_fromArray(
									['270a', '1f3fb'])),
								_Utils_Tuple2(
								'✊\ud83c\udffc',
								_List_fromArray(
									['270a', '1f3fc'])),
								_Utils_Tuple2(
								'✊\ud83c\udffd',
								_List_fromArray(
									['270a', '1f3fd'])),
								_Utils_Tuple2(
								'✊\ud83c\udffe',
								_List_fromArray(
									['270a', '1f3fe'])),
								_Utils_Tuple2(
								'✊\ud83c\udfff',
								_List_fromArray(
									['270a', '1f3ff'])),
								_Utils_Tuple2(
								'👊',
								_List_fromArray(
									['1f44a'])),
								_Utils_Tuple2(
								'👊\ud83c\udffb',
								_List_fromArray(
									['1f44a', '1f3fb'])),
								_Utils_Tuple2(
								'👊\ud83c\udffc',
								_List_fromArray(
									['1f44a', '1f3fc'])),
								_Utils_Tuple2(
								'👊\ud83c\udffd',
								_List_fromArray(
									['1f44a', '1f3fd'])),
								_Utils_Tuple2(
								'👊\ud83c\udffe',
								_List_fromArray(
									['1f44a', '1f3fe'])),
								_Utils_Tuple2(
								'👊\ud83c\udfff',
								_List_fromArray(
									['1f44a', '1f3ff'])),
								_Utils_Tuple2(
								'\ud83e\udd1b',
								_List_fromArray(
									['1f91b'])),
								_Utils_Tuple2(
								'\ud83e\udd1b\ud83c\udffb',
								_List_fromArray(
									['1f91b', '1f3fb'])),
								_Utils_Tuple2(
								'\ud83e\udd1b\ud83c\udffc',
								_List_fromArray(
									['1f91b', '1f3fc'])),
								_Utils_Tuple2(
								'\ud83e\udd1b\ud83c\udffd',
								_List_fromArray(
									['1f91b', '1f3fd'])),
								_Utils_Tuple2(
								'\ud83e\udd1b\ud83c\udffe',
								_List_fromArray(
									['1f91b', '1f3fe'])),
								_Utils_Tuple2(
								'\ud83e\udd1b\ud83c\udfff',
								_List_fromArray(
									['1f91b', '1f3ff'])),
								_Utils_Tuple2(
								'\ud83e\udd1c',
								_List_fromArray(
									['1f91c'])),
								_Utils_Tuple2(
								'\ud83e\udd1c\ud83c\udffb',
								_List_fromArray(
									['1f91c', '1f3fb'])),
								_Utils_Tuple2(
								'\ud83e\udd1c\ud83c\udffc',
								_List_fromArray(
									['1f91c', '1f3fc'])),
								_Utils_Tuple2(
								'\ud83e\udd1c\ud83c\udffd',
								_List_fromArray(
									['1f91c', '1f3fd'])),
								_Utils_Tuple2(
								'\ud83e\udd1c\ud83c\udffe',
								_List_fromArray(
									['1f91c', '1f3fe'])),
								_Utils_Tuple2(
								'\ud83e\udd1c\ud83c\udfff',
								_List_fromArray(
									['1f91c', '1f3ff'])),
								_Utils_Tuple2(
								'\ud83e\udd1a',
								_List_fromArray(
									['1f91a'])),
								_Utils_Tuple2(
								'\ud83e\udd1a\ud83c\udffb',
								_List_fromArray(
									['1f91a', '1f3fb'])),
								_Utils_Tuple2(
								'\ud83e\udd1a\ud83c\udffc',
								_List_fromArray(
									['1f91a', '1f3fc'])),
								_Utils_Tuple2(
								'\ud83e\udd1a\ud83c\udffd',
								_List_fromArray(
									['1f91a', '1f3fd'])),
								_Utils_Tuple2(
								'\ud83e\udd1a\ud83c\udffe',
								_List_fromArray(
									['1f91a', '1f3fe'])),
								_Utils_Tuple2(
								'\ud83e\udd1a\ud83c\udfff',
								_List_fromArray(
									['1f91a', '1f3ff'])),
								_Utils_Tuple2(
								'👋',
								_List_fromArray(
									['1f44b'])),
								_Utils_Tuple2(
								'👋\ud83c\udffb',
								_List_fromArray(
									['1f44b', '1f3fb'])),
								_Utils_Tuple2(
								'👋\ud83c\udffc',
								_List_fromArray(
									['1f44b', '1f3fc'])),
								_Utils_Tuple2(
								'👋\ud83c\udffd',
								_List_fromArray(
									['1f44b', '1f3fd'])),
								_Utils_Tuple2(
								'👋\ud83c\udffe',
								_List_fromArray(
									['1f44b', '1f3fe'])),
								_Utils_Tuple2(
								'👋\ud83c\udfff',
								_List_fromArray(
									['1f44b', '1f3ff'])),
								_Utils_Tuple2(
								'👏',
								_List_fromArray(
									['1f44f'])),
								_Utils_Tuple2(
								'👏\ud83c\udffb',
								_List_fromArray(
									['1f44f', '1f3fb'])),
								_Utils_Tuple2(
								'👏\ud83c\udffc',
								_List_fromArray(
									['1f44f', '1f3fc'])),
								_Utils_Tuple2(
								'👏\ud83c\udffd',
								_List_fromArray(
									['1f44f', '1f3fd'])),
								_Utils_Tuple2(
								'👏\ud83c\udffe',
								_List_fromArray(
									['1f44f', '1f3fe'])),
								_Utils_Tuple2(
								'👏\ud83c\udfff',
								_List_fromArray(
									['1f44f', '1f3ff'])),
								_Utils_Tuple2(
								'✍',
								_List_fromArray(
									['270d'])),
								_Utils_Tuple2(
								'✍\ud83c\udffb',
								_List_fromArray(
									['270d', '1f3fb'])),
								_Utils_Tuple2(
								'✍\ud83c\udffc',
								_List_fromArray(
									['270d', '1f3fc'])),
								_Utils_Tuple2(
								'✍\ud83c\udffd',
								_List_fromArray(
									['270d', '1f3fd'])),
								_Utils_Tuple2(
								'✍\ud83c\udffe',
								_List_fromArray(
									['270d', '1f3fe'])),
								_Utils_Tuple2(
								'✍\ud83c\udfff',
								_List_fromArray(
									['270d', '1f3ff'])),
								_Utils_Tuple2(
								'👐',
								_List_fromArray(
									['1f450'])),
								_Utils_Tuple2(
								'👐\ud83c\udffb',
								_List_fromArray(
									['1f450', '1f3fb']))
							]),
						_Utils_ap(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'👐\ud83c\udffc',
									_List_fromArray(
										['1f450', '1f3fc'])),
									_Utils_Tuple2(
									'👐\ud83c\udffd',
									_List_fromArray(
										['1f450', '1f3fd'])),
									_Utils_Tuple2(
									'👐\ud83c\udffe',
									_List_fromArray(
										['1f450', '1f3fe'])),
									_Utils_Tuple2(
									'👐\ud83c\udfff',
									_List_fromArray(
										['1f450', '1f3ff'])),
									_Utils_Tuple2(
									'🙌',
									_List_fromArray(
										['1f64c'])),
									_Utils_Tuple2(
									'🙌\ud83c\udffb',
									_List_fromArray(
										['1f64c', '1f3fb'])),
									_Utils_Tuple2(
									'🙌\ud83c\udffc',
									_List_fromArray(
										['1f64c', '1f3fc'])),
									_Utils_Tuple2(
									'🙌\ud83c\udffd',
									_List_fromArray(
										['1f64c', '1f3fd'])),
									_Utils_Tuple2(
									'🙌\ud83c\udffe',
									_List_fromArray(
										['1f64c', '1f3fe'])),
									_Utils_Tuple2(
									'🙌\ud83c\udfff',
									_List_fromArray(
										['1f64c', '1f3ff'])),
									_Utils_Tuple2(
									'🙏',
									_List_fromArray(
										['1f64f'])),
									_Utils_Tuple2(
									'🙏\ud83c\udffb',
									_List_fromArray(
										['1f64f', '1f3fb'])),
									_Utils_Tuple2(
									'🙏\ud83c\udffc',
									_List_fromArray(
										['1f64f', '1f3fc'])),
									_Utils_Tuple2(
									'🙏\ud83c\udffd',
									_List_fromArray(
										['1f64f', '1f3fd'])),
									_Utils_Tuple2(
									'🙏\ud83c\udffe',
									_List_fromArray(
										['1f64f', '1f3fe'])),
									_Utils_Tuple2(
									'🙏\ud83c\udfff',
									_List_fromArray(
										['1f64f', '1f3ff'])),
									_Utils_Tuple2(
									'\ud83e\udd1d',
									_List_fromArray(
										['1f91d'])),
									_Utils_Tuple2(
									'\ud83e\udd1d\ud83c\udffb',
									_List_fromArray(
										['1f91d', '1f3fb'])),
									_Utils_Tuple2(
									'\ud83e\udd1d\ud83c\udffc',
									_List_fromArray(
										['1f91d', '1f3fc'])),
									_Utils_Tuple2(
									'\ud83e\udd1d\ud83c\udffd',
									_List_fromArray(
										['1f91d', '1f3fd'])),
									_Utils_Tuple2(
									'\ud83e\udd1d\ud83c\udffe',
									_List_fromArray(
										['1f91d', '1f3fe'])),
									_Utils_Tuple2(
									'\ud83e\udd1d\ud83c\udfff',
									_List_fromArray(
										['1f91d', '1f3ff'])),
									_Utils_Tuple2(
									'💅',
									_List_fromArray(
										['1f485'])),
									_Utils_Tuple2(
									'💅\ud83c\udffb',
									_List_fromArray(
										['1f485', '1f3fb'])),
									_Utils_Tuple2(
									'💅\ud83c\udffc',
									_List_fromArray(
										['1f485', '1f3fc'])),
									_Utils_Tuple2(
									'💅\ud83c\udffd',
									_List_fromArray(
										['1f485', '1f3fd'])),
									_Utils_Tuple2(
									'💅\ud83c\udffe',
									_List_fromArray(
										['1f485', '1f3fe'])),
									_Utils_Tuple2(
									'💅\ud83c\udfff',
									_List_fromArray(
										['1f485', '1f3ff'])),
									_Utils_Tuple2(
									'👂',
									_List_fromArray(
										['1f442'])),
									_Utils_Tuple2(
									'👂\ud83c\udffb',
									_List_fromArray(
										['1f442', '1f3fb'])),
									_Utils_Tuple2(
									'👂\ud83c\udffc',
									_List_fromArray(
										['1f442', '1f3fc'])),
									_Utils_Tuple2(
									'👂\ud83c\udffd',
									_List_fromArray(
										['1f442', '1f3fd'])),
									_Utils_Tuple2(
									'👂\ud83c\udffe',
									_List_fromArray(
										['1f442', '1f3fe'])),
									_Utils_Tuple2(
									'👂\ud83c\udfff',
									_List_fromArray(
										['1f442', '1f3ff'])),
									_Utils_Tuple2(
									'👃',
									_List_fromArray(
										['1f443'])),
									_Utils_Tuple2(
									'👃\ud83c\udffb',
									_List_fromArray(
										['1f443', '1f3fb'])),
									_Utils_Tuple2(
									'👃\ud83c\udffc',
									_List_fromArray(
										['1f443', '1f3fc'])),
									_Utils_Tuple2(
									'👃\ud83c\udffd',
									_List_fromArray(
										['1f443', '1f3fd'])),
									_Utils_Tuple2(
									'👃\ud83c\udffe',
									_List_fromArray(
										['1f443', '1f3fe'])),
									_Utils_Tuple2(
									'👃\ud83c\udfff',
									_List_fromArray(
										['1f443', '1f3ff'])),
									_Utils_Tuple2(
									'👣',
									_List_fromArray(
										['1f463'])),
									_Utils_Tuple2(
									'👀',
									_List_fromArray(
										['1f440'])),
									_Utils_Tuple2(
									'👁',
									_List_fromArray(
										['1f441'])),
									_Utils_Tuple2(
									'👁\u200d🗨',
									_List_fromArray(
										['1f441', '200d', '1f5e8'])),
									_Utils_Tuple2(
									'👅',
									_List_fromArray(
										['1f445'])),
									_Utils_Tuple2(
									'👄',
									_List_fromArray(
										['1f444'])),
									_Utils_Tuple2(
									'💋',
									_List_fromArray(
										['1f48b'])),
									_Utils_Tuple2(
									'💘',
									_List_fromArray(
										['1f498'])),
									_Utils_Tuple2(
									'❤',
									_List_fromArray(
										['2764'])),
									_Utils_Tuple2(
									'💓',
									_List_fromArray(
										['1f493'])),
									_Utils_Tuple2(
									'💔',
									_List_fromArray(
										['1f494'])),
									_Utils_Tuple2(
									'💕',
									_List_fromArray(
										['1f495'])),
									_Utils_Tuple2(
									'💖',
									_List_fromArray(
										['1f496'])),
									_Utils_Tuple2(
									'💗',
									_List_fromArray(
										['1f497'])),
									_Utils_Tuple2(
									'💙',
									_List_fromArray(
										['1f499'])),
									_Utils_Tuple2(
									'💚',
									_List_fromArray(
										['1f49a'])),
									_Utils_Tuple2(
									'💛',
									_List_fromArray(
										['1f49b'])),
									_Utils_Tuple2(
									'💜',
									_List_fromArray(
										['1f49c'])),
									_Utils_Tuple2(
									'\ud83d\udda4',
									_List_fromArray(
										['1f5a4'])),
									_Utils_Tuple2(
									'💝',
									_List_fromArray(
										['1f49d'])),
									_Utils_Tuple2(
									'💞',
									_List_fromArray(
										['1f49e'])),
									_Utils_Tuple2(
									'💟',
									_List_fromArray(
										['1f49f'])),
									_Utils_Tuple2(
									'❣',
									_List_fromArray(
										['2763'])),
									_Utils_Tuple2(
									'💌',
									_List_fromArray(
										['1f48c'])),
									_Utils_Tuple2(
									'💤',
									_List_fromArray(
										['1f4a4'])),
									_Utils_Tuple2(
									'💢',
									_List_fromArray(
										['1f4a2'])),
									_Utils_Tuple2(
									'💣',
									_List_fromArray(
										['1f4a3'])),
									_Utils_Tuple2(
									'💥',
									_List_fromArray(
										['1f4a5'])),
									_Utils_Tuple2(
									'💦',
									_List_fromArray(
										['1f4a6'])),
									_Utils_Tuple2(
									'💨',
									_List_fromArray(
										['1f4a8'])),
									_Utils_Tuple2(
									'💫',
									_List_fromArray(
										['1f4ab'])),
									_Utils_Tuple2(
									'💬',
									_List_fromArray(
										['1f4ac'])),
									_Utils_Tuple2(
									'🗨',
									_List_fromArray(
										['1f5e8'])),
									_Utils_Tuple2(
									'🗯',
									_List_fromArray(
										['1f5ef'])),
									_Utils_Tuple2(
									'💭',
									_List_fromArray(
										['1f4ad'])),
									_Utils_Tuple2(
									'🕳',
									_List_fromArray(
										['1f573'])),
									_Utils_Tuple2(
									'👓',
									_List_fromArray(
										['1f453'])),
									_Utils_Tuple2(
									'🕶',
									_List_fromArray(
										['1f576'])),
									_Utils_Tuple2(
									'👔',
									_List_fromArray(
										['1f454'])),
									_Utils_Tuple2(
									'👕',
									_List_fromArray(
										['1f455'])),
									_Utils_Tuple2(
									'👖',
									_List_fromArray(
										['1f456'])),
									_Utils_Tuple2(
									'👗',
									_List_fromArray(
										['1f457'])),
									_Utils_Tuple2(
									'👘',
									_List_fromArray(
										['1f458'])),
									_Utils_Tuple2(
									'👙',
									_List_fromArray(
										['1f459'])),
									_Utils_Tuple2(
									'👚',
									_List_fromArray(
										['1f45a'])),
									_Utils_Tuple2(
									'👛',
									_List_fromArray(
										['1f45b'])),
									_Utils_Tuple2(
									'👜',
									_List_fromArray(
										['1f45c'])),
									_Utils_Tuple2(
									'👝',
									_List_fromArray(
										['1f45d'])),
									_Utils_Tuple2(
									'🛍',
									_List_fromArray(
										['1f6cd'])),
									_Utils_Tuple2(
									'🎒',
									_List_fromArray(
										['1f392'])),
									_Utils_Tuple2(
									'👞',
									_List_fromArray(
										['1f45e'])),
									_Utils_Tuple2(
									'👟',
									_List_fromArray(
										['1f45f'])),
									_Utils_Tuple2(
									'👠',
									_List_fromArray(
										['1f460'])),
									_Utils_Tuple2(
									'👡',
									_List_fromArray(
										['1f461'])),
									_Utils_Tuple2(
									'👢',
									_List_fromArray(
										['1f462'])),
									_Utils_Tuple2(
									'👑',
									_List_fromArray(
										['1f451'])),
									_Utils_Tuple2(
									'👒',
									_List_fromArray(
										['1f452'])),
									_Utils_Tuple2(
									'🎩',
									_List_fromArray(
										['1f3a9'])),
									_Utils_Tuple2(
									'🎓',
									_List_fromArray(
										['1f393']))
								]),
							_Utils_ap(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'⛑',
										_List_fromArray(
											['26d1'])),
										_Utils_Tuple2(
										'\ud83d\udcff',
										_List_fromArray(
											['1f4ff'])),
										_Utils_Tuple2(
										'💄',
										_List_fromArray(
											['1f484'])),
										_Utils_Tuple2(
										'💍',
										_List_fromArray(
											['1f48d'])),
										_Utils_Tuple2(
										'💎',
										_List_fromArray(
											['1f48e'])),
										_Utils_Tuple2(
										'🐵',
										_List_fromArray(
											['1f435'])),
										_Utils_Tuple2(
										'🐒',
										_List_fromArray(
											['1f412'])),
										_Utils_Tuple2(
										'\ud83e\udd8d',
										_List_fromArray(
											['1f98d'])),
										_Utils_Tuple2(
										'🐶',
										_List_fromArray(
											['1f436'])),
										_Utils_Tuple2(
										'🐕',
										_List_fromArray(
											['1f415'])),
										_Utils_Tuple2(
										'🐩',
										_List_fromArray(
											['1f429'])),
										_Utils_Tuple2(
										'🐺',
										_List_fromArray(
											['1f43a'])),
										_Utils_Tuple2(
										'\ud83e\udd8a',
										_List_fromArray(
											['1f98a'])),
										_Utils_Tuple2(
										'🐱',
										_List_fromArray(
											['1f431'])),
										_Utils_Tuple2(
										'🐈',
										_List_fromArray(
											['1f408'])),
										_Utils_Tuple2(
										'\ud83e\udd81',
										_List_fromArray(
											['1f981'])),
										_Utils_Tuple2(
										'🐯',
										_List_fromArray(
											['1f42f'])),
										_Utils_Tuple2(
										'🐅',
										_List_fromArray(
											['1f405'])),
										_Utils_Tuple2(
										'🐆',
										_List_fromArray(
											['1f406'])),
										_Utils_Tuple2(
										'🐴',
										_List_fromArray(
											['1f434'])),
										_Utils_Tuple2(
										'🐎',
										_List_fromArray(
											['1f40e'])),
										_Utils_Tuple2(
										'\ud83e\udd8c',
										_List_fromArray(
											['1f98c'])),
										_Utils_Tuple2(
										'\ud83e\udd84',
										_List_fromArray(
											['1f984'])),
										_Utils_Tuple2(
										'🐮',
										_List_fromArray(
											['1f42e'])),
										_Utils_Tuple2(
										'🐂',
										_List_fromArray(
											['1f402'])),
										_Utils_Tuple2(
										'🐃',
										_List_fromArray(
											['1f403'])),
										_Utils_Tuple2(
										'🐄',
										_List_fromArray(
											['1f404'])),
										_Utils_Tuple2(
										'🐷',
										_List_fromArray(
											['1f437'])),
										_Utils_Tuple2(
										'🐖',
										_List_fromArray(
											['1f416'])),
										_Utils_Tuple2(
										'🐗',
										_List_fromArray(
											['1f417'])),
										_Utils_Tuple2(
										'🐽',
										_List_fromArray(
											['1f43d'])),
										_Utils_Tuple2(
										'🐏',
										_List_fromArray(
											['1f40f'])),
										_Utils_Tuple2(
										'🐑',
										_List_fromArray(
											['1f411'])),
										_Utils_Tuple2(
										'🐐',
										_List_fromArray(
											['1f410'])),
										_Utils_Tuple2(
										'🐪',
										_List_fromArray(
											['1f42a'])),
										_Utils_Tuple2(
										'🐫',
										_List_fromArray(
											['1f42b'])),
										_Utils_Tuple2(
										'🐘',
										_List_fromArray(
											['1f418'])),
										_Utils_Tuple2(
										'\ud83e\udd8f',
										_List_fromArray(
											['1f98f'])),
										_Utils_Tuple2(
										'🐭',
										_List_fromArray(
											['1f42d'])),
										_Utils_Tuple2(
										'🐁',
										_List_fromArray(
											['1f401'])),
										_Utils_Tuple2(
										'🐀',
										_List_fromArray(
											['1f400'])),
										_Utils_Tuple2(
										'🐹',
										_List_fromArray(
											['1f439'])),
										_Utils_Tuple2(
										'🐰',
										_List_fromArray(
											['1f430'])),
										_Utils_Tuple2(
										'🐇',
										_List_fromArray(
											['1f407'])),
										_Utils_Tuple2(
										'🐿',
										_List_fromArray(
											['1f43f'])),
										_Utils_Tuple2(
										'\ud83e\udd87',
										_List_fromArray(
											['1f987'])),
										_Utils_Tuple2(
										'🐻',
										_List_fromArray(
											['1f43b'])),
										_Utils_Tuple2(
										'🐨',
										_List_fromArray(
											['1f428'])),
										_Utils_Tuple2(
										'🐼',
										_List_fromArray(
											['1f43c'])),
										_Utils_Tuple2(
										'🐾',
										_List_fromArray(
											['1f43e'])),
										_Utils_Tuple2(
										'\ud83e\udd83',
										_List_fromArray(
											['1f983'])),
										_Utils_Tuple2(
										'🐔',
										_List_fromArray(
											['1f414'])),
										_Utils_Tuple2(
										'🐓',
										_List_fromArray(
											['1f413'])),
										_Utils_Tuple2(
										'🐣',
										_List_fromArray(
											['1f423'])),
										_Utils_Tuple2(
										'🐤',
										_List_fromArray(
											['1f424'])),
										_Utils_Tuple2(
										'🐥',
										_List_fromArray(
											['1f425'])),
										_Utils_Tuple2(
										'🐦',
										_List_fromArray(
											['1f426'])),
										_Utils_Tuple2(
										'🐧',
										_List_fromArray(
											['1f427'])),
										_Utils_Tuple2(
										'🕊',
										_List_fromArray(
											['1f54a'])),
										_Utils_Tuple2(
										'\ud83e\udd85',
										_List_fromArray(
											['1f985'])),
										_Utils_Tuple2(
										'\ud83e\udd86',
										_List_fromArray(
											['1f986'])),
										_Utils_Tuple2(
										'\ud83e\udd89',
										_List_fromArray(
											['1f989'])),
										_Utils_Tuple2(
										'🐸',
										_List_fromArray(
											['1f438'])),
										_Utils_Tuple2(
										'🐊',
										_List_fromArray(
											['1f40a'])),
										_Utils_Tuple2(
										'🐢',
										_List_fromArray(
											['1f422'])),
										_Utils_Tuple2(
										'\ud83e\udd8e',
										_List_fromArray(
											['1f98e'])),
										_Utils_Tuple2(
										'🐍',
										_List_fromArray(
											['1f40d'])),
										_Utils_Tuple2(
										'🐲',
										_List_fromArray(
											['1f432'])),
										_Utils_Tuple2(
										'🐉',
										_List_fromArray(
											['1f409'])),
										_Utils_Tuple2(
										'🐳',
										_List_fromArray(
											['1f433'])),
										_Utils_Tuple2(
										'🐋',
										_List_fromArray(
											['1f40b'])),
										_Utils_Tuple2(
										'🐬',
										_List_fromArray(
											['1f42c'])),
										_Utils_Tuple2(
										'🐟',
										_List_fromArray(
											['1f41f'])),
										_Utils_Tuple2(
										'🐠',
										_List_fromArray(
											['1f420'])),
										_Utils_Tuple2(
										'🐡',
										_List_fromArray(
											['1f421'])),
										_Utils_Tuple2(
										'\ud83e\udd88',
										_List_fromArray(
											['1f988'])),
										_Utils_Tuple2(
										'🐙',
										_List_fromArray(
											['1f419'])),
										_Utils_Tuple2(
										'🐚',
										_List_fromArray(
											['1f41a'])),
										_Utils_Tuple2(
										'\ud83e\udd80',
										_List_fromArray(
											['1f980'])),
										_Utils_Tuple2(
										'\ud83e\udd90',
										_List_fromArray(
											['1f990'])),
										_Utils_Tuple2(
										'\ud83e\udd91',
										_List_fromArray(
											['1f991'])),
										_Utils_Tuple2(
										'\ud83e\udd8b',
										_List_fromArray(
											['1f98b'])),
										_Utils_Tuple2(
										'🐌',
										_List_fromArray(
											['1f40c'])),
										_Utils_Tuple2(
										'🐛',
										_List_fromArray(
											['1f41b'])),
										_Utils_Tuple2(
										'🐜',
										_List_fromArray(
											['1f41c'])),
										_Utils_Tuple2(
										'🐝',
										_List_fromArray(
											['1f41d'])),
										_Utils_Tuple2(
										'🐞',
										_List_fromArray(
											['1f41e'])),
										_Utils_Tuple2(
										'🕷',
										_List_fromArray(
											['1f577'])),
										_Utils_Tuple2(
										'🕸',
										_List_fromArray(
											['1f578'])),
										_Utils_Tuple2(
										'\ud83e\udd82',
										_List_fromArray(
											['1f982'])),
										_Utils_Tuple2(
										'💐',
										_List_fromArray(
											['1f490'])),
										_Utils_Tuple2(
										'🌸',
										_List_fromArray(
											['1f338'])),
										_Utils_Tuple2(
										'💮',
										_List_fromArray(
											['1f4ae'])),
										_Utils_Tuple2(
										'🏵',
										_List_fromArray(
											['1f3f5'])),
										_Utils_Tuple2(
										'🌹',
										_List_fromArray(
											['1f339'])),
										_Utils_Tuple2(
										'\ud83e\udd40',
										_List_fromArray(
											['1f940'])),
										_Utils_Tuple2(
										'🌺',
										_List_fromArray(
											['1f33a'])),
										_Utils_Tuple2(
										'🌻',
										_List_fromArray(
											['1f33b'])),
										_Utils_Tuple2(
										'🌼',
										_List_fromArray(
											['1f33c']))
									]),
								_Utils_ap(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'🌷',
											_List_fromArray(
												['1f337'])),
											_Utils_Tuple2(
											'🌱',
											_List_fromArray(
												['1f331'])),
											_Utils_Tuple2(
											'🌲',
											_List_fromArray(
												['1f332'])),
											_Utils_Tuple2(
											'🌳',
											_List_fromArray(
												['1f333'])),
											_Utils_Tuple2(
											'🌴',
											_List_fromArray(
												['1f334'])),
											_Utils_Tuple2(
											'🌵',
											_List_fromArray(
												['1f335'])),
											_Utils_Tuple2(
											'🌾',
											_List_fromArray(
												['1f33e'])),
											_Utils_Tuple2(
											'🌿',
											_List_fromArray(
												['1f33f'])),
											_Utils_Tuple2(
											'☘',
											_List_fromArray(
												['2618'])),
											_Utils_Tuple2(
											'🍀',
											_List_fromArray(
												['1f340'])),
											_Utils_Tuple2(
											'🍁',
											_List_fromArray(
												['1f341'])),
											_Utils_Tuple2(
											'🍂',
											_List_fromArray(
												['1f342'])),
											_Utils_Tuple2(
											'🍃',
											_List_fromArray(
												['1f343'])),
											_Utils_Tuple2(
											'🍇',
											_List_fromArray(
												['1f347'])),
											_Utils_Tuple2(
											'🍈',
											_List_fromArray(
												['1f348'])),
											_Utils_Tuple2(
											'🍉',
											_List_fromArray(
												['1f349'])),
											_Utils_Tuple2(
											'🍊',
											_List_fromArray(
												['1f34a'])),
											_Utils_Tuple2(
											'🍋',
											_List_fromArray(
												['1f34b'])),
											_Utils_Tuple2(
											'🍌',
											_List_fromArray(
												['1f34c'])),
											_Utils_Tuple2(
											'🍍',
											_List_fromArray(
												['1f34d'])),
											_Utils_Tuple2(
											'🍎',
											_List_fromArray(
												['1f34e'])),
											_Utils_Tuple2(
											'🍏',
											_List_fromArray(
												['1f34f'])),
											_Utils_Tuple2(
											'🍐',
											_List_fromArray(
												['1f350'])),
											_Utils_Tuple2(
											'🍑',
											_List_fromArray(
												['1f351'])),
											_Utils_Tuple2(
											'🍒',
											_List_fromArray(
												['1f352'])),
											_Utils_Tuple2(
											'🍓',
											_List_fromArray(
												['1f353'])),
											_Utils_Tuple2(
											'\ud83e\udd5d',
											_List_fromArray(
												['1f95d'])),
											_Utils_Tuple2(
											'🍅',
											_List_fromArray(
												['1f345'])),
											_Utils_Tuple2(
											'\ud83e\udd51',
											_List_fromArray(
												['1f951'])),
											_Utils_Tuple2(
											'🍆',
											_List_fromArray(
												['1f346'])),
											_Utils_Tuple2(
											'\ud83e\udd54',
											_List_fromArray(
												['1f954'])),
											_Utils_Tuple2(
											'\ud83e\udd55',
											_List_fromArray(
												['1f955'])),
											_Utils_Tuple2(
											'🌽',
											_List_fromArray(
												['1f33d'])),
											_Utils_Tuple2(
											'🌶',
											_List_fromArray(
												['1f336'])),
											_Utils_Tuple2(
											'\ud83e\udd52',
											_List_fromArray(
												['1f952'])),
											_Utils_Tuple2(
											'🍄',
											_List_fromArray(
												['1f344'])),
											_Utils_Tuple2(
											'\ud83e\udd5c',
											_List_fromArray(
												['1f95c'])),
											_Utils_Tuple2(
											'🌰',
											_List_fromArray(
												['1f330'])),
											_Utils_Tuple2(
											'🍞',
											_List_fromArray(
												['1f35e'])),
											_Utils_Tuple2(
											'\ud83e\udd50',
											_List_fromArray(
												['1f950'])),
											_Utils_Tuple2(
											'\ud83e\udd56',
											_List_fromArray(
												['1f956'])),
											_Utils_Tuple2(
											'\ud83e\udd5e',
											_List_fromArray(
												['1f95e'])),
											_Utils_Tuple2(
											'\ud83e\uddc0',
											_List_fromArray(
												['1f9c0'])),
											_Utils_Tuple2(
											'🍖',
											_List_fromArray(
												['1f356'])),
											_Utils_Tuple2(
											'🍗',
											_List_fromArray(
												['1f357'])),
											_Utils_Tuple2(
											'\ud83e\udd53',
											_List_fromArray(
												['1f953'])),
											_Utils_Tuple2(
											'🍔',
											_List_fromArray(
												['1f354'])),
											_Utils_Tuple2(
											'🍟',
											_List_fromArray(
												['1f35f'])),
											_Utils_Tuple2(
											'🍕',
											_List_fromArray(
												['1f355'])),
											_Utils_Tuple2(
											'\ud83c\udf2d',
											_List_fromArray(
												['1f32d'])),
											_Utils_Tuple2(
											'\ud83c\udf2e',
											_List_fromArray(
												['1f32e'])),
											_Utils_Tuple2(
											'\ud83c\udf2f',
											_List_fromArray(
												['1f32f'])),
											_Utils_Tuple2(
											'\ud83e\udd59',
											_List_fromArray(
												['1f959'])),
											_Utils_Tuple2(
											'\ud83e\udd5a',
											_List_fromArray(
												['1f95a'])),
											_Utils_Tuple2(
											'🍳',
											_List_fromArray(
												['1f373'])),
											_Utils_Tuple2(
											'\ud83e\udd58',
											_List_fromArray(
												['1f958'])),
											_Utils_Tuple2(
											'🍲',
											_List_fromArray(
												['1f372'])),
											_Utils_Tuple2(
											'\ud83e\udd57',
											_List_fromArray(
												['1f957'])),
											_Utils_Tuple2(
											'\ud83c\udf7f',
											_List_fromArray(
												['1f37f'])),
											_Utils_Tuple2(
											'🍱',
											_List_fromArray(
												['1f371'])),
											_Utils_Tuple2(
											'🍘',
											_List_fromArray(
												['1f358'])),
											_Utils_Tuple2(
											'🍙',
											_List_fromArray(
												['1f359'])),
											_Utils_Tuple2(
											'🍚',
											_List_fromArray(
												['1f35a'])),
											_Utils_Tuple2(
											'🍛',
											_List_fromArray(
												['1f35b'])),
											_Utils_Tuple2(
											'🍜',
											_List_fromArray(
												['1f35c'])),
											_Utils_Tuple2(
											'🍝',
											_List_fromArray(
												['1f35d'])),
											_Utils_Tuple2(
											'🍠',
											_List_fromArray(
												['1f360'])),
											_Utils_Tuple2(
											'🍢',
											_List_fromArray(
												['1f362'])),
											_Utils_Tuple2(
											'🍣',
											_List_fromArray(
												['1f363'])),
											_Utils_Tuple2(
											'🍤',
											_List_fromArray(
												['1f364'])),
											_Utils_Tuple2(
											'🍥',
											_List_fromArray(
												['1f365'])),
											_Utils_Tuple2(
											'🍡',
											_List_fromArray(
												['1f361'])),
											_Utils_Tuple2(
											'🍦',
											_List_fromArray(
												['1f366'])),
											_Utils_Tuple2(
											'🍧',
											_List_fromArray(
												['1f367'])),
											_Utils_Tuple2(
											'🍨',
											_List_fromArray(
												['1f368'])),
											_Utils_Tuple2(
											'🍩',
											_List_fromArray(
												['1f369'])),
											_Utils_Tuple2(
											'🍪',
											_List_fromArray(
												['1f36a'])),
											_Utils_Tuple2(
											'🎂',
											_List_fromArray(
												['1f382'])),
											_Utils_Tuple2(
											'🍰',
											_List_fromArray(
												['1f370'])),
											_Utils_Tuple2(
											'🍫',
											_List_fromArray(
												['1f36b'])),
											_Utils_Tuple2(
											'🍬',
											_List_fromArray(
												['1f36c'])),
											_Utils_Tuple2(
											'🍭',
											_List_fromArray(
												['1f36d'])),
											_Utils_Tuple2(
											'🍮',
											_List_fromArray(
												['1f36e'])),
											_Utils_Tuple2(
											'🍯',
											_List_fromArray(
												['1f36f'])),
											_Utils_Tuple2(
											'🍼',
											_List_fromArray(
												['1f37c'])),
											_Utils_Tuple2(
											'\ud83e\udd5b',
											_List_fromArray(
												['1f95b'])),
											_Utils_Tuple2(
											'☕',
											_List_fromArray(
												['2615'])),
											_Utils_Tuple2(
											'🍵',
											_List_fromArray(
												['1f375'])),
											_Utils_Tuple2(
											'🍶',
											_List_fromArray(
												['1f376'])),
											_Utils_Tuple2(
											'\ud83c\udf7e',
											_List_fromArray(
												['1f37e'])),
											_Utils_Tuple2(
											'🍷',
											_List_fromArray(
												['1f377'])),
											_Utils_Tuple2(
											'🍸',
											_List_fromArray(
												['1f378'])),
											_Utils_Tuple2(
											'🍹',
											_List_fromArray(
												['1f379'])),
											_Utils_Tuple2(
											'🍺',
											_List_fromArray(
												['1f37a'])),
											_Utils_Tuple2(
											'🍻',
											_List_fromArray(
												['1f37b'])),
											_Utils_Tuple2(
											'\ud83e\udd42',
											_List_fromArray(
												['1f942'])),
											_Utils_Tuple2(
											'\ud83e\udd43',
											_List_fromArray(
												['1f943'])),
											_Utils_Tuple2(
											'🍽',
											_List_fromArray(
												['1f37d'])),
											_Utils_Tuple2(
											'🍴',
											_List_fromArray(
												['1f374']))
										]),
									_Utils_ap(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'\ud83e\udd44',
												_List_fromArray(
													['1f944'])),
												_Utils_Tuple2(
												'🔪',
												_List_fromArray(
													['1f52a'])),
												_Utils_Tuple2(
												'\ud83c\udffa',
												_List_fromArray(
													['1f3fa'])),
												_Utils_Tuple2(
												'🌍',
												_List_fromArray(
													['1f30d'])),
												_Utils_Tuple2(
												'🌎',
												_List_fromArray(
													['1f30e'])),
												_Utils_Tuple2(
												'🌏',
												_List_fromArray(
													['1f30f'])),
												_Utils_Tuple2(
												'🌐',
												_List_fromArray(
													['1f310'])),
												_Utils_Tuple2(
												'🗺',
												_List_fromArray(
													['1f5fa'])),
												_Utils_Tuple2(
												'🗾',
												_List_fromArray(
													['1f5fe'])),
												_Utils_Tuple2(
												'🏔',
												_List_fromArray(
													['1f3d4'])),
												_Utils_Tuple2(
												'⛰',
												_List_fromArray(
													['26f0'])),
												_Utils_Tuple2(
												'🌋',
												_List_fromArray(
													['1f30b'])),
												_Utils_Tuple2(
												'🗻',
												_List_fromArray(
													['1f5fb'])),
												_Utils_Tuple2(
												'🏕',
												_List_fromArray(
													['1f3d5'])),
												_Utils_Tuple2(
												'🏖',
												_List_fromArray(
													['1f3d6'])),
												_Utils_Tuple2(
												'🏜',
												_List_fromArray(
													['1f3dc'])),
												_Utils_Tuple2(
												'🏝',
												_List_fromArray(
													['1f3dd'])),
												_Utils_Tuple2(
												'🏞',
												_List_fromArray(
													['1f3de'])),
												_Utils_Tuple2(
												'🏟',
												_List_fromArray(
													['1f3df'])),
												_Utils_Tuple2(
												'🏛',
												_List_fromArray(
													['1f3db'])),
												_Utils_Tuple2(
												'🏗',
												_List_fromArray(
													['1f3d7'])),
												_Utils_Tuple2(
												'🏘',
												_List_fromArray(
													['1f3d8'])),
												_Utils_Tuple2(
												'🏙',
												_List_fromArray(
													['1f3d9'])),
												_Utils_Tuple2(
												'🏚',
												_List_fromArray(
													['1f3da'])),
												_Utils_Tuple2(
												'🏠',
												_List_fromArray(
													['1f3e0'])),
												_Utils_Tuple2(
												'🏡',
												_List_fromArray(
													['1f3e1'])),
												_Utils_Tuple2(
												'🏢',
												_List_fromArray(
													['1f3e2'])),
												_Utils_Tuple2(
												'🏣',
												_List_fromArray(
													['1f3e3'])),
												_Utils_Tuple2(
												'🏤',
												_List_fromArray(
													['1f3e4'])),
												_Utils_Tuple2(
												'🏥',
												_List_fromArray(
													['1f3e5'])),
												_Utils_Tuple2(
												'🏦',
												_List_fromArray(
													['1f3e6'])),
												_Utils_Tuple2(
												'🏨',
												_List_fromArray(
													['1f3e8'])),
												_Utils_Tuple2(
												'🏩',
												_List_fromArray(
													['1f3e9'])),
												_Utils_Tuple2(
												'🏪',
												_List_fromArray(
													['1f3ea'])),
												_Utils_Tuple2(
												'🏫',
												_List_fromArray(
													['1f3eb'])),
												_Utils_Tuple2(
												'🏬',
												_List_fromArray(
													['1f3ec'])),
												_Utils_Tuple2(
												'🏭',
												_List_fromArray(
													['1f3ed'])),
												_Utils_Tuple2(
												'🏯',
												_List_fromArray(
													['1f3ef'])),
												_Utils_Tuple2(
												'🏰',
												_List_fromArray(
													['1f3f0'])),
												_Utils_Tuple2(
												'💒',
												_List_fromArray(
													['1f492'])),
												_Utils_Tuple2(
												'🗼',
												_List_fromArray(
													['1f5fc'])),
												_Utils_Tuple2(
												'🗽',
												_List_fromArray(
													['1f5fd'])),
												_Utils_Tuple2(
												'⛪',
												_List_fromArray(
													['26ea'])),
												_Utils_Tuple2(
												'\ud83d\udd4c',
												_List_fromArray(
													['1f54c'])),
												_Utils_Tuple2(
												'\ud83d\udd4d',
												_List_fromArray(
													['1f54d'])),
												_Utils_Tuple2(
												'⛩',
												_List_fromArray(
													['26e9'])),
												_Utils_Tuple2(
												'\ud83d\udd4b',
												_List_fromArray(
													['1f54b'])),
												_Utils_Tuple2(
												'⛲',
												_List_fromArray(
													['26f2'])),
												_Utils_Tuple2(
												'⛺',
												_List_fromArray(
													['26fa'])),
												_Utils_Tuple2(
												'🌁',
												_List_fromArray(
													['1f301'])),
												_Utils_Tuple2(
												'🌃',
												_List_fromArray(
													['1f303'])),
												_Utils_Tuple2(
												'🌄',
												_List_fromArray(
													['1f304'])),
												_Utils_Tuple2(
												'🌅',
												_List_fromArray(
													['1f305'])),
												_Utils_Tuple2(
												'🌆',
												_List_fromArray(
													['1f306'])),
												_Utils_Tuple2(
												'🌇',
												_List_fromArray(
													['1f307'])),
												_Utils_Tuple2(
												'🌉',
												_List_fromArray(
													['1f309'])),
												_Utils_Tuple2(
												'♨',
												_List_fromArray(
													['2668'])),
												_Utils_Tuple2(
												'🌌',
												_List_fromArray(
													['1f30c'])),
												_Utils_Tuple2(
												'🎠',
												_List_fromArray(
													['1f3a0'])),
												_Utils_Tuple2(
												'🎡',
												_List_fromArray(
													['1f3a1'])),
												_Utils_Tuple2(
												'🎢',
												_List_fromArray(
													['1f3a2'])),
												_Utils_Tuple2(
												'💈',
												_List_fromArray(
													['1f488'])),
												_Utils_Tuple2(
												'🎪',
												_List_fromArray(
													['1f3aa'])),
												_Utils_Tuple2(
												'🎭',
												_List_fromArray(
													['1f3ad'])),
												_Utils_Tuple2(
												'🖼',
												_List_fromArray(
													['1f5bc'])),
												_Utils_Tuple2(
												'🎨',
												_List_fromArray(
													['1f3a8'])),
												_Utils_Tuple2(
												'🎰',
												_List_fromArray(
													['1f3b0'])),
												_Utils_Tuple2(
												'🚂',
												_List_fromArray(
													['1f682'])),
												_Utils_Tuple2(
												'🚃',
												_List_fromArray(
													['1f683'])),
												_Utils_Tuple2(
												'🚄',
												_List_fromArray(
													['1f684'])),
												_Utils_Tuple2(
												'🚅',
												_List_fromArray(
													['1f685'])),
												_Utils_Tuple2(
												'🚆',
												_List_fromArray(
													['1f686'])),
												_Utils_Tuple2(
												'🚇',
												_List_fromArray(
													['1f687'])),
												_Utils_Tuple2(
												'🚈',
												_List_fromArray(
													['1f688'])),
												_Utils_Tuple2(
												'🚉',
												_List_fromArray(
													['1f689'])),
												_Utils_Tuple2(
												'🚊',
												_List_fromArray(
													['1f68a'])),
												_Utils_Tuple2(
												'🚝',
												_List_fromArray(
													['1f69d'])),
												_Utils_Tuple2(
												'🚞',
												_List_fromArray(
													['1f69e'])),
												_Utils_Tuple2(
												'🚋',
												_List_fromArray(
													['1f68b'])),
												_Utils_Tuple2(
												'🚌',
												_List_fromArray(
													['1f68c'])),
												_Utils_Tuple2(
												'🚍',
												_List_fromArray(
													['1f68d'])),
												_Utils_Tuple2(
												'🚎',
												_List_fromArray(
													['1f68e'])),
												_Utils_Tuple2(
												'🚐',
												_List_fromArray(
													['1f690'])),
												_Utils_Tuple2(
												'🚑',
												_List_fromArray(
													['1f691'])),
												_Utils_Tuple2(
												'🚒',
												_List_fromArray(
													['1f692'])),
												_Utils_Tuple2(
												'🚓',
												_List_fromArray(
													['1f693'])),
												_Utils_Tuple2(
												'🚔',
												_List_fromArray(
													['1f694'])),
												_Utils_Tuple2(
												'🚕',
												_List_fromArray(
													['1f695'])),
												_Utils_Tuple2(
												'🚖',
												_List_fromArray(
													['1f696'])),
												_Utils_Tuple2(
												'🚗',
												_List_fromArray(
													['1f697'])),
												_Utils_Tuple2(
												'🚘',
												_List_fromArray(
													['1f698'])),
												_Utils_Tuple2(
												'🚙',
												_List_fromArray(
													['1f699'])),
												_Utils_Tuple2(
												'🚚',
												_List_fromArray(
													['1f69a'])),
												_Utils_Tuple2(
												'🚛',
												_List_fromArray(
													['1f69b'])),
												_Utils_Tuple2(
												'🚜',
												_List_fromArray(
													['1f69c'])),
												_Utils_Tuple2(
												'🚲',
												_List_fromArray(
													['1f6b2'])),
												_Utils_Tuple2(
												'\ud83d\udef4',
												_List_fromArray(
													['1f6f4'])),
												_Utils_Tuple2(
												'\ud83d\udef5',
												_List_fromArray(
													['1f6f5'])),
												_Utils_Tuple2(
												'🚏',
												_List_fromArray(
													['1f68f']))
											]),
										_Utils_ap(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'🛣',
													_List_fromArray(
														['1f6e3'])),
													_Utils_Tuple2(
													'🛤',
													_List_fromArray(
														['1f6e4'])),
													_Utils_Tuple2(
													'⛽',
													_List_fromArray(
														['26fd'])),
													_Utils_Tuple2(
													'🚨',
													_List_fromArray(
														['1f6a8'])),
													_Utils_Tuple2(
													'🚥',
													_List_fromArray(
														['1f6a5'])),
													_Utils_Tuple2(
													'🚦',
													_List_fromArray(
														['1f6a6'])),
													_Utils_Tuple2(
													'🚧',
													_List_fromArray(
														['1f6a7'])),
													_Utils_Tuple2(
													'\ud83d\uded1',
													_List_fromArray(
														['1f6d1'])),
													_Utils_Tuple2(
													'⚓',
													_List_fromArray(
														['2693'])),
													_Utils_Tuple2(
													'⛵',
													_List_fromArray(
														['26f5'])),
													_Utils_Tuple2(
													'\ud83d\udef6',
													_List_fromArray(
														['1f6f6'])),
													_Utils_Tuple2(
													'🚤',
													_List_fromArray(
														['1f6a4'])),
													_Utils_Tuple2(
													'🛳',
													_List_fromArray(
														['1f6f3'])),
													_Utils_Tuple2(
													'⛴',
													_List_fromArray(
														['26f4'])),
													_Utils_Tuple2(
													'🛥',
													_List_fromArray(
														['1f6e5'])),
													_Utils_Tuple2(
													'🚢',
													_List_fromArray(
														['1f6a2'])),
													_Utils_Tuple2(
													'✈',
													_List_fromArray(
														['2708'])),
													_Utils_Tuple2(
													'🛩',
													_List_fromArray(
														['1f6e9'])),
													_Utils_Tuple2(
													'🛫',
													_List_fromArray(
														['1f6eb'])),
													_Utils_Tuple2(
													'🛬',
													_List_fromArray(
														['1f6ec'])),
													_Utils_Tuple2(
													'💺',
													_List_fromArray(
														['1f4ba'])),
													_Utils_Tuple2(
													'🚁',
													_List_fromArray(
														['1f681'])),
													_Utils_Tuple2(
													'🚟',
													_List_fromArray(
														['1f69f'])),
													_Utils_Tuple2(
													'🚠',
													_List_fromArray(
														['1f6a0'])),
													_Utils_Tuple2(
													'🚡',
													_List_fromArray(
														['1f6a1'])),
													_Utils_Tuple2(
													'🚀',
													_List_fromArray(
														['1f680'])),
													_Utils_Tuple2(
													'🛰',
													_List_fromArray(
														['1f6f0'])),
													_Utils_Tuple2(
													'🛎',
													_List_fromArray(
														['1f6ce'])),
													_Utils_Tuple2(
													'🚪',
													_List_fromArray(
														['1f6aa'])),
													_Utils_Tuple2(
													'🛌',
													_List_fromArray(
														['1f6cc'])),
													_Utils_Tuple2(
													'🛏',
													_List_fromArray(
														['1f6cf'])),
													_Utils_Tuple2(
													'🛋',
													_List_fromArray(
														['1f6cb'])),
													_Utils_Tuple2(
													'🚽',
													_List_fromArray(
														['1f6bd'])),
													_Utils_Tuple2(
													'🚿',
													_List_fromArray(
														['1f6bf'])),
													_Utils_Tuple2(
													'🛀',
													_List_fromArray(
														['1f6c0'])),
													_Utils_Tuple2(
													'🛀\ud83c\udffb',
													_List_fromArray(
														['1f6c0', '1f3fb'])),
													_Utils_Tuple2(
													'🛀\ud83c\udffc',
													_List_fromArray(
														['1f6c0', '1f3fc'])),
													_Utils_Tuple2(
													'🛀\ud83c\udffd',
													_List_fromArray(
														['1f6c0', '1f3fd'])),
													_Utils_Tuple2(
													'🛀\ud83c\udffe',
													_List_fromArray(
														['1f6c0', '1f3fe'])),
													_Utils_Tuple2(
													'🛀\ud83c\udfff',
													_List_fromArray(
														['1f6c0', '1f3ff'])),
													_Utils_Tuple2(
													'🛁',
													_List_fromArray(
														['1f6c1'])),
													_Utils_Tuple2(
													'⌛',
													_List_fromArray(
														['231b'])),
													_Utils_Tuple2(
													'⏳',
													_List_fromArray(
														['23f3'])),
													_Utils_Tuple2(
													'⌚',
													_List_fromArray(
														['231a'])),
													_Utils_Tuple2(
													'⏰',
													_List_fromArray(
														['23f0'])),
													_Utils_Tuple2(
													'⏱',
													_List_fromArray(
														['23f1'])),
													_Utils_Tuple2(
													'⏲',
													_List_fromArray(
														['23f2'])),
													_Utils_Tuple2(
													'🕰',
													_List_fromArray(
														['1f570'])),
													_Utils_Tuple2(
													'🕛',
													_List_fromArray(
														['1f55b'])),
													_Utils_Tuple2(
													'🕧',
													_List_fromArray(
														['1f567'])),
													_Utils_Tuple2(
													'🕐',
													_List_fromArray(
														['1f550'])),
													_Utils_Tuple2(
													'🕜',
													_List_fromArray(
														['1f55c'])),
													_Utils_Tuple2(
													'🕑',
													_List_fromArray(
														['1f551'])),
													_Utils_Tuple2(
													'🕝',
													_List_fromArray(
														['1f55d'])),
													_Utils_Tuple2(
													'🕒',
													_List_fromArray(
														['1f552'])),
													_Utils_Tuple2(
													'🕞',
													_List_fromArray(
														['1f55e'])),
													_Utils_Tuple2(
													'🕓',
													_List_fromArray(
														['1f553'])),
													_Utils_Tuple2(
													'🕟',
													_List_fromArray(
														['1f55f'])),
													_Utils_Tuple2(
													'🕔',
													_List_fromArray(
														['1f554'])),
													_Utils_Tuple2(
													'🕠',
													_List_fromArray(
														['1f560'])),
													_Utils_Tuple2(
													'🕕',
													_List_fromArray(
														['1f555'])),
													_Utils_Tuple2(
													'🕡',
													_List_fromArray(
														['1f561'])),
													_Utils_Tuple2(
													'🕖',
													_List_fromArray(
														['1f556'])),
													_Utils_Tuple2(
													'🕢',
													_List_fromArray(
														['1f562'])),
													_Utils_Tuple2(
													'🕗',
													_List_fromArray(
														['1f557'])),
													_Utils_Tuple2(
													'🕣',
													_List_fromArray(
														['1f563'])),
													_Utils_Tuple2(
													'🕘',
													_List_fromArray(
														['1f558'])),
													_Utils_Tuple2(
													'🕤',
													_List_fromArray(
														['1f564'])),
													_Utils_Tuple2(
													'🕙',
													_List_fromArray(
														['1f559'])),
													_Utils_Tuple2(
													'🕥',
													_List_fromArray(
														['1f565'])),
													_Utils_Tuple2(
													'🕚',
													_List_fromArray(
														['1f55a'])),
													_Utils_Tuple2(
													'🕦',
													_List_fromArray(
														['1f566'])),
													_Utils_Tuple2(
													'🌑',
													_List_fromArray(
														['1f311'])),
													_Utils_Tuple2(
													'🌒',
													_List_fromArray(
														['1f312'])),
													_Utils_Tuple2(
													'🌓',
													_List_fromArray(
														['1f313'])),
													_Utils_Tuple2(
													'🌔',
													_List_fromArray(
														['1f314'])),
													_Utils_Tuple2(
													'🌕',
													_List_fromArray(
														['1f315'])),
													_Utils_Tuple2(
													'🌖',
													_List_fromArray(
														['1f316'])),
													_Utils_Tuple2(
													'🌗',
													_List_fromArray(
														['1f317'])),
													_Utils_Tuple2(
													'🌘',
													_List_fromArray(
														['1f318'])),
													_Utils_Tuple2(
													'🌙',
													_List_fromArray(
														['1f319'])),
													_Utils_Tuple2(
													'🌚',
													_List_fromArray(
														['1f31a'])),
													_Utils_Tuple2(
													'🌛',
													_List_fromArray(
														['1f31b'])),
													_Utils_Tuple2(
													'🌜',
													_List_fromArray(
														['1f31c'])),
													_Utils_Tuple2(
													'🌡',
													_List_fromArray(
														['1f321'])),
													_Utils_Tuple2(
													'☀',
													_List_fromArray(
														['2600'])),
													_Utils_Tuple2(
													'🌝',
													_List_fromArray(
														['1f31d'])),
													_Utils_Tuple2(
													'🌞',
													_List_fromArray(
														['1f31e'])),
													_Utils_Tuple2(
													'⭐',
													_List_fromArray(
														['2b50'])),
													_Utils_Tuple2(
													'🌟',
													_List_fromArray(
														['1f31f'])),
													_Utils_Tuple2(
													'🌠',
													_List_fromArray(
														['1f320'])),
													_Utils_Tuple2(
													'☁',
													_List_fromArray(
														['2601'])),
													_Utils_Tuple2(
													'⛅',
													_List_fromArray(
														['26c5'])),
													_Utils_Tuple2(
													'⛈',
													_List_fromArray(
														['26c8'])),
													_Utils_Tuple2(
													'🌤',
													_List_fromArray(
														['1f324'])),
													_Utils_Tuple2(
													'🌥',
													_List_fromArray(
														['1f325'])),
													_Utils_Tuple2(
													'🌦',
													_List_fromArray(
														['1f326'])),
													_Utils_Tuple2(
													'🌧',
													_List_fromArray(
														['1f327'])),
													_Utils_Tuple2(
													'🌨',
													_List_fromArray(
														['1f328']))
												]),
											_Utils_ap(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'🌩',
														_List_fromArray(
															['1f329'])),
														_Utils_Tuple2(
														'🌪',
														_List_fromArray(
															['1f32a'])),
														_Utils_Tuple2(
														'🌫',
														_List_fromArray(
															['1f32b'])),
														_Utils_Tuple2(
														'🌬',
														_List_fromArray(
															['1f32c'])),
														_Utils_Tuple2(
														'🌀',
														_List_fromArray(
															['1f300'])),
														_Utils_Tuple2(
														'🌈',
														_List_fromArray(
															['1f308'])),
														_Utils_Tuple2(
														'🌂',
														_List_fromArray(
															['1f302'])),
														_Utils_Tuple2(
														'☂',
														_List_fromArray(
															['2602'])),
														_Utils_Tuple2(
														'☔',
														_List_fromArray(
															['2614'])),
														_Utils_Tuple2(
														'⛱',
														_List_fromArray(
															['26f1'])),
														_Utils_Tuple2(
														'⚡',
														_List_fromArray(
															['26a1'])),
														_Utils_Tuple2(
														'❄',
														_List_fromArray(
															['2744'])),
														_Utils_Tuple2(
														'☃',
														_List_fromArray(
															['2603'])),
														_Utils_Tuple2(
														'⛄',
														_List_fromArray(
															['26c4'])),
														_Utils_Tuple2(
														'☄',
														_List_fromArray(
															['2604'])),
														_Utils_Tuple2(
														'🔥',
														_List_fromArray(
															['1f525'])),
														_Utils_Tuple2(
														'💧',
														_List_fromArray(
															['1f4a7'])),
														_Utils_Tuple2(
														'🌊',
														_List_fromArray(
															['1f30a'])),
														_Utils_Tuple2(
														'🎃',
														_List_fromArray(
															['1f383'])),
														_Utils_Tuple2(
														'🎄',
														_List_fromArray(
															['1f384'])),
														_Utils_Tuple2(
														'🎆',
														_List_fromArray(
															['1f386'])),
														_Utils_Tuple2(
														'🎇',
														_List_fromArray(
															['1f387'])),
														_Utils_Tuple2(
														'✨',
														_List_fromArray(
															['2728'])),
														_Utils_Tuple2(
														'🎈',
														_List_fromArray(
															['1f388'])),
														_Utils_Tuple2(
														'🎉',
														_List_fromArray(
															['1f389'])),
														_Utils_Tuple2(
														'🎊',
														_List_fromArray(
															['1f38a'])),
														_Utils_Tuple2(
														'🎋',
														_List_fromArray(
															['1f38b'])),
														_Utils_Tuple2(
														'🎍',
														_List_fromArray(
															['1f38d'])),
														_Utils_Tuple2(
														'🎎',
														_List_fromArray(
															['1f38e'])),
														_Utils_Tuple2(
														'🎏',
														_List_fromArray(
															['1f38f'])),
														_Utils_Tuple2(
														'🎐',
														_List_fromArray(
															['1f390'])),
														_Utils_Tuple2(
														'🎑',
														_List_fromArray(
															['1f391'])),
														_Utils_Tuple2(
														'🎀',
														_List_fromArray(
															['1f380'])),
														_Utils_Tuple2(
														'🎁',
														_List_fromArray(
															['1f381'])),
														_Utils_Tuple2(
														'🎗',
														_List_fromArray(
															['1f397'])),
														_Utils_Tuple2(
														'🎟',
														_List_fromArray(
															['1f39f'])),
														_Utils_Tuple2(
														'🎫',
														_List_fromArray(
															['1f3ab'])),
														_Utils_Tuple2(
														'🎖',
														_List_fromArray(
															['1f396'])),
														_Utils_Tuple2(
														'🏆',
														_List_fromArray(
															['1f3c6'])),
														_Utils_Tuple2(
														'🏅',
														_List_fromArray(
															['1f3c5'])),
														_Utils_Tuple2(
														'\ud83e\udd47',
														_List_fromArray(
															['1f947'])),
														_Utils_Tuple2(
														'\ud83e\udd48',
														_List_fromArray(
															['1f948'])),
														_Utils_Tuple2(
														'\ud83e\udd49',
														_List_fromArray(
															['1f949'])),
														_Utils_Tuple2(
														'⚽',
														_List_fromArray(
															['26bd'])),
														_Utils_Tuple2(
														'⚾',
														_List_fromArray(
															['26be'])),
														_Utils_Tuple2(
														'🏀',
														_List_fromArray(
															['1f3c0'])),
														_Utils_Tuple2(
														'\ud83c\udfd0',
														_List_fromArray(
															['1f3d0'])),
														_Utils_Tuple2(
														'🏈',
														_List_fromArray(
															['1f3c8'])),
														_Utils_Tuple2(
														'🏉',
														_List_fromArray(
															['1f3c9'])),
														_Utils_Tuple2(
														'🎾',
														_List_fromArray(
															['1f3be'])),
														_Utils_Tuple2(
														'🎱',
														_List_fromArray(
															['1f3b1'])),
														_Utils_Tuple2(
														'🎳',
														_List_fromArray(
															['1f3b3'])),
														_Utils_Tuple2(
														'\ud83c\udfcf',
														_List_fromArray(
															['1f3cf'])),
														_Utils_Tuple2(
														'\ud83c\udfd1',
														_List_fromArray(
															['1f3d1'])),
														_Utils_Tuple2(
														'\ud83c\udfd2',
														_List_fromArray(
															['1f3d2'])),
														_Utils_Tuple2(
														'\ud83c\udfd3',
														_List_fromArray(
															['1f3d3'])),
														_Utils_Tuple2(
														'\ud83c\udff8',
														_List_fromArray(
															['1f3f8'])),
														_Utils_Tuple2(
														'\ud83e\udd4a',
														_List_fromArray(
															['1f94a'])),
														_Utils_Tuple2(
														'\ud83e\udd4b',
														_List_fromArray(
															['1f94b'])),
														_Utils_Tuple2(
														'\ud83e\udd45',
														_List_fromArray(
															['1f945'])),
														_Utils_Tuple2(
														'🎯',
														_List_fromArray(
															['1f3af'])),
														_Utils_Tuple2(
														'⛳',
														_List_fromArray(
															['26f3'])),
														_Utils_Tuple2(
														'⛸',
														_List_fromArray(
															['26f8'])),
														_Utils_Tuple2(
														'🎣',
														_List_fromArray(
															['1f3a3'])),
														_Utils_Tuple2(
														'🎽',
														_List_fromArray(
															['1f3bd'])),
														_Utils_Tuple2(
														'🎿',
														_List_fromArray(
															['1f3bf'])),
														_Utils_Tuple2(
														'🎮',
														_List_fromArray(
															['1f3ae'])),
														_Utils_Tuple2(
														'🕹',
														_List_fromArray(
															['1f579'])),
														_Utils_Tuple2(
														'🎲',
														_List_fromArray(
															['1f3b2'])),
														_Utils_Tuple2(
														'♠',
														_List_fromArray(
															['2660'])),
														_Utils_Tuple2(
														'♥',
														_List_fromArray(
															['2665'])),
														_Utils_Tuple2(
														'♦',
														_List_fromArray(
															['2666'])),
														_Utils_Tuple2(
														'♣',
														_List_fromArray(
															['2663'])),
														_Utils_Tuple2(
														'🃏',
														_List_fromArray(
															['1f0cf'])),
														_Utils_Tuple2(
														'🀄',
														_List_fromArray(
															['1f004'])),
														_Utils_Tuple2(
														'🎴',
														_List_fromArray(
															['1f3b4'])),
														_Utils_Tuple2(
														'🔇',
														_List_fromArray(
															['1f507'])),
														_Utils_Tuple2(
														'🔈',
														_List_fromArray(
															['1f508'])),
														_Utils_Tuple2(
														'🔉',
														_List_fromArray(
															['1f509'])),
														_Utils_Tuple2(
														'🔊',
														_List_fromArray(
															['1f50a'])),
														_Utils_Tuple2(
														'📢',
														_List_fromArray(
															['1f4e2'])),
														_Utils_Tuple2(
														'📣',
														_List_fromArray(
															['1f4e3'])),
														_Utils_Tuple2(
														'📯',
														_List_fromArray(
															['1f4ef'])),
														_Utils_Tuple2(
														'🔔',
														_List_fromArray(
															['1f514'])),
														_Utils_Tuple2(
														'🔕',
														_List_fromArray(
															['1f515'])),
														_Utils_Tuple2(
														'🎼',
														_List_fromArray(
															['1f3bc'])),
														_Utils_Tuple2(
														'🎵',
														_List_fromArray(
															['1f3b5'])),
														_Utils_Tuple2(
														'🎶',
														_List_fromArray(
															['1f3b6'])),
														_Utils_Tuple2(
														'🎙',
														_List_fromArray(
															['1f399'])),
														_Utils_Tuple2(
														'🎚',
														_List_fromArray(
															['1f39a'])),
														_Utils_Tuple2(
														'🎛',
														_List_fromArray(
															['1f39b'])),
														_Utils_Tuple2(
														'🎤',
														_List_fromArray(
															['1f3a4'])),
														_Utils_Tuple2(
														'🎧',
														_List_fromArray(
															['1f3a7'])),
														_Utils_Tuple2(
														'📻',
														_List_fromArray(
															['1f4fb'])),
														_Utils_Tuple2(
														'🎷',
														_List_fromArray(
															['1f3b7'])),
														_Utils_Tuple2(
														'🎸',
														_List_fromArray(
															['1f3b8'])),
														_Utils_Tuple2(
														'🎹',
														_List_fromArray(
															['1f3b9'])),
														_Utils_Tuple2(
														'🎺',
														_List_fromArray(
															['1f3ba'])),
														_Utils_Tuple2(
														'🎻',
														_List_fromArray(
															['1f3bb']))
													]),
												_Utils_ap(
													_List_fromArray(
														[
															_Utils_Tuple2(
															'\ud83e\udd41',
															_List_fromArray(
																['1f941'])),
															_Utils_Tuple2(
															'📱',
															_List_fromArray(
																['1f4f1'])),
															_Utils_Tuple2(
															'📲',
															_List_fromArray(
																['1f4f2'])),
															_Utils_Tuple2(
															'☎',
															_List_fromArray(
																['260e'])),
															_Utils_Tuple2(
															'📞',
															_List_fromArray(
																['1f4de'])),
															_Utils_Tuple2(
															'📟',
															_List_fromArray(
																['1f4df'])),
															_Utils_Tuple2(
															'📠',
															_List_fromArray(
																['1f4e0'])),
															_Utils_Tuple2(
															'🔋',
															_List_fromArray(
																['1f50b'])),
															_Utils_Tuple2(
															'🔌',
															_List_fromArray(
																['1f50c'])),
															_Utils_Tuple2(
															'💻',
															_List_fromArray(
																['1f4bb'])),
															_Utils_Tuple2(
															'🖥',
															_List_fromArray(
																['1f5a5'])),
															_Utils_Tuple2(
															'🖨',
															_List_fromArray(
																['1f5a8'])),
															_Utils_Tuple2(
															'⌨',
															_List_fromArray(
																['2328'])),
															_Utils_Tuple2(
															'🖱',
															_List_fromArray(
																['1f5b1'])),
															_Utils_Tuple2(
															'🖲',
															_List_fromArray(
																['1f5b2'])),
															_Utils_Tuple2(
															'💽',
															_List_fromArray(
																['1f4bd'])),
															_Utils_Tuple2(
															'💾',
															_List_fromArray(
																['1f4be'])),
															_Utils_Tuple2(
															'💿',
															_List_fromArray(
																['1f4bf'])),
															_Utils_Tuple2(
															'📀',
															_List_fromArray(
																['1f4c0'])),
															_Utils_Tuple2(
															'🎥',
															_List_fromArray(
																['1f3a5'])),
															_Utils_Tuple2(
															'🎞',
															_List_fromArray(
																['1f39e'])),
															_Utils_Tuple2(
															'📽',
															_List_fromArray(
																['1f4fd'])),
															_Utils_Tuple2(
															'🎬',
															_List_fromArray(
																['1f3ac'])),
															_Utils_Tuple2(
															'📺',
															_List_fromArray(
																['1f4fa'])),
															_Utils_Tuple2(
															'📷',
															_List_fromArray(
																['1f4f7'])),
															_Utils_Tuple2(
															'📸',
															_List_fromArray(
																['1f4f8'])),
															_Utils_Tuple2(
															'📹',
															_List_fromArray(
																['1f4f9'])),
															_Utils_Tuple2(
															'📼',
															_List_fromArray(
																['1f4fc'])),
															_Utils_Tuple2(
															'🔍',
															_List_fromArray(
																['1f50d'])),
															_Utils_Tuple2(
															'🔎',
															_List_fromArray(
																['1f50e'])),
															_Utils_Tuple2(
															'🔬',
															_List_fromArray(
																['1f52c'])),
															_Utils_Tuple2(
															'🔭',
															_List_fromArray(
																['1f52d'])),
															_Utils_Tuple2(
															'📡',
															_List_fromArray(
																['1f4e1'])),
															_Utils_Tuple2(
															'🕯',
															_List_fromArray(
																['1f56f'])),
															_Utils_Tuple2(
															'💡',
															_List_fromArray(
																['1f4a1'])),
															_Utils_Tuple2(
															'🔦',
															_List_fromArray(
																['1f526'])),
															_Utils_Tuple2(
															'🏮',
															_List_fromArray(
																['1f3ee'])),
															_Utils_Tuple2(
															'📔',
															_List_fromArray(
																['1f4d4'])),
															_Utils_Tuple2(
															'📕',
															_List_fromArray(
																['1f4d5'])),
															_Utils_Tuple2(
															'📖',
															_List_fromArray(
																['1f4d6'])),
															_Utils_Tuple2(
															'📗',
															_List_fromArray(
																['1f4d7'])),
															_Utils_Tuple2(
															'📘',
															_List_fromArray(
																['1f4d8'])),
															_Utils_Tuple2(
															'📙',
															_List_fromArray(
																['1f4d9'])),
															_Utils_Tuple2(
															'📚',
															_List_fromArray(
																['1f4da'])),
															_Utils_Tuple2(
															'📓',
															_List_fromArray(
																['1f4d3'])),
															_Utils_Tuple2(
															'📒',
															_List_fromArray(
																['1f4d2'])),
															_Utils_Tuple2(
															'📃',
															_List_fromArray(
																['1f4c3'])),
															_Utils_Tuple2(
															'📜',
															_List_fromArray(
																['1f4dc'])),
															_Utils_Tuple2(
															'📄',
															_List_fromArray(
																['1f4c4'])),
															_Utils_Tuple2(
															'📰',
															_List_fromArray(
																['1f4f0'])),
															_Utils_Tuple2(
															'🗞',
															_List_fromArray(
																['1f5de'])),
															_Utils_Tuple2(
															'📑',
															_List_fromArray(
																['1f4d1'])),
															_Utils_Tuple2(
															'🔖',
															_List_fromArray(
																['1f516'])),
															_Utils_Tuple2(
															'🏷',
															_List_fromArray(
																['1f3f7'])),
															_Utils_Tuple2(
															'💰',
															_List_fromArray(
																['1f4b0'])),
															_Utils_Tuple2(
															'💴',
															_List_fromArray(
																['1f4b4'])),
															_Utils_Tuple2(
															'💵',
															_List_fromArray(
																['1f4b5'])),
															_Utils_Tuple2(
															'💶',
															_List_fromArray(
																['1f4b6'])),
															_Utils_Tuple2(
															'💷',
															_List_fromArray(
																['1f4b7'])),
															_Utils_Tuple2(
															'💸',
															_List_fromArray(
																['1f4b8'])),
															_Utils_Tuple2(
															'💳',
															_List_fromArray(
																['1f4b3'])),
															_Utils_Tuple2(
															'💹',
															_List_fromArray(
																['1f4b9'])),
															_Utils_Tuple2(
															'💱',
															_List_fromArray(
																['1f4b1'])),
															_Utils_Tuple2(
															'💲',
															_List_fromArray(
																['1f4b2'])),
															_Utils_Tuple2(
															'✉',
															_List_fromArray(
																['2709'])),
															_Utils_Tuple2(
															'📧',
															_List_fromArray(
																['1f4e7'])),
															_Utils_Tuple2(
															'📨',
															_List_fromArray(
																['1f4e8'])),
															_Utils_Tuple2(
															'📩',
															_List_fromArray(
																['1f4e9'])),
															_Utils_Tuple2(
															'📤',
															_List_fromArray(
																['1f4e4'])),
															_Utils_Tuple2(
															'📥',
															_List_fromArray(
																['1f4e5'])),
															_Utils_Tuple2(
															'📦',
															_List_fromArray(
																['1f4e6'])),
															_Utils_Tuple2(
															'📫',
															_List_fromArray(
																['1f4eb'])),
															_Utils_Tuple2(
															'📪',
															_List_fromArray(
																['1f4ea'])),
															_Utils_Tuple2(
															'📬',
															_List_fromArray(
																['1f4ec'])),
															_Utils_Tuple2(
															'📭',
															_List_fromArray(
																['1f4ed'])),
															_Utils_Tuple2(
															'📮',
															_List_fromArray(
																['1f4ee'])),
															_Utils_Tuple2(
															'🗳',
															_List_fromArray(
																['1f5f3'])),
															_Utils_Tuple2(
															'✏',
															_List_fromArray(
																['270f'])),
															_Utils_Tuple2(
															'✒',
															_List_fromArray(
																['2712'])),
															_Utils_Tuple2(
															'🖋',
															_List_fromArray(
																['1f58b'])),
															_Utils_Tuple2(
															'🖊',
															_List_fromArray(
																['1f58a'])),
															_Utils_Tuple2(
															'🖌',
															_List_fromArray(
																['1f58c'])),
															_Utils_Tuple2(
															'🖍',
															_List_fromArray(
																['1f58d'])),
															_Utils_Tuple2(
															'📝',
															_List_fromArray(
																['1f4dd'])),
															_Utils_Tuple2(
															'💼',
															_List_fromArray(
																['1f4bc'])),
															_Utils_Tuple2(
															'📁',
															_List_fromArray(
																['1f4c1'])),
															_Utils_Tuple2(
															'📂',
															_List_fromArray(
																['1f4c2'])),
															_Utils_Tuple2(
															'🗂',
															_List_fromArray(
																['1f5c2'])),
															_Utils_Tuple2(
															'📅',
															_List_fromArray(
																['1f4c5'])),
															_Utils_Tuple2(
															'📆',
															_List_fromArray(
																['1f4c6'])),
															_Utils_Tuple2(
															'🗒',
															_List_fromArray(
																['1f5d2'])),
															_Utils_Tuple2(
															'🗓',
															_List_fromArray(
																['1f5d3'])),
															_Utils_Tuple2(
															'📇',
															_List_fromArray(
																['1f4c7'])),
															_Utils_Tuple2(
															'📈',
															_List_fromArray(
																['1f4c8'])),
															_Utils_Tuple2(
															'📉',
															_List_fromArray(
																['1f4c9'])),
															_Utils_Tuple2(
															'📊',
															_List_fromArray(
																['1f4ca'])),
															_Utils_Tuple2(
															'📋',
															_List_fromArray(
																['1f4cb'])),
															_Utils_Tuple2(
															'📌',
															_List_fromArray(
																['1f4cc'])),
															_Utils_Tuple2(
															'📍',
															_List_fromArray(
																['1f4cd']))
														]),
													_Utils_ap(
														_List_fromArray(
															[
																_Utils_Tuple2(
																'📎',
																_List_fromArray(
																	['1f4ce'])),
																_Utils_Tuple2(
																'🖇',
																_List_fromArray(
																	['1f587'])),
																_Utils_Tuple2(
																'📏',
																_List_fromArray(
																	['1f4cf'])),
																_Utils_Tuple2(
																'📐',
																_List_fromArray(
																	['1f4d0'])),
																_Utils_Tuple2(
																'✂',
																_List_fromArray(
																	['2702'])),
																_Utils_Tuple2(
																'🗃',
																_List_fromArray(
																	['1f5c3'])),
																_Utils_Tuple2(
																'🗄',
																_List_fromArray(
																	['1f5c4'])),
																_Utils_Tuple2(
																'🗑',
																_List_fromArray(
																	['1f5d1'])),
																_Utils_Tuple2(
																'🔒',
																_List_fromArray(
																	['1f512'])),
																_Utils_Tuple2(
																'🔓',
																_List_fromArray(
																	['1f513'])),
																_Utils_Tuple2(
																'🔏',
																_List_fromArray(
																	['1f50f'])),
																_Utils_Tuple2(
																'🔐',
																_List_fromArray(
																	['1f510'])),
																_Utils_Tuple2(
																'🔑',
																_List_fromArray(
																	['1f511'])),
																_Utils_Tuple2(
																'🗝',
																_List_fromArray(
																	['1f5dd'])),
																_Utils_Tuple2(
																'🔨',
																_List_fromArray(
																	['1f528'])),
																_Utils_Tuple2(
																'⛏',
																_List_fromArray(
																	['26cf'])),
																_Utils_Tuple2(
																'⚒',
																_List_fromArray(
																	['2692'])),
																_Utils_Tuple2(
																'🛠',
																_List_fromArray(
																	['1f6e0'])),
																_Utils_Tuple2(
																'🗡',
																_List_fromArray(
																	['1f5e1'])),
																_Utils_Tuple2(
																'⚔',
																_List_fromArray(
																	['2694'])),
																_Utils_Tuple2(
																'🔫',
																_List_fromArray(
																	['1f52b'])),
																_Utils_Tuple2(
																'\ud83c\udff9',
																_List_fromArray(
																	['1f3f9'])),
																_Utils_Tuple2(
																'🛡',
																_List_fromArray(
																	['1f6e1'])),
																_Utils_Tuple2(
																'🔧',
																_List_fromArray(
																	['1f527'])),
																_Utils_Tuple2(
																'🔩',
																_List_fromArray(
																	['1f529'])),
																_Utils_Tuple2(
																'⚙',
																_List_fromArray(
																	['2699'])),
																_Utils_Tuple2(
																'🗜',
																_List_fromArray(
																	['1f5dc'])),
																_Utils_Tuple2(
																'⚗',
																_List_fromArray(
																	['2697'])),
																_Utils_Tuple2(
																'⚖',
																_List_fromArray(
																	['2696'])),
																_Utils_Tuple2(
																'🔗',
																_List_fromArray(
																	['1f517'])),
																_Utils_Tuple2(
																'⛓',
																_List_fromArray(
																	['26d3'])),
																_Utils_Tuple2(
																'💉',
																_List_fromArray(
																	['1f489'])),
																_Utils_Tuple2(
																'💊',
																_List_fromArray(
																	['1f48a'])),
																_Utils_Tuple2(
																'🚬',
																_List_fromArray(
																	['1f6ac'])),
																_Utils_Tuple2(
																'⚰',
																_List_fromArray(
																	['26b0'])),
																_Utils_Tuple2(
																'⚱',
																_List_fromArray(
																	['26b1'])),
																_Utils_Tuple2(
																'🗿',
																_List_fromArray(
																	['1f5ff'])),
																_Utils_Tuple2(
																'🛢',
																_List_fromArray(
																	['1f6e2'])),
																_Utils_Tuple2(
																'🔮',
																_List_fromArray(
																	['1f52e'])),
																_Utils_Tuple2(
																'\ud83d\uded2',
																_List_fromArray(
																	['1f6d2'])),
																_Utils_Tuple2(
																'🏧',
																_List_fromArray(
																	['1f3e7'])),
																_Utils_Tuple2(
																'🚮',
																_List_fromArray(
																	['1f6ae'])),
																_Utils_Tuple2(
																'🚰',
																_List_fromArray(
																	['1f6b0'])),
																_Utils_Tuple2(
																'♿',
																_List_fromArray(
																	['267f'])),
																_Utils_Tuple2(
																'🚹',
																_List_fromArray(
																	['1f6b9'])),
																_Utils_Tuple2(
																'🚺',
																_List_fromArray(
																	['1f6ba'])),
																_Utils_Tuple2(
																'🚻',
																_List_fromArray(
																	['1f6bb'])),
																_Utils_Tuple2(
																'🚼',
																_List_fromArray(
																	['1f6bc'])),
																_Utils_Tuple2(
																'🚾',
																_List_fromArray(
																	['1f6be'])),
																_Utils_Tuple2(
																'🛂',
																_List_fromArray(
																	['1f6c2'])),
																_Utils_Tuple2(
																'🛃',
																_List_fromArray(
																	['1f6c3'])),
																_Utils_Tuple2(
																'🛄',
																_List_fromArray(
																	['1f6c4'])),
																_Utils_Tuple2(
																'🛅',
																_List_fromArray(
																	['1f6c5'])),
																_Utils_Tuple2(
																'⚠',
																_List_fromArray(
																	['26a0'])),
																_Utils_Tuple2(
																'🚸',
																_List_fromArray(
																	['1f6b8'])),
																_Utils_Tuple2(
																'⛔',
																_List_fromArray(
																	['26d4'])),
																_Utils_Tuple2(
																'🚫',
																_List_fromArray(
																	['1f6ab'])),
																_Utils_Tuple2(
																'🚳',
																_List_fromArray(
																	['1f6b3'])),
																_Utils_Tuple2(
																'🚭',
																_List_fromArray(
																	['1f6ad'])),
																_Utils_Tuple2(
																'🚯',
																_List_fromArray(
																	['1f6af'])),
																_Utils_Tuple2(
																'🚱',
																_List_fromArray(
																	['1f6b1'])),
																_Utils_Tuple2(
																'🚷',
																_List_fromArray(
																	['1f6b7'])),
																_Utils_Tuple2(
																'📵',
																_List_fromArray(
																	['1f4f5'])),
																_Utils_Tuple2(
																'🔞',
																_List_fromArray(
																	['1f51e'])),
																_Utils_Tuple2(
																'☢',
																_List_fromArray(
																	['2622'])),
																_Utils_Tuple2(
																'☣',
																_List_fromArray(
																	['2623'])),
																_Utils_Tuple2(
																'⬆',
																_List_fromArray(
																	['2b06'])),
																_Utils_Tuple2(
																'↗',
																_List_fromArray(
																	['2197'])),
																_Utils_Tuple2(
																'➡',
																_List_fromArray(
																	['27a1'])),
																_Utils_Tuple2(
																'↘',
																_List_fromArray(
																	['2198'])),
																_Utils_Tuple2(
																'⬇',
																_List_fromArray(
																	['2b07'])),
																_Utils_Tuple2(
																'↙',
																_List_fromArray(
																	['2199'])),
																_Utils_Tuple2(
																'⬅',
																_List_fromArray(
																	['2b05'])),
																_Utils_Tuple2(
																'↖',
																_List_fromArray(
																	['2196'])),
																_Utils_Tuple2(
																'↕',
																_List_fromArray(
																	['2195'])),
																_Utils_Tuple2(
																'↔',
																_List_fromArray(
																	['2194'])),
																_Utils_Tuple2(
																'↩',
																_List_fromArray(
																	['21a9'])),
																_Utils_Tuple2(
																'↪',
																_List_fromArray(
																	['21aa'])),
																_Utils_Tuple2(
																'⤴',
																_List_fromArray(
																	['2934'])),
																_Utils_Tuple2(
																'⤵',
																_List_fromArray(
																	['2935'])),
																_Utils_Tuple2(
																'🔃',
																_List_fromArray(
																	['1f503'])),
																_Utils_Tuple2(
																'🔄',
																_List_fromArray(
																	['1f504'])),
																_Utils_Tuple2(
																'🔙',
																_List_fromArray(
																	['1f519'])),
																_Utils_Tuple2(
																'🔚',
																_List_fromArray(
																	['1f51a'])),
																_Utils_Tuple2(
																'🔛',
																_List_fromArray(
																	['1f51b'])),
																_Utils_Tuple2(
																'🔜',
																_List_fromArray(
																	['1f51c'])),
																_Utils_Tuple2(
																'🔝',
																_List_fromArray(
																	['1f51d'])),
																_Utils_Tuple2(
																'\ud83d\uded0',
																_List_fromArray(
																	['1f6d0'])),
																_Utils_Tuple2(
																'⚛',
																_List_fromArray(
																	['269b'])),
																_Utils_Tuple2(
																'🕉',
																_List_fromArray(
																	['1f549'])),
																_Utils_Tuple2(
																'✡',
																_List_fromArray(
																	['2721'])),
																_Utils_Tuple2(
																'☸',
																_List_fromArray(
																	['2638'])),
																_Utils_Tuple2(
																'☯',
																_List_fromArray(
																	['262f'])),
																_Utils_Tuple2(
																'✝',
																_List_fromArray(
																	['271d'])),
																_Utils_Tuple2(
																'☦',
																_List_fromArray(
																	['2626'])),
																_Utils_Tuple2(
																'☪',
																_List_fromArray(
																	['262a'])),
																_Utils_Tuple2(
																'☮',
																_List_fromArray(
																	['262e'])),
																_Utils_Tuple2(
																'\ud83d\udd4e',
																_List_fromArray(
																	['1f54e'])),
																_Utils_Tuple2(
																'🔯',
																_List_fromArray(
																	['1f52f']))
															]),
														_Utils_ap(
															_List_fromArray(
																[
																	_Utils_Tuple2(
																	'♈',
																	_List_fromArray(
																		['2648'])),
																	_Utils_Tuple2(
																	'♉',
																	_List_fromArray(
																		['2649'])),
																	_Utils_Tuple2(
																	'♊',
																	_List_fromArray(
																		['264a'])),
																	_Utils_Tuple2(
																	'♋',
																	_List_fromArray(
																		['264b'])),
																	_Utils_Tuple2(
																	'♌',
																	_List_fromArray(
																		['264c'])),
																	_Utils_Tuple2(
																	'♍',
																	_List_fromArray(
																		['264d'])),
																	_Utils_Tuple2(
																	'♎',
																	_List_fromArray(
																		['264e'])),
																	_Utils_Tuple2(
																	'♏',
																	_List_fromArray(
																		['264f'])),
																	_Utils_Tuple2(
																	'♐',
																	_List_fromArray(
																		['2650'])),
																	_Utils_Tuple2(
																	'♑',
																	_List_fromArray(
																		['2651'])),
																	_Utils_Tuple2(
																	'♒',
																	_List_fromArray(
																		['2652'])),
																	_Utils_Tuple2(
																	'♓',
																	_List_fromArray(
																		['2653'])),
																	_Utils_Tuple2(
																	'⛎',
																	_List_fromArray(
																		['26ce'])),
																	_Utils_Tuple2(
																	'🔀',
																	_List_fromArray(
																		['1f500'])),
																	_Utils_Tuple2(
																	'🔁',
																	_List_fromArray(
																		['1f501'])),
																	_Utils_Tuple2(
																	'🔂',
																	_List_fromArray(
																		['1f502'])),
																	_Utils_Tuple2(
																	'▶',
																	_List_fromArray(
																		['25b6'])),
																	_Utils_Tuple2(
																	'⏩',
																	_List_fromArray(
																		['23e9'])),
																	_Utils_Tuple2(
																	'⏭',
																	_List_fromArray(
																		['23ed'])),
																	_Utils_Tuple2(
																	'⏯',
																	_List_fromArray(
																		['23ef'])),
																	_Utils_Tuple2(
																	'◀',
																	_List_fromArray(
																		['25c0'])),
																	_Utils_Tuple2(
																	'⏪',
																	_List_fromArray(
																		['23ea'])),
																	_Utils_Tuple2(
																	'⏮',
																	_List_fromArray(
																		['23ee'])),
																	_Utils_Tuple2(
																	'🔼',
																	_List_fromArray(
																		['1f53c'])),
																	_Utils_Tuple2(
																	'⏫',
																	_List_fromArray(
																		['23eb'])),
																	_Utils_Tuple2(
																	'🔽',
																	_List_fromArray(
																		['1f53d'])),
																	_Utils_Tuple2(
																	'⏬',
																	_List_fromArray(
																		['23ec'])),
																	_Utils_Tuple2(
																	'⏸',
																	_List_fromArray(
																		['23f8'])),
																	_Utils_Tuple2(
																	'⏹',
																	_List_fromArray(
																		['23f9'])),
																	_Utils_Tuple2(
																	'⏺',
																	_List_fromArray(
																		['23fa'])),
																	_Utils_Tuple2(
																	'⏏',
																	_List_fromArray(
																		['23cf'])),
																	_Utils_Tuple2(
																	'🎦',
																	_List_fromArray(
																		['1f3a6'])),
																	_Utils_Tuple2(
																	'🔅',
																	_List_fromArray(
																		['1f505'])),
																	_Utils_Tuple2(
																	'🔆',
																	_List_fromArray(
																		['1f506'])),
																	_Utils_Tuple2(
																	'📶',
																	_List_fromArray(
																		['1f4f6'])),
																	_Utils_Tuple2(
																	'📳',
																	_List_fromArray(
																		['1f4f3'])),
																	_Utils_Tuple2(
																	'📴',
																	_List_fromArray(
																		['1f4f4'])),
																	_Utils_Tuple2(
																	'♻',
																	_List_fromArray(
																		['267b'])),
																	_Utils_Tuple2(
																	'📛',
																	_List_fromArray(
																		['1f4db'])),
																	_Utils_Tuple2(
																	'⚜',
																	_List_fromArray(
																		['269c'])),
																	_Utils_Tuple2(
																	'🔰',
																	_List_fromArray(
																		['1f530'])),
																	_Utils_Tuple2(
																	'🔱',
																	_List_fromArray(
																		['1f531'])),
																	_Utils_Tuple2(
																	'⭕',
																	_List_fromArray(
																		['2b55'])),
																	_Utils_Tuple2(
																	'✅',
																	_List_fromArray(
																		['2705'])),
																	_Utils_Tuple2(
																	'☑',
																	_List_fromArray(
																		['2611'])),
																	_Utils_Tuple2(
																	'✔',
																	_List_fromArray(
																		['2714'])),
																	_Utils_Tuple2(
																	'✖',
																	_List_fromArray(
																		['2716'])),
																	_Utils_Tuple2(
																	'❌',
																	_List_fromArray(
																		['274c'])),
																	_Utils_Tuple2(
																	'❎',
																	_List_fromArray(
																		['274e'])),
																	_Utils_Tuple2(
																	'➕',
																	_List_fromArray(
																		['2795'])),
																	_Utils_Tuple2(
																	'➖',
																	_List_fromArray(
																		['2796'])),
																	_Utils_Tuple2(
																	'➗',
																	_List_fromArray(
																		['2797'])),
																	_Utils_Tuple2(
																	'➰',
																	_List_fromArray(
																		['27b0'])),
																	_Utils_Tuple2(
																	'➿',
																	_List_fromArray(
																		['27bf'])),
																	_Utils_Tuple2(
																	'〽',
																	_List_fromArray(
																		['303d'])),
																	_Utils_Tuple2(
																	'✳',
																	_List_fromArray(
																		['2733'])),
																	_Utils_Tuple2(
																	'✴',
																	_List_fromArray(
																		['2734'])),
																	_Utils_Tuple2(
																	'❇',
																	_List_fromArray(
																		['2747'])),
																	_Utils_Tuple2(
																	'‼',
																	_List_fromArray(
																		['203c'])),
																	_Utils_Tuple2(
																	'⁉',
																	_List_fromArray(
																		['2049'])),
																	_Utils_Tuple2(
																	'❓',
																	_List_fromArray(
																		['2753'])),
																	_Utils_Tuple2(
																	'❔',
																	_List_fromArray(
																		['2754'])),
																	_Utils_Tuple2(
																	'❕',
																	_List_fromArray(
																		['2755'])),
																	_Utils_Tuple2(
																	'❗',
																	_List_fromArray(
																		['2757'])),
																	_Utils_Tuple2(
																	'〰',
																	_List_fromArray(
																		['3030'])),
																	_Utils_Tuple2(
																	'©',
																	_List_fromArray(
																		['a9'])),
																	_Utils_Tuple2(
																	'®',
																	_List_fromArray(
																		['ae'])),
																	_Utils_Tuple2(
																	'™',
																	_List_fromArray(
																		['2122'])),
																	_Utils_Tuple2(
																	'#️⃣',
																	_List_fromArray(
																		['23', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'*️⃣',
																	_List_fromArray(
																		['2a', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'0️⃣',
																	_List_fromArray(
																		['30', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'1️⃣',
																	_List_fromArray(
																		['31', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'2️⃣',
																	_List_fromArray(
																		['32', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'3️⃣',
																	_List_fromArray(
																		['33', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'4️⃣',
																	_List_fromArray(
																		['34', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'5️⃣',
																	_List_fromArray(
																		['35', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'6️⃣',
																	_List_fromArray(
																		['36', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'7️⃣',
																	_List_fromArray(
																		['37', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'8️⃣',
																	_List_fromArray(
																		['38', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'9️⃣',
																	_List_fromArray(
																		['39', 'fe0f', '20e3'])),
																	_Utils_Tuple2(
																	'🔟',
																	_List_fromArray(
																		['1f51f'])),
																	_Utils_Tuple2(
																	'💯',
																	_List_fromArray(
																		['1f4af'])),
																	_Utils_Tuple2(
																	'🔠',
																	_List_fromArray(
																		['1f520'])),
																	_Utils_Tuple2(
																	'🔡',
																	_List_fromArray(
																		['1f521'])),
																	_Utils_Tuple2(
																	'🔢',
																	_List_fromArray(
																		['1f522'])),
																	_Utils_Tuple2(
																	'🔣',
																	_List_fromArray(
																		['1f523'])),
																	_Utils_Tuple2(
																	'🔤',
																	_List_fromArray(
																		['1f524'])),
																	_Utils_Tuple2(
																	'🅰',
																	_List_fromArray(
																		['1f170'])),
																	_Utils_Tuple2(
																	'🆎',
																	_List_fromArray(
																		['1f18e'])),
																	_Utils_Tuple2(
																	'🅱',
																	_List_fromArray(
																		['1f171'])),
																	_Utils_Tuple2(
																	'🆑',
																	_List_fromArray(
																		['1f191'])),
																	_Utils_Tuple2(
																	'🆒',
																	_List_fromArray(
																		['1f192'])),
																	_Utils_Tuple2(
																	'🆓',
																	_List_fromArray(
																		['1f193'])),
																	_Utils_Tuple2(
																	'ℹ',
																	_List_fromArray(
																		['2139'])),
																	_Utils_Tuple2(
																	'🆔',
																	_List_fromArray(
																		['1f194'])),
																	_Utils_Tuple2(
																	'ⓜ',
																	_List_fromArray(
																		['24c2'])),
																	_Utils_Tuple2(
																	'🆕',
																	_List_fromArray(
																		['1f195'])),
																	_Utils_Tuple2(
																	'🆖',
																	_List_fromArray(
																		['1f196'])),
																	_Utils_Tuple2(
																	'🅾',
																	_List_fromArray(
																		['1f17e']))
																]),
															_Utils_ap(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'🆗',
																		_List_fromArray(
																			['1f197'])),
																		_Utils_Tuple2(
																		'🅿',
																		_List_fromArray(
																			['1f17f'])),
																		_Utils_Tuple2(
																		'🆘',
																		_List_fromArray(
																			['1f198'])),
																		_Utils_Tuple2(
																		'🆙',
																		_List_fromArray(
																			['1f199'])),
																		_Utils_Tuple2(
																		'🆚',
																		_List_fromArray(
																			['1f19a'])),
																		_Utils_Tuple2(
																		'🈁',
																		_List_fromArray(
																			['1f201'])),
																		_Utils_Tuple2(
																		'🈂',
																		_List_fromArray(
																			['1f202'])),
																		_Utils_Tuple2(
																		'🈷',
																		_List_fromArray(
																			['1f237'])),
																		_Utils_Tuple2(
																		'🈶',
																		_List_fromArray(
																			['1f236'])),
																		_Utils_Tuple2(
																		'🈯',
																		_List_fromArray(
																			['1f22f'])),
																		_Utils_Tuple2(
																		'🉐',
																		_List_fromArray(
																			['1f250'])),
																		_Utils_Tuple2(
																		'🈹',
																		_List_fromArray(
																			['1f239'])),
																		_Utils_Tuple2(
																		'🈚',
																		_List_fromArray(
																			['1f21a'])),
																		_Utils_Tuple2(
																		'🈲',
																		_List_fromArray(
																			['1f232'])),
																		_Utils_Tuple2(
																		'🉑',
																		_List_fromArray(
																			['1f251'])),
																		_Utils_Tuple2(
																		'🈸',
																		_List_fromArray(
																			['1f238'])),
																		_Utils_Tuple2(
																		'🈴',
																		_List_fromArray(
																			['1f234'])),
																		_Utils_Tuple2(
																		'🈳',
																		_List_fromArray(
																			['1f233'])),
																		_Utils_Tuple2(
																		'㊗',
																		_List_fromArray(
																			['3297'])),
																		_Utils_Tuple2(
																		'㊙',
																		_List_fromArray(
																			['3299'])),
																		_Utils_Tuple2(
																		'🈺',
																		_List_fromArray(
																			['1f23a'])),
																		_Utils_Tuple2(
																		'🈵',
																		_List_fromArray(
																			['1f235'])),
																		_Utils_Tuple2(
																		'▪',
																		_List_fromArray(
																			['25aa'])),
																		_Utils_Tuple2(
																		'▫',
																		_List_fromArray(
																			['25ab'])),
																		_Utils_Tuple2(
																		'◻',
																		_List_fromArray(
																			['25fb'])),
																		_Utils_Tuple2(
																		'◼',
																		_List_fromArray(
																			['25fc'])),
																		_Utils_Tuple2(
																		'◽',
																		_List_fromArray(
																			['25fd'])),
																		_Utils_Tuple2(
																		'◾',
																		_List_fromArray(
																			['25fe'])),
																		_Utils_Tuple2(
																		'⬛',
																		_List_fromArray(
																			['2b1b'])),
																		_Utils_Tuple2(
																		'⬜',
																		_List_fromArray(
																			['2b1c'])),
																		_Utils_Tuple2(
																		'🔶',
																		_List_fromArray(
																			['1f536'])),
																		_Utils_Tuple2(
																		'🔷',
																		_List_fromArray(
																			['1f537'])),
																		_Utils_Tuple2(
																		'🔸',
																		_List_fromArray(
																			['1f538'])),
																		_Utils_Tuple2(
																		'🔹',
																		_List_fromArray(
																			['1f539'])),
																		_Utils_Tuple2(
																		'🔺',
																		_List_fromArray(
																			['1f53a'])),
																		_Utils_Tuple2(
																		'🔻',
																		_List_fromArray(
																			['1f53b'])),
																		_Utils_Tuple2(
																		'💠',
																		_List_fromArray(
																			['1f4a0'])),
																		_Utils_Tuple2(
																		'🔘',
																		_List_fromArray(
																			['1f518'])),
																		_Utils_Tuple2(
																		'🔲',
																		_List_fromArray(
																			['1f532'])),
																		_Utils_Tuple2(
																		'🔳',
																		_List_fromArray(
																			['1f533'])),
																		_Utils_Tuple2(
																		'⚪',
																		_List_fromArray(
																			['26aa'])),
																		_Utils_Tuple2(
																		'⚫',
																		_List_fromArray(
																			['26ab'])),
																		_Utils_Tuple2(
																		'🔴',
																		_List_fromArray(
																			['1f534'])),
																		_Utils_Tuple2(
																		'🔵',
																		_List_fromArray(
																			['1f535'])),
																		_Utils_Tuple2(
																		'🏁',
																		_List_fromArray(
																			['1f3c1'])),
																		_Utils_Tuple2(
																		'🚩',
																		_List_fromArray(
																			['1f6a9'])),
																		_Utils_Tuple2(
																		'🎌',
																		_List_fromArray(
																			['1f38c'])),
																		_Utils_Tuple2(
																		'🏴',
																		_List_fromArray(
																			['1f3f4'])),
																		_Utils_Tuple2(
																		'🏳',
																		_List_fromArray(
																			['1f3f3'])),
																		_Utils_Tuple2(
																		'🇦🇨',
																		_List_fromArray(
																			['1f1e6', '1f1e8'])),
																		_Utils_Tuple2(
																		'🇦🇩',
																		_List_fromArray(
																			['1f1e6', '1f1e9'])),
																		_Utils_Tuple2(
																		'🇦🇪',
																		_List_fromArray(
																			['1f1e6', '1f1ea'])),
																		_Utils_Tuple2(
																		'🇦🇫',
																		_List_fromArray(
																			['1f1e6', '1f1eb'])),
																		_Utils_Tuple2(
																		'🇦🇬',
																		_List_fromArray(
																			['1f1e6', '1f1ec'])),
																		_Utils_Tuple2(
																		'🇦🇮',
																		_List_fromArray(
																			['1f1e6', '1f1ee'])),
																		_Utils_Tuple2(
																		'🇦🇱',
																		_List_fromArray(
																			['1f1e6', '1f1f1'])),
																		_Utils_Tuple2(
																		'🇦🇲',
																		_List_fromArray(
																			['1f1e6', '1f1f2'])),
																		_Utils_Tuple2(
																		'🇦🇴',
																		_List_fromArray(
																			['1f1e6', '1f1f4'])),
																		_Utils_Tuple2(
																		'🇦🇶',
																		_List_fromArray(
																			['1f1e6', '1f1f6'])),
																		_Utils_Tuple2(
																		'🇦🇷',
																		_List_fromArray(
																			['1f1e6', '1f1f7'])),
																		_Utils_Tuple2(
																		'🇦🇸',
																		_List_fromArray(
																			['1f1e6', '1f1f8'])),
																		_Utils_Tuple2(
																		'🇦🇹',
																		_List_fromArray(
																			['1f1e6', '1f1f9'])),
																		_Utils_Tuple2(
																		'🇦🇺',
																		_List_fromArray(
																			['1f1e6', '1f1fa'])),
																		_Utils_Tuple2(
																		'🇦🇼',
																		_List_fromArray(
																			['1f1e6', '1f1fc'])),
																		_Utils_Tuple2(
																		'🇦🇽',
																		_List_fromArray(
																			['1f1e6', '1f1fd'])),
																		_Utils_Tuple2(
																		'🇦🇿',
																		_List_fromArray(
																			['1f1e6', '1f1ff'])),
																		_Utils_Tuple2(
																		'🇧🇦',
																		_List_fromArray(
																			['1f1e7', '1f1e6'])),
																		_Utils_Tuple2(
																		'🇧🇧',
																		_List_fromArray(
																			['1f1e7', '1f1e7'])),
																		_Utils_Tuple2(
																		'🇧🇩',
																		_List_fromArray(
																			['1f1e7', '1f1e9'])),
																		_Utils_Tuple2(
																		'🇧🇪',
																		_List_fromArray(
																			['1f1e7', '1f1ea'])),
																		_Utils_Tuple2(
																		'🇧🇫',
																		_List_fromArray(
																			['1f1e7', '1f1eb'])),
																		_Utils_Tuple2(
																		'🇧🇬',
																		_List_fromArray(
																			['1f1e7', '1f1ec'])),
																		_Utils_Tuple2(
																		'🇧🇭',
																		_List_fromArray(
																			['1f1e7', '1f1ed'])),
																		_Utils_Tuple2(
																		'🇧🇮',
																		_List_fromArray(
																			['1f1e7', '1f1ee'])),
																		_Utils_Tuple2(
																		'🇧🇯',
																		_List_fromArray(
																			['1f1e7', '1f1ef'])),
																		_Utils_Tuple2(
																		'🇧🇱',
																		_List_fromArray(
																			['1f1e7', '1f1f1'])),
																		_Utils_Tuple2(
																		'🇧🇲',
																		_List_fromArray(
																			['1f1e7', '1f1f2'])),
																		_Utils_Tuple2(
																		'🇧🇳',
																		_List_fromArray(
																			['1f1e7', '1f1f3'])),
																		_Utils_Tuple2(
																		'🇧🇴',
																		_List_fromArray(
																			['1f1e7', '1f1f4'])),
																		_Utils_Tuple2(
																		'🇧🇶',
																		_List_fromArray(
																			['1f1e7', '1f1f6'])),
																		_Utils_Tuple2(
																		'🇧🇷',
																		_List_fromArray(
																			['1f1e7', '1f1f7'])),
																		_Utils_Tuple2(
																		'🇧🇸',
																		_List_fromArray(
																			['1f1e7', '1f1f8'])),
																		_Utils_Tuple2(
																		'🇧🇹',
																		_List_fromArray(
																			['1f1e7', '1f1f9'])),
																		_Utils_Tuple2(
																		'🇧🇻',
																		_List_fromArray(
																			['1f1e7', '1f1fb'])),
																		_Utils_Tuple2(
																		'🇧🇼',
																		_List_fromArray(
																			['1f1e7', '1f1fc'])),
																		_Utils_Tuple2(
																		'🇧🇾',
																		_List_fromArray(
																			['1f1e7', '1f1fe'])),
																		_Utils_Tuple2(
																		'🇧🇿',
																		_List_fromArray(
																			['1f1e7', '1f1ff'])),
																		_Utils_Tuple2(
																		'🇨🇦',
																		_List_fromArray(
																			['1f1e8', '1f1e6'])),
																		_Utils_Tuple2(
																		'🇨🇨',
																		_List_fromArray(
																			['1f1e8', '1f1e8'])),
																		_Utils_Tuple2(
																		'🇨🇩',
																		_List_fromArray(
																			['1f1e8', '1f1e9'])),
																		_Utils_Tuple2(
																		'🇨🇫',
																		_List_fromArray(
																			['1f1e8', '1f1eb'])),
																		_Utils_Tuple2(
																		'🇨🇬',
																		_List_fromArray(
																			['1f1e8', '1f1ec'])),
																		_Utils_Tuple2(
																		'🇨🇭',
																		_List_fromArray(
																			['1f1e8', '1f1ed'])),
																		_Utils_Tuple2(
																		'🇨🇮',
																		_List_fromArray(
																			['1f1e8', '1f1ee'])),
																		_Utils_Tuple2(
																		'🇨🇰',
																		_List_fromArray(
																			['1f1e8', '1f1f0'])),
																		_Utils_Tuple2(
																		'🇨🇱',
																		_List_fromArray(
																			['1f1e8', '1f1f1'])),
																		_Utils_Tuple2(
																		'🇨🇲',
																		_List_fromArray(
																			['1f1e8', '1f1f2'])),
																		_Utils_Tuple2(
																		'🇨🇳',
																		_List_fromArray(
																			['1f1e8', '1f1f3'])),
																		_Utils_Tuple2(
																		'🇨🇴',
																		_List_fromArray(
																			['1f1e8', '1f1f4']))
																	]),
																_Utils_ap(
																	_List_fromArray(
																		[
																			_Utils_Tuple2(
																			'🇨🇵',
																			_List_fromArray(
																				['1f1e8', '1f1f5'])),
																			_Utils_Tuple2(
																			'🇨🇷',
																			_List_fromArray(
																				['1f1e8', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇨🇺',
																			_List_fromArray(
																				['1f1e8', '1f1fa'])),
																			_Utils_Tuple2(
																			'🇨🇻',
																			_List_fromArray(
																				['1f1e8', '1f1fb'])),
																			_Utils_Tuple2(
																			'🇨🇼',
																			_List_fromArray(
																				['1f1e8', '1f1fc'])),
																			_Utils_Tuple2(
																			'🇨🇽',
																			_List_fromArray(
																				['1f1e8', '1f1fd'])),
																			_Utils_Tuple2(
																			'🇨🇾',
																			_List_fromArray(
																				['1f1e8', '1f1fe'])),
																			_Utils_Tuple2(
																			'🇨🇿',
																			_List_fromArray(
																				['1f1e8', '1f1ff'])),
																			_Utils_Tuple2(
																			'🇩🇪',
																			_List_fromArray(
																				['1f1e9', '1f1ea'])),
																			_Utils_Tuple2(
																			'🇩🇬',
																			_List_fromArray(
																				['1f1e9', '1f1ec'])),
																			_Utils_Tuple2(
																			'🇩🇯',
																			_List_fromArray(
																				['1f1e9', '1f1ef'])),
																			_Utils_Tuple2(
																			'🇩🇰',
																			_List_fromArray(
																				['1f1e9', '1f1f0'])),
																			_Utils_Tuple2(
																			'🇩🇲',
																			_List_fromArray(
																				['1f1e9', '1f1f2'])),
																			_Utils_Tuple2(
																			'🇩🇴',
																			_List_fromArray(
																				['1f1e9', '1f1f4'])),
																			_Utils_Tuple2(
																			'🇩🇿',
																			_List_fromArray(
																				['1f1e9', '1f1ff'])),
																			_Utils_Tuple2(
																			'🇪🇦',
																			_List_fromArray(
																				['1f1ea', '1f1e6'])),
																			_Utils_Tuple2(
																			'🇪🇨',
																			_List_fromArray(
																				['1f1ea', '1f1e8'])),
																			_Utils_Tuple2(
																			'🇪🇪',
																			_List_fromArray(
																				['1f1ea', '1f1ea'])),
																			_Utils_Tuple2(
																			'🇪🇬',
																			_List_fromArray(
																				['1f1ea', '1f1ec'])),
																			_Utils_Tuple2(
																			'🇪🇭',
																			_List_fromArray(
																				['1f1ea', '1f1ed'])),
																			_Utils_Tuple2(
																			'🇪🇷',
																			_List_fromArray(
																				['1f1ea', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇪🇸',
																			_List_fromArray(
																				['1f1ea', '1f1f8'])),
																			_Utils_Tuple2(
																			'🇪🇹',
																			_List_fromArray(
																				['1f1ea', '1f1f9'])),
																			_Utils_Tuple2(
																			'🇪🇺',
																			_List_fromArray(
																				['1f1ea', '1f1fa'])),
																			_Utils_Tuple2(
																			'🇫🇮',
																			_List_fromArray(
																				['1f1eb', '1f1ee'])),
																			_Utils_Tuple2(
																			'🇫🇯',
																			_List_fromArray(
																				['1f1eb', '1f1ef'])),
																			_Utils_Tuple2(
																			'🇫🇰',
																			_List_fromArray(
																				['1f1eb', '1f1f0'])),
																			_Utils_Tuple2(
																			'🇫🇲',
																			_List_fromArray(
																				['1f1eb', '1f1f2'])),
																			_Utils_Tuple2(
																			'🇫🇴',
																			_List_fromArray(
																				['1f1eb', '1f1f4'])),
																			_Utils_Tuple2(
																			'🇫🇷',
																			_List_fromArray(
																				['1f1eb', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇬🇦',
																			_List_fromArray(
																				['1f1ec', '1f1e6'])),
																			_Utils_Tuple2(
																			'🇬🇧',
																			_List_fromArray(
																				['1f1ec', '1f1e7'])),
																			_Utils_Tuple2(
																			'🇬🇩',
																			_List_fromArray(
																				['1f1ec', '1f1e9'])),
																			_Utils_Tuple2(
																			'🇬🇪',
																			_List_fromArray(
																				['1f1ec', '1f1ea'])),
																			_Utils_Tuple2(
																			'🇬🇫',
																			_List_fromArray(
																				['1f1ec', '1f1eb'])),
																			_Utils_Tuple2(
																			'🇬🇬',
																			_List_fromArray(
																				['1f1ec', '1f1ec'])),
																			_Utils_Tuple2(
																			'🇬🇭',
																			_List_fromArray(
																				['1f1ec', '1f1ed'])),
																			_Utils_Tuple2(
																			'🇬🇮',
																			_List_fromArray(
																				['1f1ec', '1f1ee'])),
																			_Utils_Tuple2(
																			'🇬🇱',
																			_List_fromArray(
																				['1f1ec', '1f1f1'])),
																			_Utils_Tuple2(
																			'🇬🇲',
																			_List_fromArray(
																				['1f1ec', '1f1f2'])),
																			_Utils_Tuple2(
																			'🇬🇳',
																			_List_fromArray(
																				['1f1ec', '1f1f3'])),
																			_Utils_Tuple2(
																			'🇬🇵',
																			_List_fromArray(
																				['1f1ec', '1f1f5'])),
																			_Utils_Tuple2(
																			'🇬🇶',
																			_List_fromArray(
																				['1f1ec', '1f1f6'])),
																			_Utils_Tuple2(
																			'🇬🇷',
																			_List_fromArray(
																				['1f1ec', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇬🇸',
																			_List_fromArray(
																				['1f1ec', '1f1f8'])),
																			_Utils_Tuple2(
																			'🇬🇹',
																			_List_fromArray(
																				['1f1ec', '1f1f9'])),
																			_Utils_Tuple2(
																			'🇬🇺',
																			_List_fromArray(
																				['1f1ec', '1f1fa'])),
																			_Utils_Tuple2(
																			'🇬🇼',
																			_List_fromArray(
																				['1f1ec', '1f1fc'])),
																			_Utils_Tuple2(
																			'🇬🇾',
																			_List_fromArray(
																				['1f1ec', '1f1fe'])),
																			_Utils_Tuple2(
																			'🇭🇰',
																			_List_fromArray(
																				['1f1ed', '1f1f0'])),
																			_Utils_Tuple2(
																			'🇭🇲',
																			_List_fromArray(
																				['1f1ed', '1f1f2'])),
																			_Utils_Tuple2(
																			'🇭🇳',
																			_List_fromArray(
																				['1f1ed', '1f1f3'])),
																			_Utils_Tuple2(
																			'🇭🇷',
																			_List_fromArray(
																				['1f1ed', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇭🇹',
																			_List_fromArray(
																				['1f1ed', '1f1f9'])),
																			_Utils_Tuple2(
																			'🇭🇺',
																			_List_fromArray(
																				['1f1ed', '1f1fa'])),
																			_Utils_Tuple2(
																			'🇮🇨',
																			_List_fromArray(
																				['1f1ee', '1f1e8'])),
																			_Utils_Tuple2(
																			'🇮🇩',
																			_List_fromArray(
																				['1f1ee', '1f1e9'])),
																			_Utils_Tuple2(
																			'🇮🇪',
																			_List_fromArray(
																				['1f1ee', '1f1ea'])),
																			_Utils_Tuple2(
																			'🇮🇱',
																			_List_fromArray(
																				['1f1ee', '1f1f1'])),
																			_Utils_Tuple2(
																			'🇮🇲',
																			_List_fromArray(
																				['1f1ee', '1f1f2'])),
																			_Utils_Tuple2(
																			'🇮🇳',
																			_List_fromArray(
																				['1f1ee', '1f1f3'])),
																			_Utils_Tuple2(
																			'🇮🇴',
																			_List_fromArray(
																				['1f1ee', '1f1f4'])),
																			_Utils_Tuple2(
																			'🇮🇶',
																			_List_fromArray(
																				['1f1ee', '1f1f6'])),
																			_Utils_Tuple2(
																			'🇮🇷',
																			_List_fromArray(
																				['1f1ee', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇮🇸',
																			_List_fromArray(
																				['1f1ee', '1f1f8'])),
																			_Utils_Tuple2(
																			'🇮🇹',
																			_List_fromArray(
																				['1f1ee', '1f1f9'])),
																			_Utils_Tuple2(
																			'🇯🇪',
																			_List_fromArray(
																				['1f1ef', '1f1ea'])),
																			_Utils_Tuple2(
																			'🇯🇲',
																			_List_fromArray(
																				['1f1ef', '1f1f2'])),
																			_Utils_Tuple2(
																			'🇯🇴',
																			_List_fromArray(
																				['1f1ef', '1f1f4'])),
																			_Utils_Tuple2(
																			'🇯🇵',
																			_List_fromArray(
																				['1f1ef', '1f1f5'])),
																			_Utils_Tuple2(
																			'🇰🇪',
																			_List_fromArray(
																				['1f1f0', '1f1ea'])),
																			_Utils_Tuple2(
																			'🇰🇬',
																			_List_fromArray(
																				['1f1f0', '1f1ec'])),
																			_Utils_Tuple2(
																			'🇰🇭',
																			_List_fromArray(
																				['1f1f0', '1f1ed'])),
																			_Utils_Tuple2(
																			'🇰🇮',
																			_List_fromArray(
																				['1f1f0', '1f1ee'])),
																			_Utils_Tuple2(
																			'🇰🇲',
																			_List_fromArray(
																				['1f1f0', '1f1f2'])),
																			_Utils_Tuple2(
																			'🇰🇳',
																			_List_fromArray(
																				['1f1f0', '1f1f3'])),
																			_Utils_Tuple2(
																			'🇰🇵',
																			_List_fromArray(
																				['1f1f0', '1f1f5'])),
																			_Utils_Tuple2(
																			'🇰🇷',
																			_List_fromArray(
																				['1f1f0', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇰🇼',
																			_List_fromArray(
																				['1f1f0', '1f1fc'])),
																			_Utils_Tuple2(
																			'🇰🇾',
																			_List_fromArray(
																				['1f1f0', '1f1fe'])),
																			_Utils_Tuple2(
																			'🇰🇿',
																			_List_fromArray(
																				['1f1f0', '1f1ff'])),
																			_Utils_Tuple2(
																			'🇱🇦',
																			_List_fromArray(
																				['1f1f1', '1f1e6'])),
																			_Utils_Tuple2(
																			'🇱🇧',
																			_List_fromArray(
																				['1f1f1', '1f1e7'])),
																			_Utils_Tuple2(
																			'🇱🇨',
																			_List_fromArray(
																				['1f1f1', '1f1e8'])),
																			_Utils_Tuple2(
																			'🇱🇮',
																			_List_fromArray(
																				['1f1f1', '1f1ee'])),
																			_Utils_Tuple2(
																			'🇱🇰',
																			_List_fromArray(
																				['1f1f1', '1f1f0'])),
																			_Utils_Tuple2(
																			'🇱🇷',
																			_List_fromArray(
																				['1f1f1', '1f1f7'])),
																			_Utils_Tuple2(
																			'🇱🇸',
																			_List_fromArray(
																				['1f1f1', '1f1f8'])),
																			_Utils_Tuple2(
																			'🇱🇹',
																			_List_fromArray(
																				['1f1f1', '1f1f9'])),
																			_Utils_Tuple2(
																			'🇱🇺',
																			_List_fromArray(
																				['1f1f1', '1f1fa'])),
																			_Utils_Tuple2(
																			'🇱🇻',
																			_List_fromArray(
																				['1f1f1', '1f1fb'])),
																			_Utils_Tuple2(
																			'🇱🇾',
																			_List_fromArray(
																				['1f1f1', '1f1fe'])),
																			_Utils_Tuple2(
																			'🇲🇦',
																			_List_fromArray(
																				['1f1f2', '1f1e6'])),
																			_Utils_Tuple2(
																			'🇲🇨',
																			_List_fromArray(
																				['1f1f2', '1f1e8'])),
																			_Utils_Tuple2(
																			'🇲🇩',
																			_List_fromArray(
																				['1f1f2', '1f1e9'])),
																			_Utils_Tuple2(
																			'🇲🇪',
																			_List_fromArray(
																				['1f1f2', '1f1ea'])),
																			_Utils_Tuple2(
																			'🇲🇫',
																			_List_fromArray(
																				['1f1f2', '1f1eb'])),
																			_Utils_Tuple2(
																			'🇲🇬',
																			_List_fromArray(
																				['1f1f2', '1f1ec'])),
																			_Utils_Tuple2(
																			'🇲🇭',
																			_List_fromArray(
																				['1f1f2', '1f1ed']))
																		]),
																	_Utils_ap(
																		_List_fromArray(
																			[
																				_Utils_Tuple2(
																				'🇲🇰',
																				_List_fromArray(
																					['1f1f2', '1f1f0'])),
																				_Utils_Tuple2(
																				'🇲🇱',
																				_List_fromArray(
																					['1f1f2', '1f1f1'])),
																				_Utils_Tuple2(
																				'🇲🇲',
																				_List_fromArray(
																					['1f1f2', '1f1f2'])),
																				_Utils_Tuple2(
																				'🇲🇳',
																				_List_fromArray(
																					['1f1f2', '1f1f3'])),
																				_Utils_Tuple2(
																				'🇲🇴',
																				_List_fromArray(
																					['1f1f2', '1f1f4'])),
																				_Utils_Tuple2(
																				'🇲🇵',
																				_List_fromArray(
																					['1f1f2', '1f1f5'])),
																				_Utils_Tuple2(
																				'🇲🇶',
																				_List_fromArray(
																					['1f1f2', '1f1f6'])),
																				_Utils_Tuple2(
																				'🇲🇷',
																				_List_fromArray(
																					['1f1f2', '1f1f7'])),
																				_Utils_Tuple2(
																				'🇲🇸',
																				_List_fromArray(
																					['1f1f2', '1f1f8'])),
																				_Utils_Tuple2(
																				'🇲🇹',
																				_List_fromArray(
																					['1f1f2', '1f1f9'])),
																				_Utils_Tuple2(
																				'🇲🇺',
																				_List_fromArray(
																					['1f1f2', '1f1fa'])),
																				_Utils_Tuple2(
																				'🇲🇻',
																				_List_fromArray(
																					['1f1f2', '1f1fb'])),
																				_Utils_Tuple2(
																				'🇲🇼',
																				_List_fromArray(
																					['1f1f2', '1f1fc'])),
																				_Utils_Tuple2(
																				'🇲🇽',
																				_List_fromArray(
																					['1f1f2', '1f1fd'])),
																				_Utils_Tuple2(
																				'🇲🇾',
																				_List_fromArray(
																					['1f1f2', '1f1fe'])),
																				_Utils_Tuple2(
																				'🇲🇿',
																				_List_fromArray(
																					['1f1f2', '1f1ff'])),
																				_Utils_Tuple2(
																				'🇳🇦',
																				_List_fromArray(
																					['1f1f3', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇳🇨',
																				_List_fromArray(
																					['1f1f3', '1f1e8'])),
																				_Utils_Tuple2(
																				'🇳🇪',
																				_List_fromArray(
																					['1f1f3', '1f1ea'])),
																				_Utils_Tuple2(
																				'🇳🇫',
																				_List_fromArray(
																					['1f1f3', '1f1eb'])),
																				_Utils_Tuple2(
																				'🇳🇬',
																				_List_fromArray(
																					['1f1f3', '1f1ec'])),
																				_Utils_Tuple2(
																				'🇳🇮',
																				_List_fromArray(
																					['1f1f3', '1f1ee'])),
																				_Utils_Tuple2(
																				'🇳🇱',
																				_List_fromArray(
																					['1f1f3', '1f1f1'])),
																				_Utils_Tuple2(
																				'🇳🇴',
																				_List_fromArray(
																					['1f1f3', '1f1f4'])),
																				_Utils_Tuple2(
																				'🇳🇵',
																				_List_fromArray(
																					['1f1f3', '1f1f5'])),
																				_Utils_Tuple2(
																				'🇳🇷',
																				_List_fromArray(
																					['1f1f3', '1f1f7'])),
																				_Utils_Tuple2(
																				'🇳🇺',
																				_List_fromArray(
																					['1f1f3', '1f1fa'])),
																				_Utils_Tuple2(
																				'🇳🇿',
																				_List_fromArray(
																					['1f1f3', '1f1ff'])),
																				_Utils_Tuple2(
																				'🇴🇲',
																				_List_fromArray(
																					['1f1f4', '1f1f2'])),
																				_Utils_Tuple2(
																				'🇵🇦',
																				_List_fromArray(
																					['1f1f5', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇵🇪',
																				_List_fromArray(
																					['1f1f5', '1f1ea'])),
																				_Utils_Tuple2(
																				'🇵🇫',
																				_List_fromArray(
																					['1f1f5', '1f1eb'])),
																				_Utils_Tuple2(
																				'🇵🇬',
																				_List_fromArray(
																					['1f1f5', '1f1ec'])),
																				_Utils_Tuple2(
																				'🇵🇭',
																				_List_fromArray(
																					['1f1f5', '1f1ed'])),
																				_Utils_Tuple2(
																				'🇵🇰',
																				_List_fromArray(
																					['1f1f5', '1f1f0'])),
																				_Utils_Tuple2(
																				'🇵🇱',
																				_List_fromArray(
																					['1f1f5', '1f1f1'])),
																				_Utils_Tuple2(
																				'🇵🇲',
																				_List_fromArray(
																					['1f1f5', '1f1f2'])),
																				_Utils_Tuple2(
																				'🇵🇳',
																				_List_fromArray(
																					['1f1f5', '1f1f3'])),
																				_Utils_Tuple2(
																				'🇵🇷',
																				_List_fromArray(
																					['1f1f5', '1f1f7'])),
																				_Utils_Tuple2(
																				'🇵🇸',
																				_List_fromArray(
																					['1f1f5', '1f1f8'])),
																				_Utils_Tuple2(
																				'🇵🇹',
																				_List_fromArray(
																					['1f1f5', '1f1f9'])),
																				_Utils_Tuple2(
																				'🇵🇼',
																				_List_fromArray(
																					['1f1f5', '1f1fc'])),
																				_Utils_Tuple2(
																				'🇵🇾',
																				_List_fromArray(
																					['1f1f5', '1f1fe'])),
																				_Utils_Tuple2(
																				'🇶🇦',
																				_List_fromArray(
																					['1f1f6', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇷🇪',
																				_List_fromArray(
																					['1f1f7', '1f1ea'])),
																				_Utils_Tuple2(
																				'🇷🇴',
																				_List_fromArray(
																					['1f1f7', '1f1f4'])),
																				_Utils_Tuple2(
																				'🇷🇸',
																				_List_fromArray(
																					['1f1f7', '1f1f8'])),
																				_Utils_Tuple2(
																				'🇷🇺',
																				_List_fromArray(
																					['1f1f7', '1f1fa'])),
																				_Utils_Tuple2(
																				'🇷🇼',
																				_List_fromArray(
																					['1f1f7', '1f1fc'])),
																				_Utils_Tuple2(
																				'🇸🇦',
																				_List_fromArray(
																					['1f1f8', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇸🇧',
																				_List_fromArray(
																					['1f1f8', '1f1e7'])),
																				_Utils_Tuple2(
																				'🇸🇨',
																				_List_fromArray(
																					['1f1f8', '1f1e8'])),
																				_Utils_Tuple2(
																				'🇸🇩',
																				_List_fromArray(
																					['1f1f8', '1f1e9'])),
																				_Utils_Tuple2(
																				'🇸🇪',
																				_List_fromArray(
																					['1f1f8', '1f1ea'])),
																				_Utils_Tuple2(
																				'🇸🇬',
																				_List_fromArray(
																					['1f1f8', '1f1ec'])),
																				_Utils_Tuple2(
																				'🇸🇭',
																				_List_fromArray(
																					['1f1f8', '1f1ed'])),
																				_Utils_Tuple2(
																				'🇸🇮',
																				_List_fromArray(
																					['1f1f8', '1f1ee'])),
																				_Utils_Tuple2(
																				'🇸🇯',
																				_List_fromArray(
																					['1f1f8', '1f1ef'])),
																				_Utils_Tuple2(
																				'🇸🇰',
																				_List_fromArray(
																					['1f1f8', '1f1f0'])),
																				_Utils_Tuple2(
																				'🇸🇱',
																				_List_fromArray(
																					['1f1f8', '1f1f1'])),
																				_Utils_Tuple2(
																				'🇸🇲',
																				_List_fromArray(
																					['1f1f8', '1f1f2'])),
																				_Utils_Tuple2(
																				'🇸🇳',
																				_List_fromArray(
																					['1f1f8', '1f1f3'])),
																				_Utils_Tuple2(
																				'🇸🇴',
																				_List_fromArray(
																					['1f1f8', '1f1f4'])),
																				_Utils_Tuple2(
																				'🇸🇷',
																				_List_fromArray(
																					['1f1f8', '1f1f7'])),
																				_Utils_Tuple2(
																				'🇸🇸',
																				_List_fromArray(
																					['1f1f8', '1f1f8'])),
																				_Utils_Tuple2(
																				'🇸🇹',
																				_List_fromArray(
																					['1f1f8', '1f1f9'])),
																				_Utils_Tuple2(
																				'🇸🇻',
																				_List_fromArray(
																					['1f1f8', '1f1fb'])),
																				_Utils_Tuple2(
																				'🇸🇽',
																				_List_fromArray(
																					['1f1f8', '1f1fd'])),
																				_Utils_Tuple2(
																				'🇸🇾',
																				_List_fromArray(
																					['1f1f8', '1f1fe'])),
																				_Utils_Tuple2(
																				'🇸🇿',
																				_List_fromArray(
																					['1f1f8', '1f1ff'])),
																				_Utils_Tuple2(
																				'🇹🇦',
																				_List_fromArray(
																					['1f1f9', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇹🇨',
																				_List_fromArray(
																					['1f1f9', '1f1e8'])),
																				_Utils_Tuple2(
																				'🇹🇩',
																				_List_fromArray(
																					['1f1f9', '1f1e9'])),
																				_Utils_Tuple2(
																				'🇹🇫',
																				_List_fromArray(
																					['1f1f9', '1f1eb'])),
																				_Utils_Tuple2(
																				'🇹🇬',
																				_List_fromArray(
																					['1f1f9', '1f1ec'])),
																				_Utils_Tuple2(
																				'🇹🇭',
																				_List_fromArray(
																					['1f1f9', '1f1ed'])),
																				_Utils_Tuple2(
																				'🇹🇯',
																				_List_fromArray(
																					['1f1f9', '1f1ef'])),
																				_Utils_Tuple2(
																				'🇹🇰',
																				_List_fromArray(
																					['1f1f9', '1f1f0'])),
																				_Utils_Tuple2(
																				'🇹🇱',
																				_List_fromArray(
																					['1f1f9', '1f1f1'])),
																				_Utils_Tuple2(
																				'🇹🇲',
																				_List_fromArray(
																					['1f1f9', '1f1f2'])),
																				_Utils_Tuple2(
																				'🇹🇳',
																				_List_fromArray(
																					['1f1f9', '1f1f3'])),
																				_Utils_Tuple2(
																				'🇹🇴',
																				_List_fromArray(
																					['1f1f9', '1f1f4'])),
																				_Utils_Tuple2(
																				'🇹🇷',
																				_List_fromArray(
																					['1f1f9', '1f1f7'])),
																				_Utils_Tuple2(
																				'🇹🇹',
																				_List_fromArray(
																					['1f1f9', '1f1f9'])),
																				_Utils_Tuple2(
																				'🇹🇻',
																				_List_fromArray(
																					['1f1f9', '1f1fb'])),
																				_Utils_Tuple2(
																				'🇹🇼',
																				_List_fromArray(
																					['1f1f9', '1f1fc'])),
																				_Utils_Tuple2(
																				'🇹🇿',
																				_List_fromArray(
																					['1f1f9', '1f1ff'])),
																				_Utils_Tuple2(
																				'🇺🇦',
																				_List_fromArray(
																					['1f1fa', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇺🇬',
																				_List_fromArray(
																					['1f1fa', '1f1ec'])),
																				_Utils_Tuple2(
																				'🇺🇲',
																				_List_fromArray(
																					['1f1fa', '1f1f2'])),
																				_Utils_Tuple2(
																				'🇺🇸',
																				_List_fromArray(
																					['1f1fa', '1f1f8'])),
																				_Utils_Tuple2(
																				'🇺🇾',
																				_List_fromArray(
																					['1f1fa', '1f1fe'])),
																				_Utils_Tuple2(
																				'🇺🇿',
																				_List_fromArray(
																					['1f1fa', '1f1ff'])),
																				_Utils_Tuple2(
																				'🇻🇦',
																				_List_fromArray(
																					['1f1fb', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇻🇨',
																				_List_fromArray(
																					['1f1fb', '1f1e8'])),
																				_Utils_Tuple2(
																				'🇻🇪',
																				_List_fromArray(
																					['1f1fb', '1f1ea'])),
																				_Utils_Tuple2(
																				'🇻🇬',
																				_List_fromArray(
																					['1f1fb', '1f1ec'])),
																				_Utils_Tuple2(
																				'🇻🇮',
																				_List_fromArray(
																					['1f1fb', '1f1ee'])),
																				_Utils_Tuple2(
																				'🇻🇳',
																				_List_fromArray(
																					['1f1fb', '1f1f3']))
																			]),
																		_List_fromArray(
																			[
																				_Utils_Tuple2(
																				'🇻🇺',
																				_List_fromArray(
																					['1f1fb', '1f1fa'])),
																				_Utils_Tuple2(
																				'🇼🇫',
																				_List_fromArray(
																					['1f1fc', '1f1eb'])),
																				_Utils_Tuple2(
																				'🇼🇸',
																				_List_fromArray(
																					['1f1fc', '1f1f8'])),
																				_Utils_Tuple2(
																				'🇽🇰',
																				_List_fromArray(
																					['1f1fd', '1f1f0'])),
																				_Utils_Tuple2(
																				'🇾🇪',
																				_List_fromArray(
																					['1f1fe', '1f1ea'])),
																				_Utils_Tuple2(
																				'🇾🇹',
																				_List_fromArray(
																					['1f1fe', '1f1f9'])),
																				_Utils_Tuple2(
																				'🇿🇦',
																				_List_fromArray(
																					['1f1ff', '1f1e6'])),
																				_Utils_Tuple2(
																				'🇿🇲',
																				_List_fromArray(
																					['1f1ff', '1f1f2'])),
																				_Utils_Tuple2(
																				'🇿🇼',
																				_List_fromArray(
																					['1f1ff', '1f1fc']))
																			])))))))))))))))))));
var stephenreddek$elm_emoji$Emoji$Internal$Valid$longest = A3(
	elm$core$List$foldl,
	elm$core$Basics$max,
	0,
	A2(
		elm$core$List$map,
		A2(elm$core$Basics$composeL, elm$core$String$length, elm$core$Tuple$first),
		stephenreddek$elm_emoji$Emoji$Internal$Valid$pairs));
var stephenreddek$elm_emoji$Emoji$Internal$Parse$findPrefix = F4(
	function (lastFound, count, string, store) {
		findPrefix:
		while (true) {
			if (_Utils_cmp(count, stephenreddek$elm_emoji$Emoji$Internal$Valid$longest) > 0) {
				return lastFound;
			} else {
				var _n0 = store;
				var foundCode = _n0.a;
				var children = _n0.b;
				var bestMatch = A2(
					elm$core$Maybe$withDefault,
					lastFound,
					A2(
						elm$core$Maybe$map,
						function (code) {
							return _Utils_Tuple2(count, code);
						},
						foundCode));
				var _n1 = elm$core$String$uncons(string);
				if (_n1.$ === 'Nothing') {
					return bestMatch;
				} else {
					var _n2 = _n1.a;
					var _char = _n2.a;
					var rest = _n2.b;
					var _n3 = A2(elm$core$Dict$get, _char, children);
					if (_n3.$ === 'Nothing') {
						return bestMatch;
					} else {
						var childStore = _n3.a;
						var $temp$lastFound = bestMatch,
							$temp$count = count + 1,
							$temp$string = rest,
							$temp$store = childStore;
						lastFound = $temp$lastFound;
						count = $temp$count;
						string = $temp$string;
						store = $temp$store;
						continue findPrefix;
					}
				}
			}
		}
	});
var stephenreddek$elm_emoji$Emoji$Internal$Valid$Store = F2(
	function (a, b) {
		return {$: 'Store', a: a, b: b};
	});
var stephenreddek$elm_emoji$Emoji$Internal$Valid$empty = A2(stephenreddek$elm_emoji$Emoji$Internal$Valid$Store, elm$core$Maybe$Nothing, elm$core$Dict$empty);
var stephenreddek$elm_emoji$Emoji$Internal$Valid$load = F2(
	function (_n0, store_) {
		var bytes = _n0.a;
		var codePoints = _n0.b;
		var _n1 = store_;
		var code = _n1.a;
		var ch = _n1.b;
		var _n2 = elm$core$String$uncons(bytes);
		if (_n2.$ === 'Nothing') {
			return A2(
				stephenreddek$elm_emoji$Emoji$Internal$Valid$Store,
				elm$core$Maybe$Just(codePoints),
				ch);
		} else {
			var _n3 = _n2.a;
			var c = _n3.a;
			var rest = _n3.b;
			return A2(
				stephenreddek$elm_emoji$Emoji$Internal$Valid$Store,
				code,
				A3(
					elm$core$Dict$update,
					c,
					A2(
						elm$core$Basics$composeL,
						A2(
							elm$core$Basics$composeL,
							elm$core$Maybe$Just,
							stephenreddek$elm_emoji$Emoji$Internal$Valid$load(
								_Utils_Tuple2(rest, codePoints))),
						elm$core$Maybe$withDefault(stephenreddek$elm_emoji$Emoji$Internal$Valid$empty)),
					ch));
		}
	});
var stephenreddek$elm_emoji$Emoji$Internal$Valid$store = A3(elm$core$List$foldl, stephenreddek$elm_emoji$Emoji$Internal$Valid$load, stephenreddek$elm_emoji$Emoji$Internal$Valid$empty, stephenreddek$elm_emoji$Emoji$Internal$Valid$pairs);
var stephenreddek$elm_emoji$Emoji$Internal$Parse$splitPrefix = function (string) {
	var _n0 = A4(
		stephenreddek$elm_emoji$Emoji$Internal$Parse$findPrefix,
		_Utils_Tuple2(0, _List_Nil),
		0,
		string,
		stephenreddek$elm_emoji$Emoji$Internal$Valid$store);
	var len = _n0.a;
	var code = _n0.b;
	return _Utils_Tuple2(
		_Utils_Tuple2(len, code),
		A2(stephenreddek$elm_emoji$Emoji$Internal$Parse$dropLeft, len, string));
};
var stephenreddek$elm_emoji$Emoji$Internal$Parse$parse_ = F3(
	function (buf, accum, string) {
		parse_:
		while (true) {
			var _n0 = _Utils_Tuple2(string, buf);
			if (_n0.a === '') {
				if (_n0.b === '') {
					return accum;
				} else {
					return A2(
						elm$core$List$cons,
						stephenreddek$elm_emoji$Emoji$Internal$Parse$StringChunk(
							elm$core$String$reverse(buf)),
						accum);
				}
			} else {
				var _n1 = stephenreddek$elm_emoji$Emoji$Internal$Parse$splitPrefix(string);
				if (!_n1.a.a) {
					var _n2 = _n1.a;
					var _n3 = elm$core$String$uncons(string);
					if (_n3.$ === 'Nothing') {
						return accum;
					} else {
						var _n4 = _n3.a;
						var c = _n4.a;
						var rest = _n4.b;
						var $temp$buf = A2(elm$core$String$cons, c, buf),
							$temp$accum = accum,
							$temp$string = rest;
						buf = $temp$buf;
						accum = $temp$accum;
						string = $temp$string;
						continue parse_;
					}
				} else {
					var _n5 = _n1.a;
					var matchLen = _n5.a;
					var matchCodes = _n5.b;
					var remaining = _n1.b;
					var nextAccum = (buf === '') ? accum : A2(
						elm$core$List$cons,
						stephenreddek$elm_emoji$Emoji$Internal$Parse$StringChunk(
							elm$core$String$reverse(buf)),
						accum);
					var $temp$buf = '',
						$temp$accum = A2(
						elm$core$List$cons,
						stephenreddek$elm_emoji$Emoji$Internal$Parse$CodeChunk(matchCodes),
						nextAccum),
						$temp$string = remaining;
					buf = $temp$buf;
					accum = $temp$accum;
					string = $temp$string;
					continue parse_;
				}
			}
		}
	});
var stephenreddek$elm_emoji$Emoji$Internal$Parse$parse = function (string) {
	var string_ = A3(stephenreddek$elm_emoji$Emoji$Internal$Parse$parse_, '', _List_Nil, string);
	return stephenreddek$elm_emoji$Emoji$Internal$Parse$String_(
		elm$core$List$reverse(string_));
};
var stephenreddek$elm_emoji$Emoji$textWith = F2(
	function (replacer, body) {
		var _n0 = stephenreddek$elm_emoji$Emoji$Internal$Parse$parse(body);
		var chunks = _n0.a;
		return A2(
			elm$core$List$map,
			function (chunk) {
				if (chunk.$ === 'StringChunk') {
					var s = chunk.a;
					return elm$html$Html$text(s);
				} else {
					var codepts = chunk.a;
					return replacer(codepts);
				}
			},
			chunks);
	});
var author$project$ViewUtil$viewTwitterName = function (info) {
	return A2(
		stephenreddek$elm_emoji$Emoji$textWith,
		stephenreddek$elm_emoji$Emoji$replaceWithTwemoji,
		function () {
			var _n0 = info.liverUpdateInfo;
			if (_n0.$ === 'Just') {
				var updateInfo = _n0.a;
				return updateInfo.twitterName;
			} else {
				return info.liverName;
			}
		}());
};
var elm$html$Html$h6 = _VirtualDom_node('h6');
var elm$html$Html$p = _VirtualDom_node('p');
var elm$html$Html$small = _VirtualDom_node('small');
var author$project$ViewUtil$viewNameInfo = function (info) {
	return A2(
		elm$html$Html$li,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('media message')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$img,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('mr-3 message-thumbnail'),
						elm$html$Html$Attributes$src('assets/member/' + (info.liverName + '.png'))
					]),
				_List_Nil),
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('media-body message-body')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$h6,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('message-meta')
							]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$a,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('message-owner'),
										author$project$Route$href(
										author$project$Route$Liver(
											elm$core$Maybe$Just(info.liverName)))
									]),
								_List_fromArray(
									[
										elm$html$Html$text(info.liverName + (' @' + info.liverTwitterScreenName))
									])),
								A2(
								elm$html$Html$small,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('text-muted message-time')
									]),
								_List_fromArray(
									[
										elm$html$Html$text(
										function () {
											var _n0 = info.liverUpdateInfo;
											if (_n0.$ === 'Nothing') {
												return '';
											} else {
												var updateInfo = _n0.a;
												return updateInfo.updateTime;
											}
										}())
									]))
							])),
						A2(
						elm$html$Html$p,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('message-text')
							]),
						author$project$ViewUtil$viewTwitterName(info))
					]))
			]));
};
var elm$html$Html$hr = _VirtualDom_node('hr');
var author$project$Page$Liver$view = function (model) {
	return {
		content: A2(
			elm$html$Html$main_,
			_List_fromArray(
				[
					elm$html$Html$Attributes$id('content'),
					elm$html$Html$Attributes$class('container'),
					elm$html$Html$Attributes$tabindex(-1)
				]),
			function () {
				var _n0 = model.mliver;
				if (_n0.$ === 'Nothing') {
					return _List_fromArray(
						[author$project$Page$Liver$viewLiverList]);
				} else {
					var _n1 = model.data;
					if (_n1.$ === 'Err') {
						var msg = _n1.a;
						return _List_fromArray(
							[
								elm$html$Html$text(msg)
							]);
					} else {
						var data = _n1.a;
						return A2(
							elm$core$List$intersperse,
							A2(elm$html$Html$hr, _List_Nil, _List_Nil),
							A2(
								elm$core$List$map,
								author$project$ViewUtil$viewNameInfo,
								author$project$Page$Liver$dataToNameInfo(data)));
					}
				}
			}()),
		title: function () {
			var _n2 = model.mliver;
			if (_n2.$ === 'Nothing') {
				return 'Liver';
			} else {
				var liver = _n2.a;
				return liver;
			}
		}()
	};
};
var elm$html$Html$h1 = _VirtualDom_node('h1');
var author$project$Page$NotFound$view = {
	content: A2(
		elm$html$Html$main_,
		_List_fromArray(
			[
				elm$html$Html$Attributes$id('content'),
				elm$html$Html$Attributes$class('container'),
				elm$html$Html$Attributes$tabindex(-1)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$h1,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text('Not Found')
					]))
			])),
	title: 'Page Not Found'
};
var author$project$Page$Snapshot$DateUpdate = function (a) {
	return {$: 'DateUpdate', a: a};
};
var author$project$Page$Snapshot$TimeUpdate = function (a) {
	return {$: 'TimeUpdate', a: a};
};
var elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var author$project$Page$Snapshot$dataToNameInfo = F2(
	function (groups, data) {
		return A2(
			elm$core$List$map,
			function (member) {
				return {
					liverName: member.name,
					liverTwitterScreenName: member.screenName,
					liverUpdateInfo: A2(elm$core$Dict$get, member.name, data)
				};
			},
			A2(
				elm$core$List$filter,
				function (m) {
					return A2(elm$core$List$member, m.exGroup, groups);
				},
				author$project$Nijisanji$allMembers));
	});
var author$project$Page$Snapshot$FilterUpdate = function (a) {
	return {$: 'FilterUpdate', a: a};
};
var elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			elm$core$List$any,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, isOkay),
			list);
	});
var elm_community$list_extra$List$Extra$remove = F2(
	function (x, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var y = xs.a;
			var ys = xs.b;
			return _Utils_eq(x, y) ? ys : A2(
				elm$core$List$cons,
				y,
				A2(elm_community$list_extra$List$Extra$remove, x, ys));
		}
	});
var elm$html$Html$input = _VirtualDom_node('input');
var elm$html$Html$label = _VirtualDom_node('label');
var elm$html$Html$Attributes$autocomplete = function (bool) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var elm$html$Html$Attributes$checked = elm$html$Html$Attributes$boolProperty('checked');
var rundis$elm_bootstrap$Bootstrap$Button$checkboxButton = F3(
	function (checked, options, children) {
		return A2(
			elm$html$Html$label,
			A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('active', checked)
						])),
				rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options)),
			A2(
				elm$core$List$cons,
				A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$type_('checkbox'),
							elm$html$Html$Attributes$checked(checked),
							elm$html$Html$Attributes$autocomplete(false)
						]),
					_List_Nil),
				children));
	});
var elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var rundis$elm_bootstrap$Bootstrap$Button$onClick = function (message) {
	return rundis$elm_bootstrap$Bootstrap$Button$attrs(
		_List_fromArray(
			[
				A2(
				elm$html$Html$Events$preventDefaultOn,
				'click',
				elm$json$Json$Decode$succeed(
					_Utils_Tuple2(message, true)))
			]));
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary = {$: 'Primary'};
var rundis$elm_bootstrap$Bootstrap$Button$primary = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary));
var author$project$Page$Snapshot$viewHeader = function (model) {
	var toggle = function (g) {
		return A2(elm$core$List$member, g, model.filter) ? A2(elm_community$list_extra$List$Extra$remove, g, model.filter) : A2(elm$core$List$cons, g, model.filter);
	};
	var mkButton = F2(
		function (g, name) {
			return A3(
				rundis$elm_bootstrap$Bootstrap$Button$checkboxButton,
				A2(elm$core$List$member, g, model.filter),
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Button$primary,
						rundis$elm_bootstrap$Bootstrap$Button$onClick(
						author$project$Page$Snapshot$FilterUpdate(
							toggle(g)))
					]),
				_List_fromArray(
					[
						elm$html$Html$text(name)
					]));
		});
	var allButton = function () {
		var b = A2(
			elm$core$List$all,
			function (g) {
				return A2(elm$core$List$member, g, model.filter);
			},
			_List_fromArray(
				[author$project$Nijisanji$Nijisanji, author$project$Nijisanji$Gamers, author$project$Nijisanji$SEEDs]));
		return A3(
			rundis$elm_bootstrap$Bootstrap$Button$checkboxButton,
			b,
			_List_fromArray(
				[
					rundis$elm_bootstrap$Bootstrap$Button$primary,
					rundis$elm_bootstrap$Bootstrap$Button$onClick(
					author$project$Page$Snapshot$FilterUpdate(
						b ? _List_Nil : _List_fromArray(
							[author$project$Nijisanji$Nijisanji, author$project$Nijisanji$Gamers, author$project$Nijisanji$SEEDs])))
				]),
			_List_fromArray(
				[
					elm$html$Html$text('全員')
				]));
	}();
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('contents-filter')
			]),
		_List_fromArray(
			[
				A2(mkButton, author$project$Nijisanji$Nijisanji, '一期生二期生出身'),
				A2(mkButton, author$project$Nijisanji$Gamers, 'ゲーマーズ出身'),
				A2(mkButton, author$project$Nijisanji$SEEDs, 'SEEDs出身+α')
			]));
};
var elm$html$Html$Attributes$max = elm$html$Html$Attributes$stringProperty('max');
var elm$html$Html$Attributes$min = elm$html$Html$Attributes$stringProperty('min');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var author$project$Page$Snapshot$view = function (model) {
	return {
		content: A2(
			elm$html$Html$main_,
			_List_fromArray(
				[
					elm$html$Html$Attributes$id('content'),
					elm$html$Html$Attributes$class('container'),
					elm$html$Html$Attributes$tabindex(-1)
				]),
			function () {
				var _n0 = model.data;
				if (_n0.$ === 'Err') {
					var msg = _n0.a;
					return _List_fromArray(
						[
							author$project$Page$Snapshot$viewHeader(model),
							A2(
							elm$html$Html$input,
							_List_fromArray(
								[
									elm$html$Html$Attributes$type_('date'),
									elm$html$Html$Attributes$min('2018-08-22T00:00'),
									elm$html$Html$Attributes$max('2018-12-31T00:00'),
									elm$html$Html$Events$onInput(author$project$Page$Snapshot$DateUpdate)
								]),
							_List_Nil),
							A2(
							elm$html$Html$input,
							_List_fromArray(
								[
									elm$html$Html$Attributes$type_('time'),
									elm$html$Html$Events$onInput(author$project$Page$Snapshot$TimeUpdate)
								]),
							_List_Nil),
							elm$html$Html$text(msg)
						]);
				} else {
					var data = _n0.a;
					return elm$core$List$concat(
						_List_fromArray(
							[
								_List_fromArray(
								[
									author$project$Page$Snapshot$viewHeader(model),
									A2(
									elm$html$Html$input,
									_List_fromArray(
										[
											elm$html$Html$Attributes$type_('date'),
											elm$html$Html$Attributes$min('2018-08-22T00:00'),
											elm$html$Html$Attributes$max('2018-12-31T00:00'),
											elm$html$Html$Events$onInput(author$project$Page$Snapshot$DateUpdate)
										]),
									_List_Nil),
									A2(
									elm$html$Html$input,
									_List_fromArray(
										[
											elm$html$Html$Attributes$type_('time'),
											elm$html$Html$Events$onInput(author$project$Page$Snapshot$TimeUpdate)
										]),
									_List_Nil)
								]),
								A2(
								elm$core$List$intersperse,
								A2(elm$html$Html$hr, _List_Nil, _List_Nil),
								A2(
									elm$core$List$map,
									author$project$ViewUtil$viewNameInfo,
									A2(author$project$Page$Snapshot$dataToNameInfo, model.filter, data)))
							]));
				}
			}()),
		title: 'Home'
	};
};
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (_n0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return A2(elm$core$Dict$member, key, dict);
	});
var author$project$Page$Timeline$dataToNameInfo = F2(
	function (filter, data) {
		return A2(
			elm$core$List$map,
			function (_n1) {
				var name = _n1.a;
				var updateInfo = _n1.b;
				return {
					liverName: name,
					liverTwitterScreenName: function () {
						var _n2 = author$project$Nijisanji$getScreenName(name);
						if (_n2.$ === 'Nothing') {
							return A2(elm$core$Debug$log, 'ほんまかー', '');
						} else {
							var screenName = _n2.a;
							return screenName;
						}
					}(),
					liverUpdateInfo: elm$core$Maybe$Just(updateInfo)
				};
			},
			A2(
				elm$core$List$filter,
				function (_n0) {
					var name = _n0.a;
					return A2(elm$core$Set$member, name, filter);
				},
				data));
	});
var author$project$Page$Timeline$FilterUpdate = function (a) {
	return {$: 'FilterUpdate', a: a};
};
var elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var elm$core$Set$diff = F2(
	function (_n0, _n1) {
		var dict1 = _n0.a;
		var dict2 = _n1.a;
		return elm$core$Set$Set_elm_builtin(
			A2(elm$core$Dict$diff, dict1, dict2));
	});
var elm$core$Set$remove = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A2(elm$core$Dict$remove, key, dict));
	});
var elm$core$Set$union = F2(
	function (_n0, _n1) {
		var dict1 = _n0.a;
		var dict2 = _n1.a;
		return elm$core$Set$Set_elm_builtin(
			A2(elm$core$Dict$union, dict1, dict2));
	});
var author$project$Page$Timeline$viewHeader = function (model) {
	var mkGroup = F2(
		function (g, name) {
			var mkSubBottun = function (liverName) {
				var isActiveSub = A2(elm$core$Set$member, liverName, model.filter);
				var newSubFilter = isActiveSub ? A2(elm$core$Set$remove, liverName, model.filter) : A2(elm$core$Set$insert, liverName, model.filter);
				return A3(
					rundis$elm_bootstrap$Bootstrap$Button$checkboxButton,
					isActiveSub,
					_List_fromArray(
						[
							rundis$elm_bootstrap$Bootstrap$Button$secondary,
							rundis$elm_bootstrap$Bootstrap$Button$onClick(
							author$project$Page$Timeline$FilterUpdate(newSubFilter))
						]),
					_List_fromArray(
						[
							elm$html$Html$text(liverName)
						]));
			};
			var groupMembersName = A2(
				elm$core$List$map,
				function (m) {
					return m.name;
				},
				author$project$Nijisanji$groupMembers(g));
			var isActive = A2(
				elm$core$List$all,
				function (n) {
					return A2(elm$core$Set$member, n, model.filter);
				},
				groupMembersName);
			var newFilter = isActive ? A2(
				elm$core$Set$diff,
				model.filter,
				elm$core$Set$fromList(groupMembersName)) : A2(
				elm$core$Set$union,
				model.filter,
				elm$core$Set$fromList(groupMembersName));
			return A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A3(
						rundis$elm_bootstrap$Bootstrap$Button$checkboxButton,
						isActive,
						_List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$Button$primary,
								rundis$elm_bootstrap$Bootstrap$Button$onClick(
								author$project$Page$Timeline$FilterUpdate(newFilter))
							]),
						_List_fromArray(
							[
								elm$html$Html$text(name)
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						A2(elm$core$List$map, mkSubBottun, groupMembersName))
					]));
		});
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('contents-filter')
			]),
		_List_fromArray(
			[
				A2(mkGroup, author$project$Nijisanji$Nijisanji, '一期生二期生出身'),
				A2(mkGroup, author$project$Nijisanji$Gamers, 'ゲーマーズ出身'),
				A2(mkGroup, author$project$Nijisanji$SEEDs, 'SEEDs出身+α')
			]));
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Block = {$: 'Block'};
var rundis$elm_bootstrap$Bootstrap$Button$block = rundis$elm_bootstrap$Bootstrap$Internal$Button$Block;
var rundis$elm_bootstrap$Bootstrap$Button$button = F2(
	function (options, children) {
		return A2(
			elm$html$Html$button,
			rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options),
			children);
	});
var author$project$Page$Timeline$view = function (model) {
	return {
		content: A2(
			elm$html$Html$main_,
			_List_fromArray(
				[
					elm$html$Html$Attributes$id('content'),
					elm$html$Html$Attributes$class('container'),
					elm$html$Html$Attributes$tabindex(-1)
				]),
			function () {
				var _n0 = model.data;
				if (_n0.$ === 'Err') {
					var msg = _n0.a;
					return _List_fromArray(
						[
							elm$html$Html$text(msg)
						]);
				} else {
					var data = _n0.a;
					return elm$core$List$concat(
						_List_fromArray(
							[
								_List_fromArray(
								[
									author$project$Page$Timeline$viewHeader(model)
								]),
								A2(
								elm$core$List$intersperse,
								A2(elm$html$Html$hr, _List_Nil, _List_Nil),
								A2(
									elm$core$List$map,
									author$project$ViewUtil$viewNameInfo,
									A2(author$project$Page$Timeline$dataToNameInfo, model.filter, data))),
								model.remainNoData ? _List_Nil : _List_fromArray(
								[
									A2(
									rundis$elm_bootstrap$Bootstrap$Button$button,
									_List_fromArray(
										[
											rundis$elm_bootstrap$Bootstrap$Button$secondary,
											rundis$elm_bootstrap$Bootstrap$Button$onClick(author$project$Page$Timeline$LoadMore),
											rundis$elm_bootstrap$Bootstrap$Button$block
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Load More')
										]))
								])
							]));
				}
			}()),
		title: 'Timeline'
	};
};
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var author$project$Main$view = function (model) {
	var viewPage = F3(
		function (page, toMsg, doc) {
			return A4(
				author$project$Page$view,
				author$project$Main$toSession(model),
				page,
				{
					navbarHandler: A2(elm$core$Basics$composeL, author$project$Main$PageCtrl, author$project$Main$NavMsg)
				},
				{
					content: A2(elm$html$Html$map, toMsg, doc.content),
					title: doc.title
				});
		});
	switch (model.$) {
		case 'Redirect':
			return A3(
				viewPage,
				author$project$Page$Other,
				function (_n1) {
					return author$project$Main$Ignored;
				},
				author$project$Page$Blank$view);
		case 'NotFound':
			return A3(
				viewPage,
				author$project$Page$Other,
				function (_n2) {
					return author$project$Main$Ignored;
				},
				author$project$Page$NotFound$view);
		case 'Home':
			var subModel = model.a;
			return A3(
				viewPage,
				author$project$Page$Home,
				author$project$Main$GotHomeMsg,
				author$project$Page$Home$view(subModel));
		case 'About':
			var subModel = model.a;
			return A3(
				viewPage,
				author$project$Page$About,
				author$project$Main$GotAboutMsg,
				author$project$Page$About$view(subModel));
		case 'Snapshot':
			var subModel = model.a;
			return A3(
				viewPage,
				author$project$Page$Snapshot,
				author$project$Main$GotSnapshotMsg,
				author$project$Page$Snapshot$view(subModel));
		case 'Timeline':
			var subModel = model.a;
			return A3(
				viewPage,
				author$project$Page$Snapshot,
				author$project$Main$GotTimelineMsg,
				author$project$Page$Timeline$view(subModel));
		default:
			var subModel = model.a;
			return A3(
				viewPage,
				author$project$Page$Snapshot,
				author$project$Main$GotLiverMsg,
				author$project$Page$Liver$view(subModel));
	}
};
var elm$browser$Browser$application = _Browser_application;
var author$project$Main$main = elm$browser$Browser$application(
	{init: author$project$Main$init, onUrlChange: author$project$Main$ChangedUrl, onUrlRequest: author$project$Main$ClickedLink, subscriptions: author$project$Main$subscriptions, update: author$project$Main$update, view: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	elm$json$Json$Decode$succeed(
		{}))(0)}});}(this));