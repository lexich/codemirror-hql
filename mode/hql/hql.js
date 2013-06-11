// Generated by CoffeeScript 1.6.2
(function() {
  var atomsWords, builtinWords, dateSQLWords, sqlKeywords;

  sqlKeywords = ['and', 'as', 'by', 'delete', 'distinct', 'entry', 'fetch', 'from', 'group', 'in', 'index', 'is', 'join', 'key', 'left', 'like', 'not', 'or', 'or', 'order', 'select', 'set', 'update', 'value', 'where', 'when', 'with', "inner", "outer", "left", "rigth", "join", "fetch", "all"].join(" ");

  builtinWords = [].join(" ");

  atomsWords = ["false", "true", "null", "unknown"].join(" ");

  dateSQLWords = ['abs', 'avg', 'bit_length', 'cast', 'concat', 'count', 'current_date', 'current_time', 'current_timestamp', 'day', 'elements', 'extract', 'hour', 'indices', 'length', 'locate', 'lower', 'max', 'maxelement', 'maxindex', 'min', 'minelement', 'minindex', 'minute', 'mod', 'month', 'second', 'size', 'sqrt', 'str', 'substring', 'sum', 'trim', 'upper', 'year'].join(" ");

  CodeMirror.defineMode("hql", function(config, parserConfig) {
    var atoms, builtin, client, dateSQL, hooks, keywords, operatorChars, popContext, pushContext, support, tokenBase, tokenComment, tokenLiteral;

    client = parserConfig.client || {};
    atoms = parserConfig.atoms || {
      "false": true,
      "true": true,
      "null": true
    };
    builtin = parserConfig.builtin || {};
    keywords = parserConfig.keywords || {};
    operatorChars = parserConfig.operatorChars || /^[*+\-%<>!=&|~^]/;
    support = parserConfig.support || {};
    hooks = parserConfig.hooks || {};
    dateSQL = parserConfig.dateSQL || {
      date: true,
      time: true,
      timestamp: true
    };
    tokenBase = function(stream, state) {
      var ch, result, word;

      ch = stream.next();
      if (hooks[ch]) {
        result = hooks[ch](stream, state);
        if (result !== false) {
          return result;
        }
      }
      if ((ch === "0" && stream.match(/^[xX][0-9a-fA-F]+/)) || (ch === "x" || ch === "X") && stream.match(/^'[0-9a-fA-F]+'/)) {
        return "number";
      } else if (((ch === "b" || ch === "B") && stream.match(/^'[01]+'/)) || (ch === "0" && stream.match(/^b[01]+/))) {
        return "number";
      } else if (ch.charCodeAt(0) > 47 && ch.charCodeAt(0) < 58) {
        stream.match(/^[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/);
        return "number";
      } else if (ch === "?" && (stream.eatSpace() || stream.eol() || stream.eat(";"))) {
        return "variable-3";
      } else if (ch === "\"" || ch === "'") {
        state.tokenize = tokenLiteral(ch);
        return state.tokenize(stream, state);
      } else if (/^[\(\),\;\[\]]/.test(ch)) {
        return null;
      } else if (ch === "/") {
        if (stream.eat("/")) {
          stream.skipToEnd();
          return "comment";
        } else if (stream.eat("*")) {
          state.tokenize = tokenComment;
          return state.tokenize(stream, state);
        }
      } else if (ch === ".") {
        if (support.zerolessFloat === true && stream.match(/^(?:\d+(?:e\d*)?|\d*e\d+)/i)) {
          return "number";
        }
        if (stream.match(/^[a-zA-Z_]+/) && support.ODBCdotTable === true) {
          return "variable-2";
        }
      } else if (operatorChars.test(ch)) {
        stream.eatWhile(operatorChars);
        return null;
      } else if (ch === "{" && (stream.match(/^( )*(d|D|t|T|ts|TS)( )*'[^']*'( )*}/) || stream.match(/^( )*(d|D|t|T|ts|TS)( )*"[^"]*"( )*}/))) {
        return "number";
      } else {
        stream.eatWhile(/^[_\w\d]/);
        word = stream.current().toLowerCase();
        if (dateSQL.hasOwnProperty(word) && (stream.match(/^( )+'[^']*'/) || stream.match(/^( )+"[^"]*"/))) {
          return "number";
        }
        if (atoms.hasOwnProperty(word)) {
          return "atom";
        }
        if (builtin.hasOwnProperty(word)) {
          return "builtin";
        }
        if (keywords.hasOwnProperty(word)) {
          return "keyword";
        }
        if (client.hasOwnProperty(word)) {
          return "string-2";
        }
        return null;
      }
    };
    tokenLiteral = function(quote) {
      return function(stream, state) {
        var ch, escaped;

        escaped = false;
        ch = void 0;
        while ((ch = stream.next()) != null) {
          if (ch === quote && !escaped) {
            state.tokenize = tokenBase;
            break;
          }
          escaped = !escaped && ch === "\\";
        }
        return "string";
      };
    };
    tokenComment = function(stream, state) {
      while (true) {
        if (stream.skipTo("*")) {
          stream.next();
          if (stream.eat("/")) {
            state.tokenize = tokenBase;
            break;
          }
        } else {
          stream.skipToEnd();
          break;
        }
      }
      return "comment";
    };
    pushContext = function(stream, state, type) {
      return state.context = {
        prev: state.context,
        indent: stream.indentation(),
        col: stream.column(),
        type: type
      };
    };
    popContext = function(state) {
      state.indent = state.context.indent;
      return state.context = state.context.prev;
    };
    return {
      startState: function() {
        return {
          tokenize: tokenBase,
          context: null
        };
      },
      token: function(stream, state) {
        var style, tok;

        if (stream.sol() ? state.context && (state.context.align == null) : void 0) {
          state.context.align = false;
        }
        if (stream.eatSpace()) {
          return null;
        }
        style = state.tokenize(stream, state);
        if (style === "comment") {
          return style;
        }
        if (state.context && (state.context.align == null)) {
          state.context.align = true;
        }
        tok = stream.current();
        if (tok === "(") {
          pushContext(stream, state, ")");
        } else if (tok === "[") {
          pushContext(stream, state, "]");
        } else {
          if (state.context && state.context.type === tok) {
            popContext(state);
          }
        }
        return style;
      },
      indent: function(state, textAfter) {
        var cx;

        cx = state.context;
        if (!cx) {
          return CodeMirror.Pass;
        }
        if (cx.align) {
          return cx.col + (textAfter.charAt(0) === cx.type ? 0 : 1);
        } else {
          return cx.indent + config.indentUnit;
        }
      }
    };
  });

  (function() {
    var hookClient, hookIdentifier, hookVar, set;

    hookIdentifier = function(stream) {
      var ch;

      ch = void 0;
      if ((function() {
        var _results;

        _results = [];
        while ((ch = stream.next()) != null) {
          _results.push(ch === "`" && !stream.eat("`"));
        }
        return _results;
      })()) {
        return "variable-2";
      }
      return null;
    };
    hookVar = function(stream) {
      if (stream.eat("@")) {
        stream.match(/^session\./);
        stream.match(/^local\./);
        stream.match(/^global\./);
      }
      if (stream.eat("'")) {
        stream.match(/^.*'/);
        return "variable-2";
      } else if (stream.eat("\"")) {
        stream.match(/^.*"/);
        return "variable-2";
      } else if (stream.eat("`")) {
        stream.match(/^.*`/);
        return "variable-2";
      } else {
        if (stream.match(/^[0-9a-zA-Z$\.\_]+/)) {
          return "variable-2";
        }
      }
      return null;
    };
    hookClient = function(stream) {
      if (stream.match(/^[a-zA-Z]\b/)) {
        return "variable-2";
      } else {
        return null;
      }
    };
    set = function(str) {
      var i, obj, words;

      obj = {};
      words = str.split(" ");
      i = 0;
      while (i < words.length) {
        obj[words[i]] = true;
        ++i;
      }
      return obj;
    };
    CodeMirror.defineMIME("hql", {
      name: "hql",
      keywords: set(sqlKeywords),
      builtin: set(builtinWords),
      atoms: set(atomsWords),
      operatorChars: /^[*+\-%<>!=]/,
      dateSQL: set(dateSQLWords),
      support: set("ODBCdotTable")
    });
    return CodeMirror.defineMIME("text/hql", "hql");
  })();

}).call(this);

/*
//@ sourceMappingURL=hql.map
*/
