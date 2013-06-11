// Generated by CoffeeScript 1.6.2
(function() {
  var Generator, Schema, Service,
    __indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

  Service = (function(__super__) {
    return __super__.prototype = {
      getCodeAfterToken: function(cm, cur, token) {
        var i, line, res, text, _i, _ref;

        text = "";
        for (i = _i = _ref = cur.line; _ref <= 0 ? _i <= 0 : _i >= 0; i = _ref <= 0 ? ++_i : --_i) {
          line = cm.getLine(i);
          if (i === cur.line) {
            line = line.slice(0, cur.ch);
          }
          if (line.indexOf("//") >= 0 || line.indexOf("*/") >= 0) {
            break;
          }
          res = line.split(token);
          if (res.length > 1) {
            text = text === "" ? res[res.length - 1] : res[res.length - 1] + " " + text;
            break;
          }
          text = text === "" ? res[0] : res[0] + " " + text;
        }
        return text;
      }
    };
  })(function() {});

  Schema = (function() {
    var __super__;

    __super__ = function(schema) {
      this.initialize(schema);
      return this;
    };
    __super__.prototype = {
      schema: null,
      initialize: function(schema) {
        return this.schema = schema;
      },
      getTypes: function() {
        var config, result, type, _ref;

        result = [];
        _ref = this.schema.types;
        for (type in _ref) {
          config = _ref[type];
          result.push(type);
        }
        return result;
      },
      getVariablesByType: function(_type) {
        var results, type, v, vars, _ref;

        results = [];
        if (vars = (_ref = this.schema.types[_type]) != null ? _ref.vars : void 0) {
          for (v in vars) {
            type = vars[v];
            results.push(v);
          }
        }
        return results;
      },
      getType: function(_baseType, variables) {
        var i, type, v, vars, _i, _ref, _ref1, _ref2;

        vars = {};
        for (i = _i = 0, _ref = variables.length - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
          v = variables[i];
          if (i === 0) {
            vars = (_ref1 = this.schema.types[_baseType]) != null ? _ref1.vars : void 0;
          } else {
            type = vars[v];
            vars = (_ref2 = this.schema.types[type]) != null ? _ref2.vars : void 0;
          }
          if (vars == null) {
            return [];
          }
        }
        return vars;
      },
      getVariables: function(_baseType, variables) {
        var result, type, v, _ref;

        result = [];
        _ref = this.getType(_baseType, variables);
        for (v in _ref) {
          type = _ref[v];
          result.push(v);
        }
        return result;
      },
      getProperties: function() {
        var item, result, _i, _len, _ref, _ref1;

        result = [];
        _ref1 = (_ref = this.schema.properties) != null ? _ref : [];
        for (_i = 0, _len = _ref1.length; _i < _len; _i++) {
          item = _ref1[_i];
          result.push(":" + item);
        }
        return result;
      }
    };
    return __super__;
  })();

  Generator = (function() {
    var __super__;

    __super__ = function() {
      this.initialize();
      return this;
    };
    __super__.prototype = {
      joinRegExp: null,
      splitterAfterFrom: null,
      afterFromAutoComplete: [],
      initialize: function() {
        var str;

        this.joinRegExp = new RegExp(this.createAfrerFrom().join("|"));
        str = this.createAfrerFrom().join("|");
        str = str.replace(/\[ \]\+/g, " ").replace(/\[ \]\*/g, " ");
        this.afterFromAutoComplete = str.split("|");
        this.splitterAfterFrom = this.createAfrerFrom().join("|");
        return this;
      },
      parse: function(text) {
        var i, options, rAfter, rx, statement, str, strRx, token;

        options = {
          types: [],
          vars: [],
          mapping: {},
          mappingVar: {},
          dataPostFrom: [],
          original: text,
          tokens: {
            select: false,
            from: false,
            postFrom: false,
            joinFetch: false
          }
        };
        str = text.replace(/\n/, "");
        str = this.parseSelect(str, options);
        str = this.parseFrom(str, options);
        if (!(options.tokens.from || options.tokens.select)) {
          return options;
        }
        strRx = "(" + this.splitterAfterFrom + ")";
        rx = new RegExp(strRx);
        rAfter = str.split(rx);
        if (rAfter.length > 1) {
          i = 1;
          while (i < rAfter.length) {
            token = rAfter[i].trim();
            statement = rAfter[i + 1].trim();
            this.parsePostFrom(token, statement, options);
            i += 2;
          }
        }
        return options;
      },
      getHints: function(str, options, _schema) {
        var hints, item, lastIndex, res, s, schema, tokens, variables, variablesString, _i, _len, _ref;

        schema = new Schema(_schema);
        tokens = options.tokens;
        hints = [];
        lastIndex = str.length - 1;
        if (str.trim().length > 0) {
          if (str[lastIndex] === ".") {
            res = str.replace(/[ ]+/g, " ").split(/([ ]+|>=|<=|!=|>|<|=)/);
            variablesString = res[res.length - 1];
            variables = variablesString.split(".");
            variables = variables.slice(0, variables.length - 1);
            this.fillVariablesAutocomplete(hints, variables, schema, options);
            return hints;
          } else if ([" ", ",", "=", ">", "<"].indexOf(str[lastIndex]) < 0) {
            return hints;
          }
        }
        if (!tokens.select && !tokens.from) {
          hints = ["select", "from"];
        } else if (tokens.select && !tokens.from) {
          if (/select[ ]+(.+)(from|)/.test(str.trim())) {
            hints = ["from"];
          }
        } else if (tokens.from && !tokens.postFrom) {
          s = str.trim();
          if (options.types.length > 0 && (s[s.length - 1] !== "," && s.slice(s.length - 2, s.length) !== "as") && s.indexOf("from") < s.length - 4) {
            _ref = ["fetch", "inner", "left", "right", "join", "where", "order"].sort();
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
              item = _ref[_i];
              hints.push(item);
            }
          }
        }
        return this.fillHints(str, options, schema, hints);
      },
      parseSelect: function(str, options) {
        var item, rFrom, rSelect, tokens, vars, _i, _len;

        tokens = options.tokens;
        rSelect = /select[ ]+(.*)/.exec(str);
        if (rSelect) {
          tokens.select = true;
          str = rSelect[1];
          rFrom = /(.+)from(.*)/.exec(str);
          if (rFrom) {
            tokens.from = true;
            vars = rFrom[1].split(",");
            str = "from " + rFrom[2];
          } else {
            vars = str.split(",");
          }
          for (_i = 0, _len = vars.length; _i < _len; _i++) {
            item = vars[_i];
            item = item.trim();
            if (item === "") {
              continue;
            }
          }
        }
        return str;
      },
      parseFrom: function(str, options) {
        var fromParams, i, item, pairParam, pairParams, postFromBuildin, rFrom, res, tokens, _i, _j, _len, _ref;

        tokens = options.tokens;
        rFrom = /from(.*)/.exec(str);
        if (rFrom) {
          tokens.from = true;
          str = rFrom[1];
          res = str.replace(/[ ]+/, " ").split(" ");
          postFromBuildin = ["inner", "fetch", "left", "right", "join", "where", "order"];
          fromParams = "";
          str = "";
          i = 0;
          for (i = _i = 0, _ref = res.length - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
            item = res[i];
            if (postFromBuildin.indexOf(item) < 0) {
              fromParams += "" + item + " ";
            } else {
              break;
            }
          }
          str = res.splice(i, res.length).join(" ");
          pairParams = fromParams.trim().split(",");
          for (_j = 0, _len = pairParams.length; _j < _len; _j++) {
            pairParam = pairParams[_j];
            this.parseParams(pairParam, options);
          }
        }
        return str;
      },
      parseParams: function(strParam, options) {
        var mapping, res, sType, sVar;

        res = strParam.trim().split(/[ ]+|[ ]+as[ ]+/);
        mapping = options.mapping;
        if (res.length === 1) {
          sType = res[0].trim();
          sVar = null;
        } else if (res.length === 2) {
          sType = res[0];
          sVar = res[1];
        } else if (res.length === 3) {
          sType = res[0];
          sVar = res[2];
        } else {
          return;
        }
        if (!(sType === "" || sVar === "")) {
          this.uniquePush(options.types, sType);
          if (sVar) {
            this.uniquePush(options.vars, sVar);
          }
          if (mapping[sType] == null) {
            mapping[sType] = [];
          }
          if (sVar) {
            this.uniquePush(mapping[sType], sVar);
          }
          options.mappingVar[sVar] = sType;
        }
        return null;
      },
      parsePostFrom: function(token, statement, options) {
        if (token !== "") {
          options.tokens.postFrom = true;
          options.dataPostFrom.push({
            token: token,
            statement: statement
          });
          if (token.indexOf("join") >= 0 || token.indexOf("fetch") >= 0) {
            options.tokens.joinFetch = true;
            return this.parseParams(statement, options);
          }
        }
      },
      createAfrerFrom: function() {
        var fetch, i1, i2, i3, i4, item, join, outer, res, tuple, typejoin, _add, _i, _j, _k, _l, _len, _len1, _len2, _len3, _len4, _len5, _m, _n, _ref;

        res = [];
        typejoin = ["inner[ ]+", "left[ ]+", "right[ ]+"];
        outer = ["outer[ ]+", ""];
        fetch = ["fetch", "fetch[ ]+all", ""];
        join = ["join[ ]*"];
        _add = function(item) {
          if (item !== "" && res.indexOf(item) < 0) {
            return res.push(item);
          }
        };
        for (_i = 0, _len = typejoin.length; _i < _len; _i++) {
          i1 = typejoin[_i];
          for (_j = 0, _len1 = outer.length; _j < _len1; _j++) {
            i2 = outer[_j];
            for (_k = 0, _len2 = join.length; _k < _len2; _k++) {
              i3 = join[_k];
              for (_l = 0, _len3 = fetch.length; _l < _len3; _l++) {
                i4 = fetch[_l];
                _add("" + i1 + i2 + i3 + i4);
              }
            }
          }
        }
        _ref = [typejoin, outer, fetch, join];
        for (_m = 0, _len4 = _ref.length; _m < _len4; _m++) {
          tuple = _ref[_m];
          for (_n = 0, _len5 = tuple.length; _n < _len5; _n++) {
            item = tuple[_n];
            _add(item);
          }
        }
        res.push("with");
        res.push("where");
        res.push("order");
        res.push("order[ ]+by");
        res.push("group");
        return res;
      },
      uniquePush: function(arr, val) {
        if (__indexOf.call(arr, val) < 0) {
          return arr.push(val.trim());
        }
      },
      fillVariablesAutocomplete: function(hints, variables, schema, options) {
        var data, firstVar, item, type, _i, _len;

        firstVar = variables[0];
        type = options.mappingVar[firstVar];
        data = schema.getVariables(type, variables);
        for (_i = 0, _len = data.length; _i < _len; _i++) {
          item = data[_i];
          hints.push(item);
        }
        return this;
      },
      fillHints: function(str, options, schema, hints) {
        var bFill, dataPF, lastData, s, statement, statements, token, tokens, type, _i, _len, _ref;

        tokens = options.tokens;
        if (tokens.select && !tokens.from) {
          if (statements = /select (.*)/.exec(str)) {
            s = statements[1].trim();
            if (s === "") {
              hints.push("distinct");
            }
          }
        } else if (tokens.from && !tokens.postFrom) {
          bFill = false;
          statements = /from (.*)/.exec(str);
          if (statements != null) {
            s = statements[1];
            if (s.length === 0) {
              bFill = true;
            } else if (s[s.length - 1] === " ") {
              s = s.trim();
              if (s === "") {
                bFill = true;
              } else if (s[s.length - 1] === ",") {
                bFill = true;
              }
            } else if (s[s.length - 1] === ",") {
              bFill = true;
            }
          }
          if (bFill) {
            _ref = schema.getTypes();
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
              type = _ref[_i];
              hints.push(type);
            }
          }
        } else if (tokens.postFrom) {
          dataPF = options.dataPostFrom;
          lastData = dataPF[dataPF.length - 1];
          token = lastData.token;
          statement = lastData.statement;
          s = statement.trim();
          this.checkWhere(hints, token, statement, s, options, schema) || this.checkOrder(hints, token, statement, s, options, schema) || this.checkInnerLeftRigth(hints, token, statement, s, options, schema) || this.checkJoinFetch(hints, token, statement, s, options, schema) || this.checkOuter(hints, token, statement, s, options, schema);
        }
        return hints;
      },
      checkOuter: function(hints, token, statement, s, options, schema) {
        var lastToken, spToken;

        spToken = token.split(" ");
        lastToken = spToken[spToken.length - 1].trim();
        if (lastToken !== "outer") {
          return false;
        }
        if (s === "") {
          hints.push("join");
        }
        return true;
      },
      checkJoinFetch: function(hints, token, statement, s, options, schema) {
        var item, lastToken, spToken, _i, _len, _ref;

        spToken = token.split(" ");
        lastToken = spToken[spToken.length - 1].trim();
        if (!(["join", "fetch"].indexOf(lastToken) >= 0)) {
          return false;
        }
        if (s === "") {
          if (lastToken === "join") {
            hints.push("fetch");
            this.pushLocalVars(hints, options, schema);
          } else if (lastToken === "fetch") {
            hints.push("all");
          }
        } else {
          _ref = ["fetch", "inner", "join", "right", "left", "order", "where", "as", "group"].sort();
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            item = _ref[_i];
            hints.push(item);
          }
        }
        return true;
      },
      checkInnerLeftRigth: function(hints, token, statement, s, options, schema) {
        var lastToken, spToken;

        spToken = token.split(" ");
        lastToken = spToken[spToken.length - 1].trim();
        if (["inner", "left", "right"].indexOf(lastToken) < 0) {
          return false;
        }
        if (s === "") {
          hints.push("join");
          hints.push("outer");
        }
        return true;
      },
      fillSchemaProperties: function(hints, schema) {
        var item, _i, _len, _ref, _results;

        _ref = schema.getProperties();
        _results = [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          item = _ref[_i];
          _results.push(hints.push(item));
        }
        return _results;
      },
      checkWhere: function(hints, token, statement, s, options, schema) {
        var lastTks, tks, _i, _j, _len, _tks;

        if (token !== "where") {
          return false;
        }
        _tks = s.replace(/(>=|<=|!=|=|<|>)/g, " $1 ").replace(/[ ]+/g, " ").split(" ");
        tks = [];
        for (_j = 0, _len = _tks.length; _j < _len; _j++) {
          _i = _tks[_j];
          _i = _i.trim();
          if (_i !== "") {
            tks.push(_i);
          }
        }
        if (tks.length === 0) {
          this.pushLocalVars(hints, options, schema);
          return true;
        }
        lastTks = tks[tks.length - 1];
        if (["and", "or", "like", "in", "exist", "=", ">=", "<=", "!=", "=", "<", ">"].indexOf(lastTks) >= 0) {
          this.pushLocalVars(hints, options, schema);
          if (["like", "in", "exist", "=", ">=", "<=", "!=", "=", "<", ">"].indexOf(lastTks) >= 0) {
            this.fillSchemaProperties(hints, schema);
          }
        } else if (["=", ">=", "<=", "!=", "=", "<", ">", "in"].indexOf(tks[tks.length - 2]) >= 0) {
          hints.push("and");
          hints.push("or");
          hints.push("order");
        } else if (["", "and", "or"].indexOf(tks[tks.length - 2])) {
          hints.push("like");
          hints.push("exist");
          hints.push("in");
        }
        return true;
      },
      checkOrder: function(hints, token, statement, s, options, schema) {
        var lastTks, tks;

        if (token !== "order") {
          return false;
        }
        if (s.indexOf("by") === 0) {
          tks = statement.split("by");
          lastTks = tks[1].trim();
          if (lastTks === "" || lastTks[lastTks.length - 1] === ",") {
            this.pushLocalVars(hints, options, schema);
          }
        } else {
          hints.push("by");
        }
        return true;
      },
      pushLocalVars: function(hints, options, schema) {
        var item, _i, _j, _k, _len, _len1, _len2, _ref, _ref1, _ref2, _type;

        if (options.vars.length > 0) {
          _ref = options.vars;
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            item = _ref[_i];
            hints.push(item);
          }
        } else {
          _ref1 = options.types.sort();
          for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
            _type = _ref1[_j];
            _ref2 = schema.getVariablesByType(_type);
            for (_k = 0, _len2 = _ref2.length; _k < _len2; _k++) {
              item = _ref2[_k];
              hints.push(item);
            }
          }
        }
        return this;
      }
    };
    return __super__;
  })();

  CodeMirror.hqlHint = [];

  window.gen = new Generator();

  CodeMirror.hqlHint = function(cm, opt) {
    var cur, hints, options, text;

    cur = cm.getCursor();
    text = Service.getCodeAfterToken(cm, cur, ";");
    options = gen.parse(text);
    hints = gen.getHints(text, options, opt.schemaInfo);
    return {
      list: hints,
      from: cur,
      to: cur
    };
  };

}).call(this);

/*
//@ sourceMappingURL=hql-hint.map
*/
