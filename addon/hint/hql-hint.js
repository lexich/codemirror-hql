// Generated by CoffeeScript 1.6.2
(function() {
  var Generator, Schema, Service, _Gen,
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
        rSelect = /(select[ ]+distinct|select)[ ]+(.*)/.exec(str);
        if (rSelect) {
          str = rSelect[2];
          tokens.select = true;
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

  _Gen = function() {
    this.initialize();
    return this;
  };

  _Gen.prototype = {
    BLOCKS: ["select", "from", "where", "order", "inner", "left", "right", "fetch", "with", "group"],
    initialize: function() {},
    _parseToken: function(token, ctx, i, tokens) {
      var block, collectionExpr, config, history;

      block = ctx.block;
      history = ctx.history;
      config = ctx.config;
      if (this.BLOCKS.indexOf(token) >= 0) {
        history.body.push(block);
        block = ctx.block = {
          name: token,
          counter: -1,
          body: []
        };
      }
      block.counter += 1;
      block.body.push(token);
      if (token[token.length - 1] === ".") {
        return ctx.hints = {
          call: "get_hints_autocomplete",
          args: token
        };
      } else if (block.name === "select") {
        if (block.counter === 0) {
          block.canAddExtract = true;
          return ctx.hints = ["distinct"];
        } else if (token === "distinct") {
          return ctx.hints = [];
        } else if (token === ",") {
          if (block.canAddExtract = false) {
            block.canAddExtract = true;
            return ctx.hints = "get_hints_extract";
          } else {
            throw "Irregular select block";
          }
        } else if (block.canAddExtract) {
          config.extract.push(token);
          block.canAddExtract = false;
          return ctx.hints = ["from", ","];
        }
      } else if (block.name === "from") {
        if (block.counter === 0) {
          block.canAddType = true;
          block.canAddVars = false;
          return ctx.hints = "get_hints_types";
        } else if (block.canAddType) {
          config.types.push(token);
          block.canAddType = false;
          block.canAddVars = true;
          return ctx.hints = ["as", "fetch", "inner", "left", "right", "join", "where", "order"];
        } else if (block.canAddVars) {
          if (token === ",") {
            block.canAddType = true;
            block.canAddVars = false;
            return ctx.hints = "get_hints_types";
          } else if (token === "as") {
            block.canAddVars = true;
            block.canAddType = false;
            return ctx.hints = [];
          } else {
            config.vars.push(token);
            config.mappingVar[token] = config.types[config.types.length - 1];
            return ctx.hints = ["fetch", "inner", "left", "right", "join", "where", "order"];
          }
        }
      } else if (block.name === "where") {
        collectionExpr = ["size", "maxelement", "maxindex", "minelement", "minindex", "elements", "indices"];
        if (block.counter === 0) {
          block.canAddFirstVal = true;
          block.canAddSigh = false;
          block.canAddSecondVal = false;
          block.openBracket = false;
          if (ctx.config.vars.length > 0) {
            return ctx.hints = {
              call: "get_hints_vars",
              add: collectionExpr
            };
          } else {
            return ctx.hints = {
              call: "get_hints_extract",
              add: collectionExpr
            };
          }
        } else if (block.canAddFirstVal) {
          if (collectionExpr.indexOf(token) >= 0) {
            return ctx.hints = ["("];
          } else if (token === "(") {
            block.openBracket = true;
            return ctx.hints = {
              call: "get_hints_vars"
            };
          } else if (token === ")") {
            block.openBracket = false;
            block.canAddFirstVal = false;
            block.canAddSigh = true;
            return ctx.hints = [">", "<", "=", "!=", ">=", "<=", "exist", "in", "like"];
          } else if (block.openBracket) {
            return ctx.hints = [")"];
          } else {
            block.canAddFirstVal = false;
            block.canAddSigh = true;
            return ctx.hints = [">", "<", "=", "!=", ">=", "<=", "exist", "in", "like"];
          }
        } else if (block.canAddSigh) {
          block.canAddSigh = false;
          block.canAddSecondVal = true;
          return ctx.hints = {
            call: "get_hints_vars_and_properties",
            add: collectionExpr
          };
        } else if (block.canAddSecondVal) {
          if (collectionExpr.indexOf(token) >= 0) {
            return ctx.hints = ["("];
          } else if (token === "(") {
            block.openBracket = true;
            return ctx.hints = {
              call: "get_hints_vars"
            };
          } else if (token === ")") {
            block.openBracket = false;
            block.canAddSecondVal = false;
            return ctx.hints = ["and", "or", "order"];
          } else if (block.openBracket) {
            return ctx.hints = [")"];
          } else {
            block.canAddSecondVal = false;
            return ctx.hints = ["and", "or", "order"];
          }
        } else if (["and", "or"].indexOf(token) >= 0) {
          block.canAddFirstVal = true;
          return ctx.hints = "get_hints_vars";
        }
      } else if (block.name === "order") {
        if (block.counter === 0) {
          block.canAddVars = true;
          return ctx.hints = ["by"];
        } else if (block.counter === 1) {
          if (token === "by") {
            return ctx.hints = "get_hints_vars";
          } else {
            throw "irregular order token";
          }
        } else {
          if (block.canAddVars) {
            if (token === ",") {
              throw "Irregular order block";
            }
            block.canAddVars = false;
            return ctx.hints = [","];
          } else {
            block.canAddVars = true;
            return ctx.hints = "get_hints_vars";
          }
        }
      } else if (block.name === "fetch") {
        if (block.counter === 0) {
          block.canAddSubType = true;
          block.canAddAlias = false;
          return ctx.hints = {
            call: "get_hints_vars",
            add: ["all"]
          };
        } else {
          if (block.canAddSubType) {
            block.canAddSubType = false;
            block.canAddAlias = true;
          } else if (block.canAddAlias) {
            block.canAddAlias = true;
            this.addAlias(ctx, token, block.body[block.body.length - 2]);
          }
          return ctx.hints = ["fetch", "inner", "left", "right", "join", "where", "order", "as", "group"];
        }
      } else if (["inner", "left", "right"].indexOf(block.name) >= 0) {
        if (block.counter === 0) {
          if (token === "inner") {
            return ctx.hints = ["join"];
          } else {
            return ctx.hints = ["join", "outer"];
          }
        } else if (token === "outer") {
          return ctx.hints = ["join"];
        } else if (token === "join") {
          block.canAddSubType = true;
          block.canAddAlias = false;
          return ctx.hints = {
            call: "get_hints_vars",
            add: ["fetch"]
          };
        } else if (block.canAddSubType) {
          block.canAddSubType = false;
          block.canAddAlias = true;
          return ctx.hints = ["fetch", "inner", "left", "right", "join", "where", "order", "group", "as", "with"];
        } else if (block.canAddAlias) {
          block.canAddAlias = false;
          this.addAlias(ctx, token, block.body[block.body.length - 2]);
          return ctx.hints = ["fetch", "inner", "left", "right", "join", "where", "order", "group", "with"];
        }
      } else if (block.name === "with") {
        if (block.counter === 0) {
          block.canAddFirstVar = true;
          block.canAddAlias = false;
          return ctx.hints = "get_hints_vars";
        } else if (block.canAddFirstVar) {
          block.canAddFirstVar = false;
          block.canAddAlias = true;
          return ctx.hints = [];
        } else if (block.canAddAlias) {
          block.canAddAlias = false;
          this.addAlias(ctx, token, block.body[block.body.length - 2]);
          return ctx.hints = ["fetch", "inner", "left", "right", "join", "where", "order", "group"];
        }
      } else if (block.name === "group") {
        if (block.counter === 0) {
          ctx.hints = ["by"];
          return block.canAddVars = false;
        } else if (block.counter === 1) {
          if (token === "by") {
            block.canAddVars = true;
            return ctx.hints = "get_hints_vars";
          } else {
            throw "Irregular group block";
          }
        } else if (block.canAddVars) {
          block.canAddVars = false;
          return ctx.hints = ["fetch", "inner", "left", "right", "join", "where", "order", "group", ","];
        } else if (token === ",") {
          block.canAddVars = true;
          return ctx.hints = "get_hints_vars";
        }
      }
    },
    parse: function(_str) {
      var ctx, i, lastCh, str, token, tokens, _i, _ref;

      str = _str.replace(/[ ]+/g, " ");
      str = str.replace(/(,|>=|<=|>|<|!=|=|\(|\))/g, " $1 ");
      tokens = str.split(" ");
      ctx = {
        str: _str,
        block: {
          name: "",
          counter: 0,
          body: []
        },
        history: {
          body: []
        },
        config: {
          extract: [],
          vars: [],
          types: [],
          mappingVar: {},
          alias: {}
        },
        hints: ["select", "from"]
      };
      if (_str.length > 0) {
        lastCh = _str[_str.length - 1];
        if (!([" ", ",", "=", ">", "<", ".", "("].indexOf(lastCh) >= 0)) {
          ctx.hints = [];
          return ctx;
        }
      }
      for (i = _i = 0, _ref = tokens.length - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
        token = tokens[i].trim();
        if (token === "") {
          continue;
        }
        this._parseToken(token, ctx, i, tokens);
      }
      return ctx;
    },
    get_hints_autocomplete: function(ctx, schema, args) {
      var firstToken, i, item, variablePath, variables, _baseType, _i, _j, _len, _ref, _vars;

      variables = args.split(".");
      variables = variables.slice(0, variables.length - 1);
      firstToken = variables[0];
      variablePath = this.findFullPath(ctx, firstToken);
      _vars = [];
      for (_i = 0, _len = variablePath.length; _i < _len; _i++) {
        item = variablePath[_i];
        _vars.push(item);
      }
      for (i = _j = 0, _ref = variables.length - 1; 0 <= _ref ? _j <= _ref : _j >= _ref; i = 0 <= _ref ? ++_j : --_j) {
        _vars.push(variables[i]);
      }
      _baseType = ctx.config.mappingVar[_vars[0]];
      return schema.getVariables(_baseType, _vars);
    },
    get_hints_extract: function(ctx, schema) {
      var i, result, t, vars, _i, _j, _len, _len1, _ref;

      result = [];
      _ref = ctx.config.types;
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        t = _ref[_i];
        vars = schema.getVariablesByType(t);
        for (_j = 0, _len1 = vars.length; _j < _len1; _j++) {
          i = vars[_j];
          result.push(i);
        }
      }
      return result;
    },
    get_hints_types: function(ctx, schema) {
      return schema.getTypes();
    },
    get_hints_vars: function(ctx, schema) {
      return ctx.config.vars;
    },
    get_hints_vars_and_properties: function(ctx, schema) {
      var i, result, _i, _j, _len, _len1, _ref, _ref1;

      result = [];
      _ref = this.get_hints_vars(ctx, schema);
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        i = _ref[_i];
        result.push(i);
      }
      _ref1 = schema.getProperties();
      for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
        i = _ref1[_j];
        result.push(i);
      }
      return result;
    },
    findFullPath: function(ctx, name) {
      var alias, i, item, path, result, tkns, tmp, _i, _len, _ref;

      alias = ctx.config.alias;
      path = name;
      result = [];
      while (true) {
        tmp = alias[path];
        if (tmp) {
          tkns = tmp.trim().split(".");
          _ref = [tkns.length - 1, 0];
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            i = _ref[_i];
            item = tkns[i].trim();
            if (item === "") {
              continue;
            }
            result.push(tkns[i]);
          }
          path = tkns[0];
        } else {
          break;
        }
      }
      return result.reverse();
    },
    addAlias: function(ctx, alias, statement) {
      ctx.config.alias[alias] = statement;
      return ctx.config.vars.push(alias);
    },
    exec: function(str, ctx, schema, args) {
      var call;

      if (call = this[str]) {
        return call.call(this, ctx, schema, args);
      } else {
        return [];
      }
    },
    getHints: function(str, ctx, _schema) {
      var data, hints, i, schema, _hints, _i, _j, _k, _l, _len, _len1, _len2, _len3, _ref;

      hints = [];
      _hints = ctx.hints;
      if (Object.prototype.toString.call(_hints) === '[object Array]') {
        for (_i = 0, _len = _hints.length; _i < _len; _i++) {
          i = _hints[_i];
          hints.push(i);
        }
      } else if (Object.prototype.toString.call(_hints) === '[object String]') {
        schema = new Schema(_schema);
        if (data = this.exec(_hints, ctx, schema, [])) {
          for (_j = 0, _len1 = data.length; _j < _len1; _j++) {
            i = data[_j];
            hints.push(i);
          }
        }
      } else if (_hints === Object(_hints)) {
        if (_hints.add != null) {
          _ref = _hints.add;
          for (_k = 0, _len2 = _ref.length; _k < _len2; _k++) {
            i = _ref[_k];
            hints.push(i);
          }
        }
        schema = new Schema(_schema);
        if (data = this.exec(_hints.call, ctx, schema, _hints.args)) {
          for (_l = 0, _len3 = data.length; _l < _len3; _l++) {
            i = data[_l];
            hints.push(i);
          }
        }
      }
      return hints.sort();
    }
  };

  window.gen = new _Gen();

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
