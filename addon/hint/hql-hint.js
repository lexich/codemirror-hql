// Generated by CoffeeScript 1.6.2
(function() {
  var Schema, Service, _Gen;

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

  CodeMirror.hqlHint = [];

  _Gen = function() {
    this.initialize();
    return this;
  };

  _Gen.prototype = {
    BLOCKS: ["select", "from", "where", "order", "inner", "left", "right", "fetch", "with", "group"],
    collectionExpr: ["size", "maxelement", "maxindex", "minelement", "minindex", "elements", "indices"],
    collectionSigh: [">", "<", "=", "!=", ">=", "<=", "exist", "in", "like"],
    collectionPostFrom: ["fetch", "inner", "left", "right", "join", "where", "order", "group", "with"],
    collectionAgregate: ["count", "avg", "min", "max", "sum"],
    initialize: function() {},
    _addHints: function(ctx, val) {
      return ctx.hints = val;
    },
    _parseToken: function(token, ctx, i, tokens) {
      var block, config, history;

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
          block.openBracket = false;
          return this._addHints(ctx, {
            call: "get_hints_extract",
            add: ["distinct", "*"].concat(this.collectionAgregate)
          });
        } else if (this.collectionAgregate.indexOf(token) >= 0) {
          return this._addHints(ctx, ["("]);
        } else if (token === "(") {
          block.openBracket = true;
          return this._addHints(ctx, {
            call: "get_hints_extract",
            add: ["*"]
          });
        } else if (block.openBracket) {
          return this._addHints(ctx, [")"]);
        } else if (token === ")") {
          block.openBracket = false;
          return this._addHints(ctx, [",", "from"]);
        } else if (token === "distinct") {
          return this._addHints(ctx, []);
        } else if (token === ",") {
          if (block.canAddExtract = false) {
            block.canAddExtract = true;
            return this._addHints(ctx, {
              call: "get_hints_extract",
              add: ["*"]
            });
          } else {
            throw "Irregular select block";
          }
        } else if (block.canAddExtract) {
          config.extract.push(token);
          block.canAddExtract = false;
          return this._addHints(ctx, ["from", ","]);
        }
      } else if (block.name === "from") {
        if (block.counter === 0) {
          block.canAddType = true;
          block.canAddVars = false;
          return this._addHints(ctx, "get_hints_types");
        } else if (block.canAddType) {
          config.types.push(token);
          block.canAddType = false;
          block.canAddVars = true;
          return this._addHints(ctx, ["as"].concat(this.collectionPostFrom));
        } else if (block.canAddVars) {
          if (token === ",") {
            block.canAddType = true;
            block.canAddVars = false;
            return this._addHints(ctx, "get_hints_types");
          } else if (token === "as") {
            block.canAddVars = true;
            block.canAddType = false;
            return this._addHints(ctx, []);
          } else {
            config.vars.push(token);
            config.mappingVar[token] = config.types[config.types.length - 1];
            return this._addHints(ctx, [].concat(this.collectionPostFrom));
          }
        }
      } else if (block.name === "where") {
        if (block.counter === 0) {
          block.canAddFirstVal = true;
          block.canAddSigh = false;
          block.canAddSecondVal = false;
          block.openBracket = false;
          if (ctx.config.vars.length > 0) {
            return this._addHints(ctx, {
              call: "get_hints_vars",
              add: this.collectionExpr
            });
          } else {
            return this._addHints(ctx, {
              call: "get_hints_extract",
              add: this.collectionExpr
            });
          }
        } else if (block.canAddFirstVal || block.canAddSecondVal) {
          if (this.collectionExpr.indexOf(token) >= 0) {
            return this._addHints(ctx, ["("]);
          } else if (token === "(") {
            block.openBracket = true;
            return this._addHints(ctx, {
              call: "get_hints_vars_and_properties"
            });
          } else if (token === ")") {
            block.openBracket = false;
            block.canAddFirstVal = false;
            if (block.canAddFirstVal) {
              block.canAddSigh = true;
              return this._addHints(ctx, [].concat(this.collectionSigh));
            } else if (block.canAddSecondVal) {
              return this._addHints(ctx, ["and", "or", "order"]);
            }
          } else if (block.openBracket) {
            return this._addHints(ctx, [")"]);
          } else {
            if (block.canAddFirstVal) {
              block.canAddFirstVal = false;
              block.canAddSigh = true;
              return this._addHints(ctx, [].concat(this.collectionSigh));
            } else if (block.canAddSecondVal) {
              block.canAddSecondVal = false;
              return this._addHints(ctx, ["and", "or", "order"]);
            }
          }
        } else if (block.canAddSigh) {
          block.canAddSigh = false;
          block.canAddSecondVal = true;
          return this._addHints(ctx, {
            call: "get_hints_vars_and_properties",
            add: this.collectionExpr
          });
        } else if (["and", "or"].indexOf(token) >= 0) {
          block.canAddFirstVal = true;
          return this._addHints(ctx, "get_hints_vars");
        }
      } else if (block.name === "order") {
        if (block.counter === 0) {
          block.canAddVars = true;
          return this._addHints(ctx, ["by"]);
        } else if (block.counter === 1) {
          if (token === "by") {
            return this._addHints(ctx, "get_hints_vars");
          } else {
            throw "irregular order token";
          }
        } else {
          if (block.canAddVars) {
            if (token === ",") {
              throw "Irregular order block";
            }
            block.canAddVars = false;
            return this._addHints(ctx, [","]);
          } else {
            block.canAddVars = true;
            return this._addHints(ctx, "get_hints_vars");
          }
        }
      } else if (block.name === "fetch") {
        if (block.counter === 0) {
          block.canAddSubType = true;
          block.canAddAlias = false;
          return this._addHints(ctx, {
            call: "get_hints_vars",
            add: ["all"]
          });
        } else {
          if (block.canAddSubType) {
            block.canAddSubType = false;
            block.canAddAlias = true;
          } else if (block.canAddAlias) {
            block.canAddAlias = true;
            this.addAlias(ctx, token, block.body[block.body.length - 2]);
          }
          return this._addHints(ctx, ["fetch", "inner", "left", "right", "join", "where", "order", "as", "group"]);
        }
      } else if (["inner", "left", "right"].indexOf(block.name) >= 0) {
        if (block.counter === 0) {
          if (token === "inner") {
            return this._addHints(ctx, ["join"]);
          } else {
            return this._addHints(ctx, ["join", "outer"]);
          }
        } else if (token === "outer") {
          return this._addHints(ctx, ["join"]);
        } else if (token === "join") {
          block.canAddSubType = true;
          block.canAddAlias = false;
          return this._addHints(ctx, {
            call: "get_hints_vars",
            add: ["fetch"]
          });
        } else if (block.canAddSubType) {
          block.canAddSubType = false;
          block.canAddAlias = true;
          return this._addHints(ctx, ["as"].concat(this.collectionPostFrom));
        } else if (block.canAddAlias) {
          block.canAddAlias = false;
          this.addAlias(ctx, token, block.body[block.body.length - 2]);
          return this._addHints(ctx, [].concat(this.collectionPostFrom));
        }
      } else if (block.name === "with") {
        if (block.counter === 0) {
          block.canAddFirstVar = true;
          block.canAddAlias = false;
          return this._addHints(ctx, "get_hints_vars");
        } else if (block.canAddFirstVar) {
          block.canAddFirstVar = false;
          block.canAddAlias = true;
          return this._addHints(ctx, []);
        } else if (block.canAddAlias) {
          block.canAddAlias = false;
          this.addAlias(ctx, token, block.body[block.body.length - 2]);
          return this._addHints(ctx, [].concat(this.collectionPostFrom));
        }
      } else if (block.name === "group") {
        if (block.counter === 0) {
          this._addHints(ctx, ["by"]);
          return block.canAddVars = false;
        } else if (block.counter === 1) {
          if (token === "by") {
            block.canAddVars = true;
            return this._addHints(ctx, "get_hints_vars");
          } else {
            throw "Irregular group block";
          }
        } else if (block.canAddVars) {
          block.canAddVars = false;
          return this._addHints(ctx, ["fetch", "inner", "left", "right", "join", "where", "order", "group", ","]);
        } else if (token === ",") {
          block.canAddVars = true;
          return this._addHints(ctx, "get_hints_vars");
        }
      }
    },
    parse: function(_str) {
      var ctx, filter, hint, hints, i, lastCh, str, token, tokens, _i, _j, _len, _ref, _ref1;

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
      filter = null;
      if (_str.length > 0) {
        lastCh = _str[_str.length - 1];
        if (!([" ", ",", "=", ">", "<", ".", "("].indexOf(lastCh) >= 0)) {
          filter = tokens[tokens.length - 1].trim();
          if ([].concat(this.collectionAgregate, this.collectionExpr).indexOf(filter) >= 0) {
            ctx.hints = ["("];
            return ctx;
          }
        }
      }
      for (i = _i = 0, _ref = tokens.length - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
        token = tokens[i].trim();
        if (token === "") {
          continue;
        }
        this._parseToken(token, ctx, i, tokens);
      }
      if (filter != null) {
        hints = [];
        _ref1 = ctx.hints;
        for (_j = 0, _len = _ref1.length; _j < _len; _j++) {
          hint = _ref1[_j];
          if (hint.indexOf(filter) === 0) {
            hints.push(hint);
          }
        }
        ctx.hints = hints;
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
      var item, result, type, variables, _i, _j, _len, _len1, _ref;

      result = [];
      if (ctx.config.vars.length > 0) {
        result = ctx.config.vars;
      } else {
        _ref = ctx.config.types;
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          type = _ref[_i];
          variables = schema.getVariablesByType(type);
          for (_j = 0, _len1 = variables.length; _j < _len1; _j++) {
            item = variables[_j];
            result.push(item);
          }
        }
      }
      return result;
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
