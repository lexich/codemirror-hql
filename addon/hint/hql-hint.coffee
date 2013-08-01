Service = do(__super__=->)->
  __super__::=
    getCodeAfterToken: (cm, cur, token)->

      text = ""
      for i  in [cur.line..0]
        line = cm.getLine(i)
        line = line.slice(0,cur.ch) if i is cur.line
        break if line.indexOf("//") >= 0 or line.indexOf("*/") >= 0

        res = line.split(token)
        if res.length > 1
          text = if text is "" then res[res.length-1] else res[res.length-1] + " " + text
          break

        text = if text is "" then res[0] else res[0] + " " + text
      text

Schema = do ->
  __super__ = (schema)->
    this.initialize schema
    this

  __super__::=
    schema:null
    initialize:(schema)->
      @schema = schema

    getTypes:->
      result = []
      for type, config of @schema.types
        result.push type
      result

    getVariablesByType:(_type)->
      results = []
      if vars = @schema.types[_type]?.vars
        for v, type of vars
          results.push v
      results

    getType:(_baseType, variables)->
      vars = {}
      for i in [0..variables.length-1]
        v = variables[i]
        if i is 0
          vars = @schema.types[_baseType]?.vars
        else
          type = vars[v]
          vars = @schema.types[type]?.vars
        return [] unless vars?
      vars

    getVariables:(_baseType, variables)->
      result = []
      for v, type of @getType(_baseType, variables)
        result.push v
      result

    getProperties:()->
      result = []
      for item in @schema.properties ? []
        result.push ":#{item}"
      result

  __super__


CodeMirror.hqlHint = []

_Gen =->
  this.initialize()
  this

_Gen::=
  BLOCKS:[
    "select"
    "from"
    "where"
    "order"
    "inner"
    "left"
    "right"
    "fetch"
    "with"
    "group"
  ]
  collectionExpr: ["size","maxelement","maxindex","minelement", "minindex","elements","indices"]
  collectionSigh: [">","<","=","!=",">=","<=", "exist","in", "like"]
  collectionPostFrom: ["fetch","inner","left","right", "join", "where", "order", "group", "with"]
  collectionAgregate: ["count","avg", "min", "max", "sum"]
  collectionQualifiedPath:["value","index","key","entry"]
  collectionFunctions:['concat', 'substring', 'upper', 'lower', 'trim', 'length', 'locate', 'abs', 'mod', 'sqrt', 'current_date', 'current_time', 'current_timestamp']
  collectionType:['bit_length', 'cast', 'extract', 'second', 'minute', 'hour', 'day', 'month', 'year', 'str']
  initialize:->

  _addHints:(ctx, val)->
    ctx.hints = val

  _parseToken:(token, ctx, i, tokens)->
    block = ctx.block
    history = ctx.history
    config = ctx.config
    if @BLOCKS.indexOf(token) >= 0
      history.body.push block
      block = ctx.block = {name: token, counter: -1, body:[]}

    block.counter += 1
    block.body.push token
    ##############################################################################
    # dot autocomplete
    ##############################################################################
    if token[token.length-1] is "."
      ctx.hints = call:"get_hints_autocomplete", args:token


    ##############################################################################
    # select
    ##############################################################################
    else if block.name is "select"
      if block.counter is 0
        block.canAddExtract = true
        block.openBracket = false
        @_addHints ctx, call:"get_hints_extract", add:["distinct", "new", "*"].concat(@collectionAgregate,@collectionQualifiedPath, @collectionFunctions, @collectionType)
      else if @collectionAgregate.indexOf(token) >= 0
        @_addHints ctx, ["("]
      else if token is "("
        block.openBracket = true
        @_addHints ctx, call:"get_hints_extract", add:["*"]
      else if token is ")"
        block.openBracket = false
        @_addHints ctx, [",","from"]
      else if block.openBracket
        @_addHints ctx, [")"]
      else if token in ["distinct","new"]
        @_addHints ctx, []
      else if token is ","
        block.canAddExtract = true
        @_addHints ctx, call:"get_hints_extract", add:["*"].concat(@collectionAgregate, @collectionQualifiedPath, @collectionFunctions, @collectionType)

      else if block.canAddExtract
        config.extract.push token
        block.canAddExtract = false
        @_addHints ctx, ["from", ","]

    ##############################################################################
    # from
    ##############################################################################
    else if block.name is "from"
      if block.counter is 0
        block.canAddType = true
        block.canAddVars = false
        @_addHints ctx, "get_hints_types"

      else if block.canAddType
        config.types.push token
        block.canAddType = false
        block.canAddVars = true
        @_addHints ctx, ["as"].concat(@collectionPostFrom)
      else if block.canAddVars
        if token is ","
          block.canAddType = true
          block.canAddVars = false
          @_addHints ctx, "get_hints_types"
        else if token is "as"
          block.canAddVars = true
          block.canAddType = false
          @_addHints ctx, []
        else
          config.vars.push token
          config.mappingVar[token] = config.types[config.types.length-1]
          @_addHints ctx, [].concat(@collectionPostFrom)

    ##############################################################################
    # where
    ##############################################################################
    else if block.name is "where"

      if block.counter is 0
        block.canAddFirstVal = true
        block.canAddSigh = false
        block.canAddSecondVal = false
        block.openBracket = false
        if ctx.config.vars.length > 0
          @_addHints ctx, call:"get_hints_vars", add:[].concat(@collectionExpr,@collectionQualifiedPath, @collectionFunctions, @collectionType)
        else
          @_addHints ctx, call:"get_hints_extract", add: [].concat(@collectionExpr, @collectionQualifiedPath, @collectionFunctions, @collectionType)
      else if block.canAddFirstVal or block.canAddSecondVal
        if @collectionExpr.indexOf(token) >= 0
          @_addHints ctx, ["("]
        else if token is "("
          block.openBracket = true
          @_addHints ctx, call:"get_hints_vars_and_properties"
        else if token is ")"
          block.openBracket = false
          block.canAddFirstVal = false
          if block.canAddFirstVal
            block.canAddSigh = true
            @_addHints ctx, [].concat(@collectionSigh)
          else if block.canAddSecondVal
            @_addHints ctx, ["and","or", "order"]
        else if block.openBracket
          @_addHints ctx, [")"]
        else
          if block.canAddFirstVal
            block.canAddFirstVal = false
            block.canAddSigh = true
            @_addHints ctx, [].concat(@collectionSigh)
          else if block.canAddSecondVal
            block.canAddSecondVal = false
            @_addHints ctx, ["and","or", "order"]


      else if block.canAddSigh
        block.canAddSigh = false
        block.canAddSecondVal = true
        @_addHints ctx, call:"get_hints_vars_and_properties", add:@collectionExpr

      else if ["and","or"].indexOf(token) >= 0
        block.canAddFirstVal = true
        @_addHints ctx, "get_hints_vars"


    ##############################################################################
    # order
    ##############################################################################
    else if block.name is "order"
      if block.counter is 0
        block.canAddVars = true
        @_addHints ctx, ["by"]
      else if block.counter is 1
        if token is "by"
          @_addHints ctx, "get_hints_vars"
        else
          throw "irregular order token"
      else
        if block.canAddVars
          throw "Irregular order block" if token is ","
          block.canAddVars = false
          @_addHints ctx, [","]

        else
          block.canAddVars = true
          @_addHints ctx, "get_hints_vars"

    ##############################################################################
    # fetch
    ##############################################################################
    else if block.name is "fetch"
      if block.counter is 0
        block.canAddSubType = true
        block.canAddAlias = false
        @_addHints ctx, call: "get_hints_vars", add:["all"]
      else
        if block.canAddSubType
          block.canAddSubType = false
          block.canAddAlias = true
        else if block.canAddAlias
          block.canAddAlias = true
          @addAlias ctx, token, block.body[block.body.length-2]
        @_addHints ctx, ["as"].concat(@collectionPostFrom)


    ##############################################################################
    # inner, left, rigth
    ##############################################################################
    else if ["inner","left","right"].indexOf(block.name) >= 0

      if block.counter is 0
        if token is "inner"
          @_addHints ctx, ["join"]
        else
          @_addHints ctx, ["join","outer"]

      else if token is "outer"
        @_addHints ctx, ["join"]

      else if token is "join"
        block.canAddSubType = true
        block.canAddAlias = false
        @_addHints ctx, call: "get_hints_vars", add:["fetch"]

      else if block.canAddSubType
        block.canAddSubType = false
        block.canAddAlias = true
        @_addHints ctx, ["as"].concat(@collectionPostFrom)
      else if block.canAddAlias
        block.canAddAlias = false
        @addAlias ctx, token, block.body[block.body.length-2]
        @_addHints ctx, [].concat(@collectionPostFrom)

    ##############################################################################
    # with
    ##############################################################################
    else if block.name is "with"
      if block.counter is 0
        block.canAddFirstVar = true
        block.canAddAlias = false
        @_addHints ctx, "get_hints_vars"
      else if block.canAddFirstVar
        block.canAddFirstVar = false
        block.canAddAlias = true
        @_addHints ctx, []
      else if block.canAddAlias
        block.canAddAlias = false
        @addAlias ctx, token, block.body[block.body.length-2]
        @_addHints ctx, [].concat(@collectionPostFrom)

    ##############################################################################
    # group
    ##############################################################################
    else if block.name is "group"
      if block.counter is 0
        @_addHints ctx, ["by"]
        block.canAddVars = false
      else if block.counter is 1
        if token is "by"
          block.canAddVars = true
          @_addHints ctx, "get_hints_vars"
        else
          throw "Irregular group block"
      else if block.canAddVars
        block.canAddVars = false
        @_addHints ctx, [","].concat(@collectionPostFrom)
      else if token is ","
        block.canAddVars = true
        @_addHints ctx, "get_hints_vars"

  parse:(_str)->
    str = _str.replace(/[ ]+/g," ")
    str = str.replace(/(,|>=|<=|>|<|!=|=|\(|\))/g," $1 ")
    tokens = str.split(" ")
    ctx =
      str:_str
      block:
        name:""
        counter:0
        body:[]
      history:
        body:[]
      config:
        extract:[]
        vars:[]
        types:[]
        mappingVar:{}
        alias:{}
      hints:["select","from"]

    filter = null
    if _str.length > 0
      lastCh = _str[_str.length-1]
      if lastCh is ")"
        ctx.hints = [","]
        return ctx
      if lastCh is "*"
        if tokens.length >= 2
          if tokens[tokens.length-1] is "*"
            if tokens[tokens.length-2] is "("
              ctx.hints = [")"]
            else
              ctx.hints = [","]
            return ctx
      unless [" ",",","=",">","<",".","("].indexOf(lastCh) >= 0
        for index in [tokens.length-1..0]
          filter = tokens[index].trim()
          if filter != "" then break
        if [].concat(@collectionAgregate, @collectionExpr).indexOf(filter) >= 0
          ctx.hints = ["("]
          return ctx
        if @BLOCKS.indexOf(filter) >= 0
          ctx.hints = []
          return ctx
        else
          tokens = tokens.splice(0,index)

    for token, i in tokens
      token = token.trim()
      continue if token is ""
      @_parseToken(token, ctx, i, tokens)
    ctx.filter = filter
    ctx

  get_hints_autocomplete:(ctx, schema, args)->
    variables = args.split(".")
    variables = variables.slice(0,variables.length-1)
    firstToken = variables[0]
    variablePath = @findFullPath(ctx, firstToken)
    _vars = []
    for item in variablePath
      _vars.push item
    for i in [0..variables.length-1]
      _vars.push variables[i]

    _baseType = ctx.config.mappingVar[_vars[0]]
    schema.getVariables(_baseType, _vars)

  get_hints_extract:(ctx, schema)->
    result = []
    for t in ctx.config.types
      vars = schema.getVariablesByType(t)
      for i in vars
        result.push i
    result

  get_hints_types:(ctx, schema)->
    schema.getTypes()

  get_hints_vars:(ctx, schema)->
    result = []
    if ctx.config.vars.length > 0
      result = ctx.config.vars
    else
      for type in ctx.config.types
        variables = schema.getVariablesByType(type)
        for item in variables
          result.push item
    result

  get_hints_vars_and_properties:(ctx, schema)->
    result = []
    for i in @get_hints_vars(ctx, schema)
      result.push i
    for i in schema.getProperties()
      result.push i
    result

  findFullPath:(ctx, name)->
    alias = ctx.config.alias
    path = name
    result = []
    while true
      tmp = alias[path]
      if tmp
        tkns = tmp.trim().split(".")
        for i in [tkns.length-1, 0]
          item = tkns[i].trim()
          if item is "" then continue
          result.push tkns[i]
        path = tkns[0]
      else
        break
    result.reverse()


  addAlias:(ctx, alias,statement)->
    ctx.config.alias[alias] = statement
    ctx.config.vars.push alias

  exec:(str, ctx, schema, args)->
    if call = this[str]
      call.call this, ctx, schema, args
    else
      []


  getHints:(str, ctx, _schema)->
    hints = []
    _hints = ctx.hints
    if Object.prototype.toString.call(_hints) == '[object Array]'
      for i in _hints
        hints.push i
    else if Object.prototype.toString.call(_hints) == '[object String]'
      schema = new Schema _schema
      if data = @exec(_hints, ctx, schema, [])
        for i in data
          hints.push i
    else if _hints is Object(_hints)
      if _hints.add?
        for i in _hints.add
          hints.push i
      schema = new Schema _schema
      if data = @exec(_hints.call, ctx, schema, _hints.args)
        for i in data
          hints.push i
    if ctx.filter?
      _hints = []
      for hint in hints
        if (hint != ctx.filter and hint.indexOf(ctx.filter) == 0 ) or [",","(",")"].indexOf(hint) >= 0
          _hints.push hint
      hints = _hints
    hints.sort()

window.gen = new _Gen()

CodeMirror.hqlHint = (cm, opt)->
  cur = cm.getCursor()

  text = Service.getCodeAfterToken cm, cur, ";"

  options = gen.parse text
  list = gen.getHints text, options, opt.schemaInfo
  if options.filter?
    offset = options.filter.length
    from = CodeMirror.Pos cur.line, cur.ch - offset
  else
    from = CodeMirror.Pos cur.line, cur.ch
  to = CodeMirror.Pos cur.line, cur.ch
  {
    list,from, to
  }


