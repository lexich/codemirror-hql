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

Generator = do->
  __super__=->
    this.initialize()
    this

  __super__:: =
    joinRegExp:null
    splitterAfterFrom:null
    afterFromAutoComplete:[]

    initialize:->
      @joinRegExp = new RegExp @createAfrerFrom().join("|")
      str = @createAfrerFrom().join("|")
      str = str.replace(/\[ \]\+/g," ").replace(/\[ \]\*/g," ")
      @afterFromAutoComplete = str.split("|")
      @splitterAfterFrom = @createAfrerFrom().join("|")
      this

    parse:(text)->
      options =
        types:[]
        vars:[]
        mapping:{}
        mappingVar:{}
        dataPostFrom:[]
        original: text
        tokens:
          select:false
          from:false
          postFrom:false
          joinFetch:false

      str = text.replace /\n/, ""

      str = @parseSelect str, options
      str = @parseFrom str, options
      unless options.tokens.from or options.tokens.select
        return options

      #where
      strRx = "(#{@splitterAfterFrom})"
      rx =  new RegExp strRx
      rAfter = str.split rx
      if rAfter.length > 1
        i = 1
        while i < rAfter.length
          token = rAfter[i].trim()
          statement = rAfter[i+1].trim()
          @parsePostFrom token, statement, options
          i+=2
      options

    getHints:(str, options, _schema)->
      schema = new Schema _schema
      tokens = options.tokens
      hints = []
      lastIndex = str.length-1
      if str.trim().length > 0
        if str[lastIndex] is "."
          res = str.replace(/[ ]+/g," ").split(/([ ]+|>=|<=|!=|>|<|=)/)
          variablesString = res[res.length-1]
          variables = variablesString.split(".")
          variables = variables.slice(0, variables.length-1)
          @fillVariablesAutocomplete hints, variables, schema, options
          return hints

        else if [" ",",","=",">","<"].indexOf(str[lastIndex]) < 0
          return hints


      if !tokens.select and !tokens.from
        hints = ["select", "from"]
      else if tokens.select and !tokens.from
        if /select[ ]+(.+)(from|)/.test str.trim()
          hints = ["from"]
      else if tokens.from and !tokens.postFrom
        s = str.trim()
        if (options.types.length > 0 and \
          (s[s.length-1]!="," and s.slice(s.length-2, s.length) != "as") and \
          s.indexOf("from") < s.length - 4
        )
          for item in ["fetch","inner","left","right", "join", "where", "order"].sort()
            hints.push item
      @fillHints str, options, schema, hints

    parseSelect:(str,options)->
      tokens = options.tokens
      rSelect = /(select[ ]+distinct|select)[ ]+(.*)/.exec str
      #if select exist
      if rSelect
        str = rSelect[2]
        tokens.select = true
        rFrom = /(.+)from(.*)/.exec str
        if rFrom
          tokens.from = true
          vars = rFrom[1].split(",")
          str = "from #{rFrom[2]}"
        else
          vars = str.split(",")
        for item in vars
          item = item.trim()
          continue if item == ""
      str

    parseFrom:(str, options)->
      tokens = options.tokens
      rFrom = /from(.*)/.exec str
      if rFrom
        tokens.from = true
        str = rFrom[1]
        res = str.replace(/[ ]+/," ").split(" ")
        postFromBuildin = ["inner","fetch","left","right","join","where","order"]
        fromParams = ""
        str = ""
        i = 0
        for i in [0..res.length-1]
          item = res[i]
          if postFromBuildin.indexOf(item) < 0
            fromParams += "#{item} "
          else break
        str = res.splice(i,res.length).join(" ")

        pairParams = fromParams.trim().split(",")
        for pairParam in pairParams
          @parseParams pairParam, options
      str

    parseParams:(strParam, options)->
      res = strParam.trim().split(/[ ]+|[ ]+as[ ]+/)
      mapping = options.mapping
      if res.length is 1
        sType = res[0].trim()
        sVar = null

      else if res.length is 2
        sType = res[0]
        sVar = res[1]

      else if res.length is 3
        sType = res[0]
        sVar = res[2]

      else return

      unless sType is "" or sVar is ""
        @uniquePush options.types, sType
        @uniquePush options.vars, sVar if sVar
        mapping[sType] = [] unless mapping[sType]?
        @uniquePush mapping[sType], sVar if sVar
        options.mappingVar[sVar] = sType
      null

    parsePostFrom:(token, statement, options)->
      if token != ""
        options.tokens.postFrom = true
        options.dataPostFrom.push {token,statement}
        if token.indexOf("join") >=0 or token.indexOf("fetch") >= 0
          options.tokens.joinFetch = true
          @parseParams statement, options



    createAfrerFrom:->
      res = []
      typejoin = [ "inner[ ]+", "left[ ]+", "right[ ]+" ]
      outer = [ "outer[ ]+", "" ]
      fetch = ["fetch", "fetch[ ]+all", ""]
      join = ["join[ ]*"]

      _add = (item)-> if item != "" and res.indexOf(item) < 0 then res.push item

      for i1 in typejoin
        for i2 in outer
          for i3 in join
            for i4 in fetch
              _add "#{i1}#{i2}#{i3}#{i4}"

      for tuple in [typejoin, outer, fetch, join]
        for item in tuple
          _add item
      res.push "with"
      res.push "where"
      res.push "order"
      res.push "order[ ]+by"
      res.push "group"
      res

    uniquePush:(arr,val)->
      unless val in arr
        arr.push val.trim()

    fillVariablesAutocomplete:(hints, variables, schema, options)->
      firstVar = variables[0]
      type = options.mappingVar[firstVar]
      data = schema.getVariables type, variables
      for item in data
        hints.push item
      this


    fillHints:(str, options, schema, hints)->
      tokens = options.tokens
      if tokens.select and !tokens.from
        if statements = /select (.*)/.exec str
          s = statements[1].trim()
          if s is ""
            hints.push "distinct"

      else if tokens.from and !tokens.postFrom
        bFill = false
        statements = /from (.*)/.exec str
        if statements?
          s = statements[1]
          if s.length is 0
            bFill = true
          else if s[s.length-1] == " "
            s = s.trim()
            if s is ""                   then bFill = true
            else if s[s.length-1] is "," then bFill = true

          else if s[s.length-1] == ","
            bFill = true
        if bFill
          for type in schema.getTypes()
            hints.push type
      else if tokens.postFrom
        dataPF = options.dataPostFrom
        lastData = dataPF[dataPF.length-1]
        token = lastData.token
        statement = lastData.statement
        s = statement.trim()

        @checkWhere(hints, token, statement, s, options, schema) or \
        @checkOrder(hints, token, statement, s, options, schema) or \
        @checkInnerLeftRigth(hints, token, statement, s, options, schema) or \
        @checkJoinFetch(hints, token, statement, s, options, schema) or \
        @checkOuter(hints, token, statement, s, options, schema)

      hints

    checkOuter:(hints, token, statement, s, options, schema)->
      spToken = token.split(" ")
      lastToken = spToken[spToken.length-1].trim()
      return false unless lastToken is "outer"
      if s is ""
        hints.push "join"
      true

    checkJoinFetch:(hints, token, statement, s, options, schema)->
      spToken = token.split(" ")
      lastToken = spToken[spToken.length-1].trim()
      return false unless ["join","fetch"].indexOf(lastToken) >= 0
      if s is ""
        if lastToken is  "join"
          hints.push "fetch"
          @pushLocalVars hints, options, schema
        else if lastToken is "fetch"
          hints.push "all"
      else
        for item in ["fetch", "inner", "join", "right", "left", "order", "where", "as", "group"].sort()
          hints.push item
      true

    checkInnerLeftRigth:(hints, token, statement, s, options, schema)->
      spToken = token.split(" ")
      lastToken = spToken[spToken.length-1].trim()
      return false if [
        "inner"
        "left"
        "right"
      ].indexOf(lastToken) < 0
      if s is ""
        hints.push "join"
        hints.push "outer"
      true

    fillSchemaProperties:(hints, schema)->
      for item in schema.getProperties()
        hints.push item

    checkWhere:(hints, token, statement, s, options, schema)->
      return false unless token is "where"
      _tks = s.replace(/(>=|<=|!=|=|<|>)/g," $1 ").replace(/[ ]+/g," ").split(" ")
      tks = []
      for _i in _tks
        _i = _i.trim()
        tks.push _i if _i != ""

      if tks.length is 0
        @pushLocalVars hints, options, schema
        return true

      lastTks = tks[tks.length-1]

      if ["and","or","like", "in", "exist", "=",">=","<=","!=","=","<",">"].indexOf(lastTks) >= 0
        @pushLocalVars hints, options, schema
        if ["like", "in", "exist", "=",">=","<=","!=","=","<",">"].indexOf(lastTks) >= 0
          @fillSchemaProperties hints, schema
      else if ["=",">=","<=","!=","=","<",">","in"].indexOf(tks[tks.length-2]) >= 0
        hints.push "and"
        hints.push "or"
        hints.push "order"
      else if ["","and","or"].indexOf(tks[tks.length-2])
        hints.push "like"
        hints.push "exist"
        hints.push "in"
      true

    checkOrder:(hints, token, statement, s, options, schema)->
      return false unless token is "order"
      if s.indexOf("by") is 0
        tks = statement.split("by")
        lastTks = tks[1].trim()
        if lastTks is "" or lastTks[lastTks.length-1] is ","
          @pushLocalVars hints, options, schema
      else
        hints.push "by"
      true

    pushLocalVars:(hints,options, schema)->
      if options.vars.length > 0
        for item in options.vars
          hints.push item
      else
        for _type in options.types.sort()
          for item in schema.getVariablesByType(_type)
            hints.push item
      this

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
  initialize:->

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
        ctx.hints = ["distinct"]

      else if token is "distinct"
        ctx.hints = []

      else if token is ","
        if block.canAddExtract = false
          block.canAddExtract = true
          ctx.hints = "get_hints_extract"
        else
          throw "Irregular select block"

      else if block.canAddExtract
        config.extract.push token
        block.canAddExtract = false
        ctx.hints = ["from", ","]

    ##############################################################################
    # from
    ##############################################################################
    else if block.name is "from"
      if block.counter is 0
        block.canAddType = true
        block.canAddVars = false
        ctx.hints = "get_hints_types"

      else if block.canAddType
        config.types.push token
        block.canAddType = false
        block.canAddVars = true
        ctx.hints = ["as", "fetch", "inner", "left", "right", "join", "where", "order"]
      else if block.canAddVars
        if token is ","
          block.canAddType = true
          block.canAddVars = false
          ctx.hints = "get_hints_types"
        else if token is "as"
          block.canAddVars = true
          block.canAddType = false
          ctx.hints = []
        else
          config.vars.push token
          config.mappingVar[token] = config.types[config.types.length-1]
          ctx.hints = ["fetch","inner","left","right", "join", "where", "order"]

    ##############################################################################
    # where
    ##############################################################################
    else if block.name is "where"
      collectionExpr = ["size","maxelement","maxindex","minelement", "minindex","elements","indices"]

      if block.counter is 0
        block.canAddFirstVal = true
        block.canAddSigh = false
        block.canAddSecondVal = false
        block.openBracket = false
        if ctx.config.vars.length > 0
          ctx.hints = call:"get_hints_vars", add:collectionExpr
        else
          ctx.hints = call:"get_hints_extract", add:collectionExpr
      else if block.canAddFirstVal
        if collectionExpr.indexOf(token) >= 0
          ctx.hints = ["("]
        else if token is "("
          block.openBracket = true
          ctx.hints = call:"get_hints_vars_and_properties"
        else if token is ")"
          block.openBracket = false
          block.canAddFirstVal = false
          block.canAddSigh = true
          ctx.hints = [">","<","=","!=",">=","<=", "exist","in", "like"]
        else if block.openBracket
          ctx.hints = [")"]
        else
          block.canAddFirstVal = false
          block.canAddSigh = true
          ctx.hints = [">","<","=","!=",">=","<=", "exist","in", "like"]
      else if block.canAddSigh
        block.canAddSigh = false
        block.canAddSecondVal = true
        ctx.hints = call:"get_hints_vars_and_properties", add:collectionExpr
      else if block.canAddSecondVal
        if collectionExpr.indexOf(token) >= 0
          ctx.hints = ["("]
        else if token is "("
          block.openBracket = true
          ctx.hints = call:"get_hints_vars_and_properties"
        else if token is ")"
          block.openBracket = false
          block.canAddSecondVal = false
          ctx.hints = ["and","or", "order"]
        else if block.openBracket
          ctx.hints = [")"]
        else
          block.canAddSecondVal = false
          ctx.hints = ["and","or", "order"]

      else if ["and","or"].indexOf(token) >= 0
        block.canAddFirstVal = true
        ctx.hints = "get_hints_vars"


    ##############################################################################
    # order
    ##############################################################################
    else if block.name is "order"
      if block.counter is 0
        block.canAddVars = true
        ctx.hints = ["by"]
      else if block.counter is 1
        if token is "by"
          ctx.hints = "get_hints_vars"
        else
          throw "irregular order token"
      else
        if block.canAddVars
          throw "Irregular order block" if token is ","
          block.canAddVars = false
          ctx.hints = [","]

        else
          block.canAddVars = true
          ctx.hints = "get_hints_vars"

    ##############################################################################
    # fetch
    ##############################################################################
    else if block.name is "fetch"
      if block.counter is 0
        block.canAddSubType = true
        block.canAddAlias = false
        ctx.hints = call: "get_hints_vars", add:["all"]
      else
        if block.canAddSubType
          block.canAddSubType = false
          block.canAddAlias = true
        else if block.canAddAlias
          block.canAddAlias = true
          @addAlias ctx, token, block.body[block.body.length-2]
        ctx.hints = ["fetch","inner","left","right", "join", "where", "order", "as", "group"]


    ##############################################################################
    # inner, left, rigth
    ##############################################################################
    else if ["inner","left","right"].indexOf(block.name) >= 0

      if block.counter is 0
        if token is "inner"
          ctx.hints = ["join"]
        else
          ctx.hints = ["join","outer"]

      else if token is "outer"
        ctx.hints = ["join"]

      else if token is "join"
        block.canAddSubType = true
        block.canAddAlias = false
        ctx.hints = call: "get_hints_vars", add:["fetch"]

      #else if token is "fetch"
      #  ctx.hints = "get_hints_vars"

      else if block.canAddSubType
        block.canAddSubType = false
        block.canAddAlias = true
        ctx.hints = ["fetch","inner","left","right", "join", "where", "order", "group", "as", "with"]
      else if block.canAddAlias
        block.canAddAlias = false
        @addAlias ctx, token, block.body[block.body.length-2]
        ctx.hints = ["fetch","inner","left","right", "join", "where", "order", "group", "with"]

    ##############################################################################
    # with
    ##############################################################################
    else if block.name is "with"
      if block.counter is 0
        block.canAddFirstVar = true
        block.canAddAlias = false
        ctx.hints = "get_hints_vars"
      else if block.canAddFirstVar
        block.canAddFirstVar = false
        block.canAddAlias = true
        ctx.hints = []
      else if block.canAddAlias
        block.canAddAlias = false
        @addAlias ctx, token, block.body[block.body.length-2]
        ctx.hints = ["fetch","inner","left","right", "join", "where", "order", "group"]

    ##############################################################################
    # group
    ##############################################################################
    else if block.name is "group"
      if block.counter is 0
        ctx.hints = ["by"]
        block.canAddVars = false
      else if block.counter is 1
        if token is "by"
          block.canAddVars = true
          ctx.hints = "get_hints_vars"
        else
          throw "Irregular group block"
      else if block.canAddVars
        block.canAddVars = false
        ctx.hints = ["fetch","inner","left","right", "join", "where", "order", "group", ","]
      else if token is ","
        block.canAddVars = true
        ctx.hints = "get_hints_vars"

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

    if _str.length > 0
      lastCh = _str[_str.length-1]
      unless [" ",",","=",">","<",".","("].indexOf(lastCh) >= 0
        ctx.hints = []
        return ctx

    for i in [0..tokens.length-1]
      token = tokens[i].trim()
      continue if token is ""
      @_parseToken(token, ctx, i, tokens)

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
    hints.sort()

window.gen = new _Gen()

CodeMirror.hqlHint = (cm, opt)->
  cur = cm.getCursor()

  text = Service.getCodeAfterToken cm, cur, ";"

  options = gen.parse text
  hints = gen.getHints text, options, opt.schemaInfo
  {
    list: hints
    from: cur
    to: cur
  }


