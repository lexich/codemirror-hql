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

    getVariables:(_type, variables)->
      vars = {}
      for i in [0..variables.length-1]
        v = variables[i]
        if i is 0
          vars = @schema.types[_type]?.vars
        else
          type = vars[v]
          vars = @schema.types[type]?.vars
        return [] unless vars?
      result = []
      for v, type of vars
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

    parseSelect:(str,options)->
      tokens = options.tokens
      rSelect = /select[ ]+(.*)/.exec str
      #if select exist
      if rSelect
        tokens.select = true
        str = rSelect[1]
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
        mapping = options.mapping
        for pairParam in pairParams
          res = pairParam.trim().split(/[ ]+|[ ]+as[ ]+/)

          if res.length is 1
            sType = res[0].trim()
            sVar = null

          else if res.length is 2
            sType = res[0]
            sVar = res[1]

          else if res.length is 3
            sType = res[0]
            sVar = res[2]

          else
            continue

          unless sType is "" and sVar is ""
            @uniquePush options.types, sType
            @uniquePush options.vars, sVar if sVar
            mapping[sType] = [] unless mapping[sType]?
            @uniquePush mapping[sType], sVar if sVar
            options.mappingVar[sVar] = sType

      str

    parsePostFrom:(token, statement, options)->
      if token != ""
        options.tokens.postFrom = true
        options.dataPostFrom.push {token,statement}


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

      #@analyseFrom(text, options)
      options

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

      res.push "where"
      res.push "order"
      res.push "order[ ]+by"
      res.push "group"
      res

    uniquePush:(arr,val)->
      unless val in arr
        arr.push val.trim()

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

    fillVariablesAutocomplete:(hints, variables, schema, options)->
      firstVar = variables[0]
      type = options.mappingVar[firstVar]
      data = schema.getVariables type, variables
      for item in data
        hints.push item
      this


    fillHints:(str, options, schema, hints)->
      tokens = options.tokens
      if tokens.from and !tokens.postFrom
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
      else if ["=","<",">"].indexOf(tks[tks.length-2]) >= 0
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
window.gen = new Generator()

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


