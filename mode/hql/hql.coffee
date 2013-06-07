
sqlKeywords = [
  'and'
  'as'
  'by'
  'delete'
  'distinct'
  'entry'
  'fetch'
  'from'
  'group'
  'in'
  'index'
  'is'
  'join'
  'key'
  'left'
  'like'
  'not'
  'or'
  'or'
  'order'
  'select'
  'set'
  'update'
  'value'
  'where'
  'with'
  "inner"
  "outer"
  "left"
  "rigth"
  "join"
  "fetch"
  "all"
].join(" ")

builtinWords = [

].join(" ")

atomsWords = [
  "false"
  "true"
  "null"
  "unknown"
].join(" ")

dateSQLWords = [
  'abs'
  'avg'
  'bit_length'
  'cast'
  'concat'
  'count'
  'current_date'
  'current_time'
  'current_timestamp'
  'day'
  'elements'
  'extract'
  'hour'
  'indices'
  'length'
  'locate'
  'lower'
  'max'
  'maxelement'
  'maxindex'
  'min'
  'minelement'
  'minindex'
  'minute'
  'mod'
  'month'
  'second'
  'size'
  'sqrt'
  'str'
  'substring'
  'sum'
  'trim'
  'upper'
  'year'
].join(" ")

CodeMirror.defineMode "hql", (config, parserConfig) ->
  client = parserConfig.client or {}
  atoms = parserConfig.atoms or
  false: true
  true: true
  null: true

  builtin = parserConfig.builtin or {}
  keywords = parserConfig.keywords or {}
  operatorChars = parserConfig.operatorChars or /^[*+\-%<>!=&|~^]/
  support = parserConfig.support or {}
  hooks = parserConfig.hooks or {}
  dateSQL = parserConfig.dateSQL or
    date: true
    time: true
    timestamp: true

  tokenBase = (stream, state) ->
    ch = stream.next()

    # call hooks from the mime type
    if hooks[ch]
      result = hooks[ch](stream, state)
      return result  if result isnt false
    if (ch is "0" and stream.match(/^[xX][0-9a-fA-F]+/)) or (ch is "x" or ch is "X") and stream.match(/^'[0-9a-fA-F]+'/)

      # hex
      "number"
    else if ((ch is "b" or ch is "B") and stream.match(/^'[01]+'/)) or (ch is "0" and stream.match(/^b[01]+/))

      # bitstring
      "number"
    else if ch.charCodeAt(0) > 47 and ch.charCodeAt(0) < 58

      # numbers
      stream.match /^[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/
      "number"
    else if ch is "?" and (stream.eatSpace() or stream.eol() or stream.eat(";"))

      # placeholders
      "variable-3"
    else if ch is "\"" or ch is "'"

      # strings
      state.tokenize = tokenLiteral(ch)
      state.tokenize stream, state
    else if /^[\(\),\;\[\]]/.test(ch)

      # no highlightning
      null
    else if ch is "/"
      if stream.eat("/")
        # 1-line comments
        stream.skipToEnd()
        "comment"
      else if stream.eat("*")
        # multi-line comments
        state.tokenize = tokenComment
        state.tokenize stream, state

    else if ch is "."

      # .1 for 0.1
      return "number"  if support.zerolessFloat is true and stream.match(/^(?:\d+(?:e\d*)?|\d*e\d+)/i)

      # .table_name (ODBC)
      "variable-2"  if stream.match(/^[a-zA-Z_]+/) and support.ODBCdotTable is true
    else if operatorChars.test(ch)

      # operators
      stream.eatWhile operatorChars
      null
    else if ch is "{" and (stream.match(/^( )*(d|D|t|T|ts|TS)( )*'[^']*'( )*}/) or stream.match(/^( )*(d|D|t|T|ts|TS)( )*"[^"]*"( )*}/))

      # dates (weird ODBC syntax)
      "number"
    else
      stream.eatWhile /^[_\w\d]/
      word = stream.current().toLowerCase()

      # dates (standard SQL syntax)
      return "number"  if dateSQL.hasOwnProperty(word) and (stream.match(/^( )+'[^']*'/) or stream.match(/^( )+"[^"]*"/))
      return "atom"  if atoms.hasOwnProperty(word)
      return "builtin"  if builtin.hasOwnProperty(word)
      return "keyword"  if keywords.hasOwnProperty(word)
      return "string-2"  if client.hasOwnProperty(word)
      null

  # 'string', with char specified in quote escaped by '\'
  tokenLiteral = (quote) ->
    (stream, state) ->
      escaped = false
      ch = undefined
      while (ch = stream.next())?
        if ch is quote and not escaped
          state.tokenize = tokenBase
          break
        escaped = not escaped and ch is "\\"
      "string"

  tokenComment = (stream, state) ->
    loop
      if stream.skipTo("*")
        stream.next()
        if stream.eat("/")
          state.tokenize = tokenBase
          break
      else
        stream.skipToEnd()
        break
    "comment"

  pushContext = (stream, state, type) ->
    state.context =
      prev: state.context
      indent: stream.indentation()
      col: stream.column()
      type: type

  popContext = (state) ->
    state.indent = state.context.indent
    state.context = state.context.prev

  return {
    startState: ->
      tokenize: tokenBase
      context: null

    token: (stream, state) ->
      state.context.align = false  if state.context and not state.context.align?  if stream.sol()
      return null  if stream.eatSpace()
      style = state.tokenize(stream, state)
      return style  if style is "comment"
      state.context.align = true  if state.context and not state.context.align?
      tok = stream.current()
      if tok is "("
        pushContext stream, state, ")"
      else if tok is "["
        pushContext stream, state, "]"
      else popContext state  if state.context and state.context.type is tok
      style

    indent: (state, textAfter) ->
      cx = state.context
      return CodeMirror.Pass  unless cx
      if cx.align
        cx.col + ((if textAfter.charAt(0) is cx.type then 0 else 1))
      else
        cx.indent + config.indentUnit
  }


do ->
  hookIdentifier = (stream) ->
    ch = undefined
    return "variable-2"  if ch is "`" and not stream.eat("`")  while (ch = stream.next())?
    null

  # variable token
  hookVar = (stream) ->

    # variables
    # @@ and prefix
    if stream.eat("@")
      stream.match /^session\./
      stream.match /^local\./
      stream.match /^global\./
    if stream.eat("'")
      stream.match /^.*'/
      return "variable-2"
    else if stream.eat("\"")
      stream.match /^.*"/
      return "variable-2"
    else if stream.eat("`")
      stream.match /^.*`/
      return "variable-2"
    else return "variable-2"  if stream.match(/^[0-9a-zA-Z$\.\_]+/)
    null

  hookClient = (stream) ->

    # \g, etc
    (if stream.match(/^[a-zA-Z]\b/) then "variable-2" else null)


  set = (str) ->
    obj = {}
    words = str.split(" ")
    i = 0

    while i < words.length
      obj[words[i]] = true
      ++i
    obj



  CodeMirror.defineMIME "hql",
    name: "hql"
    keywords: set(sqlKeywords)
    builtin: set(builtinWords)
    atoms: set(atomsWords)
    operatorChars: /^[*+\-%<>!=]/
    dateSQL: set(dateSQLWords)
    support: set("ODBCdotTable")

  CodeMirror.defineMIME "text/hql", "hql"