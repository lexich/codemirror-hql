
autocomplete = (ch) ->
  (cm)->
    cur = cm.getCursor()
    line = cm.getDoc().getLine(cur.line)
    if ch != "CS"
      line += ch
      cm.getDoc().setLine(cur.line, line)
    CodeMirror.showHint cm, CodeMirror.hqlHint, {ch, completeSingle: false}

window.init = ->
  mime = "text/hql"
  window.editor = CodeMirror.fromTextArea(document.getElementById("code"),
    mode: mime
    indentWithTabs: true
    smartIndent: true
    lineNumbers: true
    matchBrackets: true
    autofocus: true
    extraKeys:
      "Ctrl-Space": autocomplete("CS")
      "'.'": autocomplete(".")
      "'='": autocomplete("=")
  )