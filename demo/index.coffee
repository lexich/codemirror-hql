schemaInfo =
  Cat:
    vars:
      dog:"Dog"
      fish:"Fish"
  Dog:
    vars:
      dog:"Dog"
      fish:"Fish"
  Fish:
    vars:
      dog:"Dog"
      fish:"Fish"

autocomplete = (ch) ->
  (cm)->
    cur = cm.getCursor()
    line = cm.getDoc().getLine(cur.line)
    if ch != "CS"
      line += ch
      cm.getDoc().setLine(cur.line, line)
    CodeMirror.showHint cm, CodeMirror.hqlHint, {ch, completeSingle: false, schemaInfo}

window.init = ->
  mime = "hql"
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