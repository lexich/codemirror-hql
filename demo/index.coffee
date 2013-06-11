schemaInfo =
  types:
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

    "org.java.Test":
      vars:
        cat:"Cat"
        dog:"Dog"
        fish:"Fish"
  properties:
    ["one","two"]

autocomplete = (ch) ->
  (cm)->
      cur = cm.getCursor()
      doc = cm.getDoc()
      line = doc.getLine(cur.line)
      if ch != "CS"
        line = line.slice(0,cur.ch) + ch + line.slice(cur.ch, line.length)
        doc.setLine(cur.line, line)
        doc.setCursor CodeMirror.Pos(cur.line, cur.ch + 1)
      setTimeout (->
        CodeMirror.showHint cm, CodeMirror.hqlHint, {ch, completeSingle: false, schemaInfo}
      ), 100

window.init = ->
  mime = "hql"
  window.editor = CodeMirror.fromTextArea(document.getElementById("code"),
    mode: mime
    indentWithTabs: true
    smartIndent: true
    lineNumbers: true
    matchBrackets: true
    autofocus: true
    enterMode: "keep",
    tabMode: "shift"
    extraKeys:
      "Ctrl-Space": autocomplete("CS")
      "'.'": autocomplete(".")
      "'='": autocomplete("=")
  )