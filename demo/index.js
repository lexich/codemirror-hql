// Generated by CoffeeScript 1.6.2
(function() {
  var autocomplete;

  autocomplete = function(ch) {
    return function(cm) {
      var cur, line;

      cur = cm.getCursor();
      line = cm.getDoc().getLine(cur.line);
      if (ch !== "CS") {
        line += ch;
        cm.getDoc().setLine(cur.line, line);
      }
      return CodeMirror.showHint(cm, CodeMirror.hqlHint, {
        ch: ch,
        completeSingle: false
      });
    };
  };

  window.init = function() {
    var mime;

    mime = "text/hql";
    return window.editor = CodeMirror.fromTextArea(document.getElementById("code"), {
      mode: mime,
      indentWithTabs: true,
      smartIndent: true,
      lineNumbers: true,
      matchBrackets: true,
      autofocus: true,
      extraKeys: {
        "Ctrl-Space": autocomplete("CS"),
        "'.'": autocomplete("."),
        "'='": autocomplete("=")
      }
    });
  };

}).call(this);

/*
//@ sourceMappingURL=index.map
*/