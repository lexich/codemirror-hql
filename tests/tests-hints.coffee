gen = window.gen
window.schema =
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

window.schemaArr = ["Cat","Dog","Fish","org.java.Test"].sort()

_getHints = (str)->
  opt = gen.parse(str)
  gen.getHints str, opt, schema


test "Check select and from", ->
  str = ""
  hints = _getHints str
  deepEqual hints, ["select", "from"], "HQL: empty"

  str = "select"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "select "
  hints = _getHints str
  deepEqual hints, ["distinct"], "HQL: `#{str}`"

  str = "select a"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "select a "
  hints = _getHints str
  deepEqual hints, ["from"], "HQL: `#{str}`"

  str = "select a from"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "select a from "
  hints = _getHints str
  deepEqual hints, schemaArr, "HQL: `#{str}`"

  str = "from"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from "
  hints = _getHints str
  deepEqual hints, schemaArr, "HQL: `#{str}`"

  str = "from Cat"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat,"
  hints = _getHints str
  deepEqual hints, schemaArr, "HQL: `#{str}`"

  str = "from Cat "
  hints = _getHints str
  deepEqual hints, [
    "fetch","inner","left","right", "join", "where", "order"
  ].sort(), "HQL: `#{str}`"

  str = "from Cat a"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat a "
  hints = _getHints str
  deepEqual hints, [
    "fetch","inner","left","right", "join", "where", "order"
  ].sort(), "HQL: `#{str}`"

test "Check where", ->
  str = "from Cat c where"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat c where "
  hints = _getHints str
  deepEqual hints, ["c"], "HQL: `#{str}`"

  str = "from Cat c where c"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat c where c "
  hints = _getHints str
  deepEqual hints, ["like", "exist", "in"], "HQL: `#{str}`"

  str = "from Cat c where c = 1"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat c where c = 1 and"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat c where c = 1 and "
  hints = _getHints str
  deepEqual hints, ["c"], "HQL: `#{str}`"

  str = "from Cat c where c = 1 or "
  hints = _getHints str
  deepEqual hints, ["c"], "HQL: `#{str}`"

  str = "from Cat c where c = 1 or c = 2 and "
  hints = _getHints str
  deepEqual hints, ["c"], "HQL: `#{str}`"

  str = "from Cat a where a = 1 and a "
  hints = _getHints str
  deepEqual hints, ["like", "exist", "in"], "HQL: `#{str}`"


  str = "from Cat c where c.dog> "
  hints = _getHints str
  deepEqual hints, ["c", ":one", ":two"], "HQL: `#{str}`"

  str = "from Cat c where c.dog="
  hints = _getHints str
  deepEqual hints, ["c", ":one", ":two"], "HQL: `#{str}`"

  str = "from Cat where "
  hints = _getHints str
  deepEqual hints, ["dog","fish"], "HQL: `#{str}`"

  str = "select c from Cat where "
  hints = _getHints str
  deepEqual hints, ["dog", "fish"], "HQL: `#{str}`"

  str = "from Cat c where c = "
  hints = _getHints str
  deepEqual hints, ["c", ":one", ":two"], "HQL: `#{str}`"

  str = "from Cat c where c.dog like "
  hints = _getHints str
  deepEqual hints, ["c", ":one", ":two"], "HQL: `#{str}`"

  str = "from Cat c where c.dog != "
  hints = _getHints str
  deepEqual hints, ["c", ":one", ":two"], "HQL: `#{str}`"

  str = "from Cat c where c.dog = "
  hints = _getHints str
  deepEqual hints, ["c", ":one", ":two"], "HQL: `#{str}`"

  str = "from Cat c where c.dog>"
  hints = _getHints str
  deepEqual hints, ["c", ":one", ":two"], "HQL: `#{str}`"

  str = "from Cat c where c = 1 "
  hints = _getHints str
  deepEqual hints, ["and","or","order"], "HQL: `#{str}`"

  str = "from Cat c where c = 1 or c =  2 "
  hints = _getHints str
  deepEqual hints, ["and","or","order"], "HQL: `#{str}`"

  str = "from Cat c where c=1 "
  hints = _getHints str
  deepEqual hints, ["and","or","order"], "HQL: `#{str}`"

  str = "from Cat c where c in :one "
  hints = _getHints str
  deepEqual hints, ["and","or","order"], "HQL: `#{str}`"


test "Check order", ->
  str = "from Cat c order"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat c order "
  hints = _getHints str
  deepEqual hints, ["by"], "HQL: `#{str}`"

  str = "from Cat c order by "
  hints = _getHints str
  deepEqual hints, ["c"], "HQL: `#{str}`"

  str = "from Cat c, Dog d order by "
  hints = _getHints str
  deepEqual hints, ["c","d"], "HQL: `#{str}`"

  str = "from Cat c order by c"
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat c order by c "
  hints = _getHints str
  deepEqual hints, [], "HQL: `#{str}`"

  str = "from Cat c order by c,"
  hints = _getHints str
  deepEqual hints, ["c"], "HQL: `#{str}`"

  str = "from Cat c order by c, "
  hints = _getHints str
  deepEqual hints, ["c"], "HQL: `#{str}`"


test "Check join", ->
  str = "from Cat c inner "
  hints = _getHints str
  deepEqual hints, ["join","outer"], "HQL: `#{str}`"

  str = "from Cat c left "
  hints = _getHints str
  deepEqual hints, ["join","outer"], "HQL: `#{str}`"

  str = "from Cat c right "
  hints = _getHints str
  deepEqual hints, ["join","outer"], "HQL: `#{str}`"


test "Check join", ->
  str = "from Cat c fetch "
  hints = _getHints str
  deepEqual hints, ["all"], "HQL: `#{str}`"

  str = "from Cat c inner "
  hints = _getHints str
  deepEqual hints, ["join","outer"], "HQL: `#{str}`"

  str = "from Cat c inner join "
  hints = _getHints str
  deepEqual hints, ["fetch","c"], "HQL: `#{str}`"

  str = "from Cat c inner join fetch "
  hints = _getHints str
  deepEqual hints, ["all"], "HQL: `#{str}`"

  str = "from Cat c left "
  hints = _getHints str
  deepEqual hints, ["join","outer"], "HQL: `#{str}`"

  str = "from Cat c left join "
  hints = _getHints str
  deepEqual hints, ["fetch","c"], "HQL: `#{str}`"

  str = "from Cat c left join fetch "
  hints = _getHints str
  deepEqual hints, ["all"], "HQL: `#{str}`"

  str = "from Cat c right "
  hints = _getHints str
  deepEqual hints, ["join","outer"], "HQL: `#{str}`"

  str = "from Cat c right join "
  hints = _getHints str
  deepEqual hints, ["fetch","c"], "HQL: `#{str}`"

  str = "from Cat c right join fetch "
  hints = _getHints str
  deepEqual hints, ["all"], "HQL: `#{str}`"

  str = "from Cat c left join c.dog "
  hints = _getHints str
  deepEqual hints, ["as", "fetch", "inner", "join", "right", "left", "order", "where", "group" ].sort()

  str = "from Cat c left join fetch c.dog "
  hints = _getHints str
  deepEqual hints, ["as", "fetch", "inner", "join", "right", "left", "order", "where", "group" ].sort()

  str = "from Cat c where c.dog=c.cat and c.dog=c."
  hints = _getHints str
  deepEqual hints, ["dog","fish"], "HQL: `#{str}`"

  str = "from Cat c where c.dog=c.cat and c.dog=c."
  hints = _getHints str
  deepEqual hints, ["dog","fish"], "HQL: `#{str}`"

  str = "select distinct c from Cat c left join c.dog d with d.value > 5000.00"



test "additional valiable", ->
  str = "from Cat c where c."
  hints = _getHints str
  deepEqual hints, ["dog","fish"], "HQL: `#{str}`"

  str = "select f.dog from Fish f where f."
  hints = _getHints str
  deepEqual hints, ["dog","fish"], "HQL: `#{str}`"


->
  test "multiple class",->
    str = "from org."
    hints = _getHints str
    deepEqual hints, ["test"], "HQL: `#{str}`"



test "Fail", ->
  equal(1,1)
  str = "from Cat where dog in elements(cats)"


->
  test "check group by", ->
    str = "from Cat c group "
    hints = _getHints str
    deepEqual hints, ["by"], "HQL: `#{str}`"

    str = "from Cat c group by "
    hints = _getHints str
    deepEqual hints, ["c"], "HQL: `#{str}`"







