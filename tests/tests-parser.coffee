gen = window.gen

test "select b",->
  str = "select b"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 0
  equal options.vars.length, 0
  equal options.tokens.select, true
  equal options.tokens.from, false
  equal options.tokens.postFrom, false

test "select b from Cat",->
  str = "select b from Cat"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 1
  equal options.types[0], "Cat"
  equal options.vars.length, 0
  equal options.mapping["Cat"][0],null
  equal options.tokens.select, true
  equal options.tokens.from, true
  equal options.tokens.postFrom, false

test "select b from Cat c",->
  str = "select b from Cat c"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 1
  equal options.types[0], "Cat"
  equal options.vars.length, 1
  equal options.vars[0], "c"
  equal options.mapping["Cat"][0],"c"
  equal options.tokens.select, true
  equal options.tokens.from, true
  equal options.tokens.postFrom, false

test "from Cat b",->
  str = "from Cat b"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 1
  equal options.types[0], "Cat"
  equal options.vars.length, 1
  equal options.vars[0], "b"
  equal options.mapping["Cat"][0],"b"
  equal options.tokens.select, false
  equal options.tokens.from, true
  equal options.tokens.postFrom, false

test "from Cat as b",->
  str = "from Cat as b"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 1
  equal options.types[0], "Cat"
  equal options.vars.length, 1
  equal options.vars[0], "b"
  equal options.mapping["Cat"][0],"b"
  equal options.tokens.select, false
  equal options.tokens.from, true
  equal options.tokens.postFrom, false

test "from Cat b, Dog",->
  str = "from Cat b, Dog"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 2
  equal options.types[0], "Cat"
  equal options.types[1], "Dog"
  equal options.vars.length, 1
  equal options.vars[0], "b"
  equal options.mapping["Cat"][0],"b"
  equal options.mapping["Dog"][0],null
  equal options.tokens.select, false
  equal options.tokens.from, true
  equal options.tokens.postFrom, false

test "from Cat as b, Dog",->
  str = "from Cat as b, Dog"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 2
  equal options.types[0], "Cat"
  equal options.types[1], "Dog"
  equal options.vars.length, 1
  equal options.vars[0], "b"
  equal options.mapping["Cat"][0],"b"
  equal options.mapping["Dog"][0],null
  equal options.tokens.select, false
  equal options.tokens.from, true
  equal options.tokens.postFrom, false

test "from Cat as b, Dog d",->
  str = "from Cat as b, Dog d"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 2
  equal options.types[0], "Cat"
  equal options.types[1], "Dog"
  equal options.vars.length, 2
  equal options.vars[0], "b"
  equal options.vars[1], "d"
  equal options.mapping["Cat"][0],"b"
  equal options.mapping["Dog"][0],"d"
  equal options.tokens.select, false
  equal options.tokens.from, true
  equal options.tokens.postFrom, false

test "from Cat as b where b.name = 1",->
  str = "from Cat as b where b.name = 1"
  options = gen.parse str
  equal options.original, str
  equal options.types.length, 1
  equal options.types[0], "Cat"
  equal options.vars.length, 1
  equal options.vars[0], "b"
  equal options.mapping["Cat"][0],"b"
  equal options.tokens.select, false
  equal options.tokens.from, true
  equal options.tokens.postFrom, true

test "from Cat c inner join",->
  opt = gen.parse "from Cat c inner join"
  deepEqual opt.vars, ["c"]
  deepEqual opt.types, ["Cat"]
  deepEqual opt.mapping, Cat:["c"]
  equal opt.tokens.select, false
  equal opt.tokens.from, true
  equal opt.tokens.postFrom, true

test "check post from arguments",->
  base = "from Cat as b"

  equal gen.parse("#{base} where").tokens.postFrom, true
  equal gen.parse("#{base} fetch all").tokens.postFrom, true
  equal gen.parse("#{base} inner join").tokens.postFrom, true
  equal gen.parse("#{base} inner join fetch").tokens.postFrom, true
  equal gen.parse("#{base} inner join fetch all").tokens.postFrom, true
  equal gen.parse("#{base} inner outer join").tokens.postFrom, true
  equal gen.parse("#{base} inner outer join fetch").tokens.postFrom, true
  equal gen.parse("#{base} inner outer join fetch all").tokens.postFrom, true
  equal gen.parse("#{base} left join").tokens.postFrom, true
  equal gen.parse("#{base} left join fetch").tokens.postFrom, true
  equal gen.parse("#{base} left join fetch all").tokens.postFrom, true
  equal gen.parse("#{base} left outer join").tokens.postFrom, true
  equal gen.parse("#{base} left outer join fetch").tokens.postFrom, true
  equal gen.parse("#{base} left outer join fetch all").tokens.postFrom, true
  equal gen.parse("#{base} right join").tokens.postFrom, true
  equal gen.parse("#{base} right join fetch").tokens.postFrom, true
  equal gen.parse("#{base} right join fetch all").tokens.postFrom, true
  equal gen.parse("#{base} right outer join").tokens.postFrom, true
  equal gen.parse("#{base} right outer join fetch").tokens.postFrom, true
  equal gen.parse("#{base} right outer join fetch all").tokens.postFrom, true
  a = 1









