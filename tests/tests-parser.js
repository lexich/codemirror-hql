// Generated by CoffeeScript 1.6.2
(function() {
  var gen;

  gen = window.gen;

  test("select b", function() {
    var options, str;

    str = "select b";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 0);
    equal(options.vars.length, 0);
    equal(options.tokens.select, true);
    equal(options.tokens.from, false);
    return equal(options.tokens.postFrom, false);
  });

  test("select b from Cat", function() {
    var options, str;

    str = "select b from Cat";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 1);
    equal(options.types[0], "Cat");
    equal(options.vars.length, 0);
    equal(options.mapping["Cat"][0], null);
    equal(options.tokens.select, true);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, false);
  });

  test("select b from Cat c", function() {
    var options, str;

    str = "select b from Cat c";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 1);
    equal(options.types[0], "Cat");
    equal(options.vars.length, 1);
    equal(options.vars[0], "c");
    equal(options.mapping["Cat"][0], "c");
    equal(options.tokens.select, true);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, false);
  });

  test("from Cat b", function() {
    var options, str;

    str = "from Cat b";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 1);
    equal(options.types[0], "Cat");
    equal(options.vars.length, 1);
    equal(options.vars[0], "b");
    equal(options.mapping["Cat"][0], "b");
    equal(options.tokens.select, false);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, false);
  });

  test("from Cat as b", function() {
    var options, str;

    str = "from Cat as b";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 1);
    equal(options.types[0], "Cat");
    equal(options.vars.length, 1);
    equal(options.vars[0], "b");
    equal(options.mapping["Cat"][0], "b");
    equal(options.tokens.select, false);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, false);
  });

  test("from Cat b, Dog", function() {
    var options, str;

    str = "from Cat b, Dog";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 2);
    equal(options.types[0], "Cat");
    equal(options.types[1], "Dog");
    equal(options.vars.length, 1);
    equal(options.vars[0], "b");
    equal(options.mapping["Cat"][0], "b");
    equal(options.mapping["Dog"][0], null);
    equal(options.tokens.select, false);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, false);
  });

  test("from Cat as b, Dog", function() {
    var options, str;

    str = "from Cat as b, Dog";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 2);
    equal(options.types[0], "Cat");
    equal(options.types[1], "Dog");
    equal(options.vars.length, 1);
    equal(options.vars[0], "b");
    equal(options.mapping["Cat"][0], "b");
    equal(options.mapping["Dog"][0], null);
    equal(options.tokens.select, false);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, false);
  });

  test("from Cat as b, Dog d", function() {
    var options, str;

    str = "from Cat as b, Dog d";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 2);
    equal(options.types[0], "Cat");
    equal(options.types[1], "Dog");
    equal(options.vars.length, 2);
    equal(options.vars[0], "b");
    equal(options.vars[1], "d");
    equal(options.mapping["Cat"][0], "b");
    equal(options.mapping["Dog"][0], "d");
    equal(options.tokens.select, false);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, false);
  });

  test("from Cat as b where b.name = 1", function() {
    var options, str;

    str = "from Cat as b where b.name = 1";
    options = gen.parse(str);
    equal(options.original, str);
    equal(options.types.length, 1);
    equal(options.types[0], "Cat");
    equal(options.vars.length, 1);
    equal(options.vars[0], "b");
    equal(options.mapping["Cat"][0], "b");
    equal(options.tokens.select, false);
    equal(options.tokens.from, true);
    return equal(options.tokens.postFrom, true);
  });

  test("from Cat c inner join", function() {
    var opt;

    opt = gen.parse("from Cat c inner join");
    deepEqual(opt.vars, ["c"]);
    deepEqual(opt.types, ["Cat"]);
    deepEqual(opt.mapping, {
      Cat: ["c"]
    });
    equal(opt.tokens.select, false);
    equal(opt.tokens.from, true);
    return equal(opt.tokens.postFrom, true);
  });

  test("select distinct c from Cat c left join c.dog d with d.cat = c", function() {
    var opt;

    opt = gen.parse("select distinct c from Cat c left join c.dog d with d.cat = c");
    equal(opt.tokens.select, true);
    equal(opt.tokens.from, true);
    equal(opt.tokens.postFrom, true);
    equal(opt.tokens.joinFetch, true);
    return deepEqual(opt.vars, ["c", "d"]);
  });

  test("check post from arguments", function() {
    var a, base;

    base = "from Cat as b";
    equal(gen.parse("" + base + " where").tokens.postFrom, true);
    equal(gen.parse("" + base + " fetch all").tokens.postFrom, true);
    equal(gen.parse("" + base + " inner join").tokens.postFrom, true);
    equal(gen.parse("" + base + " inner join fetch").tokens.postFrom, true);
    equal(gen.parse("" + base + " inner join fetch all").tokens.postFrom, true);
    equal(gen.parse("" + base + " inner outer join").tokens.postFrom, true);
    equal(gen.parse("" + base + " inner outer join fetch").tokens.postFrom, true);
    equal(gen.parse("" + base + " inner outer join fetch all").tokens.postFrom, true);
    equal(gen.parse("" + base + " left join").tokens.postFrom, true);
    equal(gen.parse("" + base + " left join fetch").tokens.postFrom, true);
    equal(gen.parse("" + base + " left join fetch all").tokens.postFrom, true);
    equal(gen.parse("" + base + " left outer join").tokens.postFrom, true);
    equal(gen.parse("" + base + " left outer join fetch").tokens.postFrom, true);
    equal(gen.parse("" + base + " left outer join fetch all").tokens.postFrom, true);
    equal(gen.parse("" + base + " right join").tokens.postFrom, true);
    equal(gen.parse("" + base + " right join fetch").tokens.postFrom, true);
    equal(gen.parse("" + base + " right join fetch all").tokens.postFrom, true);
    equal(gen.parse("" + base + " right outer join").tokens.postFrom, true);
    equal(gen.parse("" + base + " right outer join fetch").tokens.postFrom, true);
    equal(gen.parse("" + base + " right outer join fetch all").tokens.postFrom, true);
    return a = 1;
  });

}).call(this);

/*
//@ sourceMappingURL=tests-parser.map
*/
