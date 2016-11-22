Magic String
=====

An automatic `toString` generator using Shapeless.

Example:

```scala
    case class Bar(first: String, second: Int)
    case class Foo(s: String, i: Int, doubles: List[Double], bars: List[Bar])
    val foo = Foo("Hello", 1, List(3.0, 2.7), List(Bar("firstThing", 100), Bar("second", 200)))
    val bar = Bar("hello", 1)
    println(foo.magicString)
    // Foo(s=Hello, i=1, doubles=[3.0,2.7], bars=[Bar(first=firstThing, second=100),Bar(first=second, second=200)])
```

So far just an experiment, pushing to gather some feedback.
