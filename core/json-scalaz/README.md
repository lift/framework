Scalaz support for Lift JSON
============================

This project adds a type class to parse JSON:

    trait JSON[A] {
      def read(json: JValue): Validation[String, A]
      def write(value: A): JValue
    }

Function 'read' returns an Applicative Functor, enabling parsing in an applicative style.

Simple example
--------------

    case class Address(street: String, zipCode: String)
    case class Person(name: String, age: Int, address: Address)
  
    scala> val json = JsonParser.parse(""" {"street": "Manhattan 2", "zip": "00223" } """)
    scala> field[String]("zip")(json) <*> (field[String]("street")(json) map Address.curried)
    res0: Success(Address(Manhattan 2,00223))

    scala> field[String]("zip")(json) <*> (field[String]("streets")(json) map Address.curried)
    res1: Failure("no such field 'streets'")

Notice the required explicit types when reading fields from JSON. The library comes with helpers which
can lift functions with pure values into "parsing context". This works well with Scala's type inferencer:

    scala> Address.applyJSON(field("street"), field("zip"))(json)
    res2: Success(Address(Manhattan 2,00223))

Function 'applyJSON' above lifts function 

    (String, String) => Address 

to

    (JValue => Result[String], JValue => Result[String]) => (JValue => Result[Address])

Example which adds a new type class instance
--------------------------------------------

    scala> implicit def addrJSONR: JSONR[Address] = new JSONR[Address] {
             def read(json: JValue) = Address.applyJSON(field("street"), field("zip"))(json)
           }

    scala> val p = JsonParser.parse(""" {"name":"joe","age":34,"address":{"street": "Manhattan 2", "zip": "00223" }} """)
    scala> from(p, Person)(field("name"), field("age"), field("address"))
    res0: Success(Person(joe,34,Address(Manhattan 2,00223)))

Validation
----------

Applicative style parsing works nicely with validation and data conversion. It is easy to compose 
transformations with various combinators Scalaz provides. An often used combinator is called a Kleisli 
composition >=>.

    def min(x: Int): Int => Result[Int] = (y: Int) => 
      if (y < x) Fail("min", y + " < " + x) else y.success

    def max(x: Int): Int => Result[Int] = (y: Int) => 
      if (y > x) Fail("max", y + " > " + x) else y.success

    // Creates a function JValue => Result[Person]
    Person.applyJSON(field("name"), validate[Int]("age") >=> min(18) >=> max(60))

Links
-----

* [More examples](https://github.com/lift/framework/tree/joni_json_scalaz/core/json-scalaz/src/test/scala/net/lifweb/json/scalaz)
* [Scalaz](http://code.google.com/p/scalaz/)
* [Kleisli composition](http://www.haskell.org/hoogle/?hoogle=%28a+-%3E+m+b%29+-%3E+%28b+-%3E+m+c%29+-%3E+%28a+-%3E+m+c%29)
