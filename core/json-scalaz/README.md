Scalaz support for Lift JSON

Note, this is still very much work in progress...

This project adds a type class to parse JSON:

    trait JSON[A] {
      def read(json: JValue): Validation[String, A]
      def write(value: A): JValue
    }

Function 'read' returns an Applicative Functor, enabling parsing in an applicative style.

Simple example
==============

    case class Address(street: String, zipCode: String)
    case class Person(name: String, age: Int, address: Address)
  
    scala> val json = JsonParser.parse(""" {"street": "Manhattan 2", "zip": "00223" } """)
    scala> field[String](json, "zip") <*> (field[String](json, "street") map Address.curried)
    res0: Success(Address(Manhattan 2,00223))

    scala> field[String](json, "zip") <*> (field[String](json, "streets") map Address.curried)
    res1: Failure("no such field 'streets'")

Example which adds a new type class instance
============================================

    scala> implicit def addrJSON: JSON[Address] = new JSON[Address] {
             def read(json: JValue) = 
               field[String](json, "zip") <*> (field[String](json, "street") map Address.curried)

             def write(value: Address) = error("fixme")
           }

    scala> val p = JsonParser.parse(""" {"name":"joe","age":34,"address":{"street": "Manhattan 2", "zip": "00223" }} """)
    scala> field[Address](p, "address") <*> (field[Int](p, "age") <*> (field[String](p, "name") map Person.curried))
    res0: Success(Person(joe,34,Address(Manhattan 2,00223)))
