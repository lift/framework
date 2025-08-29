import net.liftweb.json._

// Test basic JSON parsing and DSL
val json = parse("""{"name": "John", "age": 30}""")
println(s"Parsed JSON: $json")

// Test extraction
case class Person(name: String, age: Int)
val person = json.extract[Person]
println(s"Extracted person: $person")

// Test JSON DSL
import JsonDSL._
val generatedJson = ("name" -> "Jane") ~ ("age" -> 25)
println(s"Generated JSON: $generatedJson")

// Test type-based extraction (using our new Scala 3 compatible methods)
val ageValues = json \ classOf[JInt]
println(s"Age values: $ageValues")

println("✅ Scala 3 basic functionality test passed!")