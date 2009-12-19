package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class LottoExampleTest extends Runner(LottoExample) with JUnit
object LottoExample extends Specification {
  import JsonAST._
  import JsonDSL._

  implicit val formats = DefaultFormats

  case class Winner(`winner-id`: Long, numbers: List[Int])
  case class Lotto(id: Long, `winning-numbers`: List[Int], winners: List[Winner], 
                   `draw-date`: Option[java.util.Date])

  val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
  val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5, 3), winners, None)

  val json = 
    ("lotto" ->
      ("id" -> lotto.id) ~
      ("winning-numbers" -> lotto.`winning-numbers`) ~
      ("draw-date" -> lotto.`draw-date`.map(_.toString)) ~
      ("winners" ->
        lotto.winners.map { w =>
          (("winner-id" -> w.`winner-id`) ~
           ("numbers" -> w.numbers))}))

  compact(render(json)) mustEqual """{"lotto":{"id":5,"winning-numbers":[2,45,34,23,7,5,3],"winners":[{"winner-id":23,"numbers":[2,45,34,23,3,5]},{"winner-id":54,"numbers":[52,3,12,11,18,22]}]}}"""

  (json \ "lotto" \ "winners")(0).extract[Winner] mustEqual Winner(23, List(2, 45, 34, 23, 3, 5))

  (json \ "lotto").extract[Lotto] mustEqual lotto

  json.values mustEqual Map("lotto" -> Map("id" -> 5, "winning-numbers" -> List(2, 45, 34, 23, 7, 5, 3), "draw-date" -> None, "winners" -> List(Map("winner-id" -> 23, "numbers" -> List(2, 45, 34, 23, 3, 5)), Map("winner-id" -> 54, "numbers" -> List(52, 3, 12, 11, 18, 22)))))
}
