package net.liftweb.json.scalaz

import scalaz._
import Scalaz._
import JsonScalaz._
import net.liftweb.json._

import org.specs.Specification

object LottoExample extends Specification {

  case class Winner(winnerId: Long, numbers: List[Int])
  case class Lotto(id: Long, winningNumbers: List[Int], winners: List[Winner], drawDate: Option[String])

  val json = parse("""{"id":5,"winning-numbers":[2,45,34,23,7,5],"winners":[{"winner-id":23,"numbers":[2,45,34,23,3,5]},{"winner-id":54,"numbers":[52,3,12,11,18,22]}]}""")

  // Lotto line must have exactly 6 numbers
  def len(x: Int) = (xs: List[Int]) => 
    if (xs.length != x) Fail("len", xs.length + " != " + x) else xs.success

  implicit def winnerJSON: JSONR[Winner] =
    Winner.applyJSON(field("winner-id"), validate[List[Int]]("numbers") >=> len(6))

  implicit def lottoJSON: JSONR[Lotto] =
    Lotto.applyJSON(field("id")
                  , validate[List[Int]]("winning-numbers") >=> len(6)
                  , field("winners")
                  , field("draw-date"))
  
  val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
  val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5), winners, None)

  fromJSON[Lotto](json) mustEqual Success(lotto)
}
