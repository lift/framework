package net.liftweb.json.scalaz

import scala.language.reflectiveCalls

import scalaz._
import scalaz.syntax.validation._
import JsonScalaz._
import net.liftweb.json._

import org.specs2.mutable.Specification

object LottoExample extends Specification {

  case class Winner(winnerId: Long, numbers: List[Int])
  case class Lotto(id: Long, winningNumbers: List[Int], winners: List[Winner], drawDate: Option[String])

  val json = parse("""{"id":5,"winning-numbers":[2,45,34,23,7,5],"winners":[{"winner-id":23,"numbers":[2,45,34,23,3,5]},{"winner-id":54,"numbers":[52,3,12,11,18,22]}]}""")

  // Lotto line must have exactly 6 numbers
  def len(x: Int) = (xs: List[Int]) => 
    if (xs.length != x) Fail("len", xs.length + " != " + x) else xs.success

  implicit def winnerJSON: JSONR[Winner] = {
    val numbersResult = (jValue: JValue) => for {
      numbers <- field[List[Int]]("numbers")(jValue)
      _ <- len(6)(numbers)
    } yield numbers
    Winner.applyJSON(field[Long]("winner-id"), numbersResult)
  }

  implicit def lottoJSON: JSONR[Lotto] = {
    val winningNumbersResult = (jValue: JValue) => for {
      winningNumbers <- field[List[Int]]("winning-numbers")(jValue)
      _ <- len(6)(winningNumbers)
    } yield winningNumbers
    Lotto.applyJSON(field[Long]("id")
                  , winningNumbersResult
                  , field[List[Winner]]("winners")
                  , field[Option[String]]("draw-date"))
  }
  
  val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
  val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5), winners, None)

  "Parse Lotto" in {
    fromJSON[Lotto](json) mustEqual Success(lotto)
  }

}
