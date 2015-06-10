package sheepshead

import org.scalatest._, prop._

import Rank._, Suit._
import sheepshead.util.CardParser

class CardParserTest
  extends PropSpec
  with TableDrivenPropertyChecks
  with Matchers
{
  val examples = Table("card",
    "a.h" → A(heart),
    "t.c" → _10(club),
    "7.diamond" → _7(diamond)
  )

  property("examples should parse correctly") {
    forAll(examples) {
      case (string, card) ⇒
        println(card)
        CardParser.simpleCard(string) should be (Some(card))
    }
  }
}
