package sheepshead

import org.specs2._

import brain._, Rank._, Suit._
import sheepshead.util.CardParser

import scalaz.NonEmptyList

class ParseTrickTest extends mutable.Specification {
  val nextPlayer = Seat.S1
  val parseTrick = CardParser.parseTrick(nextPlayer) _

  "parse a trick" >> {
    parseTrick("a.h t.c 7.d") shouldEqual
      Some(PreviousPlays(Trick(A(heart).ssuit,
        Seat.seatNEL(nextPlayer) zip NonEmptyList(_7(diamond), _10(club), A(heart))
      )))
  }

  "parse empty trick" >> {
    parseTrick("") shouldEqual Some(YourLead)
  }

  "parse failure" >> {
    parseTrick("foo t.c") shouldEqual None
  }
}
