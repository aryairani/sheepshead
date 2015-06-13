package sheepshead

import org.specs2.mutable.Specification
import sheepshead.Seat._
import sheepshead.brain.Partnership._
import sheepshead.brain.{Picker, Brain}, Picker._

//class GameTest1 extends Specification {
//  "Play a little game" >> {

object GameTest1 extends App {
  locally {
    val (h1, h2, h3, h4, h5, (b1, b2)) = deal5
    (1 to 5) zip List(h1, h2, h3, h4, h5) foreach {
      case (i, h) ⇒
        println(s"Player $i: ${h.toSet.map(_.formatted("%4s")).mkString}")

    }
    println
    println(s"Blind   :   $b1 $b2")
    println

    val brains = List(
      Brain(S1, h1, Nil, Leaster, Seat.allSeats.map(_ → Opponent).toMap + (S1 → Self)),
      Brain(S2, h2, Nil, Leaster, Seat.allSeats.map(_ → Opponent).toMap + (S2 → Self)),
      Brain(S3, h3, Nil, Leaster, Seat.allSeats.map(_ → Opponent).toMap + (S3 → Self)),
      Brain(S4, h4, Nil, Leaster, Seat.allSeats.map(_ → Opponent).toMap + (S4 → Self)),
      Brain(S5, h5, Nil, Leaster, Seat.allSeats.map(_ → Opponent).toMap + (S5 → Self))
    ).map(b ⇒ b.mySeat → b).toMap

    // do more things;  have them play each other

  }

}
