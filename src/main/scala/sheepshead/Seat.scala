package sheepshead

import scalaz._
import scalaz.syntax.equal._

import net.arya.util.unsafe.unsafeListToNel

sealed trait Seat extends Product with Serializable
object Seat {
  case object S1 extends Seat
  case object S2 extends Seat
  case object S3 extends Seat
  case object S4 extends Seat
  case object S5 extends Seat

  val allSeats = Set(S1, S2, S3, S4, S5)

  implicit val seatEqual = Equal.equalRef[Seat]

  private def seatStream: EphemeralStream[Seat] =
    EphemeralStream(S1, S2, S3, S4, S5) ++ seatStream

  def seatList(start: Seat): List[Seat] =
    seatStream.dropWhile(_ ≠ start).take(5).toList

  def seatNEL(start: Seat): NonEmptyList[Seat] =
    unsafeListToNel(seatStream.dropWhile(_ ≠ start).take(5).toList)

  def playSequenceEnum(leader: Seat): Enum[Seat] = new Enum[Seat] {
    override def min: Option[Seat] = Some(leader)

    override def max: Option[Seat] = Some(seatList(leader)(4))

    override def succ(a: Seat): Seat = a match {
      case S1 ⇒ S2
      case S2 ⇒ S3
      case S3 ⇒ S4
      case S4 ⇒ S5
      case S5 ⇒ S1
    }

    override def pred(a: Seat): Seat = a match {
      case S3 ⇒ S2
      case S2 ⇒ S1
      case S4 ⇒ S3
      case S5 ⇒ S4
      case S1 ⇒ S5
    }

    override def order(x: Seat, y: Seat): Ordering = ???
  }
}