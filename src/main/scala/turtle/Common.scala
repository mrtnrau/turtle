package turtle

object common {

  type Distance = Double
  type Angle    = Double
  type Log      = String => Unit

  sealed trait Pen
  final case object Up   extends Pen
  final case object Down extends Pen

  sealed trait Color
  final case object Black extends Color
  final case object Red   extends Color
  final case object Blue  extends Color

  final case class Position(x: Double, y: Double) {
    override def toString(): String =
      f"($x%.2f, $y%.2f)"
  }

  final case class Turtle(
    position: Position,
    color: Color,
    pen: Pen
  )

  val initial = Turtle(
    Position(0.0, 0.0),
    Black,
    Down
  )

  def move(distance: Distance, angle: Angle, position: Position): Position = {
    val angleInRads = angle * (Math.PI / 180.0)

    val x0 = position.x
    val y0 = position.y

    val x1 = x0 + (distance * Math.cos(angleInRads))
    val y1 = y0 + (distance * Math.sin(angleInRads))

    Position(x1, y1)
  }

  def drawLine(log: Log, oldPos: Position, newPos: Position, color: Color): Unit =
    log(s"... draw ${color.toString.toLowerCase} line from $oldPos to $newPos")

}