package turtle

// Basic functional approach

object Turtle02 extends App {

  import FPTurtle._

  def drawTriangle(): FPTurtle =
    ( move(100.0)_ andThen
      turn(120.0)  andThen
      move(100.0)  andThen
      turn(120.0)  andThen
      move(100.0)  andThen
      turn(120.0)
    )(initialTurtle)

  def drawPolygon(n: Int): FPTurtle = {
    val angle = 360.0 / n

    val oneSide: FPTurtle => FPTurtle =
      move(100.0)_ andThen turn(angle)

    (1 to n).foldLeft(initialTurtle) { (t, _) => oneSide(t) }
  }

  drawPolygon(5)

}