package turtle

object Turtle02 extends App {

  /**
   *
   * Advantages:
   *  + Easy to implement and understand.
   *  + Easier to test than 01Turtle.
   *  + The functions can be reused since there is no global state.
   *
   * Disadvantages:
   *  - Client has to keep track of the state.
   *  - Client is coupled to a particular implementation.
   *
   **/

  import Common.{move => cmove, _}

  case class Turtle(
    position: Position,
    angle: Angle,
    color: Color,
    pen: Pen
  )

  val initialTurtle = Turtle(
    initialPosition,
    0.0,
    initialColor,
    initialPen
  )

  def move(distance: Distance)(turtle: Turtle): Turtle = {
    log(f"Move $distance%.1f")

    val position = cmove(distance, turtle.angle, turtle.position)

    if (turtle.pen == Down) {
      drawLine(log, turtle.position, position, turtle.color)
    }

    turtle.copy(position = position)
  }

  def turn(angle: Angle)(turtle: Turtle): Turtle = {
    log(f"Turn $angle%.1f")

    turtle.copy(angle = (turtle.angle + angle) % 360.0)
  }

  def penUp(turtle: Turtle): Turtle = {
    log("Pen up")

    turtle.copy(pen = Up)
  }

  def penDown(turtle: Turtle): Turtle = {
    log("Pen down")

    turtle.copy(pen = Down)
  }

  def setColor(color: Color)(turtle: Turtle): Turtle = {
    log(s"Set color to ${color.toString.toLowerCase()}")

    turtle.copy(color = color)
  }

  def drawTriangle(): Turtle =
    ( move(100.0)_ andThen
      turn(120.0)  andThen
      move(100.0)  andThen
      turn(120.0)  andThen
      move(100.0)  andThen
      turn(120.0)
    )(initialTurtle)

  def drawPolygon(n: Int): Turtle = {
    val angle = 180.0 - (n - 2) * 180.0 / n

    val oneSide: Turtle => Turtle =
      move(100.0)_ andThen turn(angle)

    (1 to n).foldLeft(initialTurtle) { (t, _) => oneSide(t) }
  }

  drawPolygon(5)

}