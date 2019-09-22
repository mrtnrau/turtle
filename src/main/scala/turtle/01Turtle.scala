package turtle

// Basic object-oriented approach

object Turtle01 extends App {

  import Common.log

  def drawTriangle(): Unit = {
    val turtle = new OOTurtle(log)

    turtle.move(100.0)
    turtle.turn(120.0)

    turtle.move(100.0)
    turtle.turn(120.0)

    turtle.move(100.0)
    turtle.turn(120.0)
  }

  def drawPolygon(n: Int): Unit = {
    val angle = 360.0 / n
    val turtle = new OOTurtle(log)

    def drawSide(): Unit = {
      turtle.move(100.0)
      turtle.turn(angle)
    }

    for (i <- 1 to n) {
      drawSide()
    }
  }

  drawPolygon(5)

}