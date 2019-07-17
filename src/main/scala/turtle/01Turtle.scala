package turtle

object Turtle01 extends App {

  /**
   *
   * Advantages:
   *  + Easy to implement and understand.
   *
   * Disadvantages:
   *  - Stateful code is hard to test.
   *  - Client is coupled to a particular implementation.
   *
   **/

  import Common.{move => cmove, _}

  class Turtle(log: Log) {

    var position = initialPosition
    var angle    = 0.0
    var color    = initialColor
    var pen      = initialPen

    def move(distance: Distance): Unit = {
      log(f"Move $distance%.1f")

      val position = cmove(distance, this.angle, this.position)

      if (this.pen == Down) {
        drawLine(log, this.position, position, this.color)
      }

      this.position = position
    }

    def turn(angle: Angle): Unit = {
      log(f"Turn $angle%.1f")

      this.angle = (this.angle + angle) % 360.0
    }

    def penUp(): Unit = {
      log("Pen up")

      this.pen = Up
    }

    def penDown(): Unit = {
      log("Pen down")

      this.pen = Down
    }

    def setColor(color: Color): Unit = {
      log(s"Set color to ${color.toString.toLowerCase()}")

      this.color = color
    }

  }

  def drawTriangle(): Unit = {
    val turtle = new Turtle(log)

    turtle.move(100.0)
    turtle.turn(120.0)

    turtle.move(100.0)
    turtle.turn(120.0)

    turtle.move(100.0)
    turtle.turn(120.0)
  }

  def drawPolygon(n: Int): Unit = {
    val angle = 180.0 - (n - 2) * 180.0 / n
    val turtle = new Turtle(log)

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