package turtle

// API with object-oriented core

object Turtle03 extends App {

  import Common.log
  import OOTurtle._

  class TurtleApi() {
    private val turtle = new OOTurtle(log)

    def exec(cmd: String): Unit =
      cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => turtle.move(validateDistance(distance))
        case List("Turn", angle)     => turtle.turn(validateAngle(angle))
        case List("Pen", "Up")       => turtle.penUp()
        case List("Pen", "Down")     => turtle.penDown()
        case List("SetColor", color) => turtle.setColor(validateColor(color))
        case _                       => {
          val msg = s"Instruction '$cmd' is not recognized"
          throw TurtleException(msg)
        }
      }
  }

  def drawTriangle(): Unit = {
    val api = new TurtleApi()

    api.exec("Move 100.0")
    api.exec("Turn 120.0")

    api.exec("Move 100.0")
    api.exec("Turn 120.0")

    api.exec("Move 100.0")
    api.exec("Turn 120.0")
  }

  def drawPolygon(n: Int): Unit = {
    val api = new TurtleApi()
    val angle = 360.0 / n

    def drawOneSide(): Unit = {
      api.exec("Move 100.0")
      api.exec(s"Turn $angle")
    }

    for (i <- 1 to n) {
      drawOneSide()
    }
  }

  def triggerError(): Unit = {
    val api = new TurtleApi()
    api.exec("Move bad")
  }

  drawPolygon(5)

}