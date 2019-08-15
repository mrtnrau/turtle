package turtle

// API with object-oriented core

object Turtle03 extends App {

  import Common.{move => cmove, _}

  final case class TurtleApiException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  def validateDistance(distance: String): Distance =
    try {
      distance.toDouble
    } catch {
      case e @ (_ : NullPointerException | _ : NumberFormatException) => {
        val msg = s"Invalid distance '$distance' [${e.getMessage()}]"
        throw TurtleApiException(msg, e)
      }
    }

  def validateAngle(angle: String): Angle =
    try {
      angle.toDouble
    } catch {
      case e @ (_ : NullPointerException | _ : NumberFormatException) => {
        val msg = s"Invalid angle '$angle' [${e.getMessage()}]"
        throw TurtleApiException(msg, e)
      }
    }

  def validateColor(color: String): Color =
    color match {
      case "Black" => Black
      case "Blue"  => Blue
      case "Red"   => Red
      case _       => {
        val msg = s"Color '$color' is not recognized"
        throw TurtleApiException(msg)
      }
    }

  class Turtle(log: Log) {

    private var position = initialPosition
    private var angle    = 0.0
    private var color    = initialColor
    private var pen      = initialPen

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

  class TurtleApi() {
    private val turtle = new Turtle(log)

    def exec(cmd: String): Unit =
      cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => turtle.move(validateDistance(distance))
        case List("Turn", angle)     => turtle.turn(validateAngle(angle))
        case List("Pen", "Up")       => turtle.penUp()
        case List("Pen", "Down")     => turtle.penDown()
        case List("SetColor", color) => turtle.setColor(validateColor(color))
        case _                       => {
          val msg = s"Instruction '$cmd' is not recognized"
          throw TurtleApiException(msg)
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
    val angle = 180.0 - (n - 2) * 180.0 / n

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