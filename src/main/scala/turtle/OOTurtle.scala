package turtle

import Common.{move => cmove, _}

class OOTurtle(log: Log) {

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

object OOTurtle {

  final case class TurtleException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  def validateDistance(distance: String): Distance =
    try {
      distance.toDouble
    } catch {
      case e @ (_ : NullPointerException | _ : NumberFormatException) => {
        val msg = s"Invalid distance '$distance' [${e.getMessage()}]"
        throw TurtleException(msg, e)
      }
    }

  def validateAngle(angle: String): Angle =
    try {
      angle.toDouble
    } catch {
      case e @ (_ : NullPointerException | _ : NumberFormatException) => {
        val msg = s"Invalid angle '$angle' [${e.getMessage()}]"
        throw TurtleException(msg, e)
      }
    }

  def validateColor(color: String): Color =
    color match {
      case "Black" => Black
      case "Blue"  => Blue
      case "Red"   => Red
      case _       => {
        val msg = s"Color '$color' is not recognized"
        throw TurtleException(msg)
      }
    }

}