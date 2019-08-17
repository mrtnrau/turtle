package turtle

import Common.{move => cmove, _}

case class FPTurtle(
  position: Position,
  angle: Angle,
  color: Color,
  pen: Pen
)

object FPTurtle {

  val initialTurtle = FPTurtle(
    initialPosition,
    0.0,
    initialColor,
    initialPen
  )

  def move(distance: Distance)(turtle: FPTurtle): FPTurtle = {
    log(f"Move $distance%.1f")

    val position = cmove(distance, turtle.angle, turtle.position)

    if (turtle.pen == Down) {
      drawLine(log, turtle.position, position, turtle.color)
    }

    turtle.copy(position = position)
  }

  def turn(angle: Angle)(turtle: FPTurtle): FPTurtle = {
    log(f"Turn $angle%.1f")

    turtle.copy(angle = (turtle.angle + angle) % 360.0)
  }

  def penUp(turtle: FPTurtle): FPTurtle = {
    log("Pen up")

    turtle.copy(pen = Up)
  }

  def penDown(turtle: FPTurtle): FPTurtle = {
    log("Pen down")

    turtle.copy(pen = Down)
  }

  def setColor(color: Color)(turtle: FPTurtle): FPTurtle = {
    log(s"Set color to ${color.toString.toLowerCase()}")

    turtle.copy(color = color)
  }

  sealed trait Error
  final case class InvalidDistance(msg: String) extends Error
  final case class InvalidAngle   (msg: String) extends Error
  final case class InvalidColor   (msg: String) extends Error
  final case class InvalidCommand (msg: String) extends Error

  def validateDistance(distance: String): Either[Error, Distance] =
    try {
      Right(distance.toDouble)
    } catch {
      case e @ (_ : NullPointerException | _ : NumberFormatException) => {
        val msg = s"Invalid distance '$distance' [${e.getMessage()}]"
        Left(InvalidDistance(msg))
      }
    }

  def validateAngle(angle: String): Either[Error, Angle] =
    try {
      Right(angle.toDouble)
    } catch {
      case e @ (_ : NullPointerException | _ : NumberFormatException) => {
        val msg = s"Invalid angle '$angle' [${e.getMessage()}]"
        Left(InvalidAngle(msg))
      }
    }

  def validateColor(color: String): Either[Error, Color] =
    color match {
      case "Black" => Right(Black)
      case "Blue"  => Right(Blue)
      case "Red"   => Right(Red)
      case _       => {
        val msg = s"Color '$color' is not recognized"
        Left(InvalidColor(msg))
      }
    }

}
