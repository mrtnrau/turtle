package turtle

// API with functional core

object Turtle04 extends App {

  import Common.{move => cmove, _}

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

  case class Turtle(
    position: Position,
    angle: Angle,
    color: Color,
    pen: Pen
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

  class TurtleApi() {
    private var turtle = Turtle(
      initialPosition,
      0.0,
      initialColor,
      initialPen
    )

    private def update(t: Turtle): Unit =
      turtle = t

    private def lift2[A,B,C,E](f: A => B => C)(ea: Either[E, A])(eb: Either[E, B]): Either[E, C] =
      for {
        a <- ea
        b <- eb
      } yield f(a)(b)

    def exec(cmd: String): Either[Error, Unit] = {
      val state = Right(this.turtle)

      val newState: Either[Error, Turtle] = cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => lift2(move)(validateDistance(distance))(state)
        case List("Turn", angle)     => lift2(turn)(validateAngle(angle))(state)
        case List("Pen", "Up")       => Right(penUp(turtle))
        case List("Pen", "Down")     => Right(penDown(turtle))
        case List("SetColor", color) => lift2(setColor)(validateColor(color))(state)
        case _                       => {
          val msg = s"Instruction '$cmd' is not recognized"
          Left(InvalidCommand(msg))
        }
      }

      newState.map(turtle = _)
    }

  }

  def drawTriangle(): Unit = {
    val api = new TurtleApi()

    for {
      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")
    } yield ()
  }

  def drawPolygon(n: Int): Unit = {
    val api = new TurtleApi()
    val angle = 180.0 - (n - 2) * 180.0 / n

    def drawOneSide(): Unit =
      for {
        _ <- api.exec("Move 100.0")
        _ <- api.exec(s"Turn $angle")
      } yield ()

    for (i <- 1 to n) {
      drawOneSide()
    }
  }

  def triggerError(): Unit = {
    val api = new TurtleApi()
    for {
      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move BAD")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")
    } yield ()
  }

  drawPolygon(5)

}