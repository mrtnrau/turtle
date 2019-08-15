package turtle

// API in front of an agent

object Turtle05 extends App {

  import Common.{move => cmove, _}

  import scala.concurrent.Await
  import scala.concurrent.duration.Duration

  import akka.actor.Actor
  import akka.actor.ActorSystem
  import akka.actor.Props

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

  sealed trait TurtleCommand
  final case class  Move(distance: Distance) extends TurtleCommand
  final case class  Turn(angle: Angle)       extends TurtleCommand
  final case object PenUp                    extends TurtleCommand
  final case object PenDown                  extends TurtleCommand
  final case class  SetColor(color: Color)   extends TurtleCommand

  class TurtleAgent extends Actor {
    var turtle = Turtle(
      initialPosition,
      0.0,
      initialColor,
      initialPen
    )

    def receive = {
      case Move(distance)  => turtle = move(distance)(turtle)
      case Turn(angle)     => turtle = turn(angle)(turtle)
      case PenUp           => turtle = penUp(turtle)
      case PenDown         => turtle = penDown(turtle)
      case SetColor(color) => turtle = setColor(color)(turtle)
      case _               => ()
    }
  }

  class TurtleApi {

    private val system = ActorSystem("Turtle")
    private val turtleAgent = system.actorOf(Props[TurtleAgent])

    def exec(cmd: String): Either[Error, Unit] =
      cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => validateDistance(distance).map(distance => turtleAgent ! Move(distance))
        case List("Turn", angle)     => validateAngle(angle).map(angle => turtleAgent ! Turn(angle))
        case List("Pen", "Up")       => Right(turtleAgent ! PenUp)
        case List("Pen", "Down")     => Right(turtleAgent ! PenDown)
        case List("SetColor", color) => validateColor(color).map(color => turtleAgent ! SetColor(color))
        case List("Terminate")       => Await.ready(system.terminate(), Duration.Inf); Right(())
        case _                       => {
          val msg = s"Instruction '$cmd' is not recognized"
          Left(InvalidCommand(msg))
        }
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

    api.exec("Terminate")
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

    api.exec("Terminate")
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

    api.exec("Terminate")
  }

  drawPolygon(5)

}