package turtle

// API in front of an agent

object Turtle05 extends App {

  import scala.concurrent.Await
  import scala.concurrent.duration.Duration

  import akka.actor.Actor
  import akka.actor.ActorSystem
  import akka.actor.Props

  import Common.{ move => _, _ }
  import FPTurtle._

  sealed trait TurtleCommand
  final case class  Move(distance: Distance) extends TurtleCommand
  final case class  Turn(angle: Angle)       extends TurtleCommand
  final case object PenUp                    extends TurtleCommand
  final case object PenDown                  extends TurtleCommand
  final case class  SetColor(color: Color)   extends TurtleCommand

  class TurtleAgent extends Actor {
    var turtle = initialTurtle

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
        // This termination may leave dead letters because of asynchronisity
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
    val angle = 360.0 / n

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