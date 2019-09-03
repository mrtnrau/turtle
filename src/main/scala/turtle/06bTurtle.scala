package turtle

// Dependency injection using interfaces FP

object Turtle06b extends App {

  import Common.{ Distance, Angle, Color, log }
  import FPTurtle.{ initialTurtle, validateAngle, validateColor, validateDistance, Error }
  import util.lift2

  trait TurtleFunctions {

    val move    : Distance => FPTurtle => FPTurtle
    val turn    : Angle    => FPTurtle => FPTurtle
    val penUp   : FPTurtle => FPTurtle
    val penDown : FPTurtle => FPTurtle
    val setColor: Color    => FPTurtle => FPTurtle

  }

  class TurtleApi(turtleFunctions: TurtleFunctions) {

    private var turtle = initialTurtle

    def exec(cmd: String): Either[Error, Unit] = {
      val state = Right(this.turtle)

      val newState: Either[Error, FPTurtle] = cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => lift2(turtleFunctions.move)(validateDistance(distance))(state)
        case List("Turn", angle)     => lift2(turtleFunctions.turn)(validateAngle(angle))(state)
        case List("Pen", "Up")       => Right(turtleFunctions.penUp(turtle))
        case List("Pen", "Down")     => Right(turtleFunctions.penDown(turtle))
        case List("SetColor", color) => lift2(turtleFunctions.setColor)(validateColor(color))(state)
        case _                       => {
          val msg = s"Instruction '$cmd' is not recognized"
          Left(FPTurtle.InvalidCommand(msg))
        }
      }

      newState.map(turtle = _)
    }

  }

  class TurtleImpl extends TurtleFunctions {

    override val move    : Distance => FPTurtle => FPTurtle = FPTurtle.move
    override val turn    : Angle    => FPTurtle => FPTurtle = FPTurtle.turn
    override val penUp   : FPTurtle => FPTurtle             = FPTurtle.penUp
    override val penDown : FPTurtle => FPTurtle             = FPTurtle.penDown
    override val setColor: Color    => FPTurtle => FPTurtle = FPTurtle.setColor

  }

  class TurtleImplHalf extends TurtleImpl {

    override val move: Distance => FPTurtle => FPTurtle =
      distance => FPTurtle.move(distance / 2.0)

  }

  def drawTriangle(api: TurtleApi): Unit = {
    for {
      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")
    } yield ()
  }

  def drawPolygon(api: TurtleApi, n: Int): Unit = {
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

  def triggerError(api: TurtleApi): Unit =
    for {
      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move BAD")
      _ <- api.exec("Turn 120.0")

      _ <- api.exec("Move 100.0")
      _ <- api.exec("Turn 120.0")
    } yield ()

  drawPolygon(new TurtleApi(new TurtleImpl()),5)
  drawPolygon(new TurtleApi(new TurtleImplHalf()),5)

}