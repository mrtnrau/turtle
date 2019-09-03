package turtle

// Dependency injection using functions FP v1

object Turtle07a extends App {

  import Common.{ Distance, Angle, Color, log }
  import FPTurtle.{ initialTurtle, validateAngle, validateColor, validateDistance, Error }
  import util.lift2

  class TurtleApi() {

    private var turtle = initialTurtle

    def exec(move    : Distance => FPTurtle => FPTurtle)
            (turn    : Angle    => FPTurtle => FPTurtle)
            (penUp   : FPTurtle => FPTurtle)
            (penDown : FPTurtle => FPTurtle)
            (setColor: Color    => FPTurtle => FPTurtle)
            (cmd: String): Either[Error, Unit] = {

      val state = Right(this.turtle)

      val newState: Either[Error, FPTurtle] = cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => lift2(move)(validateDistance(distance))(state)
        case List("Turn", angle)     => lift2(turn)(validateAngle(angle))(state)
        case List("Pen", "Up")       => Right(penUp(turtle))
        case List("Pen", "Down")     => Right(penDown(turtle))
        case List("SetColor", color) => lift2(setColor)(validateColor(color))(state)
        case _                       => {
          val msg = s"Instruction '$cmd' is not recognized"
          Left(FPTurtle.InvalidCommand(msg))
        }
      }

      newState.map(turtle = _)
    }

  }

  object TurtleImpl {

    val move    : Distance => FPTurtle => FPTurtle = FPTurtle.move
    val turn    : Angle    => FPTurtle => FPTurtle = FPTurtle.turn
    val penUp   : FPTurtle => FPTurtle             = FPTurtle.penUp
    val penDown : FPTurtle => FPTurtle             = FPTurtle.penDown
    val setColor: Color    => FPTurtle => FPTurtle = FPTurtle.setColor

    def normalSize(): String => Either[Error, Unit] = {
      val api = new TurtleApi()
      api.exec(move)(turn)(penUp)(penDown)(setColor)
    }

    def halfSize(): String => Either[Error, Unit] = {
      val moveHalf : Distance => FPTurtle => FPTurtle =
        distance => move(distance)
      val api = new TurtleApi()
      api.exec(moveHalf)(turn)(penUp)(penDown)(setColor)
    }

  }

  type Api = String => Either[Error, Unit]

  def drawTriangle(api: Api): Unit = {
    for {
      _ <- api("Move 100.0")
      _ <- api("Turn 120.0")

      _ <- api("Move 100.0")
      _ <- api("Turn 120.0")

      _ <- api("Move 100.0")
      _ <- api("Turn 120.0")
    } yield ()
  }

  def drawPolygon(api: Api, n: Int): Unit = {
    val angle = 180.0 - (n - 2) * 180.0 / n

    def drawOneSide(): Unit =
      for {
        _ <- api("Move 100.0")
        _ <- api(s"Turn $angle")
      } yield ()

    for (i <- 1 to n) {
      drawOneSide()
    }
  }

  def triggerError(api: Api): Unit =
    for {
      _ <- api("Move 100.0")
      _ <- api("Turn 120.0")

      _ <- api("Move BAD")
      _ <- api("Turn 120.0")

      _ <- api("Move 100.0")
      _ <- api("Turn 120.0")
    } yield ()

  drawPolygon(TurtleImpl.normalSize, 5)
  drawPolygon(TurtleImpl.halfSize, 5)

}