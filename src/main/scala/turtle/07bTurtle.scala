package turtle

// Dependency injection using functions FP v2

object Turtle07b extends App {

  import Common.{ Distance, Angle, Color, log }
  import FPTurtle.{ initialTurtle, validateAngle, validateColor, validateDistance, Error }
  import util.lift2

  sealed trait TurtleCmd
  final case class  Move(distance: Distance) extends TurtleCmd
  final case class  Turn(angle: Angle)       extends TurtleCmd
  final case object PenUp                    extends TurtleCmd
  final case object PenDown                  extends TurtleCmd
  final case class  SetColor(color: Color)   extends TurtleCmd

  class TurtleApi() {
    private var turtle = initialTurtle

    def exec(turtleFn: TurtleCmd => FPTurtle => FPTurtle)(cmd: String): Either[Error, Unit] = {

      val state = Right(this.turtle)

      val newState: Either[Error, FPTurtle] = cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => lift2(turtleFn)(validateDistance(distance).map(Move(_)))(state)
        case List("Turn", angle)     => lift2(turtleFn)(validateAngle(angle).map(Turn(_)))(state)
        case List("Pen", "Up")       => Right(turtleFn(PenUp)(turtle))
        case List("Pen", "Down")     => Right(turtleFn(PenDown)(turtle))
        case List("SetColor", color) => lift2(turtleFn)(validateColor(color).map(SetColor(_)))(state)
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
      val turtleFn: TurtleCmd => FPTurtle => FPTurtle =
        turtleCmd => turtleCmd match {
          case Move(distance)  => move(distance)
          case Turn(angle)     => turn(angle)
          case PenUp           => penUp
          case PenDown         => penDown
          case SetColor(color) => setColor(color)
        }
      val api = new TurtleApi()
      api.exec(turtleFn)
    }

    def halfSize(): String => Either[Error, Unit] = {
      val moveHalf : Distance => FPTurtle => FPTurtle =
        distance => move(distance)
      val turtleFn: TurtleCmd => FPTurtle => FPTurtle =
        turtleCmd => turtleCmd match {
          case Move(distance)  => moveHalf(distance)
          case Turn(angle)     => turn(angle)
          case PenUp           => penUp
          case PenDown         => penDown
          case SetColor(color) => setColor(color)
        }
      val api = new TurtleApi()
      api.exec(turtleFn)
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
    val angle = 360.0 / n

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