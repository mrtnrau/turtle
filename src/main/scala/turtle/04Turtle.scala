package turtle

// API with functional core

object Turtle04 extends App {

  import FPTurtle._

  class TurtleApi() {

    private var turtle = initialTurtle

    private def lift2[A,B,C,E](f: A => B => C)(ea: Either[E, A])(eb: Either[E, B]): Either[E, C] =
      for {
        a <- ea
        b <- eb
      } yield f(a)(b)

    def exec(cmd: String): Either[Error, Unit] = {
      val state = Right(this.turtle)

      val newState: Either[Error, FPTurtle] = cmd.split(" ").toList.map(_.trim()) match {
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