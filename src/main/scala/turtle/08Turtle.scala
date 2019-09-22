package turtle

// Batch processing using a state monad

object Turtle08 extends App {

  import Common._

  class TurtleState[A] private (private val run: FPTurtle => (A, FPTurtle)) {
    def runS(state: FPTurtle): (A, FPTurtle) =
      run(state)

    def map[B](f: A => B): TurtleState[B] =
      TurtleState {
        state0 => {
          val (a, state1) = run(state0)
          (f(a), state1)
        }
      }

    def flatMap[B](f: A => TurtleState[B]): TurtleState[B] =
      TurtleState {
        state0 => {
          val (a, state1) = run(state0)
          f(a).run(state1)
        }
      }
  }

  object TurtleState {
    private def apply[A](run: FPTurtle => (A, FPTurtle)): TurtleState[A] =
      new TurtleState(run)

    def apply[A](a: A): TurtleState[A] =
      new TurtleState(state => (a, state))

    def lift(f: FPTurtle => FPTurtle): TurtleState[Unit] =
      new TurtleState(state => ((), f(state)))
  }

  object Client {
    val move: Distance => TurtleState[Unit] =
      distance => TurtleState.lift(FPTurtle.move(distance))

    val turn: Angle => TurtleState[Unit] =
      angle => TurtleState.lift(FPTurtle.turn(angle))

    val penDown: TurtleState[Unit] =
      TurtleState.lift(FPTurtle.penDown)

    val penUp: TurtleState[Unit] =
      TurtleState.lift(FPTurtle.penUp)

    val setColor: Color => TurtleState[Unit] =
      color => TurtleState.lift(FPTurtle.setColor(color))
  }

  def drawTriangle(): Unit = {
    import Client._

    val turtleBatch =
      for {
        _ <- move(100.0)
        _ <- turn(120.0)
        _ <- move(100.0)
        _ <- turn(120.0)
        _ <- move(100.0)
        _ <- turn(120.0)
      } yield ()

    turtleBatch.runS(FPTurtle.initialTurtle)
  }

  def drawPolygon(n: Int): Unit = {
    import Client._

    val angle = 360.0 / n

    val sideBatch: TurtleState[Unit] =
      for {
        _ <- move(100.0)
        _ <- turn(angle)
      } yield ()

    val chain: (TurtleState[Unit], TurtleState[Unit]) => TurtleState[Unit] =
      (f, g) =>
        for {
          _ <- f
          _ <- g
        } yield ()

    val turtleBatch = List.fill(n)(sideBatch).reduce(chain)

    turtleBatch.runS(FPTurtle.initialTurtle)
  }

  drawPolygon(5)

}