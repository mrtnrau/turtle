package turtle

// Dependency injection using interfaces OO

object Turtle06a extends App {

  import Common.{ Distance, Angle, Color, log }
  import OOTurtle._

  trait ITurtle {

    def move(distance: Distance): Unit
    def turn(angle: Angle): Unit
    def penUp(): Unit
    def penDown(): Unit
    def setColor(color: Color): Unit

  }

  class TurtleApi(private var turtle: ITurtle) {

    def exec(cmd: String): Unit =
      cmd.split(" ").toList.map(_.trim()) match {
        case List("Move", distance)  => turtle.move(validateDistance(distance))
        case List("Turn", angle)     => turtle.turn(validateAngle(angle))
        case List("Pen", "Up")       => turtle.penUp()
        case List("Pen", "Down")     => turtle.penDown()
        case List("SetColor", color) => turtle.setColor(validateColor(color))
        case _                       => {
          val msg = s"Instruction '$cmd' is not recognized"
          throw TurtleException(msg)
        }
      }

  }

  class TurtleImpl extends ITurtle {

    protected val turtle = new OOTurtle(log)

    override def move(distance: Distance): Unit = turtle.move(distance)
    override def turn(angle: Angle): Unit       = turtle.turn(angle)
    override def penUp(): Unit                  = turtle.penUp()
    override def penDown(): Unit                = turtle.penDown()
    override def setColor(color: Color): Unit   = turtle.setColor(color)

  }

  class TurtleImplHalf extends TurtleImpl {

    override def move(distance: Distance): Unit = turtle.move(distance / 2.0)

  }

  def drawTriangle(api: TurtleApi): Unit = {
    api.exec("Move 100.0")
    api.exec("Turn 120.0")

    api.exec("Move 100.0")
    api.exec("Turn 120.0")

    api.exec("Move 100.0")
    api.exec("Turn 120.0")
  }

  def drawPolygon(api: TurtleApi, n: Int): Unit = {
    val angle = 180.0 - (n - 2) * 180.0 / n

    def drawOneSide(): Unit = {
      api.exec("Move 100.0")
      api.exec(s"Turn $angle")
    }

    for (i <- 1 to n) {
      drawOneSide()
    }
  }

  def triggerError(api: TurtleApi): Unit =
    api.exec("Move bad")

  drawPolygon(new TurtleApi(new TurtleImpl()), 5)
  drawPolygon(new TurtleApi(new TurtleImplHalf()), 5)

}