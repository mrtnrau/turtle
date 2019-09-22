package turtle

// Batch processing using command objects

object Turtle09 extends App {

  import Common.{ move => _, _ }
  import FPTurtle._

  sealed trait TurtleCmd
  final case class  Move(distance: Distance) extends TurtleCmd
  final case class  Turn(angle: Angle)       extends TurtleCmd
  final case object PenUp                    extends TurtleCmd
  final case object PenDown                  extends TurtleCmd
  final case class  SetColor(color: Color)   extends TurtleCmd

  def exec(state: FPTurtle, cmd: TurtleCmd): FPTurtle =
    cmd match {
      case Move(distance)  => move(distance)(state)
      case Turn(angle)     => turn(angle)(state)
      case PenUp           => penUp(state)
      case PenDown         => penDown(state)
      case SetColor(color) => setColor(color)(state)
    }

  def execAll(cmds: List[TurtleCmd]): FPTurtle =
    cmds.foldLeft(initialTurtle)(exec)

  def drawTriangle(): Unit = {
    val cmds = List(
      Move(100.0),
      Turn(120.0),
      Move(100.0),
      Turn(120.0),
      Move(100.0),
      Turn(120.0)
    )
    execAll(cmds)
  }

  def drawPolygon(n: Int): Unit = {
    val angle = 360.0 / n
    val side = List(
      Move(100.0),
      Turn(angle)
    )
    val cmds = List.fill(n)(side).flatten
    execAll(cmds)
  }

  drawPolygon(5)

}