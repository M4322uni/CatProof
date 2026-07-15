package view.diagram.drawables

import scalafx.Includes.*
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import view.diagram.drawables.nodes.Node

import scala.collection.mutable.ArrayBuffer

class Line(private val anchor: Node) extends Drawable:

  var x, y = .0
  val color: Color = Color.Red
  val width: Double = 2
  private val dotted: List[Double] = List(2, 8)


  override def draw(gc: GraphicsContext): Unit =
    gc.stroke = color
    gc.lineWidth = width
    gc.setLineDashes(dotted*)
    gc.strokeLine(anchor.x, anchor.y, x, y)
    gc.setLineDashes()

  override protected def addRoutine(drawables: ArrayBuffer[Drawable]): Unit = ()

  override protected def removeRoutine(drawables: ArrayBuffer[Drawable]): Unit = ()

object Line:
  def apply(anchor: Node): Line =
    new Line(anchor)

  def unapply(d: Line): Option[Node] =
    Some(d.anchor)
