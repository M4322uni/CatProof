package view.diagram.drawables

import view.diagram.drawables.nodes.Node
import scalafx.Includes.*
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.shape.ArcType
import scalafx.scene.text.{Font, Text}
import view.diagram.drawables.Arrow.*

import scala.collection.mutable.ArrayBuffer

class Arrow private(val tag: String,
                        val dom: Node,
                        val cod: Node) extends Drawable:

  private val displayText: Text = {
    val res = Text(tag)
    res.font = Font("Serif", 20)
    res
  }
  private val bounds = displayText.layoutBounds()

  private def xSignature: Double = math.tanh((cod.x - dom.x) / SMOOTHNESS_FACTOR)
  private def ySignature: Double = math.tanh((cod.y - dom.y) / SMOOTHNESS_FACTOR)

  private def drawStraight(gc: GraphicsContext): Unit =
    val dx = cod.x - dom.x - xSignature * (cod.halfWidth + dom.halfWidth + 2 * HALO)
    val dy = cod.y - dom.y - ySignature * (cod.halfHeight + dom.halfHeight + 2 * HALO)
    val angle = math.atan2(dy, dx)
    val len = math.sqrt(math.pow(dx, 2) + math.pow(dy, 2))

    gc.save()
    gc.translate(dom.x + xSignature * (dom.halfWidth + HALO),
      dom.y + ySignature * (dom.halfHeight + HALO))
    gc.rotate(math.toDegrees(angle))
    gc.fillText(displayText.text(), len / 2 - bounds.width/2, -bounds.height/2 +bounds.maxY)
    gc.strokeLine(0, 0, len, 0)
    gc.fillPolygon(
      Array(len, len - ARR_SIZE, len - ARR_SIZE, len),
      Array(0, -ARR_SIZE, ARR_SIZE, 0),
      4
    )
    gc.restore()

  private def drawCurved(gc: GraphicsContext): Unit =
    val r = 20
    val cx = dom.x - dom.halfWidth - r * 0.35
    val cy = dom.y - dom.halfHeight - r * 0.35
    val startDeg = 335
    val extentDeg = 300
    val epsilon = 7
    val endDeg = startDeg + extentDeg
    val theta = math.toRadians(endDeg)
    val ex = cx + r * math.cos(theta)
    val ey = cy - r * math.sin(theta)
    val tangentAngle = theta + math.Pi / 2

    gc.strokeArc(
      cx - r,
      cy - r,
      2 * r,
      2 * r,
      startDeg,
      extentDeg - epsilon,
      ArcType.Open
    )
    gc.save()
    gc.translate(ex, ey)
    gc.fillText(displayText.text(), -5 -bounds.width/2, -35 -bounds.height/2 +bounds.maxY)
    gc.rotate(math.toDegrees(tangentAngle))
    gc.fillPolygon(
      Array(0.0, -ARR_SIZE, -ARR_SIZE, 0.0),
      Array(0.0, -ARR_SIZE, ARR_SIZE, 0.0),
      4
    )
    gc.restore()

  override def draw(gc: GraphicsContext): Unit =
    gc.stroke = COLOR
    gc.lineWidth = WIDTH

    if !(dom eq cod) then
      drawStraight(gc)
    else
      drawCurved(gc)

  override protected def addRoutine(drawables: ArrayBuffer[Drawable]): Unit = ()

  override protected def removeRoutine(drawables: ArrayBuffer[Drawable]): Unit = ()

object Arrow:
  private val SMOOTHNESS_FACTOR: Double = 100
  private val HALO: Double = 10
  private val COLOR: Color = Color.Black
  private val WIDTH: Double = 2
  private val ARR_SIZE: Double = 8 //refactor

  def apply(tag: String, dom: Node, cod: Node) = new Arrow(tag, dom, cod)

  def unapply(v: Arrow): Some[(String, Node, Node)] =
    Some((v.tag, v.dom, v.cod))
