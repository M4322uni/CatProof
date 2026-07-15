package view.diagram.drawables.nodes

import view.diagram.drawables.Drawable

import scalafx.Includes.*
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}

import scala.collection.mutable.ArrayBuffer

class Node(var x: Double,
               var y: Double,
               val tag: String,
              ) extends Drawable:

  private val displayText: Text = {
    val res = Text(tag)
    res.font = Font("Serif", 20)
    res
  }
  private val bounds = displayText.layoutBounds()
  var color: Color = Color.Black

  //  override def produce(commuteSection: Set[Arrow]): Set[Arrow]
  //    = component.produce(commuteSection)

  def fontFamily: String = displayText.font().getFamily

  def fontFamily_=(family: String): Unit =
    displayText.font = Font(family, displayText.font().getSize)

  def pointSize: Double = displayText.font().size

  def pointSize_=(p: Double): Unit =
    displayText.font = Font(displayText.font().family, p)

  def halfWidth: Double =
    displayText.layoutBounds().width / 2

  def halfHeight: Double =
    displayText.layoutBounds().height / 2

  override def draw(gc: GraphicsContext): Unit =
    gc.fill = color
    gc.font = displayText.font()
    //gc.setTextAntialiasing(scalafx.scene.text.TextAntialiasing.ON)
    gc.fillText(displayText.text(),
      x - bounds.width/2,
      y + bounds.height/2 - bounds.maxY)

  override protected def addRoutine(drawables: ArrayBuffer[Drawable]): Unit = ()

  override protected def removeRoutine(drawables: ArrayBuffer[Drawable]): Unit = ()

object Node:
  def apply(x: Double, y: Double, tag: String): Node =
    new Node(x, y, tag)

  def unapply(v: Node): Option[(Double, Double, String)] =
    Some((v.x, v.y, v.tag))
