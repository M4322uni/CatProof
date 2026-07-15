package view.diagram.drawables

import scalafx.scene.canvas.GraphicsContext

import scala.collection.mutable.ArrayBuffer

trait Drawable:
  def draw(gc: GraphicsContext): Unit

  final def addTo(drawables: ArrayBuffer[Drawable]): Unit =
    addRoutine(drawables: ArrayBuffer[Drawable])
    drawables.addOne(this)

  protected def addRoutine(drawables: ArrayBuffer[Drawable]): Unit

  final def removeFrom(drawables: ArrayBuffer[Drawable]): Unit =
    val index = drawables.indexWhere { d => d eq this }
    if index >= 0 then {
      removeRoutine(drawables: ArrayBuffer[Drawable])
      drawables.remove(index)
    }

  protected def removeRoutine(drawables: ArrayBuffer[Drawable]): Unit


