package view.diagram.drawables

import scalafx.scene.canvas.GraphicsContext
import scala.collection.mutable.ArrayBuffer

abstract class Drawable:
  private var highlighted = false

  final def highlight(): Unit =
    highlighted = true

  final def unhighlight(): Unit =
    highlighted = false

  final def draw(gc: GraphicsContext): Unit =
    drawActual(gc, highlighted)

  protected def drawActual(gc: GraphicsContext, highlighted: Boolean): Unit

  final def addTo(drawables: ArrayBuffer[Drawable]): Unit =
    addRoutine(drawables)
    drawables.addOne(this)

  protected def addRoutine(drawables: ArrayBuffer[Drawable]): Unit

  final def removeFrom(drawables: ArrayBuffer[Drawable]): Unit =
    val index = drawables.indexWhere { d => d eq this }
    if index >= 0 then {
      removeRoutine(drawables)
      drawables.remove(index)
    }

  protected def removeRoutine(drawables: ArrayBuffer[Drawable]): Unit


