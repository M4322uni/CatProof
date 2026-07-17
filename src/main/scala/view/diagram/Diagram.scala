package view.diagram

import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.Cursor
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{ScrollPane, TextField}
import scalafx.scene.input.MouseButton.{Middle, Primary, Secondary}
import scalafx.scene.input.{KeyCode, MouseEvent}
import scalafx.scene.layout.Pane
import view.diagram.drawables.nodes.Node
import view.diagram.drawables.{Arrow, Drawable, Line}

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ArrayBuffer

class Diagram extends ScrollPane:
  private var canvasWidth: Double = 540 // updated when object exits bounds
  private var canvasHeight: Double = 540 // ditto
  private val base = new Pane()
  private val canvas = new Canvas()
  private val drawables: ArrayBuffer[Drawable] = ArrayBuffer()
  private val selected: ArrayBuffer[Drawable] = ArrayBuffer()
  private var activeLine: Option[Line] = None

  private abstract class NamePop(
                                  prompt: String,
                                  x: Double,
                                  y: Double,
                                  canvas: Canvas,
                                  val base: Pane
                                ) extends TextField:

    private val fieldWidth = 100.0
    private val closed = new AtomicBoolean(false)

    promptText = prompt
    prefWidth = fieldWidth
    layoutX = x - fieldWidth / 2
    layoutY = y - 12
    alignment = Pos.Center

    def endText(): Unit =
      if !closed.getAndSet(true) then
        close()
        base.children.remove(this)
        canvas.requestFocus()

    def close(): Unit

  private def nameEnter(textField: NamePop): Unit =
    textField.base.children.add(textField)
    textField.requestFocus()

    textField.onAction = _ =>
      textField.endText()

    textField.focused.onChange { (_, _, hasFocus) =>
      if !hasFocus then
        textField.endText()
    }

  content = base
  pannable = true
  base.children.add(canvas)
  base.style = "-fx-background-color: white;"
  canvas.style = "-fx-background-color: white;"
  base.prefHeight = canvasHeight
  base.prefWidth = canvasWidth
  canvas.widthProperty() <== base.width
  canvas.heightProperty() <== base.height

  canvas.onKeyPressed = ke =>
    if ke.isControlDown && ke.code == KeyCode.Z && drawables.nonEmpty
    then deleteLastDrawable()

  canvas.onMousePressed = { me =>
    canvas.requestFocus()

    me.button match
      case Secondary =>
        val pop = new NamePop("Node name", me.x, me.y, canvas, base):
          override def close(): Unit =
            val name = text().trim

            if name.nonEmpty then
              Node(me.x, me.y, name).addTo(drawables)

        nameEnter(pop)

      case Middle =>
        select(me, false)

        selected match
          case ArrayBuffer() =>
          case _ => scene().cursor = Cursor.ClosedHand

      case Primary =>
        select(me, me.isControlDown)

        if !me.isControlDown then
          selected match
            case ArrayBuffer(n: Node) =>
              val line = Line(n)

              line.x = me.x
              line.y = me.y

              activeLine = Some(line)
              drawables.addOne(line)

            case _ =>

      case _ =>
    me.consume()
  }

  canvas.onMouseDragged = { me =>
    if me.isMiddleButtonDown then
      selected match
        case ArrayBuffer(n: Node) =>
          n.x = me.x
          n.y = me.y
        case _ =>

    else if me.isPrimaryButtonDown then
      activeLine match
        case Some(line) =>
          line.x = me.x
          line.y = me.y
        case None =>

    me.consume()
  }

  canvas.onMouseReleased = { me =>
    scene().cursor = Cursor.Default
    if !me.isControlDown then selected.clear()

    val lineToFinalize = activeLine

    activeLine = None

    lineToFinalize match
      case Some(line @ Line(d1)) =>
        select(me, true)
        selected match
          case ArrayBuffer(d2: Node) =>
            val pop = new NamePop(
              "Arrow name",
              (d1.x + d2.x) / 2,
              (d1.y + d2.y) / 2,
              canvas,
              base
            ):
              override def close(): Unit =
                line.removeFrom(drawables)

                val name = text().trim

                if name.nonEmpty then
                  Arrow(name, d1, d2).addTo(drawables)

            nameEnter(pop)

          case _ =>
            line.removeFrom(drawables)

      case _ =>
    me.consume()
  }

  // add canc -> wipe selected (+ nodes recursively delete their arrows)

  def redraw(): Unit =
    val gc = canvas.graphicsContext2D
    gc.clearRect(0, 0, canvas.width(), canvas.height())
    drawables.foreach(_.draw(gc))

  private def deleteLastDrawable(): Unit =
    drawables.remove(drawables.size - 1)

  private def select(me: MouseEvent, add: Boolean): Unit =
    if !add then selected.clear()
    val pick = drawables.collectFirst {
      case n: Node
        if math.abs(me.x - n.x) <= n.halfWidth &&
          math.abs(me.y - n.y) <= n.halfHeight =>
        n
    }
    pick match
      case Some(x) =>
        if !selected.contains(x)
        then selected.addOne(x)
      case _ =>

object Diagram:
  ???
