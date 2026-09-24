package view.diagram

import scalafx.Includes.*
import scalafx.animation.AnimationTimer
import scalafx.beans.binding.Bindings
import scalafx.geometry.Pos
import scalafx.scene.Cursor
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{ScrollPane, TextField}
import scalafx.scene.input.MouseButton.{Middle, Primary, Secondary}
import scalafx.scene.input.{KeyCode, MouseEvent}
import scalafx.scene.layout.Pane
import utils.Name
import view.diagram.Diagram.*
import view.diagram.drawables.nodes.Node
import view.diagram.drawables.{Arrow, Drawable, Line}

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ArrayBuffer

class Diagram(
               private[view] val categoryName: Name,
               private[view] val drawables: ArrayBuffer[Drawable] = ArrayBuffer()
             ) extends ScrollPane:

  private val base = new Pane()
  private val canvas = new Canvas()
  private val selected: ArrayBuffer[Drawable] = ArrayBuffer()
  private var activeLine: Option[Line] = None
  private val refresh = AnimationTimer {_ => redraw()}

  private abstract class NamePop(
                                  prompt: String,
                                  x: Double,
                                  y: Double,
                                  canvas: Canvas,
                                  val base: Pane
                                ) extends TextField:

    private val fieldWidth = 100.0
    private val closed = new AtomicBoolean(false)
    var name: Option[Name] = None

    text.onChange {
      name =
        try
          val validName = Name(text())
          style = "-fx-text-fill: black;"
          Some(validName)
        catch
          case _: IllegalArgumentException =>
            style = "-fx-text-fill: red;"
            None
    }

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
      if textField.name.nonEmpty || textField.text().isEmpty then
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
  base.minWidth <== Bindings.createDoubleBinding(
    () => viewportBounds().getWidth,
    viewportBounds
  )
  base.minHeight <== Bindings.createDoubleBinding(
    () => viewportBounds().getHeight,
    viewportBounds
  )
  canvas.widthProperty() <== base.width
  canvas.heightProperty() <== base.height

  drawables.foreach {
    case node: Node =>
      base.prefWidth = math.max(base.prefWidth(), node.x + node.halfWidth + GROWTH_MARGIN)
      base.prefHeight = math.max(base.prefHeight(), node.y + node.halfHeight + GROWTH_MARGIN)
    case line: Line =>
      base.prefWidth = math.max(base.prefWidth(), line.x + GROWTH_MARGIN)
      base.prefHeight = math.max(base.prefHeight(), line.y + GROWTH_MARGIN)
    case _ =>
  }

  canvas.onKeyPressed = ke =>
    if ke.isControlDown && ke.code == KeyCode.Z
      then deleteLastDrawable()
    else if ke.code == KeyCode.Delete
      then
        selected.foreach(_.removeFrom(drawables))
        clear(selected)

  canvas.onMousePressed = { me =>
    canvas.requestFocus()

    me.button match
      case Secondary =>
        val pop = new NamePop("Node name", me.x, me.y, canvas, base):
          override def close(): Unit =
            name.foreach { n => Node(me.x, me.y, n).addTo(drawables) }

        nameEnter(pop)

      case Middle =>
        select(me, false, false)

        selected match
          case ArrayBuffer() =>
          case _ => scene().cursor = Cursor.ClosedHand

      case Primary =>
        select(me, me.isControlDown, me.isControlDown)

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
          if me.x >= canvas.width() - GROWTH_MARGIN && canvas.width() < MAX_WIDTH
          then
            base.prefWidth = base.prefWidth() + GROWTH_MARGIN
            vvalue = 1

          n.y = me.y
          if me.y >= base.height() - GROWTH_MARGIN && canvas.height() < MAX_HEIGHT
          then
            base.prefHeight = base.prefHeight() + GROWTH_MARGIN
            hvalue = 1

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
    if !me.isControlDown then clear(selected)

    val lineToFinalize = activeLine

    activeLine = None

    lineToFinalize match
      case Some(line @ Line(d1)) =>
        select(me, true, me.isControlDown)
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

                name.foreach { n => Arrow(n, d1, d2).addTo(drawables) }

            nameEnter(pop)

          case _ =>
            line.removeFrom(drawables)

      case _ =>
    me.consume()
  }

  // add canc -> wipe selected

  private def redraw(): Unit =
    val gc = canvas.graphicsContext2D
    gc.clearRect(0, 0, canvas.width(), canvas.height())
    drawables.foreach(_.draw(gc))

  def startRefresh(): Unit = refresh.start()

  def stopRefresh(): Unit = refresh.stop()

  private def deleteLastDrawable(): Unit =
    if drawables.nonEmpty then
      drawables.remove(drawables.size - 1)

  private def select(me: MouseEvent, add: Boolean, highlight: Boolean): Unit =
    if !add then clear(selected)
    val pick = drawables.collectFirst {
      case n: Node
        if math.abs(me.x - n.x) <= n.halfWidth &&
          math.abs(me.y - n.y) <= n.halfHeight =>
        n
    }
    pick match
      case Some(x) =>
        if !selected.contains(x)
        then
          selected.addOne(x)
          if highlight then x.highlight()
      case _ =>

  private def clear(selected: ArrayBuffer[Drawable]): Unit =
    selected.foreach(_.unhighlight())
    selected.clear()

object Diagram:
//  val CANVAS_WIDTH: Double = 400
//  val CANVAS_HEIGHT: Double = 400
  private val GROWTH_MARGIN = 10
  private val MAX_WIDTH = 2500
  private val MAX_HEIGHT = 2500
