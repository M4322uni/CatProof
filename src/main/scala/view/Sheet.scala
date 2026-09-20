package view

import scalafx.Includes.*
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.ScrollPane
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.VBox
import javafx.scene.input.{KeyCode, KeyEvent, ScrollEvent}

class Sheet extends ScrollPane:
  val OFFSET: Int = 40
  val PAGES: Int = 1
  val MIN_ZOOM: Double = 0.25
  val MAX_ZOOM: Double = 3.0
  val ZOOM_STEP: Double = 0.1

  private val zoom = DoubleProperty(0.305)

  private val pages = new VBox:
    minWidth = 0
    padding = Insets(OFFSET / 2.0)
    spacing = 12
    alignment = Pos.TopCenter

  pages.children ++= (1 to PAGES).map { n =>
    new ImageView(
      new Image(getClass.getResource(s"/rules/page-$n.png").toExternalForm)
    ):
      preserveRatio = true
      fitWidth <== image().width * zoom
  }

  content = pages
  pannable = true
  focusTraversable = true

  delegate.addEventFilter(ScrollEvent.SCROLL, event =>
    if event.isControlDown then
      zoom.value = math.max(MIN_ZOOM, math.min(MAX_ZOOM,
        zoom() + math.signum(event.getDeltaY) * ZOOM_STEP))
      event.consume()
  )

  delegate.addEventFilter(KeyEvent.KEY_PRESSED, event =>
    if event.isControlDown then
      val direction = event.getCode match
        case KeyCode.PLUS | KeyCode.EQUALS | KeyCode.ADD => 1
        case KeyCode.MINUS | KeyCode.SUBTRACT => -1
        case _ => 0
      if direction != 0 then
        zoom.value = math.max(MIN_ZOOM, math.min(MAX_ZOOM,
          zoom() + direction * ZOOM_STEP))
        event.consume()
  )
