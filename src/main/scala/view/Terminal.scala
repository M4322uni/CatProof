package view

import scalafx.Includes.*
import scalafx.scene.control.TextArea
import View.{WINDOW_HEIGTH, WINDOW_WIDTH}

object Terminal extends TextArea:

  editable = false
  mouseTransparent = true
  focusTraversable = false
  prefWidth = WINDOW_WIDTH / 3.0
  prefHeight = WINDOW_HEIGTH / 3.0

  def display(text: String): Unit =
    this.text = text