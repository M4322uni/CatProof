package view

import scalafx.Includes.*
import scalafx.scene.control.TextArea
import View.{WINDOW_HEIGTH, WINDOW_WIDTH}

object Terminal extends TextArea:

  editable = false
//  mouseTransparent = true
//  focusTraversable = false
//  style = """
//    -fx-focus-color: -fx-box-border;
//    -fx-faint-focus-color: transparent;
//  """
  prefWidth = WINDOW_WIDTH / 3.0
  prefHeight = WINDOW_HEIGTH / 3.0

  def display(text: String, error: Boolean = false): Unit =
    style = s"-fx-text-fill: ${if error then "red" else "-fx-text-inner-color"};"
    this.text = text