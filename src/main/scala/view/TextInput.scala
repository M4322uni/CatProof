package view

import scalafx.Includes.*
import scalafx.scene.control.TextArea
import View.{WINDOW_HEIGTH, WINDOW_WIDTH}

object TextInput extends TextArea:

  prefWidth = WINDOW_WIDTH / 3.0
  prefHeight = WINDOW_HEIGTH * (2 / 3.0)

  def getText: String = text()