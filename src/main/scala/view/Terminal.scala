package view

import scalafx.Includes.*
import View.{LEFT_PANE_WIDTH_RATIO, WINDOW_HEIGTH, WINDOW_WIDTH}
import logic.derivation.procedure.graph.GoalsTree.Quality
import org.fxmisc.richtext.InlineCssTextArea

object Terminal extends InlineCssTextArea:

  setEditable(false)
  setStyle("-fx-font-family: monospace;")
  setPrefWidth(WINDOW_WIDTH * LEFT_PANE_WIDTH_RATIO)
  setPrefHeight(WINDOW_HEIGTH / 3.0)

  def display(text: Seq[(Quality, String)] | String): Unit =
    text match
      case casted: String =>
        replaceText(casted)
        setStyle(
          0,
          casted.length,
          "-fx-fill: red;"
        )
      case casted: Seq[(Quality, String)] =>
        replaceText(casted.map(_._2).mkString)
        var start = 0
        casted.foreach { (quality, part) =>
          setStyle(start, start + part.length, quality match
            case Quality.BASELINE => ""
            case Quality.PROVEN => "-fx-fill: green;"
            case Quality.UNPROVEN => "-fx-fill: blue;")
          start += part.length
        }
