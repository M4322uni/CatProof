package view

import scalafx.Includes.*
import org.fxmisc.richtext.CodeArea
import org.fxmisc.richtext.LineNumberFactory
import View.{LEFT_PANE_WIDTH_RATIO, WINDOW_HEIGTH, WINDOW_WIDTH}
import logic.derivation.Proof
import org.fxmisc.flowless.VirtualizedScrollPane
import logic.derivation.Proof.Mode.*
import view.diagram.DiagramView
import view.diagram.DiagramView.DiagramTab

object TextInput extends VirtualizedScrollPane(
    new CodeArea("ASSUMPTIONS:\nGOALS:\nPROOF:"):
      setPrefWidth(WINDOW_WIDTH * LEFT_PANE_WIDTH_RATIO)
      setPrefHeight(WINDOW_HEIGTH * (2 / 3.0))
      setParagraphGraphicFactory(LineNumberFactory.get(this))
      setStyle(
        """
        -fx-border-color: #c9c9c9;
        -fx-border-width: 1px;
        -fx-border-radius: 2px;
        """)
    ):

  private[view] def post(): Unit =
    try
      Terminal.display(
      Proof(getContent.getText, DiagramView.tabs.map {
        tab => DiagramView.bindings(tab)
      }.collect {
        case diag: DiagramTab => diag.logicTranslate()
      }.toSeq)(
        View.DISPLAY_OPTIONS.selectedToggle.value match
        case item: javafx.scene.control.RadioMenuItem =>
          item.getText match
            case "Verbose display" => VERBOSE
            case "Only types displayed" => TYPINGS
            case "Only equations displayed" => EQUATIONS
            case "Symbolic display" => SYMBOLIC
        case _ =>
          throw IllegalArgumentException("Unexpected display mode selected")
      ))
    catch
      case f: IllegalArgumentException => Terminal.display(f.getMessage)