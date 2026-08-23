package view

import scalafx.Includes.*
import org.fxmisc.richtext.CodeArea
import org.fxmisc.richtext.LineNumberFactory
import View.{WINDOW_HEIGTH, WINDOW_WIDTH}
import logic.derivation.Proof
import org.fxmisc.flowless.VirtualizedScrollPane
import scalafx.scene.input.{KeyCode, KeyEvent}
import view.diagram.DiagramView
import view.diagram.DiagramView.DiagramTab

object TextInput extends VirtualizedScrollPane(
    new CodeArea("ASSUMPTIONS:\nGOALS:\nPROOF:"):
      setPrefWidth(WINDOW_WIDTH / 3.0)
      setPrefHeight(WINDOW_HEIGTH * (2 / 3.0))
      setParagraphGraphicFactory(LineNumberFactory.get(this))
      setStyle(
        """
        -fx-border-color: #c9c9c9;
        -fx-border-width: 1px;
        -fx-border-radius: 2px;
        """)
    ):

  addEventFilter(KeyEvent.KeyPressed, event =>
    if event.controlDown && event.code == KeyCode.S then
      post()
      event.consume()
  )

  private def post(): Unit =
    try
      Terminal.display(Proof(getContent.getText, DiagramView.tabs.map {
        tab => DiagramView.bindings(tab)
      }.collect {
        case diag: DiagramTab => diag.logicTranslate()
      }.toSeq)().toString) // TODO
    catch
      case f: IllegalArgumentException => Terminal.display(f.getMessage, true)
