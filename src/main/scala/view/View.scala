package view

import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextArea}
import scalafx.scene.layout.{BorderPane, HBox}
import view.diagram.DiagramView

object View extends JFXApp3:

  override def start(): Unit =
    stage = new PrimaryStage:
      title = "CatProof"
//      resizable = false
      scene = new Scene:
        private val structure = BorderPane()
        private val leftStructure = BorderPane()
        private val tabs = DiagramView
        private val menu = HBox()
        root = structure
        structure.top = menu
        menu.alignment = Pos.CenterLeft
        menu.setPrefHeight(10)
        private val button = new Button("Diao")
        menu.children.add(button)
        button.style =
          """
          -fx-background-color: transparent;
          -fx-background-radius: 4;
          -fx-border-color: transparent;
          """
        menu.children.add(new Button("Diao"))
        structure.center = tabs
        structure.left = leftStructure
        leftStructure.center = TextArea()
        leftStructure.bottom = TextArea()

