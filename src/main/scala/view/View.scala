package view

import scalafx.Includes.*
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.image.Image
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.util.Duration
import view.diagram.DiagramView

object View extends JFXApp3:

  val WINDOW_WIDTH: Int = 900
  val WINDOW_HEIGTH: Int = 600

  override def start(): Unit =
    stage = new PrimaryStage:
      title = "CatProof"
      icons ++= Seq(
        new Image(getClass.getResourceAsStream("/icons/16x16.png")),
        new Image(getClass.getResourceAsStream("/icons/24x24.png")),
        new Image(getClass.getResourceAsStream("/icons/32x32.png")),
        new Image(getClass.getResourceAsStream("/icons/48x48.png")),
        new Image(getClass.getResourceAsStream("/icons/64x64.png")),
        new Image(getClass.getResourceAsStream("/icons/128x128.png")),
        new Image(getClass.getResourceAsStream("/icons/256x256.png"))
      )
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
        leftStructure.center = TextInput
        leftStructure.bottom = Terminal


