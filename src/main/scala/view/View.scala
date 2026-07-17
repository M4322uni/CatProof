package view

import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import view.diagram.DiagramView

object View extends JFXApp3:

  override def start(): Unit =
    stage = new PrimaryStage:
      title = "CatProof"
//      resizable = false
      scene = new Scene:
        private val tabs = DiagramView
        root = tabs

