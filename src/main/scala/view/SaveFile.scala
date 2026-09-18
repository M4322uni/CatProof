package view

import utils.Name
import view.diagram.drawables.Drawable
import view.diagram.{Diagram, DiagramView}

import scala.collection.mutable.ArrayBuffer

case class SaveFile(text: String, diagrams: Vector[(Name, ArrayBuffer[Drawable])])
