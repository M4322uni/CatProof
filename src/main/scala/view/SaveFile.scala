// SPDX-License-Identifier: AGPL-3.0-only
// Copyright (C) 2026 Matteo Piscitello

package view

import utils.Name
import view.diagram.drawables.Drawable
import view.diagram.{Diagram, DiagramView}

import scala.collection.mutable.ArrayBuffer

case class SaveFile(text: String, diagrams: Vector[(Name, Name, ArrayBuffer[Drawable])])
