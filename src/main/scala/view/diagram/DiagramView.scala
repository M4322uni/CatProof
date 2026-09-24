package view.diagram

import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.scene.control.{Alert, Tab, TabPane}
import view.View.{LEFT_PANE_WIDTH_RATIO, WINDOW_HEIGTH, WINDOW_WIDTH}
import view.diagram.Diagram
import view.diagram.drawables.nodes.Node
import view.diagram.drawables.{Arrow, Drawable}
import logic.derivation.*
import logic.derivation.semantics.{Category, Morphism, Object}
import logic.derivation.semantics.Object.*
import scalafx.scene.control.Alert.AlertType
import utils.Name

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.OptionConverters.*

object DiagramView extends TabPane:

  private val addTab = new Tab:
    text = "+"
    closable = false
  private val names: mutable.Set[Name] = mutable.Set("Example")
  private val firstTab = DiagramTab("Example", Diagram("Cat")) //TODO extend
  private[view] val bindings: mutable.Map[javafx.scene.control.Tab, Tab] = mutable.Map()

  class DiagramTab(private[view] val name: Name,
                   private[view] val diagram: Diagram) extends Tab:

    text = name
    content = diagram
    onClosed = _ =>
      unfocus()
      names -= name
      bindings.remove(delegate)

    def focus(): Unit =
      diagram.startRefresh()

    def unfocus(): Unit =
      diagram.stopRefresh()

    def logicTranslate(): logic.derivation.Diagram =
      val temp: Map[Object, Set[(Morphism, Object)]] = diagram.drawables.collect {
        case casted: Node => casted
      }.map { node => Base(node.tag) -> diagram.drawables.collect {
        case Arrow(name, dom, cod) if dom == node => (Morphism.Base(name), Base(cod.tag)) //TODO check where else to do this
      }.toSet }.toMap
      logic.derivation.Diagram(name, Category.Base(diagram.categoryName), temp)

  bindings += firstTab.delegate -> firstTab
  tabs.addOne(firstTab)
  bindings += addTab.delegate -> addTab
  tabs.addOne(addTab)
  firstTab.focus()

  selectionModel().selectedItem.onChange { (_, tabFrom, tabTo) =>
    bindings.get(tabFrom) match
      case Some(x: DiagramTab) => x.unfocus()
      case _ =>

    bindings.get(tabTo) match
      case Some(tab) if (tab eq addTab) && tabFrom != null && !tabs.exists(_.delegate == tabFrom) =>
        // Wait for tab removal to finish before clearing its automatic selection of +.
        Platform.runLater { selectionModel().clearSelection() }
      case Some(tab) if tab eq addTab =>
        if tabFrom != null && tabs.exists(_.delegate == tabFrom) then
          selectionModel().select(tabFrom)
        else selectionModel().clearSelection()
        val dialog = TabDialog()
        dialog.initOwner(scene().getWindow)
        dialog.delegate.showAndWait().toScala match
          case Some((diag, cat)) if !names.contains(diag) =>
            val newTab = DiagramTab(diag, Diagram(cat)) //TODO extend
            names += diag
            bindings += newTab.delegate -> newTab
            tabs.insert(tabs.size - 1, newTab)
            selectionModel().select(newTab)
          case Some(_) =>
            new Alert(AlertType.Error) {
              initOwner(scene().getWindow)
              title = "Error"
              headerText = "Wrong name"
              contentText = "A diagram with the same name is already " +
                "defined."
            }.showAndWait()
          case None =>
      case Some(x: DiagramTab) => x.focus()
      case _ =>
  }

  prefWidth = WINDOW_WIDTH * (1.0 - LEFT_PANE_WIDTH_RATIO)
  prefHeight = WINDOW_HEIGTH

  private[view] def load(vect: Vector[(Name, Name, ArrayBuffer[Drawable])]): Unit =
    val loaded = if vect.isEmpty then Vector(DiagramTab("Example", Diagram("Cat")))
      else vect.map { (name1, name2, arr) => DiagramTab(name1, Diagram(name2, arr)) }
    tabs.clear()
    bindings.clear()
    names.clear()
    names ++= loaded.map(_.name)
    loaded.foreach { tab =>
      bindings += tab.delegate -> tab
      tabs.addOne(tab)
    }
    bindings += addTab.delegate -> addTab
    tabs.addOne(addTab)
