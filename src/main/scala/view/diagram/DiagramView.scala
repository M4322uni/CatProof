package view.diagram

import scalafx.Includes.*
import scalafx.scene.control.{Tab, TabPane}
import view.View.{LEFT_PANE_WIDTH_RATIO, WINDOW_HEIGTH, WINDOW_WIDTH}
import view.diagram.Diagram
import view.diagram.drawables.nodes.Node
import view.diagram.drawables.{Arrow, Drawable}
import logic.derivation.*
import logic.derivation.semantics.{Category, Morphism, Object}
import logic.derivation.semantics.Object.*
import utils.Name

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DiagramView extends TabPane:

  private val addTab = new Tab:
    text = "+"
    closable = false
  private var count: Int = 1
  private val firstTab = DiagramTab(s"Diagram_$count") //TODO extend
  private[view] val bindings: mutable.Map[javafx.scene.control.Tab, Tab] = mutable.Map()

  class DiagramTab(private[view] val name: Name,
                   private[view] val diagram: Diagram = view.diagram.Diagram()) extends Tab:

    text = name
    content = diagram

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
      logic.derivation.Diagram(name, Category.Base("Cat"), temp) //TODO support for multiple categories

  bindings += firstTab.delegate -> firstTab
  tabs.addOne(firstTab)
  count += 1
  bindings += addTab.delegate -> addTab
  tabs.addOne(addTab)
  firstTab.focus()

  selectionModel().selectedItem.onChange { (_, tabFrom, tabTo) =>
    bindings.get(tabFrom) match
      case Some(x: DiagramTab) => x.unfocus()
      case _ =>

    bindings.get(tabTo) match
      case Some(tab) if tab eq addTab =>
        val newTab = DiagramTab(s"Diagram_$count") //TODO extend
        count += 1
        bindings += newTab.delegate -> newTab
        tabs.insert(tabs.size - 1, newTab)
        selectionModel().select(newTab)
      case Some(x: DiagramTab) => x.focus()
      case _ =>
  }

  prefWidth = WINDOW_WIDTH * (1.0 - LEFT_PANE_WIDTH_RATIO)
  prefHeight = WINDOW_HEIGTH

  private[view] def load(vect: Vector[(Name, ArrayBuffer[Drawable])]): Unit =
    val loaded = vect.map { (name, arr) => DiagramTab(name, Diagram(arr)) }
    tabs.clear()
    bindings.clear()
    count = vect.flatMap { (name, _) => name.toString.stripPrefix("Diagram_").toIntOption }
      .maxOption.getOrElse(0) + 1
    loaded.foreach { tab =>
      bindings += tab.delegate -> tab
      tabs.addOne(tab)
    }
    bindings += addTab.delegate -> addTab
    tabs.addOne(addTab)
