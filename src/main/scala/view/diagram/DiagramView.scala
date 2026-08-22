package view.diagram

import scalafx.Includes.*
import scalafx.scene.control.{Tab, TabPane}
import view.View.{WINDOW_HEIGTH, WINDOW_WIDTH}
import view.diagram.drawables.nodes.Node
import view.diagram.drawables.{Arrow, Drawable}
import logic.derivation.*
import logic.derivation.semantics.{Category, Morphism, Object}
import logic.derivation.semantics.Object.*
import utils.Name

import scala.collection.mutable

object DiagramView extends TabPane:

  private val addTab = new Tab:
    text = "+"
    closable = false
  private var count: Int = 1
  private val firstTab = DiagramTab(s"Diagram_$count") //TODO extend
  private[view] val bindings: mutable.Map[javafx.scene.control.Tab, Tab] = mutable.Map()

  class DiagramTab(name: String) extends Tab:
    private[view] val diagram = view.diagram.Diagram()

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
        case Arrow(name, dom, cod) if dom == cod => (Morphism.Base(name), Base(cod.tag)) //TODO check where else to do this
      }.toSet }.toMap
      logic.derivation.Diagram(name, Category.Base("Cat"), temp) //TODO support for multiple categories

  bindings += firstTab.delegate -> firstTab
  tabs.addOne(firstTab)
  count += 1
  bindings += addTab.delegate -> addTab
  tabs.addOne(addTab)
  firstTab.focus()

  selectionModel().selectedItem.onChange { (_, tabFrom, tabTo) =>
    val bound = bindings(tabTo)
    if bound eq addTab then
      val newTab = new DiagramTab(s"Diagram_$count") //TODO extend
      count += 1
      bindings += newTab.delegate -> newTab
      tabs.insert(tabs.size - 1, newTab)
      selectionModel().select(newTab)

    bound match
      case x : DiagramTab => x.focus()
      case _ =>

    bindings(tabFrom) match
      case x : DiagramTab => x.unfocus()
      case _ =>
  }

  prefWidth = WINDOW_WIDTH * (2 / 3.0)
  prefHeight = WINDOW_HEIGTH

