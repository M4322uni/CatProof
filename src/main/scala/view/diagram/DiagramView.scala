package view.diagram

import scalafx.Includes.*
import scalafx.scene.control.{Tab, TabPane}
import view.diagram.Diagram.*

import scala.collection.mutable

object DiagramView extends TabPane:

  private val addTab = new Tab:
    text = "+"
    closable = false
  private var count: Int = 1
  private val firstTab = DiagramTab(count)
  private val bindings: mutable.Map[javafx.scene.control.Tab, Tab] = mutable.Map()

  class DiagramTab(number: Int) extends Tab:
    private val diagram = Diagram()

    text = s"Diagram $number"
    content = diagram
    prefWidth = CANVAS_WIDTH
    prefHeight = CANVAS_HEIGHT

    def focus(): Unit =
      diagram.startRefresh()

    def unfocus(): Unit =
      diagram.stopRefresh()

  bindings += firstTab.delegate -> firstTab
  tabs.addOne(firstTab)
  count += 1
  bindings += addTab.delegate -> addTab
  tabs.addOne(addTab)
  firstTab.focus()

  selectionModel().selectedItem.onChange { (_, tabFrom, tabTo) =>
    val bound = bindings(tabTo)
    if bound eq addTab then
      val newTab = new DiagramTab(count)
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

