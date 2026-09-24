package view.diagram

import scalafx.Includes.*
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane
import utils.Name

class TabDialog extends Dialog[(Name, Name)]:
  title = "New tab info"
  headerText = "Input a valid string for both fields"

  private val diagramField = new TextField()
  private val categoryField = new TextField()
  private var diagramName: Option[Name] = None
  private var categoryName: Option[Name] = None

  dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
  private val okButton = dialogPane().lookupButton(ButtonType.OK)
  okButton.disable = true
  
  diagramField.text.onChange {
    diagramName =
      try
        val name = Name(diagramField.text())
        diagramField.style = "-fx-text-fill: black;"
        Some(name)
      catch
        case _: IllegalArgumentException =>
          diagramField.style = "-fx-text-fill: red;"
          None
    okButton.disable = diagramName.isEmpty || categoryName.isEmpty
  }

  categoryField.text.onChange {
    categoryName =
      try
        val name = Name(categoryField.text())
        categoryField.style = "-fx-text-fill: black;"
        Some(name)
      catch
        case _: IllegalArgumentException =>
          categoryField.style = "-fx-text-fill: red;"
          None
    okButton.disable = diagramName.isEmpty || categoryName.isEmpty
  }

  dialogPane().content = new GridPane {
    hgap = 10
    vgap = 5

    add(new Label("Diagram name:"), 0, 0)
    add(diagramField, 1, 0)

    add(new Label("Category name:"), 0, 1)
    add(categoryField, 1, 1)
  }

  resultConverter = buttonType =>
    if (buttonType == ButtonType.OK)
      (diagramName, categoryName) match
        case (Some(v1), Some(v2)) => (v1, v2)
        case _ => null
    else
      null
