package view

import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.{Button, MenuButton, MenuItem, RadioMenuItem, SplitPane, ToggleGroup}
import scalafx.scene.image.Image
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{FileChooser, Stage}
import scalafx.stage.FileChooser.ExtensionFilter
import view.diagram.DiagramView
import view.diagram.DiagramView.DiagramTab

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import scala.util.Using

object View extends JFXApp3:

  val WINDOW_WIDTH: Int = 900
  val WINDOW_HEIGTH: Int = 600
  val A4_RATIO: Double = 210.0 / 297.0
  val LEFT_PANE_WIDTH_RATIO: Double = 1.0 / 3.0
  val DISPLAY_OPTIONS: ToggleGroup = ToggleGroup()
  var output: Option[java.io.File] = None

  private def choose(save: Boolean = true): File =
    val chooser = new FileChooser()
    chooser.initialDirectory = java.io.File(System.getProperty("user.home"))
    chooser.title = "Save File"
    chooser.extensionFilters += ExtensionFilter("Save Files", "*.sav")
    if save then chooser.showSaveDialog(stage)
    else chooser.showOpenDialog(stage)

  private def saveAs(): Unit =
    val selectedFile = choose()
    if selectedFile != null then
      output = Some (
        if (selectedFile.getName.toLowerCase.endsWith(".sav"))
          selectedFile
        else
          new java.io.File(selectedFile.getAbsolutePath + ".sav")
      )
      save()

  private def save(): Unit =
    output match
      case None => saveAs()
      case Some(value) =>
        val save = SaveFile(TextInput.getContent.getText,
          DiagramView.tabs.map {
            tab => DiagramView.bindings(tab)
          }.collect {
            case diag: DiagramTab => (diag.name, diag.diagram.drawables)
          }.toVector )
        val out = ObjectOutputStream(
          FileOutputStream(value)
        )
        try out.writeObject(save)
        finally out.close()
        TextInput.post()

  private def load(): Unit =
    val selectedFile = choose(false)
    if selectedFile != null then
      Using.Manager { use =>
        val file = use(FileInputStream(selectedFile))
        val load = use(ObjectInputStream(file))
        val SaveFile(text, diag) = load.readObject().asInstanceOf[SaveFile]
        require(text != null && diag != null, "Invalid save file")
        DiagramView.load(diag)
        TextInput.getContent.replaceText(text)
        output = Some(selectedFile)
      }.failed.foreach { error =>
        Terminal.display(s"Unable to load file: ${error.getMessage}")
      }
      TextInput.post()

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
        structure.addEventFilter(KeyEvent.KeyPressed, event =>
          if event.controlDown && event.code == KeyCode.S then
            save()
            event.consume()
        )

        private val leftStructure = BorderPane()
        private val tabs = DiagramView
        private val menu = HBox()
        root = structure
        structure.top = menu
        menu.alignment = Pos.CenterLeft
        menu.setPrefHeight(10)
        private val file = new MenuButton("File")
        private val saveAsB = new MenuItem("Save as")
        saveAsB.onAction = _ => { saveAs() }
        private val saveB = new MenuItem("Save")
        saveB.onAction = _ => { save() }
        private val loadB = new MenuItem("Load")
        loadB.onAction = _ => { load() }
        file.items.addAll(
          saveAsB,
          saveB,
          loadB
        )
        menu.children.add(file)
        file.style =
          """
          -fx-background-color: transparent;
          -fx-background-radius: 4;
          -fx-border-color: transparent;
          """

        private val sheet = new Button("Sheet")
        sheet.onAction = _ => {
          new Stage:
            title = "Cheat sheet"
            initOwner(View.stage)

            scene = new Scene(WINDOW_HEIGTH * A4_RATIO,
              WINDOW_HEIGTH):
              root = Sheet()
          .show()
        }
        menu.children.add(sheet)
        sheet.style =
          """
          -fx-background-color: transparent;
          -fx-background-radius: 4;
          -fx-border-color: transparent;
          """

        private val options = MenuButton("Options")
        private val verbose = new RadioMenuItem("Verbose display"):
          toggleGroup = DISPLAY_OPTIONS
          selected = true
        private val types = new RadioMenuItem("Only types displayed"):
          toggleGroup = DISPLAY_OPTIONS
        private val equations = new RadioMenuItem("Only equations displayed"):
          toggleGroup = DISPLAY_OPTIONS
        private val symbolic = new RadioMenuItem("Symbolic display"):
          toggleGroup = DISPLAY_OPTIONS
        options.items.addAll(
          verbose,
          types,
          equations,
          symbolic
        )
        menu.children.add(options)
        options.style =
          """
          -fx-background-color: transparent;
          -fx-background-radius: 4;
          -fx-border-color: transparent;
          """

        leftStructure.center = TextInput
        leftStructure.bottom = Terminal

        private val split = new SplitPane:
          items ++= Seq(leftStructure, tabs)
          setDividerPosition(0, LEFT_PANE_WIDTH_RATIO)
        structure.center = split
