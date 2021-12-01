package GUI

import DataStructure.BinaryTree
import Traits.Action
import TypeBuilders.IntegerBuilder
import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.image.Image
import scalafx.scene.layout.VBox

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

object BinaryTreeApp extends JFXApp3 {
  class TreeTableElement(val size: String, val leftDepth: String, val rightDepth: String) {
    val treeSize = new StringProperty(size)
    val leftSubtreeDepth = new StringProperty(leftDepth)
    val rightSubtreeDepth = new StringProperty(rightDepth)
  }

  class NodeTableElement(val index: String,  val value: String,     val weight: String,
                         val parent: String, val leftChild: String, val rightChild: String) {
    val nodeIndex = new StringProperty(index)
    val nodeValue = new StringProperty(value)
    val nodeWeight = new StringProperty(weight)
    val nodeParent = new StringProperty(parent)
    val nodeLeftChild = new StringProperty(leftChild)
    val nodeRightChild = new StringProperty(rightChild)
  }

  class ObjectInputStreamWithCustomClassLoader(
                                                fileInputStream: FileInputStream
                                              ) extends ObjectInputStream(fileInputStream) {
    override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
      try { Class.forName(desc.getName, false, getClass.getClassLoader) }
      catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
    }
  }

  private var box: VBox = null
  private var typeBuilder: Traits.TypeBuilder = null
  private var intBinaryTree: BinaryTree[Int] = null
  private var stringBinaryTree: BinaryTree[String] = null
  private var nodeTable: TableView[NodeTableElement] = null

  override def start(): Unit = {
    box = new VBox {
      spacing = 15
      autosize()
    }
    stage = new PrimaryStage {
      scene = new Scene(800, 600) {
        title = "Binary Tree"
        root = box
      }
    }

    val IntButton = new ButtonType("Int")
    val StringButton = new ButtonType("String")

    val alert = new Alert(AlertType.Confirmation) {
      initOwner(stage)
      title = "Data Type For Binary Tree"
      headerText = "Please choose data type for binary tree"
      buttonTypes = Seq(
        IntButton, StringButton, ButtonType.Cancel)
    }

    val result = alert.showAndWait()

    result match {
      case Some(IntButton) =>
        typeBuilder = new IntegerBuilder()
        intBinaryTree = new BinaryTree[Int](typeBuilder.getComparator())

      case Some(StringButton) =>
        typeBuilder = new TypeBuilders.StringBuilder()
        stringBinaryTree = new BinaryTree[String](typeBuilder.getComparator())

      case _ => this.stopApp()
    }

    configureTable(box)
  }

  def configureTable(root: VBox): Unit = {
    val menuBar = new MenuBar()

    val fileMenu = new Menu("File")
    val saveFileItem = new MenuItem("Save")
    saveFileItem.onAction = (event: ActionEvent) => {
      val oos = new ObjectOutputStream(
        new FileOutputStream("res/BinaryTree.txt"))
      if (intBinaryTree != null) oos.writeObject(intBinaryTree)
      else oos.writeObject(stringBinaryTree)
      oos.close()
    }
    val loadFileItem = new MenuItem("Load")
    loadFileItem.onAction = (event: ActionEvent) => {
      val ois = new ObjectInputStreamWithCustomClassLoader(
        new FileInputStream("res/BinaryTree.txt"))
      if (intBinaryTree != null) {
        intBinaryTree = ois.readObject.asInstanceOf[BinaryTree[Int]]
        intBinaryTree.setComparator(typeBuilder.getComparator())
      }
      else {
        stringBinaryTree = ois.readObject.asInstanceOf[BinaryTree[String]]
        stringBinaryTree.setComparator(typeBuilder.getComparator())
      }
      ois.close()


      updateTables()
    }
    fileMenu.items = List(saveFileItem, loadFileItem)

    val addMenu = new Menu("Add")
    val add1Item = new MenuItem("Add 1")
    add1Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size + 1
        while (intBinaryTree.size != newSize)
          intBinaryTree.add(typeBuilder.create())
      }
      else {
        val newSize = stringBinaryTree.size + 1
        while (stringBinaryTree.size != newSize)
          stringBinaryTree.add(typeBuilder.create())
      }

      updateTables()
    }
    val add5Item = new MenuItem("Add 5")
    add5Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size + 5
        while (intBinaryTree.size != newSize)
          intBinaryTree.add(typeBuilder.create())
      }
      else {
        val newSize = stringBinaryTree.size + 5
        while (stringBinaryTree.size != newSize)
          stringBinaryTree.add(typeBuilder.create())
      }

      updateTables()
    }
    val add10Item = new MenuItem("Add 10")
    add10Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size + 10
        while (intBinaryTree.size != newSize)
          intBinaryTree.add(typeBuilder.create())
      }
      else {
        val newSize = stringBinaryTree.size + 10
        while (stringBinaryTree.size != newSize)
          stringBinaryTree.add(typeBuilder.create())
      }

      updateTables()
    }
    val add100Item = new MenuItem("Add 100")
    add100Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size + 100
        while (intBinaryTree.size != newSize)
          intBinaryTree.add(typeBuilder.create())
      }
      else {
        val newSize = stringBinaryTree.size + 100
        while (stringBinaryTree.size != newSize)
          stringBinaryTree.add(typeBuilder.create())
      }

      updateTables()
    }
    val add1000Item = new MenuItem("Add 1000")
    add1000Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size + 1000
        while (intBinaryTree.size != newSize)
          intBinaryTree.add(typeBuilder.create())
      }
      else {
        val newSize = stringBinaryTree.size + 1000
        while (stringBinaryTree.size != newSize)
          stringBinaryTree.add(typeBuilder.create())
      }

      updateTables()
    }
    addMenu.items = List(add1Item, add5Item, add10Item, add100Item, add1000Item)


    val deleteMenu = new Menu("Delete")
    val delete1Item = new MenuItem("Delete 1")
    delete1Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size - 1
        while (intBinaryTree.size != newSize && intBinaryTree.size != 0)
          intBinaryTree.deleteByIndex(0)
      }
      else {
        val newSize = stringBinaryTree.size - 1
        while (stringBinaryTree.size != newSize && stringBinaryTree.size != 0)
          stringBinaryTree.deleteByIndex(0)
      }

      updateTables()
    }
    val delete5Item = new MenuItem("Delete 5")
    delete5Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size - 5
        while (intBinaryTree.size != newSize && intBinaryTree.size != 0)
          intBinaryTree.deleteByIndex(0)
      }
      else {
        val newSize = stringBinaryTree.size - 5
        while (stringBinaryTree.size != newSize && stringBinaryTree.size != 0)
          stringBinaryTree.deleteByIndex(0)
      }

      updateTables()
    }
    val delete10Item = new MenuItem("Delete 10")
    delete10Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size - 10
        while (intBinaryTree.size != newSize && intBinaryTree.size != 0)
          intBinaryTree.deleteByIndex(0)
      }
      else {
        val newSize = stringBinaryTree.size - 10
        while (stringBinaryTree.size != newSize && stringBinaryTree.size != 0)
          stringBinaryTree.deleteByIndex(0)
      }

      updateTables()
    }
    val delete100Item = new MenuItem("Delete 100")
    delete100Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size - 100
        while (intBinaryTree.size != newSize && intBinaryTree.size != 0)
          intBinaryTree.deleteByIndex(0)
      }
      else {
        val newSize = stringBinaryTree.size - 100
        while (stringBinaryTree.size != newSize && stringBinaryTree.size != 0)
          stringBinaryTree.deleteByIndex(0)
      }

      updateTables()
    }
    val delete1000Item = new MenuItem("Delete 1000")
    delete1000Item.onAction = (event: ActionEvent) => {
      if (intBinaryTree != null) {
        val newSize = intBinaryTree.size - 1000
        while (intBinaryTree.size != newSize && intBinaryTree.size != 0)
          intBinaryTree.deleteByIndex(0)
      }
      else {
        val newSize = stringBinaryTree.size - 1000
        while (stringBinaryTree.size != newSize && stringBinaryTree.size != 0)
          stringBinaryTree.deleteByIndex(0)
      }

      updateTables()
    }
    deleteMenu.items = List(delete1Item, delete5Item, delete10Item, delete100Item, delete1000Item)

    menuBar.menus = List(fileMenu, addMenu, deleteMenu)

    val nodeData = ObservableBuffer(
      new NodeTableElement("-", "-", "-", "-", "-", "-")
    )

    nodeTable = createNodeTableView(nodeData)
    nodeTable.columnResizePolicy = TableView.ConstrainedResizePolicy

    var label: Label = null
    if (intBinaryTree != null)
      label = new Label("Tree Size: " + 0 +
        "\tLeft Subtree Depth: " + 0 +
        "\tRight Subtree Depth: " + 0)
    else if (stringBinaryTree != null)
      label = new Label("Tree Size: " + 0 +
        "\tLeft Subtree Depth: " + 0 +
        "\tRight Subtree Depth: " + 0)

    root.children = Seq(
      menuBar,
      label,
      nodeTable
    )

  }

  def updateTables() = {
    var treeData: ObservableBuffer[TreeTableElement] = null
    var nodeData: ObservableBuffer[NodeTableElement] = null
    var index = 0

    if (typeBuilder.typeName().equals("Int")) {
      intBinaryTree.forEach(new Action[DataStructure.Node[Int]] {
        override def doWith(someObject: DataStructure.Node[Int]): Unit = {
          if (nodeData == null)
            nodeData = ObservableBuffer(
              new NodeTableElement(
                index.toString(),
                someObject.getValue.toString(),
                someObject.getWeight.toString(),
                if (someObject.getParent == null) "null" else someObject.getParent.getValue.toString(),
                if (someObject.getLeftChild == null) "null" else someObject.getLeftChild.getValue.toString(),
                if (someObject.getRightChild == null) "null" else someObject.getRightChild.getValue.toString())
            )
          else nodeData.addOne(
            new NodeTableElement(
              index.toString(),
              someObject.getValue.toString(),
              someObject.getWeight.toString(),
              if (someObject.getParent == null) "null" else someObject.getParent.getValue.toString(),
              if (someObject.getLeftChild == null) "null" else someObject.getLeftChild.getValue.toString(),
              if (someObject.getRightChild == null) "null" else someObject.getRightChild.getValue.toString())
          )
          index += 1
        }
      })
    }
    else if (typeBuilder.typeName().equals("String")) {
      stringBinaryTree.forEach(new Action[DataStructure.Node[String]] {
        override def doWith(someObject: DataStructure.Node[String]): Unit = {
          if (nodeData == null)
            nodeData = ObservableBuffer(
              new NodeTableElement(
                index.toString(),
                someObject.getValue.toString(),
                someObject.getWeight.toString(),
                if (someObject.getParent == null) "null" else someObject.getParent.getValue.toString(),
                if (someObject.getLeftChild == null) "null" else someObject.getLeftChild.getValue.toString(),
                if (someObject.getRightChild == null) "null" else someObject.getRightChild.getValue.toString())
            )
          else nodeData.addOne(
            new NodeTableElement(
              index.toString(),
              someObject.getValue.toString(),
              someObject.getWeight.toString(),
              if (someObject.getParent == null) "null" else someObject.getParent.getValue.toString(),
              if (someObject.getLeftChild == null) "null" else someObject.getLeftChild.getValue.toString(),
              if (someObject.getRightChild == null) "null" else someObject.getRightChild.getValue.toString())
          )
          index += 1
        }
      })
    }

    var label: Label = null
    if (intBinaryTree != null)
      label = new Label("Tree Size: " + intBinaryTree.size +
        "\tLeft Subtree Depth: " + intBinaryTree.leftSubtreeDepth +
        "\tRight Subtree Depth: " + intBinaryTree.rightSubtreeDepth)
    else if (stringBinaryTree != null)
      label = new Label("Tree Size: " + stringBinaryTree.size +
        "\tLeft Subtree Depth: " + stringBinaryTree.leftSubtreeDepth +
        "\tRight Subtree Depth: " + stringBinaryTree.rightSubtreeDepth)

    nodeTable = createNodeTableView(nodeData)
    nodeTable.columnResizePolicy = TableView.ConstrainedResizePolicy

    box.children.set(1, label)
    box.children.set(2, nodeTable)
  }

  def createNodeTableView(data: ObservableBuffer[NodeTableElement]): TableView[NodeTableElement] = {
    val table = new TableView[NodeTableElement] {
      columns ++= Seq(
        new TableColumn[NodeTableElement, String] {
          text = "Index"
          prefWidth = 100
          cellValueFactory = _.value.nodeIndex
        },
        new TableColumn[NodeTableElement, String] {
          text = "Value"
          prefWidth = 100
          cellValueFactory = _.value.nodeValue
        },
        new TableColumn[NodeTableElement, String] {
          text = "Weight"
          prefWidth = 100
          cellValueFactory = _.value.nodeWeight
        },
        new TableColumn[NodeTableElement, String] {
          text = "Parent"
          prefWidth = 100
          cellValueFactory = _.value.nodeParent
        },
        new TableColumn[NodeTableElement, String] {
          text = "Left Child"
          prefWidth = 100
          cellValueFactory = _.value.nodeLeftChild
        },
        new TableColumn[NodeTableElement, String] {
          text = "Right Child"
          prefWidth = 100
          cellValueFactory = _.value.nodeRightChild
        }
      )
      items = data
    }
    table
  }
}
