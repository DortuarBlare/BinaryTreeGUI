package DataStructure

import scala.util.control.Breaks._

class BinaryTree[Type](@transient private var comparator: Traits.Comparator) extends Serializable {
  private var root: Node[Type] = null

  def add (value: Type): Unit = {
    var newNode = new Node(value)

    if (root == null) root = new Node(value)
    else if (findByValue(value) == null) {
      root.setWeight(root.getWeight + 1)
      var currentNode = root

      while (true) {
        //if (newNode < currentNode) { // Двигаемся влево
        if (comparator.compare(newNode.getValue, currentNode.getValue) < 0) {
          if (currentNode.getLeftChild != null) {
            currentNode = currentNode.getLeftChild
            currentNode.setWeight(currentNode.getWeight + 1)
          }
          else {
            currentNode.setLeftChild(newNode)
            currentNode.getLeftChild.setParent(currentNode)
            return
          }
        }
        else { // Двигаемся вправо
          if (currentNode.getRightChild != null) {
            currentNode = currentNode.getRightChild
            currentNode.setWeight(currentNode.getWeight + 1)
          }
          else {
            currentNode.setRightChild(newNode)
            currentNode.getRightChild.setParent(currentNode)
            return None
          }
        }
      }
    }
  }

  def balance(): Unit = {
    var node : Node[Type] = null
    var leftSubtreeDepth = getDepth(root.getLeftChild)
    var rightSubtreeDepth = getDepth(root.getRightChild)

    while (Math.abs(leftSubtreeDepth - rightSubtreeDepth) > 1) {
      for (i <- 0 until size) {
        node = findByIndex(i)

        if (node == null) return None

        var child: Node[Type] = null
        var parent: Node[Type] = null
        var leftDepth = 0
        var rightDepth = 0
        breakable(while (true) {
          leftDepth = getDepth(node.getLeftChild)
          rightDepth = getDepth(node.getRightChild)

          if (leftDepth > rightDepth && leftDepth - rightDepth > 1) {
            // Правый поворот, так как глубина левого поддерева больше
            child = node.getLeftChild
            parent = node.getParent

            if (parent != null) {
              if (parent.getRightChild eq node) parent.setRightChild(child)
              else if (parent.getLeftChild eq node) parent.setLeftChild(child)
            }
            else root = child

            child.setParent(parent)
            node.setParent(child)

            node.setLeftChild(child.getRightChild)
            if (node.getLeftChild != null) {
              node.getLeftChild.setParent(node)
            }

            child.setRightChild(node)

            node.setWeight(1 + (if (node.getLeftChild != null) node.getLeftChild.getWeight else 0) +
              (if (node.getRightChild != null) node.getRightChild.getWeight else 0))
            child.setWeight(1 + (if (child.getLeftChild != null) child.getLeftChild.getWeight else 0) +
              (if (child.getRightChild != null) child.getRightChild.getWeight else 0))

            break

          }
          else if (rightDepth > leftDepth && rightDepth - leftDepth > 1) {
            // Левый поворот, так как глубина правого поддерева больше
            child = node.getRightChild
            parent = node.getParent

            if (parent != null) {
              if (parent.getRightChild eq node) parent.setRightChild(child)
              else if (parent.getLeftChild eq node) parent.setLeftChild(child)
            }
            else root = child

            child.setParent(parent)
            node.setParent(child)

            node.setRightChild(child.getLeftChild)
            if (node.getRightChild != null) {
              node.getRightChild.setParent(node)
            }

            child.setLeftChild(node)
            node.setWeight(1 + (if (node.getLeftChild != null) node.getLeftChild.getWeight else 0) +
              (if (node.getRightChild != null) node.getRightChild.getWeight else 0))
            child.setWeight(1 + (if (child.getLeftChild != null) child.getLeftChild.getWeight else 0) +
              (if (child.getRightChild != null) child.getRightChild.getWeight else 0))

            break

          }
          if (node.getParent != null) node = node.getParent
          else break
        })

      }
      leftSubtreeDepth = getDepth(root.getLeftChild)
      rightSubtreeDepth = getDepth(root.getRightChild)
    }
  }

  def findByValue(value: Type): Node[Type] = {
    var currentNode = root
    val nodeToFind = new Node(value)

    while (comparator.compare(value, currentNode.getValue) != 0) {
      //if (nodeToFind < currentNode) currentNode = currentNode.getLeftChild
      if (comparator.compare(nodeToFind.getValue, currentNode.getValue) < 0) currentNode = currentNode.getLeftChild
      else currentNode = currentNode.getRightChild

      if (currentNode == null) return null
    }

    currentNode
  }

  def findByIndex(index: Int): Node[Type] = {
    var currentNode = root
    var currentIndex = if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0
    while (index != currentIndex) {
      if (index < currentIndex) {
        currentNode = currentNode.getLeftChild
        if (currentNode == null) return null
        currentIndex -= (if (currentNode.getRightChild != null) currentNode.getRightChild.getWeight else 0) + 1
      }
      else {
        currentNode = currentNode.getRightChild
        if (currentNode == null) return null
        currentIndex += (if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0) + 1
      }
    }
    currentNode
  }

  def deleteByIndex(index: Int): Unit = {
    root.setWeight(root.getWeight - 1)
    var currentNode = root
    var currentIndex = if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0
    var isLeftChild = true

    while (index != currentIndex) { // Поиск удаляемого узла с заданным индексом
      if (index < currentIndex) {
        isLeftChild = true
        currentNode = currentNode.getLeftChild
        if (currentNode == null) return None
          currentIndex -= (if (currentNode.getRightChild != null) currentNode.getRightChild.getWeight else 0) + 1
      }
      else {
        isLeftChild = false
        currentNode = currentNode.getRightChild
        if (currentNode == null) return None
          currentIndex += (if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0) + 1
      }
      currentNode.setWeight(currentNode.getWeight - 1)
    }

    if (currentNode.getLeftChild == null && currentNode.getRightChild == null) { // Если у узла нет потомков
      if (currentNode eq root) root = null
      else if (isLeftChild) currentNode.getParent.setLeftChild(null)
      else currentNode.getParent.setRightChild(null)
    }
    else if (currentNode.getRightChild == null) { // Если у узла нет правого потомка(замена левым поддеревом)
      if (currentNode eq root) root = currentNode.getLeftChild
      else if (isLeftChild) currentNode.getParent.setLeftChild(currentNode.getLeftChild)
      else currentNode.getParent.setRightChild(currentNode.getLeftChild)
      currentNode.getLeftChild.setParent(currentNode.getParent)
    }
    else if (currentNode.getLeftChild == null) { // Если у узла нет левого потомка(замена правым поддеревом)
      if (currentNode eq root) root = currentNode.getRightChild
      else if (isLeftChild) currentNode.getParent.setLeftChild(currentNode.getRightChild)
      else currentNode.getParent.setRightChild(currentNode.getRightChild)
      currentNode.getRightChild.setParent(currentNode.getParent)
    }
    else { // Если у узла два потомка
      val heir = findHeir(currentNode)
      System.out.println("Преемник удаляемого узла: " + heir)
      if (currentNode eq root) root = heir
      else if (isLeftChild) currentNode.getParent.setLeftChild(heir)
      else currentNode.getParent.setRightChild(heir)
    }
    currentNode.setLeftChild(null)
    currentNode.setRightChild(null)
  }

  def findHeir(nodeThatNeedHeir: Node[Type]): Node[Type] = {
    var heir = if (nodeThatNeedHeir.getRightChild != null) nodeThatNeedHeir.getRightChild else nodeThatNeedHeir
    while (heir.getLeftChild != null) {
      heir.setWeight(heir.getWeight - 1)
      heir = heir.getLeftChild
    }
    if (heir eq nodeThatNeedHeir.getRightChild)  // Если наследник правый потомок
      heir.setLeftChild(nodeThatNeedHeir.getLeftChild)
    else {
      heir.getParent.setLeftChild(heir.getRightChild)
      heir.setLeftChild(nodeThatNeedHeir.getLeftChild)
      heir.setRightChild(nodeThatNeedHeir.getRightChild)
      heir.getLeftChild.setParent(heir)
      heir.getRightChild.setParent(heir)
    }
    heir.setParent(nodeThatNeedHeir.getParent) // Меняем родительскую связь

    heir.setWeight(nodeThatNeedHeir.getWeight)
    heir
  }

  def forEach(action: Traits.Action[Node[Type]]) : Unit = {
    var nextNode: Node[Type] = null
    var currentNode: Node[Type] = null
    var size = this.size
    var foundFirstNode = false

    for (i <- 0 until size) {
      if (!foundFirstNode) { // Находим узел с индексом 0
        foundFirstNode = true
        nextNode = findByIndex(0)

        action.doWith(nextNode)
      }
      else {
        currentNode = nextNode

        // Если есть правый потомок, тогда либо он,
        // либо последний его левый потомок будет следующим узлом
        if (nextNode.getRightChild != null) {
          nextNode = nextNode.getRightChild
          while (nextNode.getLeftChild != null) {
            nextNode = nextNode.getLeftChild
          }

          //if (currentNode < nextNode)
          if (comparator.compare(currentNode.getValue, nextNode.getValue) < 0)
            action.doWith(nextNode)
        }
        else if (nextNode.getParent != null) { // Иначе поднимаемся по родителям пока не найдем узел с бо'льшим значением
          //breakable(while (currentNode > nextNode.getParent) {
          breakable(while (comparator.compare(currentNode.getValue, nextNode.getParent.getValue) > 0) {
            nextNode = nextNode.getParent
            if (nextNode.getParent == null) break
          })
          if (nextNode.getParent != null) {
            //if (currentNode < nextNode.getParent) {
            if (comparator.compare(currentNode.getValue, nextNode.getParent.getValue) < 0) {
              nextNode = nextNode.getParent
              action.doWith(nextNode)
            }
          }
        }
      }
    }
  }

  def getDepth(nodeForDepth: Node[Type]): Int = {
    var resultDepth = 0
    if (nodeForDepth != null) {
      val leftDepth = getDepth(nodeForDepth.getLeftChild)
      val rightDepth = getDepth(nodeForDepth.getRightChild)
      resultDepth = Math.max(leftDepth, rightDepth) + 1
    }
    resultDepth
  }

  def leftSubtreeDepth = if (root != null && root.getLeftChild != null) getDepth(root.getLeftChild) else 0

  def rightSubtreeDepth = if (root != null && root.getRightChild != null) getDepth(root.getRightChild) else 0

  def size = if (root == null) 0 else root.getWeight

  def getRoot = this.root

  def getComparator = this.comparator

  def setComparator(newComparator: Traits.Comparator) : Unit = {
    this.comparator = newComparator
  }
}
