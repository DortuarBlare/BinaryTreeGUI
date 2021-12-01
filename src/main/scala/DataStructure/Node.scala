package DataStructure

class Node[Type](private var value: Type) extends Serializable {
  private var weight = 1 // Вес узла (По умолчанию у всех равен 1)
  private var parent : Node[Type] = null
  private var leftChild : Node[Type] = null
  private var rightChild : Node[Type] = null

  def getValue = this.value // Геттер
  def setValue (newValue: Type) : Unit = { // Сеттер
    this.value = newValue
  }

  def getWeight = this.weight
  def setWeight (newWeight: Int) : Unit = {
    this.weight = newWeight
  }

  def getParent = this.parent
  def setParent (newParent: Node[Type]) : Unit = {
    this.parent = newParent
  }

  def getLeftChild = this.leftChild
  def setLeftChild (newLeftChild: Node[Type]) : Unit = {
    this.leftChild = newLeftChild
  }

  def getRightChild = this.rightChild
  def setRightChild (newRightChild: Node[Type]) : Unit = {
    this.rightChild = newRightChild
  }

  override def toString : String = {
    (if (parent == null) "ROOT " else "") +
      s"{ Value = $value, " +
      s"Weight = $weight, " +
      s"Parent = ${if (parent == null) "null" else parent.getValue}, " +
      s"LeftChild = ${if (leftChild == null) "null" else leftChild.getValue}, " +
      s"RightChild = ${if (rightChild == null) "null" else rightChild.getValue} }"
  }
}
