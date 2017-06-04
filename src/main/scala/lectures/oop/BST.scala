package lectures.oop


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def fold(aggregator: Int)(f: (Int, Int) => (Int)): Int
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def add(newValue: Int): BST = addImpl(newValue)

  def find(value: Int): Option[BST] = findImpl(value)

  override def toString: String = makeStringList() mkString "\n"

  def fold(aggregator: Int)(f: (Int, Int) => (Int)): Int = {
    val aggregatorLeft = left map (_.fold(f(aggregator, value))(f)) getOrElse f(aggregator, value)
    right map (_.fold(aggregatorLeft)(f)) getOrElse aggregatorLeft
  }

  def addImpl(newValue: Int): BSTImpl = {
    if (newValue < value)
      BSTImpl(value, left map (_.addImpl(newValue)) orElse Some(BSTImpl(newValue)), right)
    else if (value < newValue)
      BSTImpl(value, left, right map (_.addImpl(newValue)) orElse Some(BSTImpl(newValue)))
    else
      this
  }


  def findImpl(value: Int): Option[BSTImpl] = {
    if (value < this.value)
      left flatMap (_.findImpl(value))
    else if (this.value < value)
      right flatMap (_.findImpl(value))
    else
      Some(this)
  }


  def makeStringList(): (List[String]) = (left, right) match {

    case (None, None) => List(value.toString)

    case (Some(l), None) =>
      val leftList = l.makeStringList()
      val valueString = " " * leftList.head.length + value
      (valueString +: leftList) map (_.padTo(valueString.length, ' '))

    case (None, Some(r)) =>
      val rightList = r.makeStringList()
      val valueString = value + " " * rightList.head.length
      (valueString +: rightList) map (_.reverse.padTo(valueString.length, ' ').reverse)

    case (Some(l), Some(r)) =>
      val leftList = l.makeStringList()
      val rightList = r.makeStringList()
      val valueString = " " * leftList.head.length + value + " " * rightList.head.length
      valueString +: leftList.zipAll(rightList, "", "").map(t => t._1 + t._2.reverse.padTo(valueString.length - t._1.length, ' ').reverse)
  }

}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = (1 until nodesCount).foldLeft(root)((node, _) => node.add((Math.random() * maxValue).toInt))

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem2).isDefined)
  require(testTree.find(markerItem3).isDefined)

  println(testTree)
}