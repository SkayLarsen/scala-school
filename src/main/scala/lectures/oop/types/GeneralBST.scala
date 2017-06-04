package lectures.oop.types

import scala.util.Random
import lectures.matching.SortingStuff.Watches

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]

  def fold(aggregator: T)(f: (T, T) => (T)): T
}

case class GeneralBSTImpl[T: Ordering](value: T,
                                       left: Option[GeneralBSTImpl[T]] = None,
                                       right: Option[GeneralBSTImpl[T]] = None) extends GeneralBST[T] {
  val ord: Ordering[T] = implicitly[Ordering[T]]

  import ord.mkOrderingOps

  def add(newValue: T): GeneralBST[T] = addImpl(newValue)

  def find(value: T): Option[GeneralBST[T]] = findImpl(value)

  override def toString: String = makeStringList() mkString "\n"

  def fold(aggregator: T)(f: (T, T) => (T)): T = {
    val aggregatorLeft = left map (_.fold(f(aggregator, value))(f)) getOrElse f(aggregator, value)
    right map (_.fold(aggregatorLeft)(f)) getOrElse aggregatorLeft
  }

  def addImpl(newValue: T): GeneralBSTImpl[T] = {
    if (newValue < value)
      GeneralBSTImpl(value, left map (_.addImpl(newValue)) orElse Option(GeneralBSTImpl(newValue)), right)
    else if (value < newValue)
      GeneralBSTImpl(value, left, right map (_.addImpl(newValue)) orElse Option(GeneralBSTImpl(newValue)))
    else
      this
  }

  def findImpl(value: T): Option[GeneralBSTImpl[T]] = {
    if (value < this.value)
      left flatMap (_.findImpl(value))
    else if (this.value < value)
      right flatMap (_.findImpl(value))
    else
      Option(this)
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

object GeneralTreeTest extends App {

  implicit val watchOrd: Ordering[Watches] = Ordering.by(_.cost)

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val floatTree: GeneralBST[Float] = (1 until nodesCount).foldLeft(GeneralBSTImpl[Float](maxValue / 2))((node, _) => node.addImpl((Math.random() * maxValue).toFloat))
  val stringTree: GeneralBST[String] = (1 until nodesCount).foldLeft(GeneralBSTImpl[String]("Tree Root"))((node, _) => node.addImpl(Random.alphanumeric.take(10).mkString))
  val watchTree: GeneralBST[Watches] = (1 until nodesCount).foldLeft(GeneralBSTImpl[Watches](Watches("Root Watches", maxValue / 2)))((node, _) => node.addImpl(Watches(Random.alphanumeric.take(10).mkString, (Math.random() * maxValue).toFloat)))

}