package lectures.collections

/**
  * Постарайтесь не использовать мутабельные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */

object MergeSortImpl extends App {

  def merge(left: Seq[Int], right: Seq[Int], result: Seq[Int]): Seq[Int] = (left, right) match {
    case (_, Nil) => result ++ left
    case (Nil, _) => result ++ right
    case (lh :: lt, rh :: rt) => if (lh < rh) merge(lt, right, result :+ lh) else merge(left, rt, result :+ rh)
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = data match {
    case Nil => Nil
    case head :: Nil => data
    case head :: tail =>
      val (l, r) = data.splitAt(data.length / 2)
      merge(mergeSort(l), mergeSort(r), Nil)
  }

  println(mergeSort(Seq.fill(10)(scala.util.Random.nextInt(100))))
}