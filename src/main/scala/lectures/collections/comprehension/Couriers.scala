package lectures.collections.comprehension

/**
  * Помогите курьерам разобраться с обслуживанием адресов
  *
  * Каждый день на работу выходит 'courierCount' курьеров
  * Им нужно обслужить 'addressesCount' адресов
  * Каждый курьер может обслужить courier.canServe адресов, но только при условии, что позволит дорожная ситуация.
  * Т.е. если trafficDegree < 5, то курьер обслужит все адреса, которые может, иначе - ни одного
  *
  * Входные данные для приложения содержат 2 строки
  * В первой строке - количество адресов, которые требуется обслужить
  * Во второй - количество курьеров, вышедших на работу.
  *
  * Ваша задача:
  * Изучить код и переписать его так,
  * чтобы в нем не было ни одного цикла for, ни одной переменной или мутабильной коллекции
  *
  * Для этого используйте функции комбинаторы: filter, withFilter, fold, map, flatMap и т.д.
  *
  */

case class Traffic(degree: Double)

object Courier {
  def couriers(courierCount: Int): List[Courier] = (1 to courierCount) map (Courier(_)) toList
}

case class Courier(index: Int) {
  val canServe: Int = (Math.random() * 10).toInt
}

object Address {
  def addresses(addressesCount: Int): List[Address] = (1 to addressesCount) map (i => Address(s"$i$i$i")) toList
}

case class Address(postIndex: String)

object CouriersWithComprehension extends App {

  import Address._
  import Courier._

  val sc = new java.util.Scanner(System.in)
  val addressesCount = sc.nextInt()
  val courierCount = sc.nextInt()
  val addrs = addresses(addressesCount)
  val cours = couriers(courierCount)

  // какие адреса были обслужены
  def serveAddresses(addresses: List[Address], couriers: List[Courier]): List[Address] =
    addresses take (couriers withFilter (_ => traffic().degree < 5) map (_.canServe)).sum

  def traffic(): Traffic = Traffic(Math.random() * 10)

  def printServedAddresses(addresses: List[Address], couriers: List[Courier]): Unit =
    serveAddresses(addresses, couriers) map (_.postIndex) foreach println


  printServedAddresses(addrs, cours)

}