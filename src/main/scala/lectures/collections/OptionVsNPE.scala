package lectures.collections

import scala.util.Random

/**
  * В этом задании Вам предстоит работать с очень нестабильным внешним сервисом.
  *
  * Для успешного завершения задания вы должны реализовать метод businessLogic в объекте OptionVsNPE
  * Этот метод должен делать следующее:
  * * * * * Получить и распечатать результат или, если была ошибка ResourceException,
  *         распечатать "Try again with new resource" и повторить все заново
  * * * * * Получить ресурс через ResourceProducer
  * * * * * Если ресурс не получен, кидать ResourceException (throw new ResourceException)
  * * * * * Если ресурс удачно получен, на его основе получить Connection
  * * * * * Если соединение не получено, пробовать, пока соединение не будет получено
  * * * * * Вызвать у соединения метод result()
  * * * * * Если метод result возвращает Null, заменить его дефолтным сообщением из объекта Connection
  *
  * Для успешного решения задания:
  * * * * * Замените знаки вопроса реализацией методов
  * * * * * Нельзя менять содержимое объектов ConnectionProducer, ResourceProducer
  * * * * * Не должно быть ни одного NPE
  * * * * * Можно менять входные и выходные параметры методов Connection
  *
  * трейт FailUtil - содержит методы для эмуляции спорадических ошибок
  *
  *
  */
class ResourceException extends Exception("Ресурс не отвечает")

trait FailUtil {
  val failRate: Double

  def timeToFail: Boolean = Math.random() > failRate
}

object ResourceProducer extends FailUtil {
  def produce: Resource = if (timeToFail) null else Resource(Random.alphanumeric.take(10).mkString)

  val failRate: Double = 0.3
}

object ConnectionProducer extends FailUtil {
  val failRate: Double = 0.5

  def produce(resource: Resource): Connection = if (timeToFail) null else Connection(resource)

  def result(connection: Connection): String = if (timeToFail) null else connection.resource.name
}

case class Connection(resource: Resource) {
  private val defaultResult = "something went wrong!"

  def result(): String = Option(ConnectionProducer.result(this)) getOrElse defaultResult
}

case class Resource(name: String)

object OptionVsNPE extends App {

  def businessLogic: String = try {
    // ResourceProducer

    def getResult(resource: Resource): String = Option(ConnectionProducer.produce(resource)) match {
      case None => getResult(resource)
      case Some(connection) => connection.result()
    }

    val result: String = Option(ResourceProducer.produce) match {
      case None => throw new ResourceException
      case Some(resource) => getResult(resource)
    }
    println(result)
    result
  } catch {
    case e: ResourceException => println("Try again with new resource")
      businessLogic
  }

  businessLogic
}
