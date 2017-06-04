package lectures.concurrent

import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Smooth - это своебразный функциональный кэш, предназначенный для исключения повторных вызовов кода
  * до того, как получен результат первого вызова.
  * Он работает следующим образом:
  * * * * в объект Smooth в метод apply передается код, который может выполняться какое-то время, и возвращает какое-то значение
  * * * * apply создаст инстанс Smooth
  * * * * созданный инстанс при вызове apply возвращает Future
  * * * * * и запускает код, если код еще не запущен
  * * * * * и не запускает код, если код еще не завершился с момента предыдущего запуска
  *
  * Подсказка: можно использовать AtomicReference
  *
  */
object Smooth{
   def apply[T](thunk: => T): Smooth[T] = new Smooth[T](thunk)
}


class Smooth[T](thunk: => T) {
   @volatile private var result: Option[Future[T]] = None

   def apply(): Future[T] = {
      result.getOrElse(initSmooth)
   }

   private def initSmooth: Future[T] = {
      synchronized {
         result.getOrElse {
            val f = Future(thunk)
            result = Some(f)

            f.onComplete { _ =>
               result = None // Reset state, prepare for next task
            }

            f
         }
      }
   }
}