package lectures.functions

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import Authentication._
import AuthenticationData._

/**
  * Авторизация - это очень важно, поэтому нам необходимо покрыть тестами ответсвенный за нее код
  * (lectures.functions.Authentication)
  *
  * Для этого
  * * * * уберите extends App у Authentication
  * * * * замените AuthenticationData.testUsers соответствующими генераторами
  * * * * напишите
  * * * * * 2 теста на authByCard
  * * * * * 2 теста на authByLP
  * * * * * 1 тест на их композицию
  *
  */
class AuthenticationTest extends WordSpec with Matchers with PropertyChecks {
  val cardUserGen: Gen[CardUser] = Gen zip(Gen.choose(0, 10000), Gen.choose(0, 10000)) map (c => CardUser(c._1, CardCredentials(c._2)))
  val regCardUserGen: Gen[CardUser] = Gen zip(Gen.choose(0, 10000), Gen.oneOf(registeredCards.toList)) map (c => CardUser(c._1, c._2))
  val lpUserGen: Gen[LPUser] = Gen zip(Gen.choose(0, 10000), Gen.alphaStr, Gen.alphaStr) map (c => LPUser(c._1, LPCredentials(c._2, c._3)))
  val regLPUserGen: Gen[LPUser] = Gen zip(Gen.choose(0, 10000), Gen.oneOf(registeredLoginAndPassword.toList)) map (c => LPUser(c._1, c._2))
  val anonUserGen: Gen[AnonymousUser] = Gen.const(AnonymousUser())

  "An user" which {
    "has registered card" should {
      "be successfully authenticated" in {
        forAll(regCardUserGen) {
          authByCard.isDefinedAt(_) shouldBe true
        }
      }
    }

    "doesn't have registered card" should {
      "not be successfully authenticated" in {
        forAll(cardUserGen filter (user => !registeredCards.contains(user.credentials))) {
          authByCard.isDefinedAt(_) shouldBe false
        }
      }
    }

    "has login and password" should {
      "be successfully authenticated" in {
        forAll(regLPUserGen) {
          authByLP.isDefinedAt(_) shouldBe true
        }
      }

    }

    "doesn't have login and password" should {
      "not be successfully authenticated" in {
        forAll(lpUserGen filter (user => !registeredLoginAndPassword.contains(user.credentials))) {
          authByLP.isDefinedAt(_) shouldBe false
        }
      }
    }

    "has registered card or login and password" should {
      "be successfully authenticated" in {
        forAll(Gen.oneOf(regCardUserGen, regLPUserGen)) {
          (authByCard orElse authByLP).lift(_).isDefined shouldBe true
        }
      }

    }
  }

}
