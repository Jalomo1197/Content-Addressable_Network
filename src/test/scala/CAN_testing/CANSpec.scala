package CAN_testing

import java.lang.Thread.sleep
import java.util.concurrent.TimeUnit

import CAN.User
import CAN.User.{Command, insertConfig, insertConfirmed, queryResponse}
import akka.actor.TypedActor.context
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.scaladsl.adapter.ClassicActorContextOps
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class CANSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike{
  import CAN.User.lookup
  // Initialize zones and Huge config file inserted into CAN
  val config: Config = ConfigFactory.load("application.conf")
  var dictionary: Map[String, String] = Map.empty[String, String]
  // Creating dictionary defined in config file: application.conf
  dictionary = config.as[Map[String, String]]("dictionary")

  /*
  "User actor import values from application.conf" in {
    val DNS = spawn(CAN.DNS(), "DNS")
    val receiver = createTestProbe[insertConfirmed]()
    // Sleep to Construct Nodes
    // used by 'time' method
    implicit val baseTime: Long = System.currentTimeMillis
    // 2 - create a Future
    val f = Future {
      sleep(100)
    }
    // Non-blocking
    f.onComplete {
      case Success(value) => println(s"Zones and nodes initialized! = $value")
      case Failure(e) => e.printStackTrace()
    }
    val user = spawn(User(), "User")

    val config: Config = ConfigFactory.load("simpleData.conf")
    val configDictionary = config.as[Map[String, String]]("dictionary")

    user ! insertConfig(config)
    val message = receiver.receiveMessage()
    dictionary.foreach(e => {
      configDictionary.get(e._1) should not equal None
    })
  }
  */
  "CAN movie title lookups" in {

    val DNS = spawn(CAN.DNS(), "DNS")
    // Sleep to Construct Nodes
    // used by 'time' method
    implicit val baseTime: Long = System.currentTimeMillis
    // 2 - create a Future
    val f = Future {
      sleep(100)
    }
    // Non-blocking
    f.onComplete {
      case Success(value) => println(s"Zones and nodes initialized! = $value")
      case Failure(e) => e.printStackTrace()
    }
    val receiver = createTestProbe[queryResponse]()
    val user = createTestProbe[Command]()


    val user2 = spawn(User(), "User")

    val config: Config = ConfigFactory.load("application.conf")
    //val message = receiver.receiveMessage()
    user2 ! insertConfig(config)
    // Lookup
    dictionary.foreach(movie => {
      user2 ! lookup(movie._1)
      /*
      val answer = user.receiveMessage(new FiniteDuration(20, TimeUnit.SECONDS))
      answer match {
        case queryResponse(key, value) =>
          value should not equal None
          println(" KEY:"+key+ " FOUND VALUE: "+ value.getOrElse("notFound"))
      }

       */
    })
    // One minute wait
    sleep(6000)
    // Response
    //val message2 = receiver.receiveMessage()
    //println(message2.value.get)

  }

}
