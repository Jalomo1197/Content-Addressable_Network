package CAN

import java.lang.Thread.sleep
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import com.typesafe.config.Config

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object Simulation {
  def main(args: Array[String]): Unit = {
    // Insert config
    val system: ActorSystem[Driver.lookup] =
      ActorSystem(Driver(), "driver")

    //system ! Driver.lookup("Jumanji")
    //system ! Driver.lookup("FatheroftheBridePartII")
  }
}

object Driver {
  import User.{insertConfig, queryResponse}
  final case class lookup(key: String)

  def apply(): Behavior[lookup] =
    Behaviors.setup { context =>

      val config: Config = ConfigFactory.load("simpleData.conf")
      val DNS = context.spawn(DNS(), "DNS")
      context.log.info("DNS Actor Created: " + DNS.path.name)
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
      val user = context.spawn(User(), "User")
      context.log.info("User Actor Created: " + user.path.name)
      // InsertConfig
      user ! insertConfig(config)
      // Reply From Config
      // Lookup
      Behaviors.receiveMessage { message =>
        val replyTo = context.spawn(User(), message.key)
        //user ! User.keyLookup("MoneyTrain", replyTo)
        //val value = Some(DNS.getDNSActor.dictionary(message.key))
        //replyTo ! User.queryResponse(message.key, value)
        Behaviors.same
      }
    }

}
