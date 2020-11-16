import Chord.Chord
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.wordspec.AnyWordSpecLike
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration._

class ChordSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  import Chord.{distributedMapInitialized, initializeNodesWithConfig}
  "Chord actor import values from application.conf" in {
    val receiver = createTestProbe[distributedMapInitialized]()
    val ChordActor = spawn(Chord())

    val config: Config = ConfigFactory.load()
    val configDictionary = config.as[Map[String, String]]("dictionary")

    ChordActor ! initializeNodesWithConfig(config, receiver.ref)
    val message = receiver.receiveMessage()
    val dictionary = message.dictionary
    dictionary.foreach(e => {
      configDictionary.get(e._1) should not equal(None)
    })
  }

}
