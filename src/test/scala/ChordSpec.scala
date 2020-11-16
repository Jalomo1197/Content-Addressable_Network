import Chord.Chord
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._

class ChordSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  import Chord.{distributedMapInitialized, initializeNodesWithConfig}
  "Device actor" in {
    val receiver = createTestProbe[distributedMapInitialized]()
    val ChordActor = spawn(Chord())

    val config: Config = ConfigFactory.load()

    ChordActor ! initializeNodesWithConfig(config, receiver.ref)
    val message = receiver.receiveMessage()
    val dictionary = message.dictionary
    dictionary.foreach(e => println(e._1))
  }

}
