package Chord

import akka.actor.ActorSystem
import akka.actor.typed.Props

class Driver {
  def main(args: Array[String]): Unit = {
    val driver: ActorSystem = ActorSystem("Driver")
    val chord  = driver.actorOf(Props[Chord], name = "Chord")
    chord ! Chord.initializeNodesWithConfig
  }
}
