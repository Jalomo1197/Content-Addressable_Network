package Chord_Algo

import akka.actor.ActorSystem
import akka.actor.typed.Props
import com.typesafe.config.{Config, ConfigFactory}

class Driver {
  def main(args: Array[String]): Unit = {
    val driver: ActorSystem = ActorSystem("Driver")
    //val chord  = driver.actorOf(Props[Chord], name = "Chord")
    //val config: Config = ConfigFactory.load()
    //chord ! Chord.initializeNodesWithConfig(config, )
  }
}
