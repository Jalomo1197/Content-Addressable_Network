package CAN

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}

object Simulation {
  def main(args: Array[String]): Unit = {
    val system: ActorSystem[Driver.lookup] =
      ActorSystem(Driver(), "driver")

    system ! Driver.lookup("ToyStory")
    system ! Driver.lookup("MoneyTrain")
  }
}

object Driver {

  final case class lookup(key: String)

  def apply(): Behavior[lookup] =
    Behaviors.setup { context =>
      val DNS = context.spawn(DNS(), "CAN")

      Behaviors.receiveMessage { message =>
        val replyTo = context.spawn(, message.key)
        DNS ! D.keyLookup("MoneyTrain", replyTo)
        val value = Some(Chord.getChordActor.dictionary(message.key))
        replyTo ! User.queryResponse(message.key, value)
        Behaviors.same
      }
    }

}
