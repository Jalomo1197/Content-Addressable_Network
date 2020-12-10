package CAN

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import net.ceedubs.ficus.Ficus._
import com.typesafe.config.Config

object User{
  def apply(): Behavior[Command] =
    Behaviors.setup(new User(_))

  trait Command
  final case class queryResponse(key: String, value: Option[String]) extends Command
  // Insert (K, V) pairs from config
  final case class insertConfig(config: Config) extends Command
  case class insertConfirmed(key: String, value: String) extends Command
}


class User(context: ActorContext[User.Command]) extends AbstractBehavior[User.Command](context) {
  import User._
  import DNS.insert
  import Procedure.{KEY_STORE, KEY_LOOKUP}
  val dns: ActorRef[DNS.Command] = DNS.getDNSActor.connectToDNS
  var dictionary: Map[String, String] = Map.empty[String, String]

  override def onMessage(msg: Command): Behavior[User.Command] = {
    msg match {
      // Insert (K, V) pairs from config
      case insertConfig(config) =>
        // Creating dictionary defined in config file: application.conf
        dictionary = config.as[Map[String, String]]("dictionary")
        dictionary.foreach( pair => {
          // Send message to insert (K, V) pair
          dns ! insert(Procedure[Node.Command]()
            .withUser(context.self)
            .withKeyValueToStore(pair)
            .withLocation(Zone.findLocation(pair._1))
            .withRoutingPurpose(KEY_STORE))
        })
      case insertConfirmed(key: String, value: String) =>
      case queryResponse(key, value) =>
        value match {
          case None =>
            context.log.info("KEY: " + key + " is not in distributed map")
          case Some(v) =>
            context.log.info("KEY: " + key + " found, VALUE: " + v)
        }
    }
    this
  }
  /*
      Asynchronous function:
        Sends keyLookup command to Chord

  def queryKey(key: String): Unit = {
    context.log.info("USER sent query to CHORD, KEY: " + key)
    chord ! keyLookup(key, context.self)
  }
   */

  /* Signal handling */
  override def onSignal: PartialFunction[Signal, Behavior[User.Command]] = {
    case PostStop =>
      context.log.info("User actor stopped")
      this
  }

}

