package Chord
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}

object User{
  def apply(): Behavior[Command] =
    Behaviors.setup(new User(_))

  trait Command
  case class gotIT(value: String, key: Int) extends Command
  case class whereIsIt(key: Int) extends Command
}


class User(context: ActorContext[User.Command]) extends AbstractBehavior[User.Command](context) {
  import User._

  override def onMessage(msg: Command): Behavior[User.Command] = {
    msg match {
      case gotIT(value, key) =>
        println(key + ": "+ value)
      case whereIsIt(key) =>
        println("Key entry DNE: " + key )
    }
    this
  }


  // Obtain Chord ACTOR, submit query
  // Chord ACTOR ! query
}
