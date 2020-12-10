package CAN

import CAN.DNS.Command
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}


object DNS{
  // Allowing one instances of chord actor for now.
  private var DNS_Singleton: Option[DNS] = None
  // Gets the Chord class
  def getDNSActor: DNS = DNS_Singleton.get
  // Apply function called by Chord(...)
  def apply(): Behavior[Command] =
    Behaviors.setup(context => {
      DNS_Singleton = Some(new DNS(context))
      DNS_Singleton.get
    })

  trait Command
  case class bootstrap(p: Procedure[Node.Command]) extends Command
  case class insert(p: Procedure[Node.Command]) extends Command with Bootstrap.Command
}

class DNS(context: ActorContext[DNS.Command]) extends AbstractBehavior[DNS.Command](context) {
  import DNS.{insert,bootstrap}
  import Node.acquiredBootstrap
  import Bootstrap.initializeZones
  var dictionary: Map[String, String] = Map.empty[String, String]
  var boots = 1
  var bootstraps: List[ActorRef[Bootstrap.Command]] = List(context.spawn(Bootstrap(), s"node-bootstrap-$boots"))
  bootstraps.head ! initializeZones

  override def onMessage(msg: DNS.Command): Behavior[DNS.Command] = {
    msg match {
      case bootstrap(procedure) =>
        context.log.info(this.getClass +" : acquiredBootsrap(procedure) => Bootstrap")
        procedure.getReplyTo.get ! acquiredBootstrap(Procedure[Bootstrap.Command]().withReference(bootstraps.head))
        this

      // DNS to Boot to Zone,
      // for item in config file, we receive the (key,value) and send to bootstrap here.
      case insert(procedure) =>
        context.log.info(this.getClass +" : inserting(procedure) => Bootstrap ")
        bootstraps.head ! insert(procedure)
        this
    }

  }
  /* User obtain ActorRef to Chord Singleton via User.getChordActor.getReference */
  def getReference: ActorRef[Command] = context.self
}
