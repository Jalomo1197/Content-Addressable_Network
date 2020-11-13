package Chord

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

// Set up for cases
case class foundKey()
case class forward_lookup(key: Int, node: Int)


case class GuardianNode(node_count: Int, query_count: Int) extends Actor{
  val system: ActorSystem = ActorSystem("workerNodes")
  var nodeList = new Array[ActorRef](node_count)

  // Hash Ring
  for( i <- 0 until node_count) {
  {
    nodeList(i) = context.actorOf(Props[Node],"node: " + i)
  }
  // Generate HashCode Here
    var key = Hash.getHash("node: " + i.toString)
  }

  override def receive: Receive = {
    case foundKey() => {

    }
    case forward_lookup(key, node) => {

    }
    case _ => println("Default case")

  }
}
