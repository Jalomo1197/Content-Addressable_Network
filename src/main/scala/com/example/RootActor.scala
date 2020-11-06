package com.example

import akka.actor.{Actor, ActorSystem, Props}

class RootActor extends Actor{
  override def receive: Receive = {
    case msg: String => println(msg + " " + self.path.name)
    val childActor = context.actorOf(Props[Child], "Child1")    // context => child
    childActor ! "I am a child!"                                       // Call childActor From Root
    childActor ! 3.14
  }
}
class Child extends Actor{
  override def receive: Receive = {
    case msg: String => println(msg + " " + self.path.name)
    case _ => println("Not a valid String for Child")
  }
}
object ChildActorExample{
  def main(args: Array[String]): Unit ={
    val actorSystem = ActorSystem()
    val actor = actorSystem.actorOf(Props[RootActor], "RootActor")      // actorSystem => root
    actor ! "I Am Root!"
  }
}