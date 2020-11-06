package com.example

import akka.actor.{Actor, ActorSystem, Props}

class creatingActor extends Actor{
  override def receive: Receive = {
    case msg: String => println(msg + " " + self.path.name + "\n")
  }
}
object creatingActorExample{
  def main(args: Array[String]): Unit ={
    val actorSystem = ActorSystem("ActorSystem")
    val props1 = Props[creatingActor]
    val actor1 = actorSystem.actorOf(props1, "Actor1")  // Passing props reference (If name not given reference is)
    val actor2 = actorSystem.actorOf(Props[creatingActor], "Actor2")  // Passing Props explicitly && giving name
    actor1 ! "I am actor1!"
    actor2 ! "I am actor2!"
  }
}