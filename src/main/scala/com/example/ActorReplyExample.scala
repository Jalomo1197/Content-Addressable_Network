package com.example

import akka.actor.{Actor, ActorSystem, Props}

class ActorReplyExample extends Actor{
  override def receive: Receive = {
    case msg: String => println("Msg received  from" + sender().path.name + "msg: " + msg)
    val child = context.actorOf(Props[ActorChildReplyExample], "ActorChild")
    child ! "Hello Child!"
  }
}
class ActorChildReplyExample extends Actor{
  override def receive: Receive = {
    case msg: String => println("Msg received  from" + sender().path.name + "msg: " + msg)
    println("Replying to " + sender.path.name)
    sender() ! "Just got your msg!"                   // Reply from ActorChild
  }
}
object ActorReplyExample{
  def main(args:Array[String]): Unit ={
    val actorSystem = ActorSystem("ActorSytem")
    val actor = actorSystem.actorOf(Props[ActorReplyExample], "Root")
    actor ! "This is a test"                // Replying to actor via 'dead-letter' actor reference
  }
}