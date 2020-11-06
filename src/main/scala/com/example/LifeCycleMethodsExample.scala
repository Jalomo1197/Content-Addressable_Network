package com.example

import akka.actor.{Actor, ActorSystem, Props}

class LifeCycleMethodsExample extends Actor{
  override def receive: Receive = {
    case msg: String => println(msg + " " + self.path.name)
    10/0
    case _ => println("Not a valid String")
  }
  override def preStart(): Unit = {
    println("preStart() Method is invoked")
  }

  override def postStop(): Unit = {
    println("postStop() Method is invoked")
  }
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    println("preRestart() Method is invoked")
    println("Reason: " + reason)
  }

  override def postRestart(reason: Throwable): Unit = {
    println("postRestart() Method is invoked")
    println("Reason: " + reason)
  }
}
object ActorMain{
  def main(args: Array[String]): Unit ={
    val actorSystem = ActorSystem("ActorSytem")
    val actor = actorSystem.actorOf(Props[LifeCycleMethodsExample], "RootActor")
    actor ! "Beginning of lifecycle"
    actor ! None
    actorSystem.stop(actor)       // Stopping Actor by passing Reference
  }
}