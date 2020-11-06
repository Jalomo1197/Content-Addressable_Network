package com.example

import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class ActorExample extends Actor{
  override def receive: Receive = {
    case msg: String => println("Message received: " + msg + "from -" + self.path.name)
    println("Sender: " + sender())      // Returns reference to Actor
    val senderName = sender()           // Sends to tell
    senderName ! "Hello, I got your message!"
  }
}
object ActorExample{
  def main(args:Array[String]): Unit ={
    val actorSystem = ActorSystem("ActorSystem")
    val actor = actorSystem.actorOf(Props[ActorExample], "RootActor")
    actor ! "Sending msg by using !"
    actor.tell("Hello", null)           // Send msg by using tell method (deadLetters Actor Reference)

    implicit val timeout: Timeout = Timeout(2 seconds)
    val future = actor ? "I am wating for a reply"
    val result = Await.result(future, timeout.duration)
    println("Message Received: " + result)
  }
}