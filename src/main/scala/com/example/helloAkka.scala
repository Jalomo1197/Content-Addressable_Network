package com.example

import akka.actor.{Actor, ActorSystem, Props}

class helloAkka extends Actor{
  override def receive: Receive = {
    case msg: String => println(msg)
    case _ => println("Not a valid string")
  }
}

object Main{
  def main(args: Array[String]): Unit ={
    val actorSystem = ActorSystem("Test")
    val actor = actorSystem.actorOf(Props[helloAkka], "helloAkka")
    actor ! "One small step for mankind"
    actor ! 3.14
  }
}