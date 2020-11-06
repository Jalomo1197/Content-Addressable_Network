package Futures

import java.lang.Thread.sleep

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Futures1 extends App{
  implicit val baseTime = System.currentTimeMillis()

  // Create A Future
  val f = Future {
      sleep(500)
      1 + 1
  }
  // BLOCKING
  val result = Await.result(f, 1 second)
  println(result)
  sleep(1000)
}
