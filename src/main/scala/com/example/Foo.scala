package com.example
/* BAD WAY */
case class Foo(i: Int)
class Bar(i: Int, s: String) extends Foo(i)
case class Message(sender: String, recipient: String, body: String)

object Foo extends App{
  //println(new Bar(1, "foo") == new Bar(1, "bar"))
  case class Book(isbn: String)
  val frankenstein = Book("978-0486282114")
  /* Very Nice */
  val message2 = Message("jorge@catalonia.es", "guillaume@quebec.ca", "Comva?")  // Comparison
  val message3 = Message("jorge@catalonia.es", "guillaume@quebec.ca", "Com va?")
  val messagesAreDifferent = message2 == message3  // false
  println(messagesAreDifferent)
}

