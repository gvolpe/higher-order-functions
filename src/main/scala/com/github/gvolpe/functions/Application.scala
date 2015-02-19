package com.github.gvolpe.functions

object Application extends App with EmailService {

  val emailFilter: EmailFilter = notSentByAnyOf(Set("foo@bar.com"))
  val mails = Email(
    subject = "It's me again, your stalker friend!",
    text = "Hello my friend! How are you?",
    sender = "johndoe@example.com",
    recipient = "me@example.com") :: Nil

  val filtered: Seq[Email] = newMailsForUser(mails, emailFilter) // returns an empty list

  println(filtered)

  val filter: EmailFilter = every(
    notSentByAnyOf(Set("gvolpe@github.com")),
    minimumSize(10),
    maximumSize(100)
  )

  // More higher order functions

  val mail2 = Email(
    subject = "",
    text = "Hi everyone! We like to introduce the new Activator!",
    sender = "gvolpe@github.com",
    recipient = "me@example.com")

  // The same as using a chain of "andThen(f)"
  val pipeline: (Email) => Email = Function.chain(Seq(
    addMissingSubject,
    checkSpelling,
    removeInappropriateLanguage,
    addAdvertismentToFooter))

  val mail: Email = pipeline(mail2)

  println(mail)

  // Partial functions

  def fooHandler(value: String): Option[String] =
    if (value.contains("foo")) Some(value)
    else None

  def barHandler: PartialFunction[String, String] = {
    case value =>
      if (value.contains("bar")) value
      else ""
  }

  def catHandler: PartialFunction[String, String] = {
    case value =>
      if (value.contains("cat")) value
      else ""
  }

  val liftedBarHandler: (String) => Option[String] = barHandler.lift

  val unliftedBarHandler: PartialFunction[String, String] = Function.unlift(liftedBarHandler)

  val value = "bartender"
  val handler: Option[String] = fooHandler(value) orElse liftedBarHandler(value) orElse catHandler.lift(value)
  println(handler.getOrElse("No handler found!"))

  // Currying

  val sum: (Int, Int) => Int = _ + _
  val sumCurried: Int => Int => Int = sum.curried

  val sumUncurried: (Int, Int) => Int = Function.uncurried(sumCurried)

  println("SUM >> " + sum(1, 2))
  println("CURRIED SUM >> " + sumCurried(2)(3))

  val newEmails: Seq[Email] = MockMailBoxService.newEmails(User("Gabriel"))
  println(newEmails)

}
