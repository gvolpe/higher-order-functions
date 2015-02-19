package com.github.gvolpe.functions

case class Email(subject: String, text: String, sender: String, recipient: String)

trait EmailService extends EmailFiltering with EmailUtils {

  def newMailsForUser(mails: Seq[Email], f: EmailFilter) = mails.filter(f)

}

trait EmailFiltering extends GenericFunctions {

  type EmailFilter = Email => Boolean
  type SizeChecker = Int => Boolean

  val sentByOneOf: Set[String] => EmailFilter =
    senders => email => senders.contains(email.sender)

  val notSentByAnyOf = sentByOneOf andThen(complement(_))

  val sizeConstraint: SizeChecker => EmailFilter = f => email => f(email.text.size)

  val minimumSize: Int => EmailFilter = n => sizeConstraint(_ >= n)
  val maximumSize: Int => EmailFilter = n => sizeConstraint(_ <= n)

  def any[A](predicates: (A => Boolean)*): A => Boolean = a => predicates.exists(pred => pred(a))
  def none[A](predicates: (A => Boolean)*) = complement(any(predicates: _*))
  def every[A](predicates: (A => Boolean)*) = none(predicates.view.map(complement(_)): _*)

}

trait EmailUtils {

  val addMissingSubject = (email: Email) =>
    if (email.subject.isEmpty) email.copy(subject = "No subject")
    else email
  val checkSpelling = (email: Email) =>
    email.copy(text = email.text.replaceAll("your", "you're"))
  val removeInappropriateLanguage = (email: Email) =>
    email.copy(text = email.text.replaceAll("dynamic typing", "**CENSORED**"))
  val addAdvertismentToFooter = (email: Email) =>
    email.copy(text = email.text + "\nThis mail sent via Super Awesome Free Mail")

}

trait GenericFunctions {

  def complement[A](predicate: A => Boolean) = (a: A) => !predicate(a)

}
