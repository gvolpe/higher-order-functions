package com.github.gvolpe.functions

case class Email(subject: String, text: String, sender: String, recipient: String)

trait EmailService extends EmailFiltering with EmailUtils {

  def newMailsForUser(mails: Seq[Email], f: EmailFilter) = mails.filter(f)

}

trait CurriedEmailFiltering extends EmailCustomTypes {

  type IntPairPred = (Int, Int) => Boolean

  val gt: IntPairPred = _ > _
  val ge: IntPairPred = _ >= _
  val lt: IntPairPred = _ < _
  val le: IntPairPred = _ <= _
  val eq: IntPairPred = _ == _

  def sizeConstraint(pred: IntPairPred, n: Int, email: Email) = pred(email.text.size, n)

  val minimumSize: (Int, Email) => Boolean = sizeConstraint(ge, _: Int, _: Email)
  val maximumSize: (Int, Email) => Boolean = sizeConstraint(le, _: Int, _: Email)

  val constr20: (IntPairPred, Email) => Boolean = sizeConstraint(_: IntPairPred, 20, _: Email)
  val constr30: (IntPairPred, Email) => Boolean = sizeConstraint(_: IntPairPred, 30, _: Email)

  val sizeConstraintFn: (IntPairPred, Int, Email) => Boolean = sizeConstraint _

  val min20: EmailFilter = minimumSize(20, _: Email)
  val max20: EmailFilter = constr20(le, _: Email)

  def curriedSizeConstraint(pred: IntPairPred)(n: Int)(email: Email): Boolean =
    pred(email.text.size, n)

  val curriedSizeConstraintFn: IntPairPred => Int => Email => Boolean = curriedSizeConstraint _

  val minSize: Int => Email => Boolean = curriedSizeConstraint(ge)
  val maxSize: Int => Email => Boolean = curriedSizeConstraint(le)

  val curriedMin20: EmailFilter = curriedSizeConstraintFn(ge)(20)
  val curriedMax20: EmailFilter = curriedSizeConstraintFn(le)(20)

}

trait EmailFiltering extends EmailCustomTypes with GenericFunctions {

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

trait EmailCustomTypes {

  type EmailFilter = Email => Boolean

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
