package com.github.gvolpe.functions

// Dependency Injection using curried functions

object MockEmailRepository extends EmailRepository {
  def getEmails(user: User, unread: Boolean): Seq[Email] = Nil
}

object MockFilterRepository extends FilterRepository {
  override def getEmailFilter(user: User): EmailFilter = _ => true
}

object MockMailBoxService extends MailBoxService {
  val newEmails: (User) => Seq[Email] = getNewEmails(MockEmailRepository)(MockFilterRepository)
}

case class User(name: String)

trait EmailRepository {
  def getEmails(user: User, unread: Boolean): Seq[Email]
}

trait FilterRepository extends EmailCustomTypes {
  def getEmailFilter(user: User): EmailFilter
}

trait MailBoxService {
  def getNewEmails(emailRepo: EmailRepository)(filterRepo: FilterRepository)(user: User) =
    emailRepo.getEmails(user, true) filter (filterRepo.getEmailFilter(user))

  val newEmails: User => Seq[Email]
}