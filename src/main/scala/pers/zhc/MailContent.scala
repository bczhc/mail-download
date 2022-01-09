package pers.zhc

/**
  * @author bczhc
  */
class MailContent()

case object MailContent {
  case class Normal(text: Text, attachments: Option[Array[Attachment]])
      extends MailContent

  case class Report(
      report: Text,
      event: String,
      returned: Option[Array[Byte]]
  ) extends MailContent
}
