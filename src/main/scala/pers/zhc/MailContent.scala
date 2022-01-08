package pers.zhc

/**
  * @author bczhc
  */
class MailContent()

case object MailContent {
  case class PlainText(text: String, attachments: Option[Array[Attachment]])
      extends MailContent

  case class Html(
      alternative: AlternativeContent,
      inlineResource: Option[Array[HttpInlineAttachment]],
      attachments: Option[Array[Attachment]]
  ) extends MailContent

  case class PlainHtml(html: String) extends MailContent

  case class Report(
      report: Text,
      event: String,
      returned: Option[String]
  ) extends MailContent

  class Text

  case object Text {
    case class Plain(text: String) extends Text

    case class Html(alternative: AlternativeContent) extends Text
  }
}
