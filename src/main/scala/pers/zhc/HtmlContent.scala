package pers.zhc

/**
  * @author bczhc
  */
class HtmlContent

case object HtmlContent {
  case class Alternative(
      alternative: AlternativeContent,
      related: Option[Array[HttpInlineAttachment]]
  ) extends HtmlContent
  case class PlainHtml(
      html: String,
      related: Option[Array[HttpInlineAttachment]]
  ) extends HtmlContent
}
