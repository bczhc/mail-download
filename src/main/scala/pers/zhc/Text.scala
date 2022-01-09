package pers.zhc

/**
  * @author bczhc
  */
class Text {
  override def toString: String = {
    this match {
      case Text.Html(html) =>
        html match {
          case HtmlContent.Alternative(alternative, _) => alternative.html
          case HtmlContent.PlainHtml(html, _)          => html
        }
      case Text.PlainText(text) => text
    }
  }
}

case object Text {
  case class PlainText(text: String) extends Text

  case class Html(html: HtmlContent) extends Text
}
