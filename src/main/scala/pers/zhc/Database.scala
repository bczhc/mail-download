package pers.zhc

import org.json.JSONArray
import pers.zhc.jni.sqlite.SQLite3

/**
  * @author bczhc
  */
class Database(path: String) {
  private val db = SQLite3.open(path)

  init()
  private val insertStatement =
    db.compileStatement("""INSERT INTO mail(headers_json,
      |                 subject,
      |                 sent_time,
      |                 received_time,
      |                 from_addr,
      |                 to_addr,
      |                 cc_addr,
      |                 bcc_addr,
      |                 size,
      |                 mime_data,
      |                 msg_id,
      |                 msg_num,
      |                 content_plain,
      |                 content_html)
      |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin)

  def init(): Unit = {
    db.exec(
      """CREATE TABLE IF NOT EXISTS mail
        |(
        |    headers_json  TEXT NOT NULL,
        |    subject       TEXT,
        |    sent_time     INTEGER,
        |    received_time INTEGER,
        |    from_addr     TEXT NOT NULL,
        |    to_addr       TEXT NOT NULL,
        |    cc_addr       TEXT NOT NULL,
        |    bcc_addr      TEXT NOT NULL,
        |    size          INTEGER,
        |    mime_data     BLOB NOT NULL,
        |    msg_id        TEXT,
        |    msg_num       INTEGER,
        |    content_plain TEXT,
        |    content_html  TEXT
        |)""".stripMargin
    )
  }

  def beginTransaction(): Unit = db.beginTransaction()

  def commit(): Unit = db.commit()

  def insert(
      headersJson: JSONArray,
      subject: Option[String],
      sentTime: Long,
      receivedTime: Long,
      addresses: Addresses,
      size: Long,
      mimeData: Array[Byte],
      messageId: Option[String],
      messageNumber: Int,
      content: MailContent
  ): Unit = {
    val makeAddrSeqString = { a: IterableOnce[Address] =>
      a.iterator.map(_.address).mkString("", ", ", "")
    }

    val fromAddrSeqString = makeAddrSeqString(addresses.from)
    val toAddrSeqString = makeAddrSeqString(addresses.recipients.to)
    val ccAddrSeqString = makeAddrSeqString(addresses.recipients.cc)
    val bccAddrSeqString = makeAddrSeqString(addresses.recipients.bcc)

    var plainContent: Option[String] = None
    var htmlContent: Option[String] = None

    val handleMailTextAndSet = { text: Text =>
      text match {
        case Text.Html(html) =>
          html match {
            case HtmlContent.Alternative(alternative, _) =>
              plainContent = Some(alternative.text)
              htmlContent = Some(alternative.html)
            case HtmlContent.PlainHtml(html, _) =>
              plainContent = None
              htmlContent = Some(html)
          }
        case Text.PlainText(text) => text
      }
    }

    content match {
      case MailContent.Normal(text, _)      => handleMailTextAndSet(text)
      case MailContent.Report(report, _, _) => handleMailTextAndSet(report)
    }

    insertStatement.reset()
    insertStatement.bind(
      Array(
        headersJson.toString,
        subject.orNull,
        sentTime,
        receivedTime,
        fromAddrSeqString,
        toAddrSeqString,
        ccAddrSeqString,
        bccAddrSeqString,
        size,
        mimeData,
        messageId.orNull,
        messageNumber,
        plainContent.orNull,
        htmlContent.orNull
      )
    )
    insertStatement.step()
  }

  def close(): Unit = {
    insertStatement.release()
    db.close()
  }
}
