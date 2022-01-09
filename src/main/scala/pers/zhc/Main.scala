package pers.zhc

import com.sun.mail.imap.{IMAPBodyPart, IMAPMessage, IMAPStore}
import org.apache.commons.codec.DecoderException
import org.apache.commons.codec.binary.Base64
import org.apache.commons.codec.net.QuotedPrintableCodec
import org.json.{JSONArray, JSONObject}

import java.io._
import java.nio.charset.StandardCharsets
import java.util
import java.util.Properties
import java.util.regex.Pattern
import javax.mail.Message.RecipientType
import javax.mail.internet.InternetAddress
import javax.mail.{Folder, Header, Message, Multipart, Part, Session}

/**
  * @author bczhc
  */
object Main {
  private type JMailAddress = javax.mail.Address

  System.load("/usr/lib/x86_64-linux-gnu/libjni-lib.so")

  def main(args: Array[String]): Unit = {
    val authProperties = new Properties()
    val is = new FileInputStream(new File("./mail-auth.properties"))
    authProperties.load(is)
    is.close()

    val database = new Database("/home/zhc/mail.db")
    database.beginTransaction()

    val props = new Properties
    props.setProperty("mail.store.protocol", "imap")
    props.setProperty("mail.imap.host", "imap.126.com")
    props.setProperty("mail.imaps.port", "993")

    val iam: util.HashMap[String, String] = new util.HashMap[String, String]
    // 带上IMAP ID信息，由key和value组成，例如name，version，vendor，support-email等。
    iam.put("name", authProperties.getProperty("name"))
    iam.put("version", "1.0.0")
    iam.put("vendor", "myclient")
    iam.put("support-email", authProperties.getProperty("email"))
    val session = Session.getInstance(props)

    val store: IMAPStore = session.getStore("imap").asInstanceOf[IMAPStore]
    // 下方替换对应帐号和授权码
    store.connect(
      authProperties.getProperty("user"),
      authProperties.getProperty("password")
    )

    store.id(iam)

    val inbox: Folder = store.getFolder("INBOX")
    inbox.open(Folder.READ_ONLY)

    val messageCount = inbox.getMessageCount
    System.out.println(messageCount)
    val messages = inbox.getMessages

    val debug = (false, 256)
    val iterator =
      if (debug._1) List(messages(debug._2))
      else List.from(messages.slice(0, messages.length))
    for (m <- iterator) {
      val message = m.asInstanceOf[IMAPMessage]

      println(s"${message.getMessageNumber} of ${messages.length}")

      val headersJSON = headers2JSON(message.getAllHeaders)
      val subject = Option(message.getSubject)
      val sentTime = message.getSentDate.getTime
      val receivedTime = message.getReceivedDate.getTime
      val addresses = getAddresses(message)
      val size = message.getSizeLong

      val mimeData = readInputStreamBytes(message.getMimeStream)
      val messageId = Option(message.getMessageID)
      val messageNumber = message.getMessageNumber

      try {
        val mailContent = getMailContent(message)
        database.insert(
          headersJSON,
          subject,
          sentTime,
          receivedTime,
          addresses,
          size,
          mimeData,
          messageId,
          messageNumber,
          mailContent
        )

      } catch {
        case e: Exception =>
          e.printStackTrace()
      }
    }

    database.commit()
    database.close()
  }

  private def readInputStreamBytes(inputStream: InputStream): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    streamWrite(inputStream, baos)
    baos.toByteArray
  }

  private val mimeCharsetPattern = Pattern.compile("charset=(.*)$")

  private def getCharsetFromMime(mime: String) = {
    val matcher = mimeCharsetPattern.matcher(mime)
    if (matcher.find) {
      assert(matcher.groupCount() == 1)
      Some(matcher.group(1))
    } else None
  }

  def streamWrite(is: InputStream, os: OutputStream): Unit = {
    val buf = new Array[Byte](4096)
    var readLen = -1
    while ({
      readLen = is.read(buf)
      readLen
    } != -1) {
      os.write(buf, 0, readLen)
      os.flush()
    }
  }

  def readStreamToString(is: InputStream, charset: String = "UTF-8"): String = {
    val baos = new ByteArrayOutputStream
    streamWrite(is, baos)
    baos.toString(charset)
  }

  /**
    * encoded-word: ?=charset?encoding?encoded-text?=
    */
  private val rfc2047Pattern = Pattern.compile("=\\?(.*)\\?([qQbB])\\?(.*)\\?=")

  def decodeRfc2047(s: String): String = {
    if (!rfc2047Pattern.matcher(s).find()) {
      // not RFC2047 encoded
      return s
    }

    val sb = new StringBuilder
    val matcher = rfc2047Pattern.matcher(s)
    while ({
      matcher.find
    }) {
      assert(matcher.groupCount == 3)
      val charset = matcher.group(1)
      val encoding = matcher.group(2).toLowerCase.charAt(0)
      val encodedText = matcher.group(3)
      encoding match {
        case 'b' =>
          // base64
          try sb.append(new String(Base64.decodeBase64(encodedText), charset))
          catch {
            case e: UnsupportedEncodingException =>
              throw new RuntimeException(e)
          }

        case 'q' =>
          // quoted-printable
          try {
            val bytes = QuotedPrintableCodec.decodeQuotedPrintable(
              encodedText.getBytes(StandardCharsets.US_ASCII)
            )
            sb.append(new String(bytes, charset))
          } catch {
            case e @ (_: DecoderException | _: UnsupportedEncodingException) =>
              throw new RuntimeException(e)
          }

        case _ =>
          throw new RuntimeException("Encoding not supported")
      }
    }
    sb.toString
  }

  private def readPlainTextAny(message: Any) = {
    val supportedMime = List("text/plain", "text/html")
    val checkMime = { checkF: (String => Boolean) =>
      var r = false
      for (t <- supportedMime) {
        r |= checkF(t)
      }
      r
    }

    val infos = message match {
      case m: IMAPMessage =>
        (
          checkMime(m.isMimeType),
          m.getEncoding,
          m.getRawInputStream,
          m.getContentType
        )
      case m: IMAPBodyPart =>
        (
          checkMime(m.isMimeType),
          m.getEncoding,
          m.getRawInputStream,
          m.getContentType
        )
      case _ =>
        throw new RuntimeException(
          "Require message be IMAPMessage or IMAPBodyPart type"
        )
    }

    require(infos._1)
    if (infos._2.equalsIgnoreCase("quoted-printable")) {
      val charset = getCharsetFromMime(infos._4) match {
        case Some(value) =>
          value
        case None => "utf-8"
      }

      new String(
        QuotedPrintableCodec.decodeQuotedPrintable(
          readInputStreamBytes(infos._3)
        ),
        charset
      )
    } else {
      message.asInstanceOf[Part].getContent.asInstanceOf[String]
    }
  }

  private def readPlainText(message: IMAPBodyPart): String =
    readPlainTextAny(message)

  private def readPlainText(message: IMAPMessage): String =
    readPlainTextAny(message)

  private def readAlternative(multipart: Multipart): AlternativeContent = {
    val handleSingleAlternative = { multipart: Multipart =>
      var textPart: Option[IMAPBodyPart] = None
      var htmlPart: Option[IMAPBodyPart] = None

      // lock up the specified two parts
      for (i <- 0 until multipart.getCount) {
        val part = multipart.getBodyPart(i)
        if (part.isMimeType("text/plain")) {
          textPart = Some(part.asInstanceOf[IMAPBodyPart])
        } else if (part.isMimeType("text/html")) {
          htmlPart = Some(part.asInstanceOf[IMAPBodyPart])
        }
      }

      AlternativeContent(
        readPlainText(textPart.get),
        readPlainText(htmlPart.get)
      )
    }

    def readRecursiveAlternative: Multipart => AlternativeContent = {
      multipart: Multipart =>
        assert(multipart.getCount >= 1)
        val part0 = multipart.getBodyPart(0)
        if (part0.isMimeType("multipart/alternative")) {
          readRecursiveAlternative(part0.getContent.asInstanceOf[Multipart])
        } else {
          handleSingleAlternative(multipart)
        }
    }
    readRecursiveAlternative(multipart)
  }

  private def readAttachmentsFromMultipart(
      start: Int = 1,
      multipart: Multipart
  ) = {
    val attachments = new util.ArrayList[Attachment]()

    for (i <- start until multipart.getCount) {
      val attachmentPart =
        multipart.getBodyPart(i).asInstanceOf[IMAPBodyPart]
      val filename = decodeRfc2047(attachmentPart.getFileName)

      attachments.add(
        new Attachment(
          filename,
          attachmentPart.getContentType,
          readInputStreamBytes(
            attachmentPart.getInputStream
          )
        )
      )
    }
    attachments.toArray(new Array[Attachment](0))
  }

  private def readHtmlInlineAttachmentsFromMultipart(
      start: Int = 1,
      multipart: Multipart
  ): Array[HttpInlineAttachment] = {
    val list = new util.ArrayList[HttpInlineAttachment]()

    for (i <- start until multipart.getCount) {
      val inlineResourcePart = multipart.getBodyPart(i)
      val contentId = inlineResourcePart.getHeader("Content-ID")(0)
      list.add(
        new HttpInlineAttachment(
          contentId,
          inlineResourcePart.getContentType,
          readInputStreamBytes(
            inlineResourcePart.getContent.asInstanceOf[InputStream]
          )
        )
      )
    }
    list.toArray(new Array[HttpInlineAttachment](0))
  }

  private class InvalidMimeException(val msg: String) extends Exception(msg) {}

  /**
    * read plain text or HTML content
    * @param bodyPart body part
    * @return
    */
  private def readText(bodyPart: IMAPBodyPart): Text = {

    if (bodyPart.isMimeType("text/plain")) {

      Text.PlainText(readPlainText(bodyPart))

    } else if (bodyPart.isMimeType("text/html")) {

      Text.Html(HtmlContent.PlainHtml(readPlainText(bodyPart), None))

    } else if (bodyPart.isMimeType("multipart/alternative")) {

      Text.Html(
        HtmlContent.Alternative(
          readAlternative(bodyPart.getContent.asInstanceOf[Multipart]),
          None
        )
      )
    } else if (bodyPart.isMimeType("multipart/related")) {

      val html = handleRelatedMimeMultipart(
        bodyPart.getContent.asInstanceOf[Multipart]
      )
      Text.Html(html)

    } else {
      throw new InvalidMimeException("MIME type requirement not satisfied")
    }
  }

  private def getMailContent(message: Message): MailContent = {
    if (message.isMimeType("text/plain")) {
      MailContent.Normal(
        Text.PlainText(readPlainText(message.asInstanceOf[IMAPMessage])),
        None
      )
    } else if (message.isMimeType("multipart/alternative")) {

      MailContent.Normal(
        Text.Html(
          HtmlContent.Alternative(
            readAlternative(message.getContent.asInstanceOf[Multipart]),
            None
          )
        ),
        None
      )
    } else if (message.isMimeType("multipart/mixed")) {

      val multipart = message.getContent.asInstanceOf[Multipart]
      assert(multipart.getCount >= 1)
      val part0 = multipart.getBodyPart(0).asInstanceOf[IMAPBodyPart]
      if (part0.isMimeType("multipart/alternative")) {
        // HTML with attachments
        assert(multipart.getCount >= 2)
        assert(multipart.getBodyPart(0).isMimeType("multipart/alternative"))

        MailContent.Normal(
          Text.Html(
            HtmlContent.Alternative(
              readAlternative(
                multipart.getBodyPart(0).getContent.asInstanceOf[Multipart]
              ),
              None
            )
          ),
          Some(readAttachmentsFromMultipart(multipart = multipart))
        )

      } else if (part0.isMimeType("multipart/related")) {
        // HTML with its inline resource, and with attachments

        val attachments = Some(
          readAttachmentsFromMultipart(multipart = multipart)
        )

        val html = handleRelatedMimeMultipart(
          part0.getContent.asInstanceOf[Multipart]
        )

        MailContent.Normal(
          Text.Html(html),
          attachments
        )

      } else if (part0.isMimeType("text/plain")) {

        // plain text with attachments
        MailContent.Normal(
          Text.PlainText(readPlainText(part0)),
          Some(readAttachmentsFromMultipart(multipart = multipart))
        )

      } else if (part0.isMimeType("text/html")) {

        // plain html with attachments
        MailContent.Normal(
          Text.Html(HtmlContent.PlainHtml(readPlainText(part0), None)),
          Some(readAttachmentsFromMultipart(multipart = multipart))
        )

      } else {
        throw new RuntimeException(
          s"Unknown mail multipart part 0 MIME type ${part0.getContentType}"
        )
      }
    } else if (message.isMimeType("multipart/report")) {

      val multipart = message.getContent.asInstanceOf[Multipart]
      // according to RFC3462
      assert(multipart.getCount == 2 || multipart.getCount == 3)

      MailContent.Report(
        readText(multipart.getBodyPart(0).asInstanceOf[IMAPBodyPart]), {
          val eventPart = multipart.getBodyPart(1).asInstanceOf[IMAPBodyPart]
          if (eventPart.isMimeType("message/delivery-status")) {
            val bytes = readInputStreamBytes(eventPart.getRawInputStream)
            getCharsetFromMime(eventPart.getContentType) match {
              case Some(encoding) =>
                new String(bytes, encoding)
              case None =>
                assert(checkAllASCII(bytes))
                new String(bytes, StandardCharsets.US_ASCII)
            }
          } else {
            readText(eventPart).toString
          }
        },
        if (multipart.getCount == 3) {
          Some(
            readInputStreamBytes(
              multipart
                .getBodyPart(2)
                .asInstanceOf[IMAPBodyPart]
                .getRawInputStream
            )
          )
        } else None
      )
    } else if (message.isMimeType("text/html")) {

      MailContent.Normal(
        Text.Html(
          HtmlContent
            .PlainHtml(readPlainText(message.asInstanceOf[IMAPMessage]), None)
        ),
        None
      )
    } else if (message.isMimeType("multipart/related")) {

      // html with inline resource, no attachments
      val html = handleRelatedMimeMultipart(
        message.getContent.asInstanceOf[Multipart]
      )
      MailContent.Normal(Text.Html(html), None)

    } else {
      throw new RuntimeException(
        s"Unknown mail content MIME type: ${message.getContentType}"
      )
    }
  }

  private def headers2JSON(headers: util.Enumeration[Header]): JSONArray = {
    val iterator = new IterableEnumeration(headers)
    val jsonArray = new JSONArray()
    iterator.foreach({ header =>
      jsonArray.put({
        val jo = new JSONObject()
        jo.put("name", header.getName)
        jo.put("value", header.getValue)
        jo
      })
    })
    jsonArray
  }

  private def getAddresses(message: IMAPMessage): Addresses = {
    val resolveAddress = { a: JMailAddress =>
      val ia = a.asInstanceOf[InternetAddress]
      Address(Option(ia.getPersonal), ia.getAddress)
    }
    val handleNullableRecipients = { a: Array[JMailAddress] =>
      Option(a) match {
        case Some(value) =>
          value.map(resolveAddress)
        case None => new Array[Address](0)
      }
    }

    val sender = {
      val a = message.getSender.asInstanceOf[InternetAddress]
      Address(Option(a.getPersonal), a.getAddress)
    }
    val from = message.getFrom.map(resolveAddress)
    val to = handleNullableRecipients(message.getRecipients(RecipientType.TO))
    val cc = handleNullableRecipients(message.getRecipients(RecipientType.CC))
    val bcc = handleNullableRecipients(message.getRecipients(RecipientType.BCC))

    Addresses(
      sender,
      from.toList,
      Addresses.Recipients(to.toList, cc.toList, bcc.toList)
    )
  }

  def checkAllASCII(bytes: Array[Byte]): Boolean = {
    for (b <- bytes) {
      if (b <= 0 || b > 127) return false
    }
    true
  }

  def handleRelatedMimeMultipart(relatedMultipart: Multipart): HtmlContent = {
    assert(relatedMultipart.getCount >= 1)

    val inlineAttachments = if (relatedMultipart.getCount > 1) {
      Some(
        readHtmlInlineAttachmentsFromMultipart(multipart = relatedMultipart)
      )
    } else None

    readText(
      relatedMultipart.getBodyPart(0).asInstanceOf[IMAPBodyPart]
    ).asInstanceOf[Text.Html].html match {
      case HtmlContent.Alternative(alternative, _) =>
        HtmlContent.Alternative(alternative, inlineAttachments)
      case HtmlContent.PlainHtml(html, _) =>
        HtmlContent.PlainHtml(html, inlineAttachments)
    }
  }
}
