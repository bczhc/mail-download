package pers.zhc

import com.sun.mail.imap.{IMAPBodyPart, IMAPMessage, IMAPStore}
import org.apache.commons.codec.DecoderException
import org.apache.commons.codec.binary.Base64
import org.apache.commons.codec.net.QuotedPrintableCodec

import java.io._
import java.nio.charset.StandardCharsets
import java.util
import java.util.Properties
import java.util.regex.Pattern
import javax.mail.{Folder, Message, Multipart, Part, Session}

/**
  * @author bczhc
  */
object Main {
  def main(args: Array[String]): Unit = {
//    System.load("/usr/lib/x86_64-linux-gnu/libjni-lib.so")

    val authProperties = new Properties()
    val is = new FileInputStream(new File("./mail-auth.properties"))
    authProperties.load(is)
    is.close()

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

    for (message <- messages.takeRight(20)) {
      val mailContent = getMailContent(message)
      println(mailContent)
    }
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

  def readStreamToString(is: InputStream): String = {
    val baos = new ByteArrayOutputStream
    streamWrite(is, baos)
    baos.toString
  }

  /**
    * encoded-word: ?=charset?encoding?encoded-text?=
    */
  private val rfc2047Pattern = Pattern.compile("=\\?(.*)\\?([qQbB])\\?(.*)\\?=")

  def decodeRfc2047(s: String): String = {
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
    val rawContent = readStreamToString(infos._3)
    if (infos._2.equalsIgnoreCase("quoted-printable")) {
      val charset = getCharsetFromMime(infos._4) match {
        case Some(value) =>
          value
        case None => "utf-8"
      }

      new String(
        QuotedPrintableCodec.decodeQuotedPrintable(
          rawContent.getBytes(StandardCharsets.US_ASCII)
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
    assert(multipart.getCount == 2)
    val textPart = multipart.getBodyPart(0).asInstanceOf[IMAPBodyPart]
    val htmlPart = multipart.getBodyPart(1).asInstanceOf[IMAPBodyPart]
    assert(textPart.isMimeType("text/plain"))
    assert(htmlPart.isMimeType("text/html"))

    AlternativeContent(readPlainText(textPart), readPlainText(htmlPart))
  }

  private def getAttachmentsFromMultipart(
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
            attachmentPart.getContent.asInstanceOf[InputStream]
          )
        )
      )
    }
    attachments.toArray(new Array[Attachment](0))
  }

  /**
    * read plain text or HTML content
    * @param bodyPart body part
    * @return
    */
  private def readText(bodyPart: IMAPBodyPart): MailContent.Text = {
    if (bodyPart.isMimeType("text/plain")) {
      MailContent.Text.Plain(readPlainText(bodyPart))
    } else if (bodyPart.isMimeType("multipart/alternative")) {

      MailContent.Text.Html(
        readAlternative(bodyPart.getContent.asInstanceOf[Multipart])
      )
    } else {
      throw new RuntimeException("MIME type requirement not satisfied")
    }
  }

  private def getMailContent(message: Message): MailContent = {
    if (message.isMimeType("text/plain")) {
      MailContent.PlainText(
        readPlainText(message.asInstanceOf[IMAPMessage]),
        None
      )
    } else if (message.isMimeType("multipart/alternative")) {

      MailContent.Html(
        readAlternative(message.getContent.asInstanceOf[Multipart]),
        None,
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

        MailContent.Html(
          readAlternative(
            multipart.getBodyPart(0).getContent.asInstanceOf[Multipart]
          ),
          None,
          Some(getAttachmentsFromMultipart(multipart = multipart))
        )

      } else if (part0.isMimeType("multipart/related")) {
        // HTML with its inline resource, and with attachments

        val relatedMultipart = part0.getContent.asInstanceOf[Multipart]
        assert(relatedMultipart.getCount >= 2)

        val alternative = readAlternative(
          relatedMultipart.getBodyPart(0).getContent.asInstanceOf[Multipart]
        )

        val inlineAttachments = new util.ArrayList[HttpInlineAttachment]()

        for (i <- 1 until relatedMultipart.getCount) {
          val inlineResourcePart = relatedMultipart.getBodyPart(i)
          val contentId = inlineResourcePart.getHeader("Content-ID")(0)
          inlineAttachments.add(
            new HttpInlineAttachment(
              contentId,
              inlineResourcePart.getContentType,
              readInputStreamBytes(
                inlineResourcePart.getContent.asInstanceOf[InputStream]
              )
            )
          )
        }

        MailContent.Html(
          alternative,
          Some(inlineAttachments.toArray(new Array[HttpInlineAttachment](0))),
          Some(getAttachmentsFromMultipart(multipart = multipart))
        )
      } else if (part0.isMimeType("text/plain")) {
        // plain text with attachments
        MailContent.PlainText(
          readPlainText(part0),
          Some(getAttachmentsFromMultipart(multipart = multipart))
        )
      } else {
        throw new RuntimeException("Unknown mail multipart part 0 MIME type")
      }
    } else if (message.isMimeType("multipart/report")) {

      val multipart = message.getContent.asInstanceOf[Multipart]
      // according to RFC3462
      assert(multipart.getCount == 2 || multipart.getCount == 3)

      MailContent.Report(
        readText(multipart.getBodyPart(0).asInstanceOf[IMAPBodyPart]),
        readStreamToString(multipart.getBodyPart(1).getInputStream),
        if (multipart.getCount == 3) {
          Some(readStreamToString(multipart.getBodyPart(2).getInputStream))
        } else None
      )
    } else if (message.isMimeType("text/html")) {
      MailContent.PlainHtml(readPlainText(message.asInstanceOf[IMAPMessage]))
    } else {
      throw new RuntimeException("Unknown mail content MIME type")
    }
  }
}
