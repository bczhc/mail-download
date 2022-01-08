package demo;

import com.sun.mail.imap.IMAPMessage;
import com.sun.mail.imap.IMAPStore;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.net.QuotedPrintableCodec;
import org.jetbrains.annotations.Nullable;
import org.json.JSONArray;
import org.json.JSONObject;
import pers.zhc.util.Assertion;

import javax.mail.*;
import javax.mail.internet.InternetAddress;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Demo {

    public static void main(String[] args) throws MessagingException, IOException, DecoderException {
        System.load("/usr/lib/x86_64-linux-gnu/libjni-lib.so");

        Properties props = new Properties();
        props.setProperty("mail.store.protocol", "imap");
        props.setProperty("mail.imap.host", "imap.126.com");
        props.setProperty("mail.imaps.port", "993");

        HashMap<String, String> iam = new HashMap<>();
//带上IMAP ID信息，由key和value组成，例如name，version，vendor，support-email等。
        iam.put("name", "bczhc0");
        iam.put("version", "1.0.0");
        iam.put("vendor", "myclient");
        iam.put("support-email", "bczhc0@126.com");
        Session session = Session.getInstance(props);

        IMAPStore store = (IMAPStore) session.getStore("imap");
//下方替换对应帐号和授权码
        store.connect("bczhc0@126.com", "INBGCIJNLOXUHRHH");

        store.id(iam);

        final Folder inbox = store.getFolder("INBOX");
        inbox.open(Folder.READ_ONLY);

        final int messageCount = inbox.getMessageCount();
        System.out.println(messageCount);
        final Message[] messages = inbox.getMessages();

        for (int i = messageCount - 1; i > messageCount - 1 - 10; --i) {
            Message message = messages[i];
            IMAPMessage m = (IMAPMessage) message;

            final JSONArray headersJSONArray = headers2JSONArray(m.getAllHeaders());
            final String subject = m.getSubject();
            final long sentTime = m.getSentDate().getTime();
            final long receivedTime = m.getReceivedDate().getTime();
            final Address[] fromAddress = m.getFrom();
            Assertion.doAssertion(fromAddress.length == 1);
            final InternetAddress from = (InternetAddress) fromAddress[0];
            final InternetAddress[] to = (InternetAddress[]) m.getRecipients(Message.RecipientType.TO);
            final InternetAddress[] cc = (InternetAddress[]) m.getRecipients(Message.RecipientType.CC);
            final InternetAddress[] bcc = (InternetAddress[]) m.getRecipients(Message.RecipientType.BCC);
            final long size = m.getSizeLong();
            final String mimeMessage = readStreamToString(m.getMimeStream());
            final String messageID = m.getMessageID();
            final int messageNumber = m.getMessageNumber();

            System.out.println(messageNumber);
            decodeRfc2047(((Multipart) message.getContent()).getBodyPart(1).getFileName());
//            final MailContent mailContent = getMailContent(m);
//            System.out.println(mailContent);
        }

        inbox.close();
    }

    private static String getPlainText(IMAPMessage m) throws MessagingException, IOException, DecoderException {
        if (!m.isMimeType("text/plain")) {
            throw new RuntimeException("MIME type needs to be \"text/plain\"");
        }
        String raw = readStreamToString(m.getRawInputStream());
        if (m.getEncoding().equalsIgnoreCase("quoted-printable")) {
            String charset = getCharsetFromMime(m.getContentType());
            if (charset == null) charset = "utf-8";

            return new String(QuotedPrintableCodec.decodeQuotedPrintable(raw.getBytes(StandardCharsets.US_ASCII)), charset);
        } else {
            return (String) m.getContent();
        }
    }

    private static class AlternativeParts {
        BodyPart text;
        BodyPart html;
    }

    private static AlternativeParts getAlternativePartFromMultipart(Multipart multipart) throws MessagingException {
        final int count = multipart.getCount();
        Assertion.doAssertion(count >= 2);
        BodyPart textPart = multipart.getBodyPart(0);
        BodyPart htmlPart = multipart.getBodyPart(1);
        Assertion.doAssertion(textPart.isMimeType("text/plain"));
        Assertion.doAssertion(htmlPart.isMimeType("text/html"));

        AlternativeParts r = new AlternativeParts();
        r.html = htmlPart;
        r.text = textPart;
        return r;
    }

    private static MailContent getMailContent(IMAPMessage m) throws MessagingException, IOException, DecoderException {
        MailContent mailContent = new MailContent();

        if (m.isMimeType("text/plain")) {
            mailContent.text = getPlainText(m);
            mailContent.html = null;
        } else if (m.isMimeType("multipart/alternative")) {

            final Multipart multipart = (Multipart) m.getContent();
            Assertion.doAssertion(multipart.getCount() == 2);

            final AlternativeParts parts = getAlternativePartFromMultipart(multipart);

            // TODO: 1/3/22 check charset
            mailContent.text = (String) parts.text.getContent();
            mailContent.text = (String) parts.html.getContent();

        } else if (m.isMimeType("multipart/mixed")) {

            final Multipart multipart = (Multipart) m.getContent();

            Assertion.doAssertion(multipart.getCount() >= 1);

            final BodyPart part0 = multipart.getBodyPart(0);

            if (part0.isMimeType("multipart/related")) {
                final Multipart relatedMultipart = (Multipart) part0.getContent();

                final AlternativeParts alternativeParts = getAlternativePartFromMultipart(
                        (Multipart) relatedMultipart.getBodyPart(0).getContent()
                );

                ArrayList<Attachment> attachmentList = new ArrayList<>();

                // count > 1: handle left inlined resources bound in HTML
                for (int i = 1; i < relatedMultipart.getCount(); i++) {
                    final BodyPart part = relatedMultipart.getBodyPart(i);

                    Attachment attachment = new Attachment();
                    attachment.inline = true;
                    attachment.id = part.getHeader("Content-ID")[0];
                    attachment.size = part.getSize();
                    attachmentList.add(attachment);
                }

                mailContent.attachments = attachmentList.toArray(new Attachment[0]);
                // TODO: 1/3/22 handle charset
                mailContent.text = (String) alternativeParts.text.getContent();
                mailContent.html = (String) alternativeParts.html.getContent();
            } else {

                Assertion.doAssertion(part0.isMimeType("multipart/alternative"));
                final AlternativeParts alternativeParts = getAlternativePartFromMultipart((Multipart) part0.getContent());

                mailContent.text = (String) alternativeParts.text.getContent();
                mailContent.html = (String) alternativeParts.html.getContent();

                // handle alone (non-inline) attachments
                for (int i = 1; i < multipart.getCount(); i++) {
                    Attachment attachment = new Attachment();
                    final BodyPart part = multipart.getBodyPart(i);

                }
            }

        } else {
            throw new RuntimeException("Unsupported MIME type");
        }

        return mailContent;
    }

    private static class Attachment {
        boolean inline;
        String id;
        long size;

        @Override
        public String toString() {
            return "Attachment{" +
                    "inline=" + inline +
                    ", id='" + id + '\'' +
                    ", size=" + size +
                    '}';
        }
    }

    private static class MailContent {
        String text;
        @Nullable String html = null;
        Attachment[] attachments;

        @Override
        public String toString() {
            return "Content{" +
                    "text='" + text + '\'' +
                    ", html='" + html + '\'' +
                    ", attachments=" + Arrays.toString(attachments) +
                    '}';
        }
    }

    private enum MailContentType {
        PLAIN_TEXT,
        HTML
    }

    private static class MailHtmlContent {

    }

    private static class MailAlternativeContent {

    }

    public static void streamWrite(InputStream is, OutputStream os) throws IOException {
        byte[] buf = new byte[4096];
        int readLen;
        while ((readLen = is.read(buf)) != -1) {
            os.write(buf, 0, readLen);
            os.flush();
        }
    }

    /**
     * encoded-word: ?=charset?encoding?encoded-text?=
     */
    private static final Pattern rfc2047Pattern = Pattern.compile("=\\?(.*)\\?([qQbB])\\?(.*)\\?=");

    public static String decodeRfc2047(String s) {
        StringBuilder sb = new StringBuilder();

        final Matcher matcher = rfc2047Pattern.matcher(s);
        while (matcher.find()) {
            Assertion.doAssertion(matcher.groupCount() == 3);
            final String charset = matcher.group(1);
            final char encoding = matcher.group(2).toLowerCase().charAt(0);
            final String encodedText = matcher.group(3);

            switch (encoding) {
                case 'b':
                    // base64
                    try {
                        sb.append(new String(Base64.decodeBase64(encodedText), charset));
                    } catch (UnsupportedEncodingException e) {
                        throw new RuntimeException(e);
                    }
                    break;
                case 'q':
                    // quoted-printable
                    try {
                        final byte[] bytes = QuotedPrintableCodec.decodeQuotedPrintable(encodedText.getBytes(StandardCharsets.US_ASCII));
                        sb.append(new String(bytes, charset));
                    } catch (DecoderException | UnsupportedEncodingException e) {
                        throw new RuntimeException(e);
                    }
                    break;
                default:
                    throw new RuntimeException("Encoding not supported");
            }
        }
        return sb.toString();
    }

    private static final Pattern mimeCharsetPattern = Pattern.compile("charset=(.*)$");

    @Nullable
    private static String getCharsetFromMime(String mime) {
        final Matcher matcher = mimeCharsetPattern.matcher(mime);
        if (matcher.find()) {
            Assertion.doAssertion(matcher.groupCount() == 1);
            return matcher.group(1);
        } else {
            return null;
        }
    }

    private static JSONArray headers2JSONArray(Enumeration<Header> headers) {
        JSONArray headersJSONArray = new JSONArray();

        while (headers.hasMoreElements()) {
            final Header header = headers.nextElement();

            JSONObject headerJSONObject = new JSONObject();
            headerJSONObject.put(header.getName(), header.getValue());
            headersJSONArray.put(headerJSONObject);
        }
        return headersJSONArray;
    }

    public static String readStreamToString(InputStream is) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        streamWrite(is, baos);
        return baos.toString();
    }
}