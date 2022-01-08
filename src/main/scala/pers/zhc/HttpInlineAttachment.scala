package pers.zhc

/**
  * @author bczhc
  */
class HttpInlineAttachment(
    val contentId: String,
    val mimeType: String,
    val data: Array[Byte]
) {}
