package pers.zhc

/**
  * @author bczhc
  */
case class Addresses(
    sender: Address,
    from: List[Address],
    recipients: Addresses.Recipients
)

object Addresses {
  case class Recipients(
      to: List[Address],
      cc: List[Address],
      bcc: List[Address]
  )
}
