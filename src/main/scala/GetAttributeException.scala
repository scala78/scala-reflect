final case class GetAttributeException(attribute: String,
                                       private val message: String = "",
                                       private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
