final case class GetAttributeException(message: String)
  extends Exception(message) {

  def this(message: String, cause: Throwable) {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) {
    this(Option(cause).map(_.toString).getOrElse(""), cause)
  }

  def this() {
    this(null: String)
  }
}