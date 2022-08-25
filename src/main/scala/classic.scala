import java.sql.Date

object classic {

  def getStringOption(name: String)(implicit attr: Map[String, String]): Option[String] =
    attr.get(name) match {
      case Some(r) => Some(r)
      case _ => None
    }

  def getBooleanOption(name: String)(implicit attr: Map[String, String]): Option[Boolean] = attr.get(name) match {
    case Some(r) => try {
      r match {
        case "true" | "false" => Some(r.toBoolean)
        case "0" => Some(false)
        case "1" => Some(true)
        case _ => None
      }
    } catch {
      case e: IllegalArgumentException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => None
  }

  def getDateOption(name: String)(implicit attr: Map[String, String]): Option[Date] = attr.get(name) match {
    case Some(r) => try {
      Some(Date.valueOf(r))
    } catch {
      case e: IllegalArgumentException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => None
  }

  def getLongOption(name: String)(implicit attr: Map[String, String]): Option[Long] = attr.get(name) match {
    case Some(r) => try {
      Some(r.toLong)
    } catch {
      case e: NumberFormatException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => None
  }

  def getIntOption(name: String)(implicit attr: Map[String, String]): Option[Int] = attr.get(name) match {
    case Some(r) => try {
      Some(r.toInt)
    } catch {
      case e: NumberFormatException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => None
  }

  def getBoolean(name: String)(implicit attr: Map[String, String]): Boolean = attr.get(name) match {
    case Some(r) => try {
      r match {
        case "true" | "false" => r.toBoolean
        case "0" => false
        case "1" => true
        case _ => throw GetAttributeException(name)
      }
    } catch {
      case e: IllegalArgumentException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => throw GetAttributeException(name)
  }

  def getString(name: String)(implicit attr: Map[String, String]): String =
    attr.get(name) match {
      case Some(r) => r
      case _ => throw GetAttributeException(name, s"Attribute $name, not found.")
    }

  def getDate(name: String)(implicit attr: Map[String, String]): Date = attr.get(name) match {
    case Some(r) => try {
      Date.valueOf(r)
    } catch {
      case e: IllegalArgumentException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => throw GetAttributeException(name, s"Attribute $name, not found.")
  }

  def getLong(name: String)(implicit attr: Map[String, String]): Long = attr.get(name) match {
    case Some(r) => try {
      r.toLong
    } catch {
      case e: NumberFormatException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => throw GetAttributeException(name, s"Attribute $name, not found.")
  }

  def getInt(name: String)(implicit attr: Map[String, String]): Int = attr.get(name) match {
    case Some(r) => try {
      r.toInt
    } catch {
      case e: NumberFormatException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => throw GetAttributeException(name, s"Attribute $name, not found.")
  }

}
