import java.sql.Date
import scala.reflect.runtime.universe._

object typed {
  /**
   *
   * @param name
   * @param attr
   * @tparam T
   * @return
   */
  def getAttribute[T: TypeTag](name: String)(implicit attr: Map[String, String]): T = attr.get(name) match {

    case Some(r) => try {
      val typeT: Type = typeOf[T]
      typeT match {
        case _ if typeT =:= typeOf[Long] => r.toLong.asInstanceOf[T]
        case _ if typeT =:= typeOf[Int] => r.toInt.asInstanceOf[T]
        case _ if typeT =:= typeOf[String] => r.asInstanceOf[T]
        case _ if typeT =:= typeOf[Date] => Date.valueOf(r).asInstanceOf[T]
        case _ if typeT =:= typeOf[Boolean] => try {
          r match {
            case "true" | "false" => r.toBoolean.asInstanceOf[T]
            case "0" => false.asInstanceOf[T]
            case "1" => true.asInstanceOf[T]
            case _ => throw GetAttributeException(name)
          }
        } catch {
          case e: IllegalArgumentException => throw GetAttributeException(name, e.getMessage)
        }
     //   case _ if typeT =:= typeOf[Int] => r.toInt.asInstanceOf[T]
        case _ => r.asInstanceOf[T]
      }
    }
    catch {
      case e: RuntimeException => throw GetAttributeException(name, e.getMessage)
    }
    case _ => throw GetAttributeException(name, s"Error attribute $name is not defined.")
  }

  /**
   *
   * @param name
   * @param attr
   * @tparam T
   * @return
   */
  def getAttributeOption[T: TypeTag](name: String)(implicit attr: Map[String, String]): Option[T] = attr.get(name) match {

    case Some(r) => try {
      val typeT: Type = typeOf[T]
      typeT match {
        case _ if typeT =:= typeOf[Long] => Some(r.toLong.asInstanceOf[T])
        case _ if typeT =:= typeOf[Int] => Some(r.toInt.asInstanceOf[T])
        case _ if typeT =:= typeOf[String] => Some(r.asInstanceOf[T])
        case _ if typeT =:= typeOf[Date] => Some(Date.valueOf(r).asInstanceOf[T])
        case _ if typeT =:= typeOf[Boolean] => try {
          r match {
            case "true" | "false" => Some(r.toBoolean.asInstanceOf[T])
            case "0" => Some(false.asInstanceOf[T])
            case "1" => Some(true.asInstanceOf[T])
            case _ => None
          }
        } catch {
          case e: IllegalArgumentException => throw GetAttributeException(name, e.getMessage)
        }
        case _ => Some(r.asInstanceOf[T])
      }
    } catch {
      case e: RuntimeException => throw GetAttributeException(name, e.getMessage)
    }
    case None => None
  }
}
