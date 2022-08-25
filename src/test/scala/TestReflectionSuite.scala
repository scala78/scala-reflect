import classic.{getBoolean, getInt}
import org.scalatest.funsuite.AnyFunSuite
import typed.getAttribute

class TestReflectionSuite extends AnyFunSuite {
  def withTime[T](procName: String, f: => T): T = {
    val start = System.currentTimeMillis()
    val r = f
    val end = System.currentTimeMillis()
    print(s"$procName job done in ${end - start} ms, with result ")
    r
  }

  implicit val attribute: Map[String, String] = Map(
    "Date" -> "2022-01-01",
    "Long" -> "10000",
    "LongB" -> "b10000",
    "Int" -> "10",
    "IntB" -> "10bad",
    "DateBad" -> "2022isNot Date",
    "BoolTrue" -> "true",
    "BoolOne" -> "1",
  )
  val times = 100000

  test(s"Convert attribute boolean type with reflection, with one time") {

    assert(withTime(s"Convert attribute boolean type with reflection, with one time: ",
      getAttribute[Boolean]("BoolTrue"))
    )
  }

  test(s"Convert attribute boolean type with reflection, with $times times") {

    assert(withTime(s"Convert attribute boolean type with reflection, with $times times: ",
      (1 to times).map(_ => getAttribute[Boolean]("BoolTrue")).foldLeft(true)(_ == _))
    )
  }
  test(s"Convert attribute boolean type with out reflection, with $times times") {
    assert(withTime(s"Convert attribute boolean type with out reflection, with $times times: ",
      (1 to times).map(_ => getBoolean("BoolTrue")).foldLeft(true)(_ == _))
    )
  }
  test(s"Convert attribute integer type with reflection, with $times times") {

    assert(withTime(s"Convert attribute integer type with reflection, with $times times: ",
      (1 to times).map(_ => getAttribute[Int]("Int") == 10).foldLeft(true)(_ == _))
    )
  }
  test(s"Convert attribute integer type with out reflection, with $times times") {
    assert(withTime(s"Convert attribute integer type with out reflection, with $times times: ",
      (1 to times).map(_ => getInt("Int") == 10).foldLeft(true)(_ == _))
    )
  }
}
