package util

import org.scalatest.{ FlatSpec, Matchers, Inside }
import org.scalatest.prop.PropertyChecks

// Base class for test suites; see http://www.scalatest.org/user_guide/defining_base_classes
abstract class TestSpec extends FlatSpec with Matchers with Inside with PropertyChecks {
  def outputFrom(body: => Unit): String = {
    val out = new java.io.ByteArrayOutputStream
    Console.withOut(out) {
      body
    }
    out.close()
    out.toString
  }
}