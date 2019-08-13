package clink;

import org.scalatest.FunSuite

// http://www.scalatest.org/at_a_glance/FunSuite
class HeaderTests extends FunSuite {

  test("Single header input") {
    val got_headers = Header.to_headers(Seq[String]("a"))
    val expected_headers = Seq(Header("a", 0))
    assert(got_headers == expected_headers)
  }
  test("multi header input") {
    val got_headers = Header.to_headers(Seq[String]("a", "b"))
    val expected_headers = Seq(Header("a", 0), Header("b", 1))
    assert(got_headers == expected_headers)
  }
  test("adding alias headers") {
    val got_headers = Header.to_headers(Seq[String]("a", "b"))
    got_headers(0).add_associated_names(Seq("name", "NAME"))
    got_headers(1).add_associated_names(Seq("id"))

    val expected_headers = Seq(
      Header("a", 0, Seq("name","NAME")),
        Header("b", 1, Seq("id")))
    assert(got_headers == expected_headers)
  }
}

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments", "org.wartremover.warts.NonUnitStatements"))
class ValueTests extends FunSuite {
  test("StringToElement") {
    assert(Element.parse("abc") == Some(DfString("abc")))
  }

  test("IntToElement") {
    assert(Element.parse("1") == Some(DfInteger(1)))
    assert(Element.parse("002") == Some(DfInteger(2)))
    assert(Element.parse("-1") == Some(DfInteger(-1)))
  }
  test("DoubleToElement") {
    assert(Element.parse("1.0") == Some(DfDouble(1.0d)))
    assert(Element.parse("002.4") == Some(DfDouble(2.4d)))
    assert(Element.parse("-1e10") == Some(DfDouble(-1e10)))
  }
}

class DFTests extends FunSuite {
  test("construct df") {
    val headers = Seq(Header("a", 0), Header("b", 1))
    val df = DataFrame(headers,
      Array(Array(DfString("abc"), DfDouble(1.0d)),
        Array(DfString("xyz"), DfDouble(100.0d))))

    assert(df.width == 2)
  }
}
