package clink;
import scala.util.{Try, Success, Failure}
trait ref {
}


trait ColumnRef
case class Name(val s: String) extends ColumnRef
case class Index(val i: Int) extends ColumnRef

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments", "org.wartremover.warts.Var"))
case class Header(canonical_name: String,
  canonical_index: Int,
  var associated_names: Seq[String] = Seq[String]()) extends ColumnRef {

  def add_associated_names(names: Seq[String]): Unit =
    this.associated_names = this.associated_names ++ names
  def or_associated_names: Seq[String] = associated_names

}

object Header {
  // Notionally, we expect a list of headers from a CSV file
  def to_headers(things: Seq[String]): Seq[Header] = {

    things.zipWithIndex.map { tuple: (String, Int) =>
      val (e: String, i: Int) = tuple
      Header(e, i)
      }
  }
}


// An element is one of:
// String, Integer, Float
trait Element {
  def to_s: String
}

object Element {
  def parse(s: String): Option[Element] = {
    Try(DfInteger(s.toInt)) match {
      case Success(i) => Some(i)
      case Failure(_) => {
        Try(DfDouble(s.toDouble)) match {
          case Success(d) => Some(d)
          case Failure(_) => Some(DfString(s))
        }
      }
    }
  }
}

object Elemental {
  implicit class Element[T](self: T) {
    def value(): Unit = ???
  }
}

// Value classes
case class DfString(val s: String) extends Element {
  def to_s: String = s
}
case class DfInteger(val i: Int) extends Element {
  def to_s: String = s"$i"
}
case class DfDouble(val i: Double) extends Element {
  def to_s: String = s"$i"
}

// TODO: maybe add implicit conversions from string to df string, etc.

case class DataFrame(
  headers: Seq[Header],
  data: Array[Array[Element]]) {

  def to_s: String = {
    (headers.map(_.canonical_name).mkString(", ") ++ "\n---\n" ++
      data.map { row => row.map(_.to_s).mkString(", ") }.mkString("\n"))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var internal_data: Array[Array[Element]] = data
  lazy val length: Int = internal_data.length
  def width: Int = headers.length

  /** Slow. Array append is slow.
    */
  def add_row(new_row: Array[Element]): Unit = {
    internal_data = internal_data :+ new_row
  }

  def cutout(): DataFrame = {
    DataFrame(headers, new Array[Array[Element]](0))
  }
}

object DataFrame {
  def synthensize(data: Array[Array[Element]]): DataFrame = ???
}
