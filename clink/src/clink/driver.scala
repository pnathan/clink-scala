package clink;
import scala.util.{Try, Success, Failure}
/*
entry point for the jar; test exerciser, etc.
 */

trait BackingStore {
  def read: DataFrame
}

class CSVReader(filename: String) extends BackingStore {
  private val _ = filename
  def read: DataFrame = ???
}

// In the initial notion, a table would have multiple pages and swap,
// as one iterated through them.  However, when considering the actual
// cases, a table likely simply maps to an input file. Therefore, this
// design collapses Page and Table together (as opposed to the
// original Common Lisp).
case class Table(store: BackingStore) {
  def headers: Seq[Header] = ???
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var _data: Option[DataFrame] = None
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def data: DataFrame = {
    Try(store.read) match {
      case Success(df) =>
        this._data = Some(df)
        df
      case Failure(e) =>
        throw e
    }
  }
  def row_count: Int = ???
  def width: Int = ???

  // Canonical Index of column ->
  private val known_indicies: scala.collection.mutable.Map[Int, Map[Element, Int]] = scala.collection.mutable.Map()
  def lookup(h: Header, e: Element): Option[Array[Element]] = {
    if(known_indicies.contains(h.canonical_index)) {
      // At the cost of indirect pointer lookups, a ~~~ O(k) lookup.
      Some(data.data(known_indicies(h.canonical_index)(e)))
    } else {
      // Oh man. O(n) search.
      data.data.find( test_row => test_row(h.canonical_index) == e)
    }
  }

  // Procedure to index the page.
  def index_page(h: Header): Unit = {
    _data match {
      case Some(d) =>
        // Each row in data.data has an _inferred_ specific ID counting
        // from 0.
        //
        // Indexing thus is walking the header's and generating the reverse
        // OID given the contents of the column.
        // IE
        // (("bob", 1.0), ("sally", 5.0), ("jane", -54)) is the data.
        // Then we have two possible maps: { "bob" -> 0, "sally" -> 1, "jane" -> 2} and
        // { 1.0 -> 0, 5.0 -> 1, -54 -> 2 }.
        ///
        val element_idx = h.canonical_index
        val index_datastructure: Map[Element, Int] = data.data.indices.map { i =>
          (d.data(i)(element_idx) -> i)
        }.toMap

        val _ = known_indicies.put(element_idx, index_datastructure)
        ()
      case None => { }
        ()

    }
  }
}

object driver {
  def main(args: Array[String]): Unit = {
    println("Y0 ho ho");
  }
}
