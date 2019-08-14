package clink;
import java.io.File
import com.univocity.parsers.csv.{CsvParser, CsvParserSettings}

trait BackingStore {
  def read: DataFrame
}

case class CSVReader(filename: String) extends BackingStore {
  private val _ = filename
    @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.OptionPartial"))
  def read: DataFrame = {
    val settings = new CsvParserSettings()
    val reader = new CsvParser(settings)
    reader.beginParsing(new File(filename))

    val ab: scala.collection.mutable.ArrayBuffer[Array[Element]] = new scala.collection.mutable.ArrayBuffer()


    var row: Array[String] = Array()

    val headers = Header.to_headers(reader.parseNext())

    // java-centric code yields java-centric patterns... for now.
    //
    // Note that we stream the data in, which minimizes storing the
    // unparsed data.
    do {
      row = reader.parseNext()
      if (row != null)  {

        ab.append(row.map(e => Element.parse(e).get))
      }
    } while(row != null)

    DataFrame(headers, ab.toArray)
  }
}
