package clink;


/*
entry point for the jar; test exerciser, etc.
 */
object driver {
  def main(args: Array[String]): Unit = {
    val store = CSVReader("test.csv")
    val table = Table(store)
    val df = table.data
    println(df.to_s)
  }
}
