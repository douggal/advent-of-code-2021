object Day02 extends App {
  // created 12/02/2021
  // https://adventofcode.com/2021/day/2


  println(s"--- Day 2: Dive! ---")

  val filename = "testInput.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val course = bufferedSource
    .getLines
    .map(line => line.split(' '))  // split on char trims whitespace, regex doesn't
    .map(e => (e(0), e(1).toInt))
    .toSeq

    // course = Seq[Tuple2(String, Int)]

  course.foreach(d => println(s"${d(0)}: ${d(1)}"))

  println(s"Part 1 answer: TBD")


  bufferedSource.close

}
