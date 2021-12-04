import scala.collection.mutable.ListBuffer

object Day04 extends App {

  // created 12/04/2021
  // https://adventofcode.com/2021/day/4

  println(s"--- Day 4: Giant Squid ---")

  //val filename = "Day04Input.txt"
  val filename = "testInput.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val input = bufferedSource
    .getLines
    .map(line => line.strip)
    .toSeq

  bufferedSource.close

  class bingoBoard(val board: Vector[String]) {
    // create a list 0's/1's to hold marked squares
    private val _markerList = ListBuffer[Int]()
    for (i <- 0 until board.length) _markerList += 0

    def bingo(): Boolean = {
      // TODO: write a check for bingo
      ???
    }
  }

  // set up number caller
  val numberCaller = input.head.split(',').toList.map(_.toInt)
  //println(numberCaller)





  println(s"Day 4 Part 1 answer TBD")
  println()

  println(s"Day 4 Part 2 answer: TBD")

}
