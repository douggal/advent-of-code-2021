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

  class bingoBoard(val board: Vector[Int]) {
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

  // set up game board(s)
  val bingoBoards = ListBuffer[bingoBoard]()
  var l = ""
  for (line <- input.tail)
    if (line != "")
      l += " " + line
    else
      if (line == "" && l != "")
        bingoBoards += new bingoBoard(l.split("\\s+",-1).toVector.drop(1).map(_.toInt))
        l = ""
     // pick up last board if there is one
    if (l != "")  bingoBoards += new bingoBoard(l.split("\\s+",-1).toVector.drop(1).map(_.toInt))

  val i = 0



  println(s"Day 4 Part 1 answer TBD")
  println()

  println(s"Day 4 Part 2 answer: TBD")

}
