import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.util.{Try, Success, Failure}

object Day04 extends App {

  // created 12/04/2021
  // https://adventofcode.com/2021/day/4

  println(s"--- Day 4: Giant Squid ---")

  //val filename = "Day04Input.txt"
  val filename = "testInput.txt"

  // Better read file w/Using object from Alexander, Alvin.
  // Scala Cookbook: Recipes for Object-Oriented and Functional Programming.
  // Second Edition. Beijing Boston: O’Reilly, 2021.
  def readFileAsSeq(filename: String): Try[Vector[String]] =
    Using(io.Source.fromFile(filename)) { bufferedSource =>
      val ucLines = for
        line <- bufferedSource.getLines
      // 'line' is a String. can work with each Char here,
      // if desired, like this:
      // char <- line
      yield
        // work with each 'line' as a String here
        line.strip
      ucLines.toVector
    }

  val input: Vector[String] = readFileAsSeq(filename) match
    case Success(i) => {
      println(s"Success, file read")
      i
    }
    case Failure(s) => {
      println(s"Failed to read input file, message is: $s")
      Vector[String]()
    }

  // a bingo board, two lists one for the board's numbers
  // and the other to track which spots are marked
  class bingoBoard(val board: Vector[Int]) {
    // create a list 0's/1's to hold marked squares
    private val _markerList = ListBuffer[Int]()
    for (i <- 0 until board.length) _markerList += 0

    def bingo(): Boolean = {
      // TODO: write a check for bingo
      true
    }
  }

  // set up number caller
  val numberCaller = input.head.split(',').toList.map(_.toInt)
  //println(numberCaller)

  // set up game board(s), keep them in a list
  val bingoBoards = ListBuffer[bingoBoard]()
  var l = ""
  for (line <- input.tail) {
    if (line != "")
      l += " " + line
    else if (line == "" && l != "")
      bingoBoards += new bingoBoard(l.split("\\s+", -1).toVector.drop(1).map(_.toInt))
      l = ""
  }
  // pick up last board if there is one
  if (l != "")  bingoBoards += new bingoBoard(l.split("\\s+",-1).toVector.drop(1).map(_.toInt))

  val i = 0



  println(s"Day 4 Part 1 answer TBD")
  println()

  println(s"Day 4 Part 2 answer: TBD")

}
