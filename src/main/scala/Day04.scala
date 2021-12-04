import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.util.{Failure, Success, Try}

object Day04 extends App {

    // created 12/04/2021
    // https://adventofcode.com/2021/day/4

    println(s"--- Day 4: Giant Squid ---")

    //val filename = "Day04Input.txt"
    val filename = "testInput.txt"

    // Try out a better read file w/Using object from Alexander, Alvin.
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

    // read the input file and with a check for success or failure
    val input: Vector[String] = readFileAsSeq(filename) match
        case Success(i) =>
            println(s"Success, file read")
            i
        case Failure(s) =>
            println(s"Failed to read input file, message is: $s")
            Vector[String]()

    // model the bingo board as two lists one for the board's numbers
    // and the other to track which spots are marked
    class bingoBoard(val board: Vector[Int]) {
        private val _size = 5
        var bingo = false
        // create a list 0's/1's to hold marked squares
        private val _markerList = ListBuffer[Int]()
        for (i <- board.indices) _markerList += 0

        def play(n: Int): Unit = {
          if (!bingo) {
              val found = board.indexOf(n)
              if (found != -1){
                  _markerList(found) = 1
              }
          }

        }

        def checkBingo = {
          // check the rows
          for (grp <- _markerList.grouped(5))
              if (grp.sliding(2).sum == _size)
                  bingo = true

          // check the columns
          for (grp <- _markerList.grouped(5,5))
              if (grp.sliding(2).sum == _size)
                  bingo = true
        }
    }

    // first to do item is to set up number caller from 1st line of input
    val numberCaller = input.head.split(',').toList.map(_.toInt)
    //println(numberCaller)

    // next, set up game board(s) from the remaining file input
    // keep them in a list and each board is separated by a blank line
    // TODO: seems to work ok, but can this code be improved?
    val bingoBoards = ListBuffer[bingoBoard]()
    var l = ""
    for (line <- input.tail) {
        if (line != "")
            l += " " + line
        else if (line == "" && l != "")
            // those leading spaces are a tripping hazard :)
            // https://stackoverflow.com/questions/7899525/how-to-split-a-string-by-space/7899558
            bingoBoards += new bingoBoard(l.trim.split("\\s+", -1).toVector.map(_.toInt))
            l = ""
    }
    // pick up last board if there is one
    if (l != "") bingoBoards += new bingoBoard(l.trim.split("\\s+", -1).toVector.map(_.toInt))

    // play BINGO!
    def bingoPlay(n: Int): Boolean = {
      for (b <- bingoBoards) {
        b.play(n)
      }
    }

    def checkBingo(boards: bingoBoard): Vector[Int] = {
      bingoBoards.filter(b => b.bingo == true).indices.toVector
    }

    def scoreBingoBoard(i: Int): Int = {
      0
    }

    println("Play BINGO! with the Giant Squid")
    var bingo = false
    var round = -1
    var calledNumber = -1
    while (!bingo && round < numberCaller.length) {
      calledNumber = numberCaller(round)
      round += 1
      bingoPlay(calledNumber)
      if (checkBingo(bingoBoards).length > 0)
          bingo = true
    }

    val winningBoards = checkBingo(bingoBoards)
    val winningBoard = if (winningBoards.isEmpty) -1 else winningBoards.head

    if (winningBoard != -1)
      val score = scoreBingoBoard(winningBoard)
      println(s"Day 4 Part 1 answer $score")
    else
      println("No winner this round")

  println()

    println(s"Day 4 Part 2 answer: TBD")

}
