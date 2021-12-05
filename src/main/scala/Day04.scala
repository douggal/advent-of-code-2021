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
        var bingoNumber = -1
        var bingo = false
        // create a list 0's/1's to hold marked squares
        private val _markerList = ListBuffer[Int]()
        for (i <- board.indices) _markerList += 0

        def play(n: Int): Unit = {
            if (!bingo) {
                val found = board.indexOf(n)
                if (found != -1) {
                    _markerList(found) = 1
                    checkBingo()
                    if (bingo) bingoNumber = n
                }
            }
        }

        def checkBingo() = {
            // check the rows
            for (grp <- _markerList.grouped(_size).toVector)
                if (grp.sum == _size)
                    bingo = true

            // check the columns
            for (n <- 0 until _size)
                // list of every nth element in marker list
                //https://stackoverflow.com/questions/25227475/list-of-every-n-th-item-in-a-given-list
                val grp = _markerList.drop(n).grouped(_size).map(_.head).toList
                //println(grp)
                // if the sum of 1's in this vertical slice is 5 then bingo
                if (grp.toVector.sum == _size)
                    bingo = true
        }

        def score: Int = {
            // b = a board in the list of boards
            /*
            Start by finding the sum of all unmarked numbers on that board; in this case,
            the sum is 188. Then, multiply that sum by the number that was just called
            when the board won, 24, to get the final score
            */
            val sumUnmarked = board.zip(_markerList).filter(i => if (i(1)==0) true else false).map(i => i(0)).sum
            if (bingo) sumUnmarked * bingoNumber else -1
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
    def playBingoRound(n: Int): Unit = {
        bingoBoards
          .foreach(b => {
              b.play(n)
          })
    }

    def checkAllBoards(boards: ListBuffer[bingoBoard]): Vector[Int] = {
        val v =
            for (b <- bingoBoards.indices if bingoBoards(b).bingo)
                yield b
        v.toVector
    }

    println("Play BINGO! with the Giant Squid")
    var bingo = false
    var round = -1
    var calledNumber = -1
    while (!bingo && round < numberCaller.length-1) {
        round += 1
        calledNumber = numberCaller(round)
        playBingoRound(calledNumber)
        if (checkAllBoards(bingoBoards).length > 0)
            bingo = true
    }

    val winningBoards = checkAllBoards(bingoBoards)
    val winningBoard = if (winningBoards.isEmpty) -1 else winningBoards.head

    if (winningBoard != -1)
        val score = bingoBoards(winningBoard).score
        println(s"Day 4 Part 1 answer $score")
    else
        println("No winner this round")

    println()

    println(s"Day 4 Part 2 answer: TBD")

}
