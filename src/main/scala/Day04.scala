import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.util.{Failure, Success, Try}

object Day04 extends App {

    // created 12/04/2021
    // https://adventofcode.com/2021/day/4

    println(s"--- Day 4: Giant Squid ---")

    val filename = "Day04Input.txt"
    //val filename = "testInput.txt"

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
    class bingoBoard(val board: Vector[Int], val boardNumber: Int) {
        private val _size = 5
        var bingoNumber: Int = -1
        var bingo: Boolean = false
        // create a list 0's/1's to hold marked squares
        val markerList: ListBuffer[Int] = ListBuffer[Int]()
        for (i <- board.indices) markerList += 0

        def play(n: Int): Boolean = {
            if (!bingo) {
                val found = board.indexOf(n)
                if (found >= 0) {
                    markerList(found) = 1
                    //if (boardNumber == 24) println(s"$boardNumber: $_markerList")
                    checkBingo()
                    if (bingo) bingoNumber = n
                }
            }
            if (bingo) true else false
        }

        def checkBingo(): Unit = {
            // check the rows
            for (grp <- markerList.sliding(_size,_size).toList)
                if (grp.sum == _size) {
                    //println(s"bingo board $boardNumber")
                    bingo = true
                }

            // check the columns
            for (n <- 0 until _size)
                // list of every nth element in marker list
                // https://stackoverflow.com/questions/25227475/list-of-every-n-th-item-in-a-given-list
                val grp = markerList.drop(n).grouped(_size).map(_.head).toList
                //println(grp)
                // if the sum of 1's in this vertical slice is 5 then bingo
                if (grp.toVector.sum == _size) {
                    //println(s"bingo board $boardNumber")
                    bingo = true
                }
        }

        def score(): Int = {
            // b = a board in the list of boards
            /*
            Start by finding the sum of all unmarked numbers on that board; in this case,
            the sum is 188. Then, multiply that sum by the number that was just called
            when the board won, 24, to get the final score
            */
            if (bingo) {
                val sumUnmarked = board.zip(markerList).filter(i => if (i(1)==0) true else false).map(i => i(0)).sum
                sumUnmarked * bingoNumber
            } else -1
        }

    }

    // first to do item is to set up number caller from 1st line of input
    val numberCaller = input.head.split(',').toList.map(_.toInt)
    //println(numberCaller)

    // next, set up game board(s) from the remaining file input
    // keep them in a list and each board is separated by a blank line
    val bingoCardsList = ListBuffer[bingoBoard]()
    var l = ""
    var i = 0
    for (line <- input.tail) {
        if (line != "")
            l += " " + line
        else if (line == "" && l != "")
            // those leading spaces are a tripping hazard :)
            // https://stackoverflow.com/questions/7899525/how-to-split-a-string-by-space/7899558
            bingoCardsList += new bingoBoard(l.trim.split("\\s+", -1).toVector.map(_.toInt), i)
            i += 1
            l = ""
    }
    // pick up last board if there is one
    if (l != "") bingoCardsList += new bingoBoard(l.trim.split("\\s+", -1).toVector.map(_.toInt), i)

    val winningBoardsInOrder = ListBuffer[Int]()

    // play BINGO!
    def playBingoRound(n: Int): Unit = {
        bingoCardsList
          .zipWithIndex
          .foreach(card => {
              if (!card._1.bingo)
                  val b = card._1.play(n)
                  //println(s"Board ${board._2}, play $n, bingo ${board._1.bingo}")
                  if (b && winningBoardsInOrder.indexOf(card._2) < 0) {
                      winningBoardsInOrder += card._2
                  }
          })
    }

    def checkAllBoards(boards: ListBuffer[bingoBoard]): Vector[Int] = {
        val v =
            for (b <- bingoCardsList.indices if bingoCardsList(b).bingo)
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
        if (checkAllBoards(bingoCardsList).length > 0)
            bingo = true
    }

    val winningCards = checkAllBoards(bingoCardsList)
    val winningCard = if (winningCards.isEmpty) -1 else winningCards.head

    if (winningCard != -1)
        val y = bingoCardsList(winningCard).score()
        println(s"Day 4 Part 1 answer $y")
    else
        println("No winner this round")

    println()

    // Part Two.  Find and score the board that wins last.

    // re-set the bingo cards back to initial state
    for (b <- bingoCardsList;
        i <- b.markerList.indices)
         b.markerList(i) = 0

    for (b <- bingoCardsList)
        b.bingo = false

    println("Play BINGO! with the Giant Squid: Round Two Let the Squid Win")
    round = -1
    calledNumber = -1
    while (round < numberCaller.length-1 && winningBoardsInOrder.length <= bingoCardsList.length) {
        round += 1
        calledNumber = numberCaller(round)
        playBingoRound(calledNumber)
    }

    val lastWinningBoard = if (winningBoardsInOrder.isEmpty) -1 else winningBoardsInOrder.last

    if (lastWinningBoard != -1)
        val x = bingoCardsList(lastWinningBoard).score()
        println(s"Day 4 Part 2 score of last winning board, $lastWinningBoard, is $x")
    else
        println("Part 2 No winner this round")
}
