object Day21 extends App {

    // created 12/18/2021
    // https://adventofcode.com/2021/day/12

    println(s"--- Day 21: Dirac Dice ---")

    // Puzzle Input Data File
    //val filename = "./input/Day21Input.txt"
    //val filename = "./input/testInput.txt"

    //val input = List(6,9)
    val input = List(4,8)

    println("------------------------------------")
    println("Data Quality Control:")
    println(s"Start Timestamp ${java.time.ZonedDateTime.now()}")
    println("------------------------------------")


    // Part One
    class CircularTrack(x: Int) {
        var _position = x-1

        // 0,1,2,3,4,5,6,7,8,9
        def getPosition(): Int = {
            _position + 1
        }

        def move(dx: Int): Unit = {
            _position = (_position + dx) % 10
        }

        override def toString: String =
            s"Board position [${_position+1}]"
    }

    class detDie() {
        var N: Int = 0
        var faceValue: Int = 0

        def roll(): Int = {
            faceValue += 1
            N += 1
            faceValue
        }
        override def toString: String =
            s"Nbr rolls ($N)"
    }

    val die = new detDie()
    var p1score:Int = 0
    val p1board = new CircularTrack(input.head)
    var p2score:Int = 0
    val p2board = new CircularTrack(input(1))
    // have to alternate between the two players
    while (p1score < 1000 && p2score < 1000) {
        val p1dx = die.roll() + die.roll() + die.roll()
        p1board.move(p1dx)
        p1score += p1board.getPosition()

        if (p1score < 1000) {
            val p2dx = die.roll() + die.roll() + die.roll()
            p2board.move(p2dx)
            p2score += p2board.getPosition()
        }

        println(s"Player 1 score was ${p1score} at ${p1board}, and Player 2 score was ${p2score} at $p2board")
    }


    val answer = if (p1score > p2score) p2score * die.N else p1score*die.N

    if (p1score == p2score)
      println("They're Equal!")

    println(s"Day 21 Part 1 ${answer}.")
    println(s"Player 1 score was ${p1score}")
    println(s"Player 2 score was ${p2score}")
    println(s"Nbr rolls was ${die.N}")


    println(s"End at ${java.time.ZonedDateTime.now()}")

}
