import scala.util.{Failure, Success, Try, Using}

object Day07 extends App {

    // created 12/07/2021
    // https://adventofcode.com/2021/day/7

    println(s"--- Day 7: The Treachery of Whales ---")

    // Puzzle Input Data File
    //val filename = "Day07Input.txt"
    val filename = "testInput.txt"

    //region Read puzzle input file into Vector[String] 'input'
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
                line.strip.replace(" -> ",",")
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
    //endregion

    val t1 = System.nanoTime

    val crabStart = input.head.split(",").map(_.toInt).toVector
    println(s"Crab submarines as vector of int: ${input.mkString(", ")}")




    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Done: Part 1 run time (by the clock): $duration sec")

    println(s"Day 7 Part 1 the point of minimum fuel use is: TBD")

}
