import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Using
import scala.util.{Failure, Success, Try}

object Day06 extends App {

    // created 12/06/2021
    // https://adventofcode.com/2021/day/6

    println(s"--- Day 6: Lanternfish ---")

    // Puzzle Input Data File
    val filename = "Day06Input.txt"
    //val filename = "testInput.txt"

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

    //single number that represents the number of days until it creates a new lanternfish
    //input is ages of several hundred nearby lanternfish (your puzzle input)
    // each lanternfish creates a new lanternfish once every 7 days
    // a new lanternfish would need two more days for its first cycle
    // a new fish resets its timer to 6, not 7 (because 0 is a valid timer value)
    // new lanternfish starts with an internal timer of 8 and does not start counting down until the next day
    /*
        Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each other
        number decreases by 1 if it was present at the start of the day.
    */
    val fishStart = input.head.split(",").map(_.toInt).toVector
    //println(s"Fish as vector of int: ${input.mkString(", ")}")

    // part 1: let's try modeling the fish as an ArrayBuffer
    var fish = ArrayBuffer[Int]()
    for (f <- fishStart) fish += f

    val days = 80
    for (i <- 0 until days) {
            val nfs = fish.count(f => f == 0)  // new fishes
            fish = fish.map(f => if (f == 0) 6 else f-1)
            fish ++= (for (nf <- 0 until nfs) yield 8).toList
            if (i % 10 == 0) println(s"Generated $i")
    }

    //println(s"${fish.mkString(", ")}")


    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Done: Part 1 run time (by the clock): $duration sec")

    println(s"Day 6 Part 1 the number of lanternfish after $days is ${fish.length}")

}
