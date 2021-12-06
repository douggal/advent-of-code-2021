import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Using
import scala.util.{Failure, Success, Try}

object Day06 extends App {

    // created 12/06/2021
    // https://adventofcode.com/2021/day/6

    println(s"--- Day 6: Lanternfish ---")

    // Puzzle Input Data File
    //val filename = "Day06Input.txt"
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


    // part 2
    // try 1 failed: try divide an conquer create two threads and run have the fish simulation on one and rest on the other
    // try 2 success:  group the fish by age in a HashMap with values count of how many at that age.
    val t2 = System.nanoTime

    val days2 = 256

    def runSimulation(gen0: Vector[Int], days: Int):BigInt = {
        // gen0 = list of ages of starting group of fish
        // fishes = array tracking each fish represented by its age
        // g is group number used only to debug and track workings of this function
        var fishes = scala.collection.mutable.HashMap[Int, BigInt]()
        for (f <- 0 to 8) fishes += f -> 0
        for (f <- gen0) fishes(f) += 1  // transform list of ages into counts by age
        for (i <- 0 until days) {
            val nfs = fishes(0)  // number of new fishes = number who reached age 0
            val fs = for (j<- 1 to 8) yield fishes(j)  // everybody gets one day older, handle 0 to 6 and new fish after
            for (f <- fs.zipWithIndex) fishes(f._2) = f._1  // in the HashMap move up the cnt at 8 to 7 etc
            // spawn: each 0 becomes another fish at age 6
            fishes(6) = fishes(6) + nfs
            // add new fish count at age 8 is number of fish who spawned at age 0
            fishes(8) = nfs
            println(s"Generation $i, fish count ${fishes.values.sum} ${fishes.mkString(", ")}")
        }
        //println(s"Group $g, Generation $i, fish count ${fishes.mkString(":")}")
        fishes.values.sum
    }

    val c = runSimulation(fishStart,days2)

    val duration2 = (System.nanoTime - t2) / 1e9d
    println(s"Done: Part 2 run time (by the clock): $duration2 sec")

    println(s"Day 6 Part 2 the number of lanternfish after $days2 is ${c}")

    // 1st try: to low  1128945478452

}
