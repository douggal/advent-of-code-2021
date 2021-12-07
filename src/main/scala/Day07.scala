import scala.util.{Failure, Success, Try, Using}

object Day07 extends App {

    // created 12/07/2021
    // https://adventofcode.com/2021/day/7

    println(s"--- Day 7: The Treachery of Whales ---")

    // Puzzle Input Data File
    val filename = "Day07Input.txt"
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
            println(s"Success, puzzle input file read")
            i
        case Failure(s) =>
            println(s"Failed to read input file, message is: $s")
            Vector[String]()
    //endregion

    val t1 = System.nanoTime

    val cs = input.head.split(",").map(_.toInt).toVector  // crab submarine start positions
    //println(s"Crab submarines as vector of int: ${input.mkString(", ")}")

    val diff = cs.max - cs.min
    val ll = cs.min - diff  // left limit   ???
    val rl = cs.max + diff  // right limit  ???
    // ds = absolute value of difference (distance) at each point
    val ds = for (i <- ll to rl)  yield (i, cs.map(c => Math.abs(c - i)).sum)

    /*
    Determine the horizontal position that the crabs can align to using the least fuel possible.
    How much fuel must they spend to align to that position?
    */
    //println(ds.mkString(", "))

    val answer = ds.map(p => p._2).min

    // https://stackoverflow.com/questions/37884493/scala-get-values-from-somevalue
    val position: Int = ds.find(p => p._2 == answer).map(p => p._1) match {
        case None => -9999 //Or handle the lack of a value another way: throw an error, etc.
        case Some(i: Int) => i //return the string to set your value
    }

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Done: Part 1 run time (by the clock): $duration sec")
    println(s"Number of crab submarines ${cs.length}")

    println(s"Day 7 Part 1 the minimum fuel use is: $answer at position $position")  //323647

    // Part 2
    val t2 = System.nanoTime

    /*
    As it turns out, crab submarine engines don't burn fuel at a constant rate.
    Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last:
    the first step costs 1, the second step costs 2, the third step costs 3, and so on.
    1 to 5 then costs 1 + 2 + 3 + 4
    */
    def seriesSum(min:Int, max:Int):BigInt = {
        // sum of arithmetic series https://en.wikipedia.org/wiki/Arithmetic_progression
        val d = Math.abs(max-min)
        (d*(d + 1)) / 2
    }

    val ds2 = for (i <- ll to rl)  yield (i, cs.map(c => seriesSum(c, i)).sum)

    println(ds2.mkString(", "))

    val answer2 = ds2.map(p => p._2).min

    // https://stackoverflow.com/questions/37884493/scala-get-values-from-somevalue
    val position2: Int = ds2.find(p => p._2 == answer2).map(p => p._1) match {
        case None => -9999 //Or handle the lack of a value another way: throw an error, etc.
        case Some(i: Int) => i //return the string to set your value
    }

    val duration2 = (System.nanoTime - t2) / 1e9d
    println(s"Done: Part 2 run time (by the clock): $duration2 sec")

    println(s"Day 7 Part 2 the minimum fuel use is: ${answer2} at position $position2")
}
