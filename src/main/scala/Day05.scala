import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.util.{Failure, Success, Try}

object Day05 extends App {

    // created 12/05/2021
    // https://adventofcode.com/2021/day/5

    println(s"--- Day 5: Hydrothermal Venture ---")

    //val filename = "Day05Input.txt"
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

    // debug: check input, should be 4 numbers separated by commas
    // first two items are the start point and 2nd two are the end point of each line
    input.foreach(s => println("%25s".format(s)))

    // represent the line segments as parallel
    val x = ListBuffer[Int]()
    val y = ListBuffer[Int]()
    val z = ListBuffer[Int]()

    for (line <- input) {
        val l = line.split(",").toVector.map(_.toInt)
        val p = (l(0),l(1))
        val q = (l(2),l(3))
        // x coords
        val xs = (p._1 to q._1).toVector
        val ys = (p._1 to q._2).toVector
        // all the pairs xs with ys
        // https://stackoverflow.com/questions/27101500/scala-permutations-using-two-lists
        val xy = for {
            i1 <- xs
            i2 <- ys
        } yield (i1, i2)
        println(xy)
    }


    println(s"Day 5 Part 1 answer TBD")

}
