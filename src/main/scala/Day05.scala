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

    // represent the line segments as parallel lists
    // z is the count of hits
    // index into z is index at which (x,y) is found in list x and list y
    val x = ListBuffer[Int]()
    val y = ListBuffer[Int]()
    val z = ListBuffer[Int]()

    for (line <- input) {
        val l = line.split(",").toVector.map(_.toInt)
        val xs = (l(0) to l(2)).toVector
        val ys = (l(1) to l(3)).toVector
        // either the  x or y coordinate is same for every pair
        // generate all the pairs xs with ys
        // this is path followed from one endpoint to the other
        // ref: https://stackoverflow.com/questions/27101500/scala-permutations-using-two-lists
        val xy = for {
            i1 <- xs
            i2 <- ys
        } yield (i1, i2)
        //println(xy.mkString(" , "))
        // for each point (Tuple2) generated:
        //   populate x, y, z lists
        xy.foreach(t => {
            val (tx,ty) = t
            if (!x.zip(y).contains(t)) {
                // case 1 the point does not exist in x, y, z, add it
                x.append(tx)
                y.append(ty)
                z.append(1)
            } else {
                // case 2 point does exist, update count
                // find index of the point (x,y).  x.zip(y).zipWithIndex = Vector[((x,y),index)]
                val q = x.zip(y).zipWithIndex.filter(f => {if (f._1._1==tx && f._1._2==ty) true else false})
                // if there's more than one point in q then mistake.  q.head._2 = index of this point in parallel lists
                z(q.head._2) += 1
                val i = 0
            }

        })

        val i = 1
    }
    println(s"Day 5 Part 1 answer TBD")

}
