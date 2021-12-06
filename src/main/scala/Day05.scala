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

    def getRange(a:Int, b:Int):Range = {
        if (a<=b)
            a to b
        else
            a to b by -1
    }

    // debug: visually check input, should be 4 numbers separated by commas
    // first two items are the start point and 2nd two are the end point of each line
    // input.foreach(line => println("%s".format(line)))

    // represent the line segments as parallel lists with
    // Compressed sparse row (CSR, CRS or Yale format)
    // z is the count of hits
    // index into z is index at which (x,y) is found in list x and list y
    // Ref: https://en.wikipedia.org/wiki/Sparse_matrix
    val x = ListBuffer[Int]()
    val y = ListBuffer[Int]()
    val z = ListBuffer[Int]()

    val t1 = System.nanoTime

    var i =0
    var part = "1"
    for (line <- input) {
        i+=1
        println(f"Part $part, row $i, ${(System.nanoTime - t1) / 1e9d}")
        /*
        Part 1: For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2
        */
        val l = line.split(",").toVector.map(_.toInt)
        if (l(0)==l(2)||l(1)==l(3)) {
            val xs = getRange(l(0), l(2)).toVector
            val ys = getRange(l(1), l(3)).toVector
            // either the  x or y coordinate is same for every pair
            // generate all the pairs xs with ys
            // this is path followed from one endpoint to the other
            // ref: https://stackoverflow.com/questions/27101500/scala-permutations-using-two-lists
            val xys = if (ys.length == 1) {
                for {
                    i1 <- xs
                } yield (i1, ys.head)
            } else {
                for {
                    i2 <- ys
                } yield (xs.head, i2)
            }
            //print("xys ")
            //println(xys.mkString(" , "))
            // for each point (Tuple2) generated:
            //   populate x, y, z lists
            xys.foreach(t => {
                val (tx, ty) = t
                if (!x.zip(y).contains(t)) {
                    // case 1 the point does not exist in x, y, z, add it
                    x.append(tx)
                    y.append(ty)
                    z.append(1)
                } else {
                    // case 2 point does exist, update count
                    // find index of the point (x,y).
                    val q = x.zip(y).indexOf(t)
                    z(q) += 1
                    val i = 0
                }
            })
        }
    }
    //println(x.mkString(" , "))
    //println(y.mkString(" , "))
    //println(x.zip(y).mkString(" ,"))
    //println(z.mkString(" , "))

    val result = z.count(f => f > 1)

    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Done: Part 1 run time (by the clock): $duration sec")

    println(s"Day 5 Part 1 the number points at which line segments overlap ${result}")




    // Part 2: with diagonals

    // add "d" to variable names to distinguish.

    val xd = ListBuffer[Int]()
    val yd = ListBuffer[Int]()
    val zd = ListBuffer[Int]()

    val t1d = System.nanoTime

    i =0
    part = "2"
    for (line <- input) {
        i+=1
        println(s"Part $part, row $i, ${(System.nanoTime - t1d) / 1e9d}")
        /*
        Part 2: consider 45 degree diagonals too
        */
        val l = line.split(",").toVector.map(_.toInt)
        val xs = getRange(l(0), l(2)).toVector
        val ys = getRange(l(1), l(3)).toVector
        // either the  x or y coordinate is same for every pair
        // generate all the pairs xs with ys
        // this is path followed from one endpoint to the other
        // ref: https://stackoverflow.com/questions/27101500/scala-permutations-using-two-lists
        val xys = if (ys.length == 1) {
            for {
                i1 <- xs
            } yield (i1, ys.head)
        } else if (xs.length == 1) {
            for {
                i2 <- ys
            } yield (xs.head, i2)
        } else {
            for {t <- xs.zip(ys)} yield t
        }
        // print("xys ")
        // println(xys.mkString(" , "))
        // for each point (Tuple2) generated:
        //   populate x, y, z lists
        xys.foreach(t => {
            val (tx, ty) = t
            if (!xd.zip(yd).contains(t)) {
                // case 1 the point does not exist in x, y, z, add it
                xd.append(tx)
                yd.append(ty)
                zd.append(1)
            } else {
                // case 2 point does exist, update count
                // find index of the point (x,y).
                val q = xd.zip(yd).indexOf(t)
                zd(q) += 1
            }
        })
    }
    //println(xd.mkString(" , "))
    //println(yd.mkString(" , "))
    //println(xd.zip(yd).mkString(" ,"))
    //println(zd.mkString(" , "))

    val resultd = zd.count(f => f > 1)

    val durationd = (System.nanoTime - t1) / 1e9d
    println(s"Done: Part 2 run time (by the clock): $durationd sec")

    println(s"Day 5 Part 2 the number points at which line segments overlap ${resultd}")

}
