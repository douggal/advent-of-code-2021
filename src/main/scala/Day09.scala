import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object Day09 extends App {

    // created 12/09/2021
    // https://adventofcode.com/2021/day/9

    println(s"--- Day 9: Smoke Basin ---")

    // Puzzle Input Data File
    //val filename = "./input/Day09Input.txt"
    val filename = "./input/testInput.txt"

    //Each number corresponds to the height of a particular location,
    // where 9 is the highest and 0 is the lowest a location can be
    case class inputLine(heightmap: Vector[Int])

    val readInputData = () => {
        val source = io.Source.fromFile(filename)
        for {
            line <- source.getLines().toVector
            cols = line.strip().split("").toVector.map(_.toInt)
        } yield
            inputLine(cols)
    }

    val input = readInputData()
    for (line <- input) {
        println(line)
    }

    println("------------------------------------")
    println("Data Quality Control:")
    println(s"Start Timestamp ${java.time.ZonedDateTime.now()}")
    println(s"Input file name: $filename")
    println(s"Each line is a: ${input.getClass}")
    println(s"Number lines: ${input.length}")
    println(s"Number items per line: ${input.head.heightmap.count(_ => true)}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part 1

    //Your first goal is to find the low points - the locations that are lower than
    // any of its adjacent locations.

    // The risk level of a low point is 1 plus its height

    // lows is the list of low points stored as array of Int row, col, and height
    val lows = ArrayBuffer[Vector[Int]]()
    val maxcol = input.head.heightmap.length-1
    for (row <- input.indices;
         col <- input.head.heightmap.indices) {
        val top = if (row-1 >= 0) input(row-1).heightmap(col) else Int.MaxValue
        val bottom = if (row+1 < input.length) input(row+1).heightmap(col) else Int.MaxValue
        val left = if (col-1 >= 0) input(row).heightmap(col-1) else Int.MaxValue
        val right = if (col+1 <= maxcol) input(row).heightmap(col+1) else Int.MaxValue
        val test = input(row).heightmap(col) < Vector[Int](top, bottom, left, right).min
        if (test) lows += Vector[Int](row, col, input(row).heightmap(col))
    }
    val risk = for (l<-lows) yield l(2)+1
    val answer = risk.sum

    println(s"Day 9 Part 1 sum of the risk levels at all the low points is: $answer")


    // Part 2

    /*
    Next, you need to find the largest basins so you know what areas are most important to avoid.

    A basin is all locations that eventually flow downward to a single low point.
        every low point has a basin, although some basins are very small.
        Locations of height 9 do not count as being in any basin,
        and all other locations will always be part of exactly one basin.

    The size of a basin is the number of locations within the basin, including the low point.
    */

    import scala.collection.mutable.Set

    /* Find the three largest basins and multiply their sizes together. */

    // from Scala docs:  By default, instances of case classes are immutable, and they are
    // compared by value (unlike classes, whose instances are compared by reference).
    case class Point(row: Int, col: Int)
    val lowPoints = for (row <- lows) yield Point(row(0),row(1))  // don't need height here and want to use case class
    val basins = ListBuffer[mutable.Set[Point]]()

    // given a basin, walk the heightmap until can't go any further.  return is the size of the basin
    def walkUp(b: Point, v:mutable.Set[Point]):mutable.Set[Point] = {
        if (input(b.row).heightmap(b.col) == 9) v
        else if (b.row-1 < 0) v += b
        else {
            //println(s"walkUP b= ${b.row},${b.col},  Next = ${b.row+1},${b.col}")
            walkUp(Point(b.row-1,b.col),  v += b)
        }
    }
    def walkDown(b: Point, v: mutable.Set[Point]):mutable.Set[Point] = {
        if (input(b.row).heightmap(b.col) == 9) v
        else if (b.row+1 > input.length-1) v += b
        else {
            //println(s"walkDown b= ${b.row},${b.col},  Next = ${b.row+1},${b.col}")
            walkDown(Point(b.row+1,b.col),  v += b)
        }
    }
    def walkRight(b: Point, v:mutable.Set[Point]):mutable.Set[Point] = {
        if (input(b.row).heightmap(b.col) != 9) {
            v.addAll(walkUp(b,v))
            println(s"walked right and Up: v= $v")
            v.addAll(walkDown(b,v))
            println(s"walked right and Down: v= $v")
        }

        if (b.col+1 > input(b.row).heightmap.length-1) v += b
        else {
            println(s"walking right next col: b = $b, v= ${b.row},${b.col}")
            walkRight(Point(b.row, b.col + 1), v)
        }
    }
    def walkLeft(b: Point, v:mutable.Set[Point]):mutable.Set[Point] = {
        if (input(b.row).heightmap(b.col) != 9) {
            v.addAll(walkUp(b,v))
            println(s"walked left and Up: v= $v")
            v.addAll(walkDown(b,v))
            println(s"walked left and Up: v= $v")
        }

        if (b.col == 0) println(s"B equals 0 ${b.row},${b.col}")
        if (b.col-1 < 0) v += b
        else walkLeft(Point(b.row,b.col-1),  v)
    }

    def walkBasin(p: Point):mutable.Set[Point] = {
        var b = mutable.Set[Point]()
        var c = walkRight(p, b) ++= walkLeft(p, b)
        println(s"walked right and left: basin = $c")
        c
    }

    println(s"$lows")
    println(s"$lowPoints")
    for (l <- lowPoints) {
        basins += walkBasin(l)
    }

    val answer2 = basins.sortBy(b => b.size).take(3).map(_.size).product


    println(s"Day 9 Part 2 the product of the size of the three largest basins is: $answer2")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
