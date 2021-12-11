import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

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


    /* Find the three largest basins and multiply their sizes together. */

    // from Scala docs:  By default, instances of case classes are immutable, and they are
    // compared by value (unlike classes, whose instances are compared by reference).
    case class Point(row: Int, col: Int)
    case class StackItem(p:Point, s:String)

    // don't need height saved earlier with low points and want to use case class for each basin point
    val lowPoints = for (row <- lows) yield Point(row(0),row(1))
    val basins = ListBuffer[mutable.Set[Point]]()

    def walkBasin(p: Point):mutable.Set[Point] = {
        // the basin, b
        val b = mutable.Set[Point]()
        // a stack to help walk thru all the points in basin
        // at each point, next move is URDL, RDL, DL, L
        val s = mutable.Stack[StackItem]()

        // heightmap is a rectangular
        val colMax = input(p.row).heightmap.length - 1
        val rowMax = input.length - 1

        // U = next move to try is up; and so on, R = right; D = down; L = left
        s.push(StackItem(p,"URDL"))
        b += p
        while (s.nonEmpty) {
            // pop a stack item and proceed
            val si = s.top
            //println(s"Stack top: $si}")
            si.s match {
                case "URDL" =>
                    // move up
                    val newp = Point(si.p.row-1,si.p.col)
                    if (si.p.row-1 >= 0 && input(si.p.row-1).heightmap(si.p.col) != 9 && !b.contains(newp)) {
                        s.push(StackItem(newp, "URDL"))
                        b += newp
                    } else {
                        // can't go up any further, remaining areas are R, D, and L
                        val oldp = s.pop()
                        s.push(StackItem(oldp.p,"RDL"))
                    }
                case "RDL" =>
                    // move Right
                    val newp = Point(si.p.row,si.p.col+1)
                    if (si.p.col+1 <= colMax && input(si.p.row).heightmap(si.p.col+1) != 9 && !b.contains(newp)) {
                        s.push(StackItem(newp, "URDL"))
                        b += newp
                    } else {
                        // can't go right any further, remaining areas are D, and L
                        val oldp = s.pop()
                        s.push(StackItem(oldp.p,"DL"))
                    }
                case "DL" =>
                    // move down
                    val newp = Point(si.p.row+1,si.p.col)
                    if (si.p.row+1 <= rowMax && input(si.p.row+1).heightmap(si.p.col) != 9 && !b.contains(newp)) {
                        s.push(StackItem(newp, "URDL"))
                        b += newp
                    } else {
                        // can't go down any further, remaining area is  are  L
                        val oldp = s.pop()
                        s.push(StackItem(oldp.p,"L"))
                    }
                case "L" =>
                    // move Left
                    val newp = Point(si.p.row,si.p.col-1)
                    if (si.p.col-1 >= 0 && input(si.p.row).heightmap(si.p.col-1) != 9 && !b.contains(newp)) {
                        s.push(StackItem(newp, "URDL"))
                        b += newp
                    } else {
                        // can't go any further stack item can be discarded
                        s.pop()
                    }
            }
        }
        b
    }

    //println(walkBasin(lowPoints(2)))

    for (l <- lowPoints) {
        basins += walkBasin(l)
    }

    val answer2 = basins.sortBy(b => -b.size).take(3).map(_.size).product


    println(s"Day 9 Part 2 the product of the size of the three largest basins is: $answer2")

    println(s"End at ${java.time.ZonedDateTime.now()}")


    // 1256640  too low
}
