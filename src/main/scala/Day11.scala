import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day11 extends App {
    // created 12/16/2021
    // https://adventofcode.com/2021/day/11

    println(s"--- Day 11: Dumbo Octopus ---")

    // Puzzle Input Data File
    //val filename = "./input/Day11Input.txt"
    val filename = "./input/testInput.txt"

    // Each octopus has an energy level mapped out by the submarine
    case class inputLine(octos: Vector[String])

    val readInputData = () => {
        val source = io.Source.fromFile(filename)
        for {
            line <- source.getLines().toVector
            cols = line.strip().split("")
        } yield
            inputLine(cols.toVector)
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
    println(s"Number items per line: ${input.head.octos.count(_ => true)}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part One

    // load input data into a grid

    // List of lists to represent the sea floor grid
    // and leave the original input alone
    // Use spreadsheet style coordinates, (r, c) where r is y-axis increasing downwards,
    // and c is x-axis increasing to the right
    var grid = ListBuffer[ListBuffer[Int]]()

    // zip with index passes tuple-2 and case matches on tuple-2 passing l and i into the procedure
    input.zipWithIndex.foreach {case (l, r) =>
        grid += ListBuffer[Int]()
        for (octo <- l.octos.zipWithIndex)  grid(r) += octo._1.toInt
    }
    //printGrid

    // coordinates on sea floor are in x (columns) and y (rows) origin top-left and y increasing downwards
    // https://www.redblobgames.com/grids/parts/
    case class GridPoint(r: Int, c:Int)

    def printGrid(step: Int) : Unit = {
        // print to console a header followed by the energy level grid
        println(s"Step: $step")
        grid.foreach{ l =>
            println(l.map(_.toString.reverse.padTo(2,' ').reverse).mkString("⎦⎣"))
            //println(List.fill(l.length*2)("-").mkString(""))
        }
    }

    def addOne() : Unit = {
        // add +1 to each octopus, grid wide
        grid.zipWithIndex.foreach { case (row, r) =>
            for (c <- row.indices) grid(r)(c) += 1
        }
    }

    def addOneNeighbors(ns: List[GridPoint]) : Int = {
        // given a list of grid points, add +1 to each octopus
        ns.foreach { p => grid(p.r)(p.c) += 1 }
        // return count of new flashes
        ns.count(p => grid(p.r)(p.c)==10)
    }

    def getFlashes: List[GridPoint] = {
        // return grid points of octopuses who flashed in this step, grid wide
        val x = ListBuffer[GridPoint]()
        grid.zipWithIndex.foreach { case (row, r) =>
            for (c <- row.indices
                if grid(r)(c) == 10) x+= GridPoint(r,c)
        }
        x.toList
    }

    def setToZed() : Unit = {
        // set to 0 all the octopuses with energy level > 9 (that is, they flashed in this step)
        grid.zipWithIndex.foreach { case (row, r) =>
            for (c <- row.indices; if grid(r)(c) > 9) grid(r)(c) = 0
        }
    }

    def getNeighbors(gp:GridPoint):List[GridPoint] = {
        // clockwise from top center
        // coordinates are spreadsheet form: (r,c) where
        // rs go from top to bottom (rows) and cs go increase from left to right, origin top-left
        val rs = List(0,1,1,1,0,-1,-1,-1)
        val cs = List(-1,-1,0,1,1,1,0,-1)
        val neighbors = for (p <- rs.zip(cs)
                             if gp.r + p._1 >= 0 && gp.r + p._1 < grid.length
                             if gp.c + p._2 >= 0 && gp.c + p._2 < grid.head.length)
                        yield GridPoint(gp.r + p._1 ,gp.c + p._2)
        neighbors
    }

    @tailrec
    def propagateFlash(ps: List[Day11.GridPoint], cnt: Int):Int = {
        if (ps.isEmpty) cnt
        else {
            val ns = getNeighbors(ps.head)
            val newFlashCount = addOneNeighbors(ns)
            val newNeighbors = for (n <- ns; if grid(n.r)(n.c) == 10) yield n
            propagateFlash(ps.tail ::: newNeighbors, cnt + newFlashCount)
        }
    }

    val steps = 100
    var flashCount = 0
    printGrid(0)
    for (step <- 1 to steps) {
        // add 1 to each octopus
        addOne()
        val flashes = getFlashes
        flashCount += flashes.length
        flashCount += propagateFlash(flashes, 0)
        setToZed()
        printGrid(step)
    }

    val answer = flashCount

    println("")
    println(s"Day 11 Part 1 number of flashes in $steps steps was: $answer")
    println("")


    // Part Two

    println(s"Day 11 Part 2 tbd")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
