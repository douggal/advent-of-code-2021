import scala.collection.mutable.ArrayBuffer

object Day09 extends App {

    // created 12/09/2021
    // https://adventofcode.com/2021/day/9

    println(s"--- Day 9: Smoke Basin ---")

    // Puzzle Input Data File
    val filename = "Day09Input.txt"
    //val filename = "testInput.txt"

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

    // Use an array.   is there a better way to process the input?
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

    println(s"Day 9 Part 2 the sum of the display numbers is: TDB")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
