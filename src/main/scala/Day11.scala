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
    println(s"Number items per line: ${input.head.els.count(_ => true)}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part One

    // load input data into a grid

    // List of lists to represent the sea floor grid
    // and leave the original input alone
    var grid = ListBuffer[ListBuffer[Int]]()

    // zip with index passes tuple-2 and case matches on tuple-2
    input.zipWithIndex.foreach {case (l, i) =>
        grid(i) = ListBuffer[Int]()
        for (octo <- l.octos.zipWithIndex)  grid(i)(octo._2) += octo._1
    }




    println(s"Day 11 Part 1 TBD")
    println("")


    // Part Two

    println(s"Day 11 Part 2 tbd")

    println(s"End at ${java.time.ZonedDateTime.now()}")


}
