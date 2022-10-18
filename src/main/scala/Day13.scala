object Day13 extends App {

    // created 12/21/2021
    // https://adventofcode.com/2021/day/13

    println(s"--- Day 13: Transparent Origami ---")

    // Puzzle Input Data File
    //val filename = "./input/Day13Input.txt"
    val filename = "./input/testInput.txt"

    // TODO: read input - two sections
    // transparent paper
    // The transparent paper is marked with random dots and includes instructions
    // on how to fold it up (your puzzle input).
    case class inputLine(paper: String)

    val readInputData = () => {
        val source = io.Source.fromFile(filename)
        for {
            line <- source.getLines().toVector
        } yield
            inputLine(line)
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
    println(s"Number items per line: ${input.head.paper.count(_ => true)}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")



    // Part One
    println(s"Day 13 Part 1 TBD")

    // parse dots


    // parse instructions



    // Part Two

    println(s"Day 13 Part 2  [TBD]")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
