object Day10 extends App {
    // created 12/11/2021
    // https://adventofcode.com/2021/day/10

    println(s"--- Day 10: Syntax Scoring ---")

    // Puzzle Input Data File
    //val filename = "./input/Day10Input.txt"
    val filename = "./input/testInput.txt"

    case class inputLine(chunks: Vector[String])

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
    println(s"Number items per line: ${input.head.chunks.count(_ => true)}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part 1

    println(s"Day 10 Part 1 TBD")


    // Part 2

    println(s"Day 19 Part 2 tbd")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
