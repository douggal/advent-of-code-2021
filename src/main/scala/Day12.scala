object Day12 extends App {

    // created 12/18/2021
    // https://adventofcode.com/2021/day/12

    println(s"--- Day 12: Passage Pathing ---")

    // Puzzle Input Data File
    //val filename = "./input/Day12Input.txt"
    val filename = "./input/testInput.txt"

    // the input file is a list of how all the caves are connected
    // each row of input is a connection, "conn", between caves
    case class inputLine(conn: Vector[String])

    val readInputData = () => {
        val source = io.Source.fromFile(filename)
        for {
            line <- source.getLines().toVector
            cols = line.strip().split("-")
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
    println(s"Number items per line: ${input.head.conn.count(_ => true)}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part One





    // Part Two

    println(s"Day 12 Part 2 TBD")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
