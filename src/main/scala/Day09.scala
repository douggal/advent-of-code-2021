object Day09 extends App {

    // created 12/09/2021
    // https://adventofcode.com/2021/day/9

    println(s"--- Day 9: Smoke Basin ---")

    // Puzzle Input Data File
    //val filename = "Day09Input.txt"
    val filename = "testInput.txt"

    case class inputLine(sp: Vector[String], fdo: Vector[String])

    val readInputData = () => {
        val source = io.Source.fromFile(filename)
        for {
            line <- source.getLines().toVector
            cols = line.split("[| ]+").map(_.trim) // split on either space or | ???
        } yield
            inputLine(Vector[String](cols(0), cols(1), cols(2), cols(3), cols(4), cols(5),
                cols(6), cols(7), cols(8), cols(9)),
                Vector[String](cols(10), cols(11), cols(12), cols(13)))
    }

    val input = readInputData()
    //    for (line <- input) {
    //        println(line)
    //    }

    println("------------------------------------")
    println("Data Quality Control:")
    println(s"Start Timestamp ${java.time.ZonedDateTime.now()}")
    println(s"Input file name: $filename")
    println(s"Each line is a: ${input.getClass}")
    println(s"Number lines: ${input.length}")
    println(s"Number items per line: ${input.head.sp.length}, ${input.head.fdo.length}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part 1

    println(s"Day 9 Part 1 answer: TBD")



    // Part 2

    println(s"Day 9 Part 2 the sum of the display numbers is: TDB")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
