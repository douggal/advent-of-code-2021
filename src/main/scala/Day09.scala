object Day09 extends App {

    // created 12/09/2021
    // https://adventofcode.com/2021/day/9

    println(s"--- Day 9: Smoke Basin ---")

    // Puzzle Input Data File
    //val filename = "Day09Input.txt"
    val filename = "testInput.txt"

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

    println(s"Day 9 Part 1 answer: TBD")



    // Part 2

    println(s"Day 9 Part 2 the sum of the display numbers is: TDB")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
