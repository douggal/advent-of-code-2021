import scala.collection.mutable

object Day12 extends App {

    // created 12/18/2021
    // https://adventofcode.com/2021/day/12

    println(s"--- Day 12: Passage Pathing ---")

    // Puzzle Input Data File
    //val filename = "./input/Day12Input.txt"
    val filename = "./input/testInput.txt"

    // the input file is a list of how all the caves are connected
    // each row of input is a connection, "conn", between caves
    case class inputLine(cave: Vector[String])

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
    println(s"Number items per line: ${input.head.cave.count(_ => true)}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part One

    /*
    This is a list of how all of the caves are connected. You start in the cave named start,
    and your destination is the cave named end. An entry like b-d means that cave b is
    connected to cave d - that is, you can move between them.

    Your goal is to find the number of distinct paths that start at start, end at end, and
    don't visit small caves more than once.
    */

    // build a dictionary of caves and to which caves they are connected
    val caveMap = mutable.HashMap[String,mutable.ListBuffer[String]]()
    // "end" cave doesn't connect to anything
    caveMap("end") = mutable.ListBuffer[String]()
    for (l <- input) {
        val cave = l.cave(0)
        val to  = l.cave(1)
        if (!caveMap.contains(cave))
            caveMap(cave) = mutable.ListBuffer[String]()
        caveMap(cave) += to
        if (cave.contains(cave.capitalize) && !to.contains("end")) // end is the end
            // big cave, also add the reverse passage
            if (!caveMap.contains(to))
                caveMap(to) = mutable.ListBuffer[String]()
            caveMap(to) += cave
        if (to.contains(to.capitalize) && !to.contains("end")) // end is the end
        // big cave, also add the reverse passage
            if (!caveMap.contains(to))
                caveMap(to) = mutable.ListBuffer[String]()
            caveMap(to) += cave

    }

    caveMap.foreach(c => println(s"$c"))

    val visitedCaves = mutable.HashMap[String,Int]()
    val passages = mutable.ListBuffer[String]()

    def spelunk(c: String, p: List[String]): Unit = {

        if (visitedCaves.contains(c))
            visitedCaves(c) += 1
        else
            visitedCaves += c -> 1

        if (c == "end")
            passages.addOne(p.toString())  // the end
            ()
        else if (!caveMap.contains(c) || caveMap(c) == Nil) () // dead end
        else // keep going
            for (caveNext <- caveMap(c))
                if (!caveNext.contains(c))  // can't go from and to itself
                    if (caveNext == caveNext.capitalize)  // can go to big cave as many times as needed
                        spelunk(caveNext, p ::: List(caveNext))
                    else if (!p.contains(caveNext))  // small cave visit only once
                        spelunk(caveNext, p ::: List(caveNext))
                    else ()  // dead ended
    }

    spelunk("start",List("start"))

    passages.foreach(println(_))

    val answer = passages.length

    println(s"Day 12 Part 1 there are ${answer} paths through this cave system are there that visit small caves at most once.")

    // Part Two

    println(s"Day 12 Part 2 TBD")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
