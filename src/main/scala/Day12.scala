import scala.collection.mutable

object Day12 extends App {

    // created 12/18/2021
    // https://adventofcode.com/2021/day/12

    println(s"--- Day 12: Passage Pathing ---")

    // Puzzle Input Data File
    val filename = "./input/Day12Input.txt"
    //val filename = "./input/testInput.txt"

    // the input file is a list of how all the caves are connected
    // each row of input is a connection, "conn", between caves
    case class inputLine(cave: Vector[String])

    val readInputData = () => {
        val source = io.Source.fromFile(filename)
        for {
            line <- source.getLines().toVector
            if (line.trim.nonEmpty)
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

    def makeCaveMap(): mutable.HashMap[String,mutable.ListBuffer[String]] = {
        val caveMap = mutable.HashMap[String, mutable.ListBuffer[String]]()
        // build a dictionary of caves and to which caves they are connected
        // Edit: works in both directions! Add reverse connection too.

        // "end" cave doesn't connect to anything
        caveMap("end") = mutable.ListBuffer[String]()

        for (l <- input) {
            val cave = l.cave(0)
            val to = l.cave(1)
            if (!caveMap.contains(cave))
                caveMap(cave) = mutable.ListBuffer[String]()
            caveMap(cave) += to
            // reverse dirction is also possible - the splunking will handle not repeating small caves
            if (!caveMap.contains(to))
                caveMap(to) = mutable.ListBuffer[String]()
            caveMap(to) += cave
        }
        caveMap
    }

    val caveMap = makeCaveMap()
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
    // 3000


    // Part Two
    val caveMapPt2 = makeCaveMap()
    val visitedCavesPt2 = mutable.HashMap[String,Int]()
    val passagesPt2 = mutable.ListBuffer[String]()

    def splunkPt2(c: String, p: List[String]): Unit = {

        if (visitedCavesPt2.contains(c))
            visitedCavesPt2(c) += 1
        else
            visitedCavesPt2 += c -> 1

        if (c == "end")
            passagesPt2.addOne(p.toString())  // the end
            ()
        else if (!caveMapPt2.contains(c) || caveMapPt2(c) == Nil) () // dead end
        else // keep going
            for (caveNext <- caveMapPt2(c))
                if (!caveNext.contains(c))  // can't go from and to itself
                    if (caveNext == caveNext.capitalize)  // can go to big cave as many times as needed
                        splunkPt2(caveNext, p ::: List(caveNext))
                    else if (!p.contains(caveNext))  // small cave visit only once
                        splunkPt2(caveNext, p ::: List(caveNext))
                    else ()  // dead ended
    }

    splunkPt2("start",List("start"))

    passagesPt2.foreach(println(_))

    val answerPt2 = passagesPt2.length

    println(s"Day 12 Part 2 allowing one small cave to be revisited there are now ${answerPt2} through this cave system.")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
