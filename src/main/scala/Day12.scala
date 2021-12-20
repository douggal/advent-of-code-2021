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
            if (line.trim.nonEmpty)
            cols = line.strip().split("-")
        } yield
            inputLine(cols.toVector)
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
    //caveMap.foreach(c => println(s"$c"))

    val visitedCaves = mutable.HashMap[String,Int]()
    val passages = mutable.ListBuffer[String]()

    def splunk(c: String, p: List[String]): Unit = {

        if (visitedCaves.contains(c))
            visitedCaves(c) += 1
        else
            visitedCaves += c -> 1

        if (c == "end")
            passages.addOne(p.mkString(","))  // the end
            ()
        else if (!caveMap.contains(c) || caveMap(c) == Nil) () // dead end
        else // keep going
            for (caveNext <- caveMap(c))
                if (!caveNext.contains(c))  // can't go from and to itself
                    if (caveNext == caveNext.capitalize)  // can go to big cave as many times as needed
                        splunk(caveNext, p ::: List(caveNext))
                    else if (!p.contains(caveNext))  // small cave visit only once
                        splunk(caveNext, p ::: List(caveNext))
                    else ()  // dead ended
    }

    splunk("start",List("start"))

    //passages.sortBy(identity).foreach(println(_))

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
            passagesPt2.addOne(p.mkString(","))  // the end
            ()
        else if (!caveMapPt2.contains(c) || caveMapPt2(c) == Nil) () // dead end
        else // keep going
            for (caveNext <- caveMapPt2(c))
                if (caveNext != c && caveNext != "start")  // can't go from and to itself or back to start
                    if (caveNext == caveNext.capitalize)  // can go to big cave as many times as needed
                        splunkPt2(caveNext, p ::: List(caveNext))
                    else if (caveNext == "end") splunkPt2(caveNext, p ::: List(caveNext)) // can see the end, go there
                    else if (!caveNext.contains(caveNext.capitalize))  // a small cave
                        // Part 2 allow one small cave to be visited twice
                        // how to figure out if list has no more than 1 current small cave identifier item?
                        // https://stackoverflow.com/questions/11448685/scala-how-can-i-count-the-number-of-occurrences-in-a-list?noredirect=1&lq=1
                        // filter list by small caves (lower case letters) and then return a list of the counts of each small cave
                        val test = p.filter(a => !a.contains(a.capitalize) && a != "start") ::: List(caveNext)
                        val test2 = test.groupBy(identity).map(t => (t._1, t._2.length)).values.toList
                        // https://stackoverflow.com/questions/7802851/whats-the-best-way-to-inverse-sort-in-scala
                        // and splunk only if there are 0 or 1 small caves in the path list with count > 1
                        if ((test2.length == 1 && test2(0) <= 2))
                            println(p)
                            splunkPt2(caveNext, p ::: List(caveNext))
                        else if (test2.length == 2 && test2.count(_ >= 2) == 0)
                            splunkPt2(caveNext, p ::: List(caveNext))
                        else if (test2.length > 2 && test2.count(_ >= 2) <= 1)
                            splunkPt2(caveNext, p ::: List(caveNext))
                        else ()
                    else ()
    }

    splunkPt2("start",List("start"))

    passagesPt2.sortBy(identity).zipWithIndex.foreach((l,i) => println(s"$i: $l"))

    val answerPt2 = passagesPt2.length

    println(s"Day 12 Part 2 allowing one small cave to be revisited there are now ${answerPt2} through this cave system.")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
