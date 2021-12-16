import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day10 extends App {
    // created 12/13/2021
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

    val points = scala.collection.immutable.HashMap[String,Int](")"-> 3, "]" ->  57,  "}"->1197, ">"-> 25137)

    val brackets = scala.collection.immutable.HashMap[String, String](")" -> "(", "]"->"[","}"->"{",">"->"<")
    val openers = brackets.values.toList
    val closers = brackets.keys.toList

    class AoCStackItem(val si:String) {
        override def toString: String = {
            s"AoC Stack Item $si"
        }
    }

    class AoCStack() {

        var st = scala.collection.mutable.ListBuffer[AoCStackItem]()

        def notEmpty(): Boolean = {
            st.nonEmpty
        }

        def stackTop(): AoCStackItem = {
            st.head
        }

        def push(si: AoCStackItem): Unit = {
            st.prepend(si)
        }
        def pop():AoCStackItem = {
            val item = st.head
            st = st.drop(1)
            item
        }
        override def toString: String = {
            s"AoC Stack has ${st.length} items"
        }
    }

    val fic = ListBuffer[String]() // found illegal char
    val cor = ListBuffer[Int]()  // corrupted lines
    val inc = ListBuffer[Int]()  // incomplete lines

    // Scala for loop with index:  two options
    // Ref:  https://stackoverflow.com/questions/16883875/scala-for-loop-getting-index-in-consice-way
    for ((il, index) <- input.zipWithIndex) {
        val aocstak = new AoCStack
        var i = 0
        var found = false
        while (!found && i<il.chunks.length) {
            val c = il.chunks(i)
            if (openers.contains(c)) {
                val si = new AoCStackItem(c)
                aocstak.push(si)
            } else if (closers.contains(c)) {
                if (aocstak.notEmpty() && brackets(c) == aocstak.stackTop().si) {
                    // have a match with an opener
                    aocstak.pop()
                } else {
                    // have a mismatch = corrupted line
                    fic += c
                    cor += index
                    found = true
                }
            } else {
                // have input item that is not an opener or closer
                // TODO: are there any other chars?
                println("ERROR")
            }
            i += 1
        }
        // check for incomplete line - maybe not necessary and has to be incomplete at this point
        if (i == il.chunks.length && !found && aocstak.notEmpty()) inc += index
    }

    val answer = fic.map(v => points(v)).sum
    println(s"Quality control: sum of incomplete ${inc.length} + corrupted lines ${cor.length} should equal total lines.")

    println(s"Day 10 Part 1 sum of values of scores for each illegal char found in input: $answer")
    println("")

    // Part 2

    // get rid of corrupted lines
    // https://stackoverflow.com/questions/18814522/scala-filter-on-a-list-by-index
    val clean = input.zipWithIndex.collect { case (l,i) if (!cor.contains(i)) => l }

    //figure out the sequence of closing characters that complete all open chunks in the line
    // add them in correct order
    val closeSeq = ListBuffer[List[String]]() // closing sequences
    val lines = ListBuffer[Int]()  // line number of each closing seq
    val bracketsMatch = scala.collection.immutable.HashMap[String, String]("(" -> ")", "["->"]","{"->"}","<"->">")

    for ((il, index) <- clean.zipWithIndex) {
        val aocstak = new AoCStack

        // keep track of what we need to add
        var a = ListBuffer[String]()
        // push and pop until

        for (c <- il.chunks) {
            if (openers.contains(c)) {
                val si = new AoCStackItem(c)
                aocstak.push(si)
            } else if (closers.contains(c)) {
                if (aocstak.notEmpty() && brackets(c) == aocstak.stackTop().si) {
                    // have a match with an opener, discard it
                    aocstak.pop()
                }
            }
        }

        while (aocstak.notEmpty()) {
            val c = aocstak.pop() // item of type stack item
            if (openers.contains(c.si)) {
                // add closer bracket
                a += bracketsMatch(c.si)
            }
        }
        closeSeq += a.toList
        lines += index
    }

    val pointsPt2 = scala.collection.immutable.HashMap[String,Int](")"->1,"]"->2,"}"->3,">"->4)

    //Start with a total score of 0. Then, for each character, multiply the total score by 5 and then
    // increase the total score by the point value given for the character
    val scores = (closeSeq.map { line => line.foldLeft(0){(a,b) => (5 * a) +pointsPt2(b)} }).toList
    // amswer the score that falls exactly in the middle.
    val answerP2 = scores.sorted.drop(scores.length/2).head

    println(s"Day 10 Part 2, the middle score is $answerP2")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
