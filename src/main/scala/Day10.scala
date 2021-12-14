import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day10 extends App {
    // created 12/13/2021
    // https://adventofcode.com/2021/day/10

    println(s"--- Day 10: Syntax Scoring ---")

    // Puzzle Input Data File
    val filename = "./input/Day10Input.txt"
    //val filename = "./input/testInput.txt"

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

    val points = scala.collection.immutable.HashMap[String,Int](
    ")"-> 3, "]" ->  57,  "}"->1197, ">"-> 25137)

    val brackets = scala.collection.immutable.HashMap[String, String](")" -> "(", "]"->"[","}"->"{",">"->"<")
    val openers = brackets.values.toList
    val closers = brackets.keys.toList

    class AoCStackItem(val si:String) {
        override def toString: String = {
            s"AoC Stack Item $si"
        }
    }

    class AocStack() {

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

    val fic = ListBuffer[String]() //, Int]()  // found illegal char and line number
    val inc = ListBuffer[Int]()  // incomplete lines

    for ((il, index) <- input.zipWithIndex) {
        val aocstak = new AocStack
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
                    // have a mismatch
                    fic += c //-> i
                    found = true
                }
            } else {
                // have input item that is not an opener or closer
                // TODO: are there any other chars?
                println("ERROR")
            }
            i += 1
        }
        // check for incomplete line
        if (i == il.chunks.length && !found && aocstak.notEmpty()) inc += index
    }

    val answer = fic.map(v => points(v)).sum

    println(s"Day 10 Part 1 sum of values of scores for each illegal char found in input: $answer")


    // Part 2

    println(s"Day 10 Part 2 tbd")

    println(s"End at ${java.time.ZonedDateTime.now()}")

}
