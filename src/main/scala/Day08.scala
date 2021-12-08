import scala.util.{Failure, Success, Try, Using}

object Day08 extends App {
    // created 12/08/2021
    // https://adventofcode.com/2021/day/8

    println(s"--- Day 8: Seven Segment Search ---")

    // Puzzle Input Data File
    //val filename = "Day08Input.txt"
    val filename = "testInput.txt"

    // make a note of all ten unique signal patterns you see,
    // and then write down a single four digit output value (your puzzle input)
    // Each entry consists of
    //  sp = ten unique signal patterns, and
    //  fdo = the four digit output value
    case class inputLine(sp: Vector[String], fdo: Vector[String])

    val readInputData = () => {
        val source = io.Source.fromFile(filename)
        for {
            line <- source.getLines().toVector
            cols = line.split("[| ]+").map(_.trim)   // split on either space or | ???
        } yield
            inputLine(Vector[String](cols(0),cols(1),cols(2),cols(3),cols(4),cols(5),
            cols(6),cols(7),cols(8),cols(9)),
            Vector[String](cols(10),cols(11),cols(12),cols(13)))
    }

    val input = readInputData()

    println("------------------------------------")
    println(s"Each line is a: ${input.getClass}")
    println(s"Number lines: ${input.length}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")

    //    for (line <- input) {
//        println(line)
//    }

    // Part 1
    val t1 = System.nanoTime

    val digitMap = scala.collection.immutable.HashMap[Int,String](
        0 -> "abcefg", 1 -> "cf", 2 -> "acdeg", 3 -> "acdfg", 4->"bcdf",
        5-> "abdfg", 6->"abdefg",7->"acf",8->"abcdef",9->"abcdfg")

    // In the output values, how many times do digits 1, 4, 7, or 8 appear?
    // lod = length of each output digit string
    val lod = input.flatMap(l => l.fdo.map(s => s.length))
    //println(lod)

    val answer1 = lod.count(d => d == 2 || d == 4 || d == 3 || d == 7)

    val duration1 = (System.nanoTime - t1) / 1e9d
    println(s"Done: Part 1 run time (by the clock): $duration1 sec")

    println(s"Day 8 Part 1 the number of times do digits 1, 4, 7, or 8 appear is: $answer1")

}
