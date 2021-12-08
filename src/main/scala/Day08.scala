import scala.collection.mutable.HashMap
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
    println(s"Input file name: ${filename}")
    println(s"Each line is a: ${input.getClass}")
    println(s"Number lines: ${input.length}")
    println(s"Number items per line: ${input.head.sp.length}, ${input.head.fdo.length}")
    println(s"Input first line: ${input.head}")
    println(s"Input last line: ${input.tail.last}")
    println("------------------------------------")


    // Part 1
    println(s"Start Timestamp ${java.time.ZonedDateTime.now()}")
    val t1 = System.nanoTime

    val digitMap = scala.collection.immutable.HashMap[Int, String](
        0 -> "abcefg", 1 -> "cf", 2 -> "acdeg", 3 -> "acdfg", 4 -> "bcdf",
        5 -> "abdfg", 6 -> "abdefg", 7 -> "acf", 8 -> "abcdefg", 9 -> "abcdfg")

    // length of each digit's string represented as a 7 segment display
    val lds = for (d <- digitMap.keys.toVector.sorted) yield digitMap(d).length

    // the digits with a unique char count of its 7 segment display
    val us = Set(1, 4, 7, 8)

    // set of string lengths of the unique digits expressed as 7 segment display
    val lsu = lds.zipWithIndex.filter(d => us.contains(d._2)).map(_._1).toSet

    // lod = length of each digit string in the data input's output section (4 digits per line)
    // all of them in the whole file top to bottom
    val lod = input.flatMap(l => l.fdo.map(s => s.length))
    //println(lod)

    // answer is count of numbers in lod which have match in the set of string lengths
    // of the 7-segment displays of the unique digits
    val answer1 = lod.count(d => lsu.contains(d))

    val duration1 = (System.nanoTime - t1) / 1e9d
    println(s"Done: Part 1 run time (by the clock): $duration1 sec")

    println(s"Day 8 Part 1 the number of times do digits 1, 4, 7, or 8 appear is: $answer1")


    //Part 2:
    // find mappings between possible 7-segment representations given on the left (vector sp)
    // and the four digit output (fdo) the right

    for (line <- input) {
        // start filling the mapping of displayed segments for each digit to real number
        var d2d = HashMap[Int, Set[String]]()
        // need set of characters that make up each unique digits 7-segment display
        val one = Set(for (c <- line.sp.find(_.length == 2).head) yield c)
        val four = Set(for (c <- line.sp.find(_.length == 4).head) yield c)
        val seven = Set(for (c <- line.sp.find(_.length == 3).head) yield c)
        val eight = Set(for (c <- line.sp.find(_.length == 7).head) yield c)
        d2d(1) = one
        d2d(4) = four
        d2d(7) = seven
        d2d(8) = eight

        def test2(s: String): Boolean = {
            val two = Set(for (c <- s) yield c)
            if (two.intersect(four).count(_ => true) == 2
              && two.intersect(seven).count(_ => true) == 2
              && two.intersect(one).count(_ => true) == 1) true else false
        }
        def test3(s: String): Boolean = {
            val three = Set(for (c <- s) yield c)
            if (three.intersect(four).count(_ => true) == 2
              && three.intersect(seven).count(_ => true) == 3
              && three.intersect(one).count(_ => true) == 2) true else false
        }
        def test5(s: String): Boolean = {
            val five = Set(for (c <- s) yield c)
            if (five.intersect(four).count(_ => true) == 3
              && five.intersect(seven).count(_ => true) == 1
              && five.intersect(one).count(_ => true) == 1) true else false
        }
        def test6(s: String): Boolean = {
            val six = Set(for (c <- s) yield c)
            if (six.intersect(four).count(_ => true) == 3
              && six.intersect(seven).count(_ => true) == 2
              && six.intersect(one).count(_ => true) == 1) true else false
        }

        def test0(s: String, three:Set[String]): Boolean = {
            val zero = Set(for (c <- s) yield c)
            if (zero.intersect(four).count(_ => true) == 4
              && zero.intersect(seven).count(_ => true) == 3
              && zero.intersect(one).count(_ => true) == 2
              && zero.intersect(three).count(_ => true) == 4) true else false
        }
        def test9(s: String, three:Set[String]): Boolean = {
            val nine = Set(for (c <- s) yield c)
            if (nine.intersect(four).count(_ => true) == 4
              && nine.intersect(seven).count(_ => true) == 3
              && nine.intersect(one).count(_ => true) == 2
              && nine.intersect(three).count(_ => true) == 5) true else false
        }

        val z = 1
    }

    println(s"Day 8 Part 2 the sum of the display numbers is: ")

    println(s"End at ${java.time.ZonedDateTime.now()}")
}
