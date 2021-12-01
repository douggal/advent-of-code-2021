object Day01 extends App{

  // created 12/01/2021
  // https://adventofcode.com/2021/day/1

  println(s"--- AoC 2021 Day 1: Sonar Sweep ---")

  val filename = "Day01Input.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val lines = bufferedSource
    .getLines
    .map { line =>
      line.strip.toInt
    }.toVector

  // Part 1:  take items sliding window two at a time, a and b, and find sum where b > a.
  val result = lines.zip(lines.drop(1)).map((a,b) => if ((b-a)>0) 1 else 0).sum

  println(s"Part 1 answer: $result")

  bufferedSource.close
}
