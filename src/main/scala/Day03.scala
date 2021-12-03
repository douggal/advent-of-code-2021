import scala.annotation.tailrec

object Day03 extends App {
  // created 12/03/2021
  // https://adventofcode.com/2021/day/3

  println(s"--- Day 3: Binary Diagnostic ---")

  val filename = "Day03Input.txt"
  //val filename = "testInput.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val diagnosticRpt = bufferedSource
    .getLines
    .map(line => line.strip)
    .toSeq

  bufferedSource.close

  // debug check what diagnostic report looks like
  //diagnosticRpt.foreach(d => println(f"${d}%10s"))

  // just count up 1s in each position in each line of the diagnostic rpt?
  val ones = scala.collection.mutable.HashMap[Int, Int]()

  // assert all lines in rpt have same width?
  // set up a range which are the column numbers starting with 0 and counting to the right
  val keys = 0 until diagnosticRpt.head.length // range 0,1,2,..n

  // Initialize HashMap of counts
  for (i <- keys)
    ones += i -> 0

  // count them up
  diagnosticRpt.foreach(s => {
    for (i <- keys)
      if (s(i).asDigit == 1) ones(i) += 1
  })

  // make a HashMap for count of 0's in each column position  too
  val zeros = scala.collection.mutable.HashMap[Int, Int]()
  for (digit <- keys) zeros += digit -> (diagnosticRpt.length - ones(digit))

  // most common bit at each position
  // HashMap indexed by digit position containing count of 1's in that position in each
  // line of diagnostic report
  // TODO: what if they're equal?
  val mostCommonDigits = scala.collection.mutable.HashMap[Int, Int]()
  for (key <- keys)
    if (ones(key) > zeros(key))
      mostCommonDigits += key -> 1
    else
      mostCommonDigits += key -> 0

  println(ones)
  println(zeros)
  println(mostCommonDigits)

  //https://stackoverflow.com/questions/2213323/how-can-i-use-map-and-receive-an-index-as-well-in-scala
  //println(mostCommonDigits.values.toVector.reverse.zipWithIndex)
  val gammaRate = mostCommonDigits.values
    .toVector
    .reverse
    .zipWithIndex
    .map{ case (d, i) => d * Math.pow(2,i) }
    .sum

  println(s"Gamma Rate: ${gammaRate.toInt}")

  // least common digit is then opposite of most common
  val epsilonRate = mostCommonDigits.values
    .toVector
    .reverse
    .map(d => if (d==0) 1 else 0)
    .zipWithIndex
    .map{ case (d, i) => d * Math.pow(2,i) }
    .sum

  println(s"Epsilon Rate: ${epsilonRate.toInt}")

  println(s"Day 3 Part 1 answer ${gammaRate.toInt*epsilonRate.toInt}")
  println()

  // Part Two:
  // Next, you should verify the life support rating, which can be determined
  // by multiplying the oxygen generator rating by the CO2 scrubber rating.

  def convertBinaryStrToInt(s: String): Int = {
    s.split("")
      .map{i => i.toInt}
      .reverse
      .zipWithIndex
      .map{ case (d, i) => d * Math.pow(2,i).toInt }
      .sum
  }

  @tailrec
  def findO2rating(p: Int, d: Seq[String]): String = {
    // p is column position from right to left in diagnostic report
    // d is the diagnostic report successively filtered down until
    // it is just one
    // NOTE: will error out if d is empty and not found
    if (d.isEmpty | d.length == 1) d.head
    else {
      // f is d filtered down to lines matching most common digit
      // at the given column  position
      val f = scala.collection.mutable.ListBuffer[String]()
      d.foreach(s => {
        //If 0 and 1 are equally common, keep values with a 1 in the position being considered.
        var sum = 0
        d.foreach(s => {
            if (s(p).asDigit == 1) sum += 1
        })
        val mostCommonDig = if (sum*2 >= d.length) 1 else 0
        if (s(p).asDigit == mostCommonDig) f += s
      })
      //f.foreach(s => println(s"O2 Column $p, $s"))
      findO2rating(p+1, f.toSeq)
    }
  }

  @tailrec
  def findCO2rating(p: Int, d: Seq[String]): String = {
    // p is column position from right to left in diagnostic report
    // d is the diagnostic report successively filtered down until
    // it is just one
    // NOTE: will error out if d is empty and not found
    if (d.isEmpty | d.length == 1) d.head
    else {
      // f is d filtered down to lines matching least common digit
      // at the given column  position
      val f = scala.collection.mutable.ListBuffer[String]()
      d.foreach(s => {
        //If 0 and 1 are equally common, keep values with a 0 in the position being considered.
        var sum = 0
        d.foreach(s => {
          if (s(p).asDigit == 1) sum += 1
        })
        val leastCommonDig = if (sum*2 >= d.length) 0 else 1
        if (s(p).asDigit == leastCommonDig) f += s
      })
      //f.foreach(s => println(s"CO2 Column $p, $s"))
      findCO2rating(p+1, f.toSeq)
    }
  }
  val o2 = findO2rating(0, diagnosticRpt)
  println(s"O2 rating: ${convertBinaryStrToInt(o2)}")

  val co2 = findCO2rating(0, diagnosticRpt)
  println(s"CO2 Scrubber rating: ${convertBinaryStrToInt(co2)}")

  // to find the life support rating,
  // multiply the oxygen generator rating by the CO2 scrubber rating

  println(s"Day 3 Part 2 life support rating: ${convertBinaryStrToInt(o2)*convertBinaryStrToInt(co2)}")

}
