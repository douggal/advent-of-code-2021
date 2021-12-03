object Day03 extends App {
  // created 12/03/2021
  // https://adventofcode.com/2021/day/3

  println(s"--- Day 3: Binary Diagnostic ---")

  val filename = "Day03Input.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val diagnosticRpt = bufferedSource
    .getLines
    .map(line => line.strip)
    .toSeq

  bufferedSource.close

  // debug check what diagnostic report looks like
  //diagnosticRpt.foreach(d => println(f"${d}%10s"))

  // just count up 1s in each position in each line of the diag rpt?
  val ones = scala.collection.mutable.HashMap[Int, Int]()
  val zeros = scala.collection.mutable.HashMap[Int, Int]()

  // assert all lines in rpt have same width?
  val keys = 0 until diagnosticRpt(0).length // range 0,1,2,..n
  for (i <- keys)
    ones += i -> 0

  diagnosticRpt.foreach(s => {
    for (i <- keys)
      if (s(i).asDigit == 1) ones(i) += 1
  })

  for (digit <- keys) zeros += digit -> (diagnosticRpt.length - ones(digit))

  // most common bit at each position
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

}
