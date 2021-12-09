# advent-of-code-2021
My solutions to the Advent of Code December 2021

Advent of Code Website:  [Advent of Code](https://adventofcode.com)

My solution to each day's puzzles.  In Scala 3 unless otherwise noted.

My goals for this year's AoC are to have fun, to improve my understanding of functional
programming techniques with use, and to learn [Scala](https://www.scala-lang.org) programming language.
Some algorithms chosen were my attempt to exercise functional programming techniques.

1. Day  1: Sonar Sweep
2. Day  2: Dive!
3. Day  3: Binary Diagnostic
4. Day  4: Giant Squid
5. Day  5: Hydrothermal Venture
6. Day  6: Lanternfish
7. Day  7: The Treachery of Whales
8. Day  8: Seven Segment Search
9. Day  9: Smoke Basin
10. 

### Notes
#### Day 01
Off to a good start.

#### Day 03
I found today's puzzle harder to solve than it first appeared to be.  
If I have time and inclination I'll revisit and 
try to reduce the number of for loops by replacing with functional method calls.  
Is there a way to avoid nested loops (maybe a built-in transpose function)?

#### Day 04
Completed Part Two at 3 minutes before midnight on the 4th.  I made two mistakes: 1) somehow
typed in a "1" to first number line 1 of data in the file when saving the puzzle input, 
and 2) for part two I did not reload the data after part 1 completed.  

#### Day 05
Chose to represent the layout of the hydrothermal vents of this sparse matrix 
as parallel vectors that I thought
would work well with functional programming methods

1. Part 1: used an idea from Wikipedia 
[Compressed sparse row (CSR, CRS or Yale format)](https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_row_(CSR,_CRS_or_Yale_format)).
Rather slow.  Run time (by the clock) was 237 secs with 200 of the 500 data points done by 30 secs.
2. Part 2:  Part 2 run time (by the clock): 928 sec

#### Day 06
Part 1:  success modeled the fish as an ArrayBuffer
Part 2:
a. try 0 failed:  numbers get too big after 100 generations, part 1 method won't work
b. try 1 failed: try divide and conquer - create two threads and run have the fish simulation on one and rest on the other
c. try 2 success:  aggregate the fish by age in a HashMap with values the count of how many at that age.

#### Day 07
I had a hard time understanding the problem, and when I finally did
get it, I lost time by not first drawing out the 7-segment characters on paper
with a Sharpie to find what distinquishes them.
