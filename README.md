# Advent of Code 2021

My solutions to the Advent of Code (December 2021) programming puzzles.

Advent of Code website: https://adventofcode.com

Overview
--------
This repository contains my personal solutions to the Advent of Code 2021 puzzles. 
Solutions are implemented in Scala 3 unless stated otherwise. 
The repository intentionally excludes the original puzzle text quotes 
and puzzle input files to avoid redistributing copyrighted content.

Goals
-----
- Have fun and practice problem solving.
- Improve understanding and use of functional programming techniques.
- Learn and apply Scala (primarily Scala 3).

Usage
-----
General notes — project layout may vary by repository:
- Solutions are written in Scala 3 unless otherwise noted.
- Build and run the solutions using your preferred Scala toolchain (sbt, scala-cli, or scalac + java).
- Example with sbt (adjust main class or package as appropriate):
    - sbt compile
    - sbt "runMain <fully.qualified.MainForDayX>"
- If you want, tell me your project layout (sbt build.sbt, package names, or folder structure) and I will add exact run instructions and example commands.

Repository Conventions
----------------------
- Each day contains one or more solutions and often a short note about approach and performance.
- Puzzle inputs and the original Advent of Code problem statements are not included in this repository.
- Where a solution requires an input file, the program expects the file path to be provided at runtime (or loaded from a relative `data/` folder if present). See the code in each day's directory for exact expectations.

Notes & Highlights
------------------
Day 12 last day completed prior to contest wrap up on 25th 2021.

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

#### Day 09
Got part 1 but no cigar on part 2.  Moved data files to new folder.
- returned on 11th to puzzle and completed Part Two.  Sometimes there no better way than paper and markers
  to solve a problem.  [Computerphile](https://www.youtube.com/user/Computerphile).

#### Day 10
BigInt needed in Part Two scoring.  Scala doesn't throw error on integer overflow?

#### Day 11
Made the grid global to program. Seemed easy way to reduce visual clutter and
get the stars, but should
have passed it along as parameter.

#### Day 12
Decided a recursive procedure would be less coding, and I'll bank
on having enough heap space to handle it.

I first build a dictionary of each cave and the list of caves to which it is connected.
Start at start and keep splunking forward to the next cave until either the end is reach (record the path from start),
a dead end is found.  Fall back to last fork and go forward with next cave in the list until all
the pathways are exhausted.

Performance and implementation notes are recorded in each day's solution file where relevant.

Contributing and Issues
-----------------------
This repository contains my personal solutions. If you'd like to suggest improvements, refactors, or optimizations, open an issue or submit a pull request. Please do not add copies of Advent of Code problem statements or puzzle inputs to the repository.

Contact
-------
GitHub: @douggal

License
-------
Choose a license that suits your needs (MIT, Apache-2.0, etc.). If you want, I can add a LICENSE file and apply a default license for you.

Acknowledgements
----------------
- Advent of Code (https://adventofcode.com)
- Scala language and community resources
