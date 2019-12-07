# advent-of-code

My attempts at [Advent of Code 2019](http://adventofcode.com/2019). I'm planning on using Scala for every task.

## Usage

Use sbt to run the program to generate a solution. Most solutions can be run as follows

    sbt "run <day> <part> <input-file>"

For example, to solve part 2 of day 1's problem, run

    sbt "run 1 2 /path/to/day/1/input"

However, some problems require extra or different arguments

### Exceptions

#### Day 2, part 1

    sbt "run 2 1 <input-file> <noun> <verb>"

#### Day 2 part 2

    sbt "run 2 2 <input-file> <target-value>"

#### Day 4

    sbt "run 4 <1 or 2> <range>"

#### Day 5

    sbt "run 5 <1 or 2> <input-file> <input-value>"
