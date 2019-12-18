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

#### Day 9

    sbt "run 9 <1 or 2> <input-file> <input-value>"

#### Day 17, part 2

    sbt "run 17 2 <input-file> <main-routine> <function-a> <function-b> <function-c> <print-map>"

For example, this was my solution

    sbt "run 17 2 /path/to/day/17/input A,A,B,C,B,C,B,C,A,C R,6,L,8,R,8 R,4,R,6,R,6,R,4,R,4 L,8,R,6,L,10,L,10 n"

Part 2 is easy enough to solve by hand. Map out the route, then find the largest common blocks. Some tips if you're confused

- Prioritize moving straight as long as possible
- Once you have the route, you know the start of the route must start a function, and the end must end a function
- Use an editor that highlights other matching blocks of text as you highlight text
