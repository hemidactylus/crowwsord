# README

## Crossword generation "crowwsord" code

Compile with

    scalac CrosswordEnvironment.scala CrosswordUtilities.scala PatriciaTreeSet.scala PuzzleEnvironment.scala crowwsord.scala 

Run with

    scala Crowwsord <wordListFile> <schemaFile> [maxBlackCellCount]

e.g.

    scala Crowwsord wordlists/en/words.txt crowwsordschemas/sampleSchema.txt 3

### File specs

The wordlist is a text file with a word per line. Words shorter than three are automatically "reusable" in the schema.

The schema file is a rectangular array of dots (for free cells), pound symbols (for forced black cells) and letters
(for forced letters).

## Performance and future enhancements

Performance starts to get order of "ages-of-universe" as soon as one asks for a 7x7 schema or so with
a low number of black cells.

Future enhancements should include the possibility of starting from the middle, or from the provided
seed letters, and extend the schema both with vertical or horizontal words, with holes above and below,
in full generality.

## General puzzle-solver demo

Compile with

    scalac PuzzleEnvironment.scala SquareStepperEnvironment.scala CharLineEnvironment.scala CoinTotalEnvironment.scala puzzles.scala

Run with

    scala PuzzleDemo

## Disclaimer

The wordlists provided are not to be taken too seriously; one should provide their own
carefully generated ones.
