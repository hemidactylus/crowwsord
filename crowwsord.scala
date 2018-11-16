/*
  crowwsord.scala
    main code for the crossword-generating application
    of the puzzle environment
*/

import scala.io.Source.fromFile

import CrosswordEnvironment.{
  CrosswordConfiguration,
  CrosswordPuzzleShape,
  Position,
  CellContents,
  BlackCell,
  Letter
}

object Crowwsord{

  val alphabet: Set[Char] = ('A' to 'Z').toSet

  def parseSchemaFile(fName: String): ((Int,Int),Map[Position,CellContents]) = {
    val lines: List[String] = fromFile(fName)
      .getLines
      .filter(_!="")
      .map( _.toUpperCase )
      .toList
    val schemaH = lines.map(_.length).max
    val schemaW = lines.length
    //
    def makeCellContents(c: Char): Option[CellContents] = c match {
      case '#' => Some(BlackCell)
      case c: Char if alphabet contains c => Some(Letter(c))
      case _ => None
    }
    //
    val initialCells: Map[Position,CellContents] = (
      for (
        (line: String,y: Int) <- lines.zipWithIndex;
        (chr: Char,x: Int) <- line.zipWithIndex;
        thisCellContents: CellContents <- makeCellContents(chr)
      )
      yield Position(x,y) -> thisCellContents
    ).toMap
    (
      (schemaW,schemaH),
      initialCells
    )
  }

  def main(args: Array[String]) = {
    val wordFileName: String = args(0)
    val schemaFileName: String = args(1)
    val maxBlackCells: Option[Int] = if (args.length>2) Some(args(2).toInt) else None

    val ((puzzleHeight,puzzleWidth),initialCells) = parseSchemaFile(schemaFileName)

    val allowedWords: Set[String] = fromFile(wordFileName)
      .getLines
      .filter(_ != "")
      .map( _.toUpperCase )
      .toSet

    val crossword: CrosswordConfiguration = CrosswordEnvironment.createConfig(
      (puzzleWidth,puzzleHeight),
      allowedWords,
      initialCells,
      // Map.empty, // OR: 
      // Map[Position,CellContents]( 
      //   Position(0,0) -> BlackCell,
      //   Position(1,0) -> Letter('F'),
      //   Position(2,0) -> Letter('U'),
      //   Position(3,0) -> Letter('N'),
      //   Position(4,0) -> Letter('G'),
      //   Position(5,0) -> Letter('O')
      // ),
      maxBlackCells
    )

    println("\n[Crowwsord] Starting ...")
    ( crossword.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:\n   ${c}") } )
    println("\n[Crowwsord] Finished.")
  }
}
