/*
  crowwsord.scala
  main code
*/

object Crowwsord extends App {

  /*
  import CharLineEnvironment.{CharLineConfiguration,CharLinePuzzleShape}

  val charLinePuzzleShape=new CharLinePuzzleShape(7,Set('a','b','c','d','f','q','r','y','z'))
  val charLine: CharLineConfiguration = CharLineEnvironment.makeNewConfig(
    charLinePuzzleShape
  )

  println("\n[Crowwsord] Starting (charLine) ...")

  ( charLine.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )
  */

  /*
  import CoinTotalEnvironment.{CoinTotalConfiguration,CoinTotalPuzzleShape}

  val coinTotalPuzzleShape=new CoinTotalPuzzleShape(12,Set(1,2,5))
  val coinTotal: CoinTotalConfiguration = CoinTotalEnvironment.makeNewConfig(
    coinTotalPuzzleShape
  )

  println("\n[Crowwsord] Starting (coinTotal) ...")

  ( coinTotal.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )
  */

  /*
  import SquareStepperEnvironment.{SquareStepperConfiguration,SquareStepperPuzzleShape,FillingStrategyEnum}

  val squareStepperPuzzleShape: SquareStepperPuzzleShape = new SquareStepperPuzzleShape(
    (10,10),
    FillingStrategyEnum.Quadruplicate
  )
  val squareStepper: SquareStepperConfiguration = SquareStepperEnvironment.makeNewConfig(
    squareStepperPuzzleShape
  )

  println("\n[Crowwsord] Starting (squareStepper) ...")

  ( squareStepper.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )
  */  

  import CrosswordEnvironment.{CrosswordConfiguration, CrosswordPuzzleShape, Position, CellContents, BlackCell, EmptyCell, Letter}

  import scala.io.Source.fromFile
  val allowedWords: Set[String] = fromFile("words.txt").getLines().filter(_ != "").map( _.toUpperCase ).toSet

  val crossword: CrosswordConfiguration = CrosswordEnvironment.createConfig(
    (5,5),
    allowedWords,
    Map.empty, // OR: 
    // Map[Position,CellContents]( 
    //   Position(0,0) -> BlackCell,
    //   Position(1,0) -> Letter('F'),
    //   Position(2,0) -> Letter('U'),
    //   Position(3,0) -> Letter('N'),
    //   Position(4,0) -> Letter('G'),
    //   Position(5,0) -> Letter('O')
    // ),
    Some(4)
  )

  println("\n[Crowwsord] Starting (crossword) ...")

  ( crossword.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:\n   ${c}") } )
}
