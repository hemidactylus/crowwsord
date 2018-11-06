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
    (4,4),
    allowedWords,
    Map.empty, // OR: Map[Position,CellContents]( (Position(1,1) -> BlackCell) )
    Some(0)
  )

  println("\n[Crowwsord] Starting (crossword) ...")

  ( crossword.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:\n   ${c}") } )

  /*
    TROUBLES TO FIX
      1. if a word is created twice as vertical by inserting this horiz, it is not detected
      2. if a word is created as vert and === the horiz word being added, not detected
        (1,2 => need to count, shit)
      3. check for nonexistent prefixed (patricia?)
      4. expand on the mask concept to constrain the picking of proposals (LATER)
  */

}
