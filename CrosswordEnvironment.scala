/*
  CrosswordEnvironment.scala
*/

import net.salamandrina.crowwsord._

object CrosswordEnvironment extends PuzzleEnvironment {
  val puzzleName = "Crossword"
  type Configuration = CrosswordConfiguration
  type PuzzleShape = CrosswordPuzzleShape
  type ExtensionStep = CrosswordExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration = new Configuration(shape,Map.empty,Set.empty)
  def makeConfig(
    shape: PuzzleShape,
    startCells: Map[Position,CellContents]
  ): Configuration = new Configuration(shape,startCells,Set.empty) // FIXME find words in the provided start seq
  def makeConfig(
    shape: PuzzleShape,
    startCells: Map[Position,CellContents],
    usedWords: Set[String]
  ): Configuration = new Configuration(shape,startCells,usedWords)
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration = {
    new Configuration(
      other.shape,
      other.cells ++ (
        for(extensionCell <- extension.cellSteps )
          yield extensionCell.nPosition->extensionCell.nCellContents
      ),
      other.usedWords ++ extension.newWords
    )
  }
  override def makeNewConfig(shape: PuzzleShape): Configuration = {
    val borderedShape: PuzzleShape = CrosswordPuzzleShape(
      shape.width,
      shape.height,
      shape.wordSet
    )
    val startCells: Map[Position,CellContents] = shape.startCells ++ (
        for (
          x <- List(-1,shape.width);
          y <- 0 until shape.height
        ) yield (Position(x,y) -> BlackCell)
      ).toMap ++ (
        for (
          y <- List(-1,shape.height);
          x <- -1 to shape.width
        ) yield (Position(x,y) -> BlackCell)
      ).toMap

    makeConfig(borderedShape,startCells)
  }
  //
  case class Position(x: Int, y: Int)
  class CellContents
  case object EmptyCell extends CellContents { override def toString="." }
  case object BlackCell extends CellContents { override def toString="#" }
  case class Letter(c: Char) extends CellContents { override def toString = c.toString.toUpperCase }
  // 
  case class CrosswordPuzzleShape(
    width: Int,
    height: Int,
    wordSet: Set[String],
    startCells: Map[Position,CellContents]=Map.empty
  ) extends AbstractPuzzleShape
  //
  class CrosswordConfiguration(
    val shape: CrosswordPuzzleShape,
    val cells: Map[Position,CellContents],
    val usedWords: Set[String]
  ) extends AbstractConfiguration {
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      true // FIXME
    }
    def stepProposals: Seq[ExtensionStep] = {
      Seq.empty
      // FIXME
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = {
      true
      // FIXME
    }
    override def toString: String = {
      val usedWordDesc: String = usedWords.mkString("/")
      val strDesc: String = s"\n    +${"-"*(shape.width+2)}+" + (
        for (j <- 0 until shape.height) yield (
          for (i <- 0 until shape.width)
          yield cells.getOrElse(Position(i,j),EmptyCell).toString
        ).mkString("    | ",""," |")
      ).mkString("\n","\n","\n") + s"    +${"-"*(shape.width+2)}+\n" + (
        s"      used words: ${usedWordDesc} "
      )
      s"${puzzleName}<${strDesc}\n>"
    }
  }
  class CrosswordCellStep(val nPosition: Position, val nCellContents: CellContents)
  class CrosswordExtensionStep(
    val cellSteps: Seq[CrosswordCellStep],
    val newWords: Set[String]
  ) extends AbstractExtensionStep
}
