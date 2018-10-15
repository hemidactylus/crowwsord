/*
  CrosswordEnvironment.scala
*/

import net.salamandrina.crowwsord._

object CrosswordEnvironment extends PuzzleEnvironment {
  val puzzleName = "Crossword"
  type Configuration = CrosswordConfiguration
  type PuzzleShape = CrosswordPuzzleShape
  type ExtensionStep = CrosswordExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration = new Configuration(shape,Map.empty)
  def makeConfig(
    shape: PuzzleShape,
    startCells: Map[Position,CellContents]
  ): Configuration = new Configuration(shape,startCells)
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration = {
    new Configuration(
      other.shape,
      other.cells + (extension.nPosition->extension.nCellContents)
    )
  }
  //
  case class Position(x: Int, y: Int)
  class CellContents
  case object BlackCell extends CellContents { override def toString="#" }
  case class Letter(c: Char) extends CellContents { override def toString = c.toString.toUpperCase }
  // 
  case class CrosswordPuzzleShape(
    width: Int,
    height: Int,
    wordSet: Set[String]
  ) extends AbstractPuzzleShape
  //
  class CrosswordConfiguration(
    val shape: CrosswordPuzzleShape,
    val cells: Map[Position,CellContents]
  ) extends AbstractConfiguration {
    //
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      true // FIXME
    }
    def stepProposals: Seq[ExtensionStep] = {
      val firstFreePosition: Position = (
        for (
          j <- 0 until shape.height;
          i <- 0 until shape.width;
          if !(cells contains Position(i,j) )
        )
          yield Position(i,j)
      ).head
      Seq(new ExtensionStep(firstFreePosition,BlackCell))
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = {
      (for (i <- 0 until shape.width; j <- 0 until shape.height) yield (i,j)).
        forall( {case (ii,jj) => cells contains Position(ii,jj) } )
    }
    override def toString: String = {
      val strDesc: String = (
        for (j <- 0 until shape.height) yield (
          for (i <- 0 until shape.width)
          yield cells(Position(i,j)).toString
        ).mkString("    | ",""," |")
      ).mkString("\n","\n","\n")
      s"${puzzleName}<${strDesc}>"
    }
  }
  class CrosswordExtensionStep(val nPosition: Position, val nCellContents: CellContents) extends AbstractExtensionStep
}
