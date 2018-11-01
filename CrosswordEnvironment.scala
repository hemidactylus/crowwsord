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
      true
      // FIXME usedWords.intersect(extensionStep.newWords).isEmpty
    }
    def stepProposals: Seq[ExtensionStep] = {
      val emptiesLazyFinder = (
        for (
          y <- (0 until shape.height).view;
          x <- (0 until shape.width).view;
          tPos=Position(x,y)
          if !cells.contains(tPos)
        ) yield tPos
      )
      if (emptiesLazyFinder.isEmpty)
        Seq.empty
      else {
        val firstFreePosition: Position=emptiesLazyFinder.head
        // we propose horizontal words from here, or a black cell
        val extensionMask:CrosswordExtensionStepMask = makeExtensionMask(
          firstFreePosition
        )
        (
          shape
            .wordSet
            .filter( extensionMask.accepts(_) )
            .map(
              wordAddingResult(_,firstFreePosition)
            )
        ).toSeq :+ new ExtensionStep(Seq(new CrosswordCellStep(firstFreePosition,BlackCell)), Set.empty)
      }
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = {
      cells.size - 2*(shape.width + shape.height + 2) >= shape.width*shape.height
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
    //
    def wordAddingResult(wordToAdd: String, startPosition: Position): ExtensionStep = {
      val letterWordOptSequence: Seq[(CrosswordCellStep,Option[String])]=wordToAdd.zipWithIndex.map(
        {
          case(let,xind) => (
            new CrosswordCellStep(
              Position(startPosition.x+xind,startPosition.y),
              Letter(let)
            ),
            None
          )
        }
      ).toSeq
      val (
        letterSequence: Seq[CrosswordCellStep],
        newWordOptionSeq: Option[String]
      )=letterWordOptSequence.unzip
      val newWordAcross:Set[String] = (for(Some(wdx)<-newWordOptionSeq) yield wdx).toSet
      // complete /or not/ with the final black cell
      val closedLetterSequence: Seq[CrosswordCellStep] = if (!cells.contains(Position(startPosition.x+wordToAdd.length,startPosition.y)))
        letterSequence :+ new CrosswordCellStep(
            Position(startPosition.x+wordToAdd.length,startPosition.y),
            BlackCell
          )
      else
        letterSequence

      new ExtensionStep(
        closedLetterSequence,
        Set[String](wordToAdd) ++ newWordAcross
      )
    }
    def makeExtensionMask(startPosition: Position): CrosswordExtensionStepMask = {
      // we start from this position and prepare a mask for the extensions
      // we can propose. TEMP: max length of the word to insert
      val maxLength: Int =(
        for (
          x <- (startPosition.x to shape.width).view;
          tPos=Position(x,startPosition.y);
          if cells.getOrElse(tPos,EmptyCell)==BlackCell
        ) yield x-startPosition.x
      ).head
      CrosswordExtensionStepMask(maxLength)
    }
  }
  class CrosswordCellStep(val nPosition: Position, val nCellContents: CellContents)
  class CrosswordExtensionStep(
    val cellSteps: Seq[CrosswordCellStep],
    val newWords: Set[String]
  ) extends AbstractExtensionStep
  //
  case class CrosswordExtensionStepMask(maxLength: Int) {
    // temporary implementation
    def accepts(wd: String): Boolean = wd.length <= maxLength
  } 
}
