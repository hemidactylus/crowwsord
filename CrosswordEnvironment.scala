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
      other.usedWords ++ extension.newWords.toSet
    )
  }
  def createConfig(
    sizes: (Int,Int),
    wordSet: Set[String],
    startCells: Map[Position,CellContents]=Map.empty,
    maxBlackCellCount: Option[Int]=None
  ): Configuration = {
    //
    val fullWordSet: Set[String]=wordSet ++ Set[String]("")
    // FIXME how this repeatable words set is made
    val repeatableWords: Set[String]=fullWordSet.filter( _.length <= 2 )
    val borderedStartCells: Map[Position,CellContents] = startCells ++ (
        for (
          x <- List(-1,sizes._1);
          y <- 0 until sizes._2
        ) yield (Position(x,y) -> BlackCell)
      ).toMap ++ (
        for (
          y <- List(-1,sizes._2);
          x <- -1 to sizes._1
        ) yield (Position(x,y) -> BlackCell)
      ).toMap
    val borderedShape: PuzzleShape = CrosswordPuzzleShape(
      sizes._1,
      sizes._2,
      fullWordSet,
      fullWordSet.toList.sortBy( _.length ).reverse,
      repeatableWords,
      maxBlackCellCount.map( _ + ( 2*(sizes._1 + sizes._2 + 2) ) )
    )
    makeConfig(borderedShape,borderedStartCells)
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
    wordList: List[String], // to prioritize long words, we pick from a list
    repeatableWords: Set[String],
    maxBlackCellCount: Option[Int]
  ) extends AbstractPuzzleShape
  //
  class CrosswordConfiguration(
    val shape: CrosswordPuzzleShape,
    val cells: Map[Position,CellContents],
    val usedWords: Set[String]
  ) extends AbstractConfiguration {
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      (
        // FIXME treat the new words as a list!
        (
          usedWords
            .intersect(extensionStep.newWords.toSet) // FIXME
            .forall( shape.repeatableWords.contains(_) )
        ) && ( 
          extensionStep
            .newWords
            .toSet // FIXME
            .forall( shape.wordSet.contains(_) )
        ) && (
          shape.maxBlackCellCount match {
            case None => true
            case Some(mbcc) => (
              (
                cells
                  .values
                  .map( (cCont: CellContents) => if (cCont==BlackCell) 1 else 0 ).
                  reduce( _ + _ ) +
                extensionStep.cellSteps 
                  .map( (cStep: CrosswordCellStep) => if (cStep.nCellContents==BlackCell) 1 else 0 ).
                  reduce( _ + _ )
              ) <= mbcc
            )
          }
        )
      )
    }
    def stepProposals: Stream[ExtensionStep] = {
      val emptiesLazyFinder = (
        for (
          y <- (0 until shape.height).view;
          x <- (0 until shape.width).view;
          tPos=Position(x,y)
          if !cells.contains(tPos)
        ) yield tPos
      )
      if (emptiesLazyFinder.isEmpty)
        Stream.empty
      else {
        val firstFreePosition: Position=emptiesLazyFinder.head
        // we propose horizontal words from here, or a black cell
        val extensionMask:CrosswordExtensionStepMask = makeExtensionMask(
          firstFreePosition
        )
        (
          shape
            .wordList
            .toStream
            .filter( extensionMask.accepts(_) )
            .map(
              wordAddingResult(_,firstFreePosition)
            )
        )
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
        s"    used words: {${usedWordDesc}} "
      )
      s"${puzzleName}<${strDesc}\n>"
    }
    //
    def wordAddingResult(wordToAdd: String, startPosition: Position): ExtensionStep = {
      val letterWordLstSequence: Seq[(CrosswordCellStep,Seq[String])]=wordToAdd.zipWithIndex.map(
        {
          case(let,xind) => {
            val thisPosition=Position(startPosition.x+xind,startPosition.y)
            val tLetter=Letter(let)
            (
              new CrosswordCellStep(
                thisPosition,
                tLetter
              ),
              getCrossingCompleteWord(
                thisPosition,
                tLetter
              )
            )
          }
        }
      ).toSeq
      // complete /or not/ with the final black cell
      val closedLetterWOSequence: Seq[(CrosswordCellStep,Seq[String])] =
        if (!cells.contains(Position(startPosition.x+wordToAdd.length,startPosition.y)))
          letterWordLstSequence :+ (
            new CrosswordCellStep(
              Position(startPosition.x+wordToAdd.length,startPosition.y),
              BlackCell
            ),
            getCrossingCompleteWord(
              Position(startPosition.x+wordToAdd.length,startPosition.y),
              BlackCell
            )
          )
        else
          letterWordLstSequence
      //
      val (
        closedLetterSequence: Seq[CrosswordCellStep],
        newWordLstSeq: Seq[Seq[String]]
      )=closedLetterWOSequence.unzip
      val newWordsAcross: List[String] = (newWordLstSeq.toList.flatMap( (wList: Seq[String]) => wList.toList ))
      new ExtensionStep(
        closedLetterSequence,
        newWordsAcross :+ wordToAdd
      )
    }
    def getCrossingCompleteWord(midPosition: Position, inserteeCell: CellContents): Seq[String] = {
      // given a middle starting position, and its proposed content letter,
      // a Some(word) is returned if and only if the placement of the letter
      // *completes* a vertical word up to the first encountered black cell
      // and down to the first encountered black cell
      val forwardCutCells: List[CellContents] = (
        for(y <- (midPosition.y+1 to shape.height+1).view)
          yield cells.getOrElse(Position(midPosition.x,y),EmptyCell)
        )
        .takeWhile( _ != BlackCell ).toList
      val backwardCutCells: List[CellContents] = (
        for(y <- (midPosition.y-1 to -1 by -1     ).view)
          yield cells.getOrElse(Position(midPosition.x,y),EmptyCell)
        )
        .takeWhile( _ != BlackCell ).toList
      // now those two get to right before the first black cell.
      // what to do now? It depends on whether we are adding a black cell or a letter
      if (inserteeCell==BlackCell) {
        for (
          seqc <- Seq[Seq[CellContents]](backwardCutCells.reverse, forwardCutCells);
          if !seqc.isEmpty;
          if !seqc.exists( _ == EmptyCell )
        ) yield ( seqc.map( { case Letter(c) => c } ).mkString("") )
      } else {
        val completeSequence: Seq[CellContents]=(backwardCutCells.reverse :+ inserteeCell) ++ forwardCutCells
        if ( (!completeSequence.isEmpty) && (
          !completeSequence.exists( _ == EmptyCell )
        )) {
          Seq[String]( completeSequence.map( { case Letter(c) => c } ).mkString("") )
        } else
          Seq.empty
      }
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
    val newWords: List[String]
  ) extends AbstractExtensionStep {
    override def toString = s"CellSteps[${cellSteps}],newWords[${newWords}]"
  }
  //
  case class CrosswordExtensionStepMask(maxLength: Int) {
    // temporary implementation
    def accepts(wd: String): Boolean = wd.length <= maxLength
  } 
}
