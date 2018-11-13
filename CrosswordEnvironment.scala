/*
  CrosswordEnvironment.scala
*/

import net.salamandrina.crowwsord._
import net.salamandrina.patriciatrees.PatriciaTreeSet

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
    val fullWordPatriciaTreeSet: PatriciaTreeSet = PatriciaTreeSet(fullWordSet)
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
      fullWordPatriciaTreeSet,
      repeatableWords,
      maxBlackCellCount.map( _ + ( 2*(sizes._1 + sizes._2 + 2) ) )
    )
    makeConfig(borderedShape,borderedStartCells)
  }
  case class Position(x: Int, y: Int)
  class CellContents
  case object EmptyCell extends CellContents { override def toString="." }
  case object BlackCell extends CellContents { override def toString="#" }
  case class Letter(c: Char) extends CellContents { override def toString = c.toString.toUpperCase }
  case class CrosswordPuzzleShape(
    width: Int,
    height: Int,
    wordSet: Set[String],
    wordPatriciaTreeSet: PatriciaTreeSet,
    repeatableWords: Set[String],
    maxBlackCellCount: Option[Int]
  ) extends AbstractPuzzleShape
  class CrosswordConfiguration(
    val shape: CrosswordPuzzleShape,
    val cells: Map[Position,CellContents],
    val usedWords: Set[String]
  ) extends AbstractConfiguration {
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      val newWordCounter: Map[String,Int]=CrosswordUtilities.makeCounter(extensionStep.newWords)
      (
        (
          // words from the proposal: the non-new are all repeatable?
          usedWords
            .intersect(newWordCounter.keys.toSet)
            .forall( shape.repeatableWords.contains(_) )
        ) && ( 
          // words appearing more than once in the proposal
          newWordCounter
            .map( wc => if (wc._2>1) Some(wc._1) else None )
            .flatMap( w => w )
            .toSet
            .forall( shape.repeatableWords.contains(_) )
        ) && ( 
          newWordCounter
            .keys
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
        // we have to go back through possibly letter cells
        val startingPosition: Position=(
          (
            for (
              x <- (firstFreePosition.x to -1 by -1).view;
              sPos: Position = Position(x, firstFreePosition.y)
            ) yield (sPos, cells.get(sPos))
          )
          .takeWhile( _._2 != Some(BlackCell) )
        )
          .last
          ._1
        // we propose horizontal words from here, or a black cell
        val extensionMask:CrosswordExtensionStepMask = makeExtensionMask(
          startingPosition
        )
        val allowedLengths: Set[Int] = extensionMask.allowedLengths
        (
          shape
            .wordPatriciaTreeSet
            .maskedIterator(
              extensionMask.letterMask
            )
            .toStream
            .filter( allowedLengths contains _.length )
            .map(
              wordAddingResult(_,extensionMask)
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
    def wordAddingResult(wordToAdd: String, extensionMask: CrosswordExtensionStepMask): ExtensionStep = {
      val startPosition: Position = extensionMask.cells.head.pos
      val cellContentsToAdd: Seq[CellContents] = wordToAdd.map( (let: Char) => Letter(let) ) :+ BlackCell
      val letterWordLstSequence: Seq[(CrosswordCellStep,Seq[String])]=cellContentsToAdd
        .zip( extensionMask.cells )
        .map(
          {
            case(ccont,cellinfo) => {
              (
                new CrosswordCellStep(
                  cellinfo.pos,
                  ccont
                ),
                getCrossingCompleteWord(
                  cellinfo,
                  ccont
                )
              )
            }
          }
        )
        .toSeq
      val (
        closedLetterSequence: Seq[CrosswordCellStep],
        newWordLstSeq: Seq[Seq[String]]
      )=letterWordLstSequence.unzip
      val newWordsAcross: List[String] = (newWordLstSeq.toList.flatMap( (wList: Seq[String]) => wList.toList ))
      new ExtensionStep(
        closedLetterSequence,
        newWordsAcross :+ wordToAdd
      )
    }
    def getCrossingCompleteWord(cellInfo: CrosswordExtensionStepCellInfo,
      inserteeCell: CellContents): Seq[String] = {
      // given a middle starting position, and its proposed content letter,
      // a Seq(word) is returned, with item if and only if the placement of the letter
      // *completes* a vertical word up to the first encountered black cell
      // and down to the first encountered black cell
      val midPosition: Position = cellInfo.pos

      if (cells contains cellInfo.pos)
        Seq.empty
      else {
        // if this insertion is black cell, return one or two words
        // if it is a letter, return zero or one
        inserteeCell match {
          case BlackCell => {
            (
              for(
                cellList: List[CellContents] <- Seq(cellInfo.prefixCells, cellInfo.followingCells);
                if (!(cellList.exists( _ == EmptyCell )))
              ) yield (
                cellList.map( { case Letter(c) => c } ).mkString("")
              )
            )
          }
          case Letter(_) => {
            val allCells: List[CellContents] = (
              cellInfo.prefixCells ++ List[CellContents](inserteeCell) ++ cellInfo.followingCells
            )
            if (allCells.exists( _ == EmptyCell ))
              Seq.empty
            else {
              Seq[String](
                allCells
                  .map( { case Letter(c) => c } )
                  .mkString("")
              )
            }
          }
        }
      }
    }
    def getExtensionStepCellInfo(pos: Position): CrosswordExtensionStepCellInfo = {
      // given a position (of the region potentially filled with the proposee word)
      // a step cell info is calculated
      val previousCells: List[CellContents] = (
        for(y <- (pos.y-1 to -1 by -1 ).view)
          yield cells.getOrElse(Position(pos.x,y),EmptyCell)
        )
        .takeWhile( _ != BlackCell )
        .toList
        .reverse
      val followingCells: List[CellContents] = (
        for(y <- (pos.y+1 to shape.height).view)
          yield cells.getOrElse(Position(pos.x,y),EmptyCell)
        )
        .takeWhile( _ != BlackCell )
        .toList
      val posContentsOption: Option[CellContents] = cells.get(pos)
      val suffixFoundCharacters: Set[Char]=shape
        .wordPatriciaTreeSet
        .withPrefix(
          previousCells
          .map( { case Letter(c) => c } )
          .mkString("") 
        )
        .suffixes
        .keys
        .toSet
      CrosswordExtensionStepCellInfo(
        pos,
        posContentsOption match {
          case Some(Letter(c)) => suffixFoundCharacters.intersect(Set[Char](c))
          case _ => suffixFoundCharacters
        },
        previousCells,
        followingCells,
        posContentsOption match {
          case Some(Letter(_)) => false
          case _ => true
        }
      )
    }
    def makeExtensionMask(startPosition: Position): CrosswordExtensionStepMask = {
      // Here we use the patricia set for all formed verticals upon this
      // and prepare the mask to pick proposed words
      new CrosswordExtensionStepMask(
        // a list of mask + whether-verticalClosing
        (
          for (
            x <- (startPosition.x to shape.width).view;
            tPos=Position(x,startPosition.y)
          ) yield tPos
        )
        .takeWhile ( cells.getOrElse(_, EmptyCell)!=BlackCell)
        .map( getExtensionStepCellInfo(_) )
        .toList
      )
    }
  }
  class CrosswordCellStep(val nPosition: Position, val nCellContents: CellContents) {
    override def toString = s"CrosswordCellStep(${nPosition}:${nCellContents})"
  }
  class CrosswordExtensionStep(
    val cellSteps: Seq[CrosswordCellStep],
    val newWords: List[String]
  ) extends AbstractExtensionStep {
    override def toString = s"CellSteps[${cellSteps}],newWords[${newWords}]"
  }
  case class CrosswordExtensionStepCellInfo(pos: Position, letterMask: Set[Char],
    prefixCells: List[CellContents], followingCells: List[CellContents],
    canBeBlack: Boolean)
  case class CrosswordExtensionStepMask(cells: List[CrosswordExtensionStepCellInfo]) {
    def letterMask: List[Set[Char]] = cells.map( _.letterMask )
    def allowedLengths: Set[Int] = (
      (
        for (
          (extStpCellInfo,indPos) <- cells.zipWithIndex;
          if extStpCellInfo.canBeBlack
        )
        yield indPos
      ) ++ Iterator[Int](cells.length)
    ).toSet
  }
}
