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
    // println(s"MC oth=>${other.usedWords} ext=>${extension.newWords}")
    new Configuration(
      other.shape,
      other.cells ++ (
        for(extensionCell <- extension.cellSteps )
          yield extensionCell.nPosition->extensionCell.nCellContents
      ),
      other.usedWords ++ extension.newWords
    )
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
    wordSet: Set[String]
  ) extends AbstractPuzzleShape
  //
  class CrosswordConfiguration(
    val shape: CrosswordPuzzleShape,
    val cells: Map[Position,CellContents],
    val usedWords: Set[String]
  ) extends AbstractConfiguration {
    //
    // println(s"    born with usedWords=${usedWords}")
    // println(this)
    //

    // if ( usedWords contains "A" ) {
    //   println(s"withA: ${this.toString}!!\n")
    // }

    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      true // FIXME
    }
    def stepProposals: Seq[ExtensionStep] = {
      // what is the position to start from?
      val firstFreePosition: Position = (
        for (
          j <- 0 until shape.height;
          i <- 0 until shape.width;
          if !(cells contains Position(i,j) )
        )
          yield Position(i,j)
      ).head
      // how long can the extension be?
      val cellsAhead: Seq[CellContents] = (
        for(i <- firstFreePosition.x until shape.width)
          yield cells.getOrElse(Position(i,firstFreePosition.y),EmptyCell)
      ).takeWhile( _ != BlackCell )
      val maxLenAhead = cellsAhead.length
      // final sequence of possible extensions is returned
      // TODO: check for crossings
      // TODO handle letter-already-present-constraint
      // TODO: is is true that if len===maxlen then (and only then) one has to drop the closing '#'?
      // TOFIX: here we prepare the closing "#" case in advance
      val closingBlackCellCase: Option[Set[String]]=getCloseeWords(firstFreePosition)
      val closingBlackCellSubseq: Seq[ExtensionStep]= closingBlackCellCase match {
        case None => Seq.empty
        case Some(wds: Set[String]) => Seq(
          // this is the "just add a black cell" item in the sequence of ExtensionStep's
          new CrosswordExtensionStep(
            Seq(
              new CrosswordCellStep(
                Position(firstFreePosition.x,firstFreePosition.y),
                BlackCell
              )
            ),
            wds
          )
        )
      }
      //
      (
        for (
          word <- shape.wordSet;
          if word.length <= maxLenAhead;
          if !(usedWords contains word);
          // this is how we check that all completed vert. words are OK
          generatedCrossingWords <- determineGeneratedWords(word,firstFreePosition)
        )
          yield (
            (
              for( (chr,deltax) <- word.zipWithIndex)
                yield new CrosswordCellStep( Position(firstFreePosition.x+deltax,firstFreePosition.y), Letter(chr) )
            ) ++ (
              if (word.length == maxLenAhead) Seq.empty else Seq(new CrosswordCellStep(
                Position(firstFreePosition.x+word.length,firstFreePosition.y),
                BlackCell
              ))
            ),
            generatedCrossingWords + word
          )
      )
        .map( {case (cellSeq: Seq[CrosswordCellStep],nWordSet: Set[String]) => new ExtensionStep(cellSeq, nWordSet ) } )
        .toSeq ++ closingBlackCellSubseq }
    def getCloseeWords(clPosition: Position): Option[Set[String]] = {
      // returns a Some() of set of zero, one or two words
      // which the impending arrival of a black cell would close
      // or None if the two closee words are the same
      val above = for (
        y <- clPosition.y-1 until -1 by -1
      ) yield cells.getOrElse(Position(clPosition.x,y),EmptyCell)
      val left = for (
        x <- clPosition.x-1 until -1 by -1
      ) yield cells.getOrElse(Position(x,clPosition.y),EmptyCell)
      val (aboveIn,aboveOut) = above.span( _ match {
        case EmptyCell => false
        case BlackCell => false
        case _ => true
      } )
      val (leftIn,leftOut) = left.span( _ match {
        case EmptyCell => false
        case BlackCell => false
        case _ => true
      } )
      val closeeWordsList: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
      // println(s"[getCloseeWords] Cfg ${this.toString}. BlackCell pos ${clPosition}")
      // println(s"abIn <${aboveIn}>, abOut <${aboveOut}>")
      if (!aboveIn.isEmpty && (aboveOut.isEmpty || (aboveOut.head == BlackCell))) {
        // println(s"adding ${aboveIn.reverse.mkString} to closeeWL")
        closeeWordsList += aboveIn.reverse.mkString
      }
      if (!leftIn.isEmpty && (leftOut.isEmpty || (leftOut.head == BlackCell))) {
        closeeWordsList += leftIn.reverse.mkString
      }
      val closeeWordsSet=closeeWordsList.toSet
      // println(s"checkking: set=${closeeWordsSet}, lst=${closeeWordsList}, wordSet=${shape.wordSet}")
      if ((closeeWordsSet.size == closeeWordsList.length) && (closeeWordsSet.forall( shape.wordSet contains _ ))) {
        //
        if (closeeWordsSet.size>0){
          // println(s"Words: ${closeeWordsList.mkString("/")}\n\n")
        }
        //
        Some(closeeWordsSet)
      } else
        None
    }
    def getCompletedVerticalWord(pLetter: Char, lPosition: Position): Option[String] = {
      // inspects the half-formed schema to see if a complete word has
      // been formed (i.e. black-cells or end-of-schema must appear
      // as delimiters before unfilled cells.
      // Returns Some(completed word) or None
      val above = for (
        y <- lPosition.y-1 until -1 by -1
      ) yield cells.getOrElse(Position(lPosition.x,y),EmptyCell)
      val below = for (
        y <- lPosition.y+1 until shape.height
      ) yield cells.getOrElse(Position(lPosition.x,y),EmptyCell)
      // println(s"getC: ${above.reverse.mkString}|${pLetter}|${below.mkString}")
      val (aboveIn,aboveOut) = above.span( _ match {
        case EmptyCell => false
        case BlackCell => false
        case _ => true
      } )
      val (belowIn,belowOut) = below.span( _ match {
        case EmptyCell => false
        case BlackCell => false
        case _ => true
      } )
      // E.G. for "..#.ABcDE#FG" =>
      // aboveIn=BA
      // aboveOut=.#..
      // belowIn=DE
      // belowOut=#FG
      if (
        (
          aboveOut.isEmpty || (aboveOut.head == BlackCell)
        ) && (
          belowOut.isEmpty || (belowOut.head == BlackCell)
        )
      ) {
        val retString=(aboveIn.reverse ++ Seq(pLetter) ++ belowIn).mkString
        // println(s"ret => ${retString}")
        Some(retString)
      }
      else {
        // println("ret None")
        None
      }
    }
    def determineGeneratedWords(newHorizontalWord: String,wordStartingPosition: Position): Option[Set[String]] = {
      // given a horizontal word
      // and its starting position,
      // the current state of the schema is inspected
      // and those verticals which form a completed chunk
      // are returned as a Some(set of those)
      // *if they all belong to the available words to still employ*
      // println(newHorizontalWord)
      // println(wordStartingPosition)
      val formedVerticalWords: Seq[String] = (
        for (
          (pLetter,offset) <- newHorizontalWord.zipWithIndex;
          letterPosition = Position(
            wordStartingPosition.x + offset,
            wordStartingPosition.y
          );
          completedVerticalWord <- getCompletedVerticalWord(
            pLetter,
            letterPosition
          )
        ) yield completedVerticalWord
      )
      if (
        (
          formedVerticalWords.length == formedVerticalWords.toSet.size
        ) && (
          formedVerticalWords.forall( shape.wordSet contains _ )
        )
      ) {
        // println(s"[determineGeneratedWords] ret ${formedVerticalWords}")
        Some(formedVerticalWords.toSet)
      }
      else {
        // println("[determineGeneratedWords] ret None")
        None
      }
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = {
      (for (i <- 0 until shape.width; j <- 0 until shape.height) yield (i,j)).
        forall( {case (ii,jj) => cells contains Position(ii,jj) } )
    }
    override def toString: String = {
      val usedWordDesc: String = usedWords.mkString("/")
      val strDesc: String = (
        for (j <- 0 until shape.height) yield (
          for (i <- 0 until shape.width)
          yield cells.getOrElse(Position(i,j),EmptyCell).toString
        ).mkString("    | ",""," |")
      ).mkString("\n","\n","\n") + (
        s"    ~ used words: ${usedWordDesc} ~"
      )
      s"${puzzleName}<${strDesc}>"
    }
  }
  class CrosswordCellStep(val nPosition: Position, val nCellContents: CellContents)
  class CrosswordExtensionStep(
    val cellSteps: Seq[CrosswordCellStep],
    val newWords: Set[String]
  ) extends AbstractExtensionStep
}
