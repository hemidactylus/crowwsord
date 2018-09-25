/*
  configuration.scala

  class for holding immutable configurations
*/

package net.salamandrina.crowwsord

abstract class ConfShape[+T]
abstract class ConfContents[+T]
// cells are filled with T, items to insert are U
abstract class Configuration[T,U] (val shape: ConfShape[Configuration[T,U]],
    // val contents: Seq[T], val pieces: Set[U]) {
    val contents: ConfContents[Configuration[T,U]], val pieces: Set[U]) {
  type ExtendStep
  def stepProposals: Seq[ExtendStep]
  def isCompleted: Boolean
  def canExtendWith(c: ExtendStep): Boolean
  val objName: String = "GeneralConfig"
  val displayFiller: T
  def extendWith(stp: ExtendStep): Configuration[T,U]
  def findSolutions: Seq[Configuration[T,U]] = {
    if (isCompleted)
      Seq(this)
    else {
      {
        for ( 
          c: ExtendStep <- stepProposals;
          if canExtendWith(c)
        ) yield extendWith(c)
      }.toSeq.flatMap (_.findSolutions)
    }
  }
  override def toString = s"${objName}< . . . >"
}

class CharLine(shape: CharLineShape, contents: CharLineContents, pieces: Set[Char])
  extends Configuration[Char,Char](shape,contents,pieces) {
  case class CharLineStep(val piece: Char)
  type ExtendStep = CharLineStep
  def stepProposals: Seq[ExtendStep] = {
    ( for (c: Char <- pieces) yield CharLineStep(c)).toSeq
  }
  def isCompleted = contents.chars.length==shape.length
  val displayFiller: Char = '.'
  override val objName="CharLine"
  def extendWith(stp: ExtendStep): CharLine = new CharLine(
    shape,
    new CharLineContents(contents.chars :+ stp.piece),
    pieces
  )
  def canExtendWith(stp: ExtendStep): Boolean = {
    val newContentsBody: Seq[Char] = contents.chars :+ stp.piece
    if(newContentsBody.length < 2)
      true
    else
      newContentsBody.sliding(2) forall ( pair => pair(0) < pair(1) )
  }
  override def toString = (
      contents.chars ++
      Seq.fill(shape.length-contents.chars.length)(displayFiller)
    )
    .mkString(s"${objName}<","",">")
}
class CharLineShape(val length: Int) extends ConfShape[CharLine]
class CharLineContents(val chars: Seq[Char]) extends ConfContents[CharLine]

class IntLine(shape: IntLineShape, contents: IntLineContents, pieces: Set[Int]) 
  extends Configuration[Int,Int](shape,contents,pieces) {
  case class IntLineStep(val piece: Int)
  type ExtendStep = IntLineStep
  def stepProposals: Seq[ExtendStep] = {
    ( for (c: Int <- pieces) yield IntLineStep(c)).toSeq
  }
  def isCompleted = contents.ints.length==shape.length
  val displayFiller: Int = -1
  override val objName="IntLine"
  def extendWith(stp: IntLineStep): IntLine = new IntLine(
    shape,
    new IntLineContents(contents.ints :+ stp.piece),
    pieces
  )
  def canExtendWith(stp: IntLineStep): Boolean = {
    val newContentsBody: Seq[Int] = contents.ints :+ stp.piece
    ( newContentsBody forall ( _ > 2 ) ) &&
    (
      newContentsBody.length < shape.length ||
      ( ( newContentsBody :\ 0 ) (_ + _) ) % 2 == 0
    )
  }
  override def toString = (
      contents.ints ++ 
      Seq.fill(shape.length-contents.ints.length)(displayFiller)
    ).mkString(s"${objName}<","",">")
}
class IntLineShape(val length: Int) extends ConfShape[IntLine]
class IntLineContents(val ints: Seq[Int]) extends ConfContents[IntLine]

class SteppableIntLine(shape: SteppableIntLineShape, contents: SteppableIntLineContents) 
  extends Configuration[Int,Nothing](shape,contents,Set.empty) {
  case class SteppableIntLineStep(val pos: Int)
  type ExtendStep = SteppableIntLineStep
  def stepProposals: Seq[ExtendStep] = {
    val lastIndex = if (contents.indices.isEmpty) (shape.length+10) else contents.indices.last
    val okIndices = (0 until shape.length)
      .filter( x => ! ( contents.indices contains x ) )
      .filter(x => Math.abs(lastIndex - x)>1)
    (for (c <- okIndices) yield new SteppableIntLineStep(c)).toSeq
  }
  def isCompleted = contents.indices.size==shape.length
  val displayFiller: Int = -1
  override val objName="SteppableIntLine"
  def extendWith(stp: SteppableIntLineStep): SteppableIntLine = new SteppableIntLine(
    shape,
    new SteppableIntLineContents(contents.indices :+ stp.pos)
  )
  def canExtendWith(stp: SteppableIntLineStep): Boolean = {
    true
  }
  override def toString = {
    val indMap = (contents.indices zipWithIndex).toMap
    (0 until shape.length)
      .map( x => indMap getOrElse(x,displayFiller) )
      .mkString(s"${objName}<","",">")
  }
}
class SteppableIntLineShape(val length: Int) extends ConfShape[SteppableIntLine]
class SteppableIntLineContents(val indices: Seq[Int]) extends ConfContents[SteppableIntLine]
  // indices: list of indices reached in their order

class CoinTotal(shape: CoinTotalShape, contents: CoinTotalContents, pieces: Set[Int])
  extends Configuration[Int,Int](shape,contents,pieces) {
    // we will never pick denominations greater than the minimum already used
    // but this is not optimal and we will introduce generic state alongside the "seq"
    // or in its replacement which is even better
  case class CoinTotalStep(val piece: Int)
  type ExtendStep = CoinTotalStep
  def stepProposals: Seq[ExtendStep] = {
    val totalSoFar = (contents.coins :\ 0) ( _ + _ )
    val minUsed = if (contents.coins isEmpty) pieces.max else contents.coins.min
    (for ( pc <- pieces; if pc <= (shape.amount-totalSoFar); if pc <= minUsed) yield CoinTotalStep(pc)).toSeq
  }
  def isCompleted = ( (contents.coins :\ 0) ( _ + _ ) ) == shape.amount
  val displayFiller: Int = 0
  override val objName="CoinTotal"
  def extendWith(stp: ExtendStep): CoinTotal = new CoinTotal(
    shape,
    new CoinTotalContents(contents.coins :+ stp.piece),
    pieces
  )
  def canExtendWith(stp: ExtendStep): Boolean = true
  override def toString = {
    val ending = if (isCompleted) "" else "+..."
    val bulk = contents.coins.mkString("+")
    s"${objName}< ${shape.amount} = ${bulk}${ending} >"
  }
}
class CoinTotalShape(val amount: Int) extends ConfShape[CoinTotal]
class CoinTotalContents(val coins: Seq[Int]) extends ConfContents[CoinTotal]

class SquareStepper(shape: SquareStepperShape, contents: SquareStepperContents)
  extends Configuration[Int,Nothing](shape,contents,Set.empty) {
  case class SquareStepperStep(val posx: Int, val posy: Int)
  type ExtendStep = SquareStepperStep

  val maxNumDigits = (shape.side*shape.side).toString.length

  def stepProposals: Seq[ExtendStep] = {
    // we have to pick the available positions
    // that are (1) free and (2) reachable by the last pos
    // unless, that is, this is the first position ever, in which case: all free
    if (contents.occupancy.isEmpty) {
      for (
        x <- 0 until shape.side;
        y <- 0 until shape.side
      ) yield new SquareStepperStep(x,y)
    } else {
      val (lastX,lastY) = contents.occupancy.last
      (
        for (
          y <- 0 until shape.side;
          if !(contents.occupancy contains (lastX,y));
          if Math.abs(y-lastY)>1
        ) yield new SquareStepperStep(lastX,y)
      ) ++ (
        for (
          x <- 0 until shape.side;
          if !(contents.occupancy contains (x,lastY));
          if Math.abs(x-lastX)>1
        ) yield new SquareStepperStep(x,lastY)
      )
    }
  }
  def isCompleted = contents.occupancy.length == shape.side*shape.side
  val displayFiller: Int = -1
  override val objName="SquareStepper"
  def extendWith(stp: ExtendStep): SquareStepper = new SquareStepper(
    shape,
    new SquareStepperContents(contents.occupancy :+ (stp.posx,stp.posy))
  )
  def canExtendWith(stp: ExtendStep): Boolean = true
  override def toString = {
    // prepare the square map
    val myMatrix = Array.fill(shape.side)(Array.fill(shape.side)(displayFiller))
    for ( ((x,y),idx) <- contents.occupancy zipWithIndex )
      myMatrix(y)(x)=idx
    myMatrix.map( sa => sa.map( x => if(x<0) " ." else (x+1 formatted s"%${1+maxNumDigits}d") ).mkString("") ).mkString("\n")
  }
}
class SquareStepperShape(val side: Int) extends ConfShape[SquareStepper]
class SquareStepperContents(val occupancy: Seq[(Int,Int)]) extends ConfContents[SquareStepper]
// occupancy is a list of filled cells, in that order
