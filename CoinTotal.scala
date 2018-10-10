/*
  CoinTotal.scala
*/

import net.salamandrina.crowwsord._

//
object CoinTotalEnvironment extends PuzzleEnvironment {
  val puzzleName = "CoinTotal"
  type Configuration = CoinTotalConfiguration
  type PuzzleShape = CoinTotalPuzzleShape
  type ExtensionStep = CoinTotalExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration = new Configuration(shape,Seq.empty)
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration = {
    new Configuration(other.ctShape, other.ctContents :+ extension.newCoin)
  }
  //
  case class CoinTotalPuzzleShape(amount: Int, denominations: Set[Int]) extends AbstractPuzzleShape
  class CoinTotalConfiguration(shape: CoinTotalPuzzleShape, contents: Seq[Int]) extends AbstractConfiguration {
    private def getMinOfTwo(x: Int, y: Int): Int = if (x<y) x else y
    val ctShape: CoinTotalPuzzleShape = shape
    val ctContents: Seq[Int] = contents
    val minUsed: Int = {
      if(ctContents.isEmpty)
        ctShape.denominations.max
      else
        ctContents.min
    }
    //
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      (ctContents :\ 0)(_ + _) <= ctShape.amount - extensionStep.newCoin
    }
    def stepProposals: Seq[ExtensionStep] = {
      (
        for(newCoin <- ctShape.denominations; if newCoin <= minUsed)
        yield new CoinTotalExtensionStep(newCoin)
      ).toSeq
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = (ctContents :\ 0)(_ + _) == ctShape.amount
    override def toString: String = {
      val strDesc: String = contents.mkString(" + ")
      s"${puzzleName}<${ctShape.amount} = ${strDesc}>"
    }
  }
  class CoinTotalExtensionStep(val newCoin: Int) extends AbstractExtensionStep
}

// //

// class CoinTotal(shape: CoinTotalShape, contents: CoinTotalContents, pieces: Set[Int])
//   extends Configuration[Int,Int](shape,contents,pieces) {
//     // we will never pick denominations greater than the minimum already used
//     // but this is not optimal and we will introduce generic state alongside the "seq"
//     // or in its replacement which is even better
//   case class CoinTotalStep(val piece: Int)
//   type ExtendStep = CoinTotalStep
//   def stepProposals: Seq[ExtendStep] = {
//     val totalSoFar = (contents.coins :\ 0) ( _ + _ )
//     val minUsed = if (contents.coins isEmpty) pieces.max else contents.coins.min
//     (for ( pc <- pieces; if pc <= (shape.amount-totalSoFar); if pc <= minUsed) yield CoinTotalStep(pc)).toSeq
//   }
//   def isCompleted = ( (contents.coins :\ 0) ( _ + _ ) ) == shape.amount
//   val displayFiller: Int = 0
//   override val objName="CoinTotal"
//   def extendWith(stp: ExtendStep): CoinTotal = new CoinTotal(
//     shape,
//     new CoinTotalContents(contents.coins :+ stp.piece),
//     pieces
//   )
//   def canExtendWith(stp: ExtendStep): Boolean = true
//   override def toString = {
//     val ending = if (isCompleted) "" else "+..."
//     val bulk = contents.coins.mkString("+")
//     s"${objName}< ${shape.amount} = ${bulk}${ending} >"
//   }
// }
// class CoinTotalShape(val amount: Int) extends ConfShape[CoinTotal]
// class CoinTotalContents(val coins: Seq[Int]) extends ConfContents[CoinTotal]
