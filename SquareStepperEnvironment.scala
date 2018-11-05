/*
  SquareStepperEnvironment.scala
*/

import net.salamandrina.crowwsord._

object SquareStepperEnvironment extends PuzzleEnvironment {
  //
  case class Position(x: Int, y: Int) {
    override def toString: String = s"(${x},${y})"
  }
  object DirectionEnum extends Enumeration {
    val Forward, Backward = Value
  }
  type Direction = DirectionEnum.Value
  object FillingStrategyEnum extends Enumeration {
    val Monoplicate, Duplicate, Quadruplicate = Value
  }
  type FillingStrategy = FillingStrategyEnum.Value
  //
  val puzzleName = "SquareStepper"
  type Configuration = SquareStepperConfiguration
  type PuzzleShape = SquareStepperPuzzleShape
  type ExtensionStep = SquareStepperExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration = new Configuration(shape,Seq.empty,Seq.empty)
  def makeConfig(shape: PuzzleShape,positionPath: Seq[Position]): Configuration = new Configuration(shape,positionPath,Seq.empty)
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration = {
    extension.direction match {
      case DirectionEnum.Forward => new Configuration(
        other.sShape,
        other.sForward :+ extension.newPosition,
        other.sBackward
      )
      case DirectionEnum.Backward => new Configuration(
        other.sShape,
        other.sForward,
        other.sBackward :+ extension.newPosition
      )
    }
  }
  //
  class SquareStepperPuzzleShape(val sides: (Int,Int), val fillingStrategy: FillingStrategy) extends AbstractPuzzleShape
  //
  class SquareStepperConfiguration(
    shape: SquareStepperPuzzleShape,
    forward: Seq[Position],
    backward: Seq[Position]
  ) extends AbstractConfiguration {
    // shape-related checks
    val sShape: SquareStepperPuzzleShape = shape.fillingStrategy match {
      case FillingStrategyEnum.Monoplicate => shape
      case FillingStrategyEnum.Duplicate => {
        if (shape.sides._2 % 2 == 0)
          shape
        else
          throw new IllegalArgumentException("Shape Y must be even (Duplicate)")
      }
      case FillingStrategyEnum.Quadruplicate => {
        if ((shape.sides._2 % 2 == 0) && (shape.sides._1 % 2 == 0))
          shape
        else
          throw new IllegalArgumentException("Shape X,Y must be even (Quadruplicate)")
      }
    }
    val innerSides: (Int,Int) = shape.fillingStrategy match {
      case FillingStrategyEnum.Monoplicate => shape.sides
      case FillingStrategyEnum.Duplicate => (shape.sides._1,shape.sides._2 / 2)
      case FillingStrategyEnum.Quadruplicate => (shape.sides._1 / 2,shape.sides._2 / 2)
    }

    //
    val sForward: Seq[Position]=forward
    val sBackward: Seq[Position]=backward
    //
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      !(
        sForward contains extensionStep.newPosition
      ) && !(
        sBackward contains extensionStep.newPosition
      )
    }
    val deltas: Seq[Position] = Seq(
      Position( 0,+3),
      Position( 0,-3),
      Position(+3, 0),
      Position(-3, 0),
      Position(-2,-2),
      Position(-2,+2),
      Position(+2,-2),
      Position(+2,+2)
    )
    def expandFromPoint(sPoint: Position): Stream[Position] = {
      (
        for (
          delta <- deltas;
          nPoint = Position(sPoint.x+delta.x,sPoint.y+delta.y);
          if nPoint.x >= 0 && nPoint.y >= 0 && nPoint.x < innerSides._1 && nPoint.y < innerSides._2
        ) yield nPoint
      ).toStream
    }
    def stepProposals: Stream[ExtensionStep] = {
      // the first pos is always on forward
      val newDir: Direction = if(sForward.length > sBackward.length) DirectionEnum.Backward else DirectionEnum.Forward
      val posList: Seq[Position] = if (newDir == DirectionEnum.Forward) sForward else sBackward
      if (posList.isEmpty) {
        if ( newDir == DirectionEnum.Forward)
          // here we start from scratch
          sShape.fillingStrategy match {
            case FillingStrategyEnum.Monoplicate => {
              for( x <- (0 until innerSides._1).toStream ; y <- (0 until innerSides._2).toStream ) yield new ExtensionStep(Position(x,y),newDir)
            }
            case FillingStrategyEnum.Duplicate => {
              // we place the first pos ready for a vert flip, i.e. on the second-to-last horiz strip
              for( x <- (0 until innerSides._1).toStream ) yield new ExtensionStep(Position(x,innerSides._2-2),newDir)
            }
            case _ => {
              // we place the first pos ready for a horiz flip i.e. on the second-to-last vert stripe
              for( y <- (0 until innerSides._2).toStream ) yield new ExtensionStep(Position(innerSides._1-2,y),newDir)
            }
          }
        else
          sShape.fillingStrategy match {
            case FillingStrategyEnum.Monoplicate => {
              for( x <- (0 until innerSides._1).toStream ; y <- (0 until innerSides._2).toStream ) yield new ExtensionStep(Position(x,y),newDir)
            }
            case FillingStrategyEnum.Duplicate => {
              // the last pos in the path can be anywhere on the flippable line
              for( x <- (0 until innerSides._1).toStream ) yield new ExtensionStep(Position(x,innerSides._2-2),newDir)
            }
            case FillingStrategyEnum.Quadruplicate => {
              // the last pos in the path must be in the x<-->y point
              val startPoint: Position = sForward.head
              Stream[ExtensionStep](new ExtensionStep(
                Position(startPoint.y,startPoint.x),
                newDir
              ))
            }
          }
      } else {
        for (newPos <- expandFromPoint(posList.last) ) yield new ExtensionStep(newPos,newDir)
      }
    }
    def lastTouch: Configuration = sShape.fillingStrategy match {
      case FillingStrategyEnum.Monoplicate => this
      case FillingStrategyEnum.Duplicate => {
        val fullOriginalPath: Seq[Position] = sForward ++ sBackward.reverse
        //
        val pathTransformers: Seq[(Seq[Position]=>Seq[Position], Position=>Position)] = Seq(
          ( (pList => pList        ), pos => Position(pos.x,                   pos.y) ),
          ( (pList => pList.reverse), pos => Position(pos.x, innerSides._2*2-1-pos.y) )
        )
        val fullPath = pathTransformers.flatMap(
          { case (reversor,poschanger) => reversor(fullOriginalPath).map(poschanger) }
        )
        //
        val dupShape: SquareStepperPuzzleShape = new SquareStepperPuzzleShape(
          (sShape.sides._1,sShape.sides._2),
          FillingStrategyEnum.Monoplicate
        )
        makeConfig(dupShape,fullPath)
      }
      case FillingStrategyEnum.Quadruplicate => {
        val fullOriginalPath: Seq[Position] = sForward ++ sBackward.reverse
        //
        val pathTransformers: Seq[(Seq[Position]=>Seq[Position], Position=>Position)] = Seq(
          ( (pList =>         pList), pos => Position(                  pos.x,                   pos.y) ),
          ( (pList => pList.reverse), pos => Position(                  pos.x, innerSides._2*2-1-pos.y) ),
          ( (pList =>         pList), pos => Position(innerSides._1*2-1-pos.x, innerSides._2*2-1-pos.y) ),
          ( (pList => pList.reverse), pos => Position(innerSides._1*2-1-pos.x,                   pos.y) )
        )
        val fullPath = pathTransformers.flatMap(
          { case (reversor,poschanger) => reversor(fullOriginalPath).map(poschanger) }
        )
        //
        val dupShape: SquareStepperPuzzleShape = new SquareStepperPuzzleShape(
          (sShape.sides._1,sShape.sides._2),
          FillingStrategyEnum.Monoplicate
        )
        makeConfig(dupShape,fullPath)
      }
    }
    def isCompleted: Boolean = 
      !sForward.isEmpty &&
      !sBackward.isEmpty &&
      // now assuming neither is empty
      (sForward.length + sBackward.length == innerSides._1*innerSides._2) && {
        val lastF: Position = sForward.last
        val lastB: Position = sBackward.last
        ( expandFromPoint(lastF) ).exists( _ == lastB )
      }
    override def toString: String = {
      val nChars: Int = s"${sShape.sides._1*sShape.sides._2+1}".length+3
      val places = Array.fill(sShape.sides._2)(Array.fill(sShape.sides._1)(" "*(nChars-1)+"."))
      for ( (pos,ind) <- ( sForward ++ sBackward.reverse ).zipWithIndex) {
        val indDesc: String = s"${ind+1}"
        places(pos.y)(pos.x)=" "*(nChars-indDesc.length) + indDesc
      }
      s"${puzzleName} <\n${ places.map( _.mkString("# ","","   #") ).mkString("\n") }\n>"
    }
  }
  class SquareStepperExtensionStep(val newPosition: Position, val direction: Direction) extends AbstractExtensionStep
}
