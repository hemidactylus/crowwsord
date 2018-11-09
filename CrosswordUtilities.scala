/*
  CrosswordUtilities.scala :
    miscellaneous utilities used in the puzzle-environment
    of the cross words
*/

package net.salamandrina.crowwsord

object CrosswordUtilities {
  
  def makeCounter[T](l: List[T]): Map[T,Int] = {
      val cnt: scala.collection.mutable.Map[T,Int] = scala.collection.mutable.Map.empty
      for ( itm <- l) {
          if (cnt contains itm) cnt(itm)=cnt(itm)+1 else cnt(itm)=1
      }
      cnt.toMap
  }

}
