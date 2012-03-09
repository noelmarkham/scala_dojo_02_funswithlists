package dojo

import collection.immutable.List

object FunsWithLists {

  def labels(ls: List[Game]) = ls map (x => x.label)

  def averageRatingsOf(l:String,  ls:List[Game]) = {
    val filteredList: List[Game] = ls filter (x => x.label == l)
    (filteredList.foldLeft(0)((m: Int, n: Game) => m + n.rating)) / filteredList.size
  }

  def totalRatingsOf(ls: List[Game]): Int = ls match {
    case Nil => 0
    case x :: xs => x.rating + totalRatingsOf(xs)
  }

  def totalRatingsOfLabel(label: String, list: List[Game]):Int = {
    var total = 0;
    for(game <- list) {
      if(game.label == label) {
        total = total + game.rating
      }
    }

    total
  }

  def increaseRatingBy(inc: Int, ls: List[Game]) = ls map (x => Game(x.label, x.rating + inc))

  def decreaseRatingBy(i: Int, s: String, list: List[Game]): List[Game] = list match {
    case Nil => Nil
    case x :: xs if (x.label == s) => Game(s, x.rating - i) :: decreaseRatingBy(i, s, xs)
    case x :: xs => x :: decreaseRatingBy(i, s, xs)
  }

  def createFunctionToFindGamesByLabel(label: String):(List[Game]) => List[Game] = _ filter(x => x.label == label)

  def zipWithKey = (f: (Game) => String, ls: List[Game]) => labels(ls) map (x => x.toUpperCase) zip ls

}
