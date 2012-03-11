package dojo

import collection.immutable.List

object FunsWithLists {

  def labels(ls: List[Game]) = ls map (x => x.label)

  def averageRatingsOf(l:String,  ls:List[Game]) = {

    def functionOverListSum(f: Int => Int, sum: Int, l:List[Game]): Int = l match {
      case Nil => f(sum)
      case x :: xs => functionOverListSum(f, sum + x.rating, xs)
    }

    val filteredList = ls filter (x => x.label == l)
    functionOverListSum(x => x / filteredList.size, 0, filteredList)
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

  def zipWithKey = (f: (Game) => String, ls: List[Game]) => ls map (x => f(x)) zip ls

  def firstGameWithRating(rating: Int):(List[Game] => Game) = {
    def func(list: List[Game]): Game = list match {
      case Nil => null
      case x :: xs if (x.rating == rating) => x
      case x :: xs => func(xs)
    }

    func
  }
}
