package GameOfLife

object GoL {
  case class Board(val cells: Set[Cell])
  case class Cell(val x: Int, val y: Int)

  var board = Board(Set())

  def apply(l: List[(Int,Int)]) = {
    board = Board((for (c <- l) yield Cell(c._1, c._2)).toSet)
  }
  
  def update () = {
    board = getAlive(countOccur(getNeighbours(board)), board)
  }
  
  def display (b: Board) = ???

  def neighbours (c: Cell) : List[Cell] = c match {
    case Cell(x,y) => List(
        Cell(x-1,y-1),
        Cell(x-1,y  ),
        Cell(x-1,y+1),
        Cell(x  ,y-1),
        Cell(x  ,y+1),
        Cell(x+1,y-1),
        Cell(x+1,y  ),
        Cell(x+1,y+1))
    }

  def getNeighbours(b: Board) : List[Cell] =
    b.cells.toList.map(neighbours(_)).flatten

  def countOccur (l: List[Cell]) : Map[Cell,Int] =
    l.groupBy(identity).mapValues(_.size)
  

  def isAlive(c: Cell, n: Int, prev: Board) : Boolean = {
    if (n==3) true
    else if (n==2 && prev.cells.contains(c)) true
    else false
  }

  def getAlive (l: Map[Cell,Int], prev: Board) : Board =
    Board( l.filter(x => isAlive(x._1,x._2,prev)).keySet)
}



object Main extends App {
  GoL(List((-1,0),(0,0),(1,0)))
  println(GoL.board)
  GoL.update()
  println(GoL.board)
}



