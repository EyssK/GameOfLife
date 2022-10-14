package GameOfLife


object GoL {
  case class Board(val cells: Set[Cell])
  case class Cell(val x: Int, val y: Int)

  var board = Board(Set())

  def set(l: List[(Int,Int)]) = {
    board = Board((for (c <- l) yield Cell(c._1, c._2)).toSet)
  }
  
  def update() = {
    board = getAlive(countOccur(getNeighbours(board)), board)
  }
  
  def display() = {
    val xmin = board.cells.minBy(_.x).x -2
    val xmax = board.cells.maxBy(_.x).x +2
    val ymin = board.cells.minBy(_.y).y -2
    val ymax = board.cells.maxBy(_.y).y +2

    var myMatrix = Array.fill(xmax-xmin+1,ymax-ymin+1)('.')
    for (c <- board.cells) myMatrix(c.x-xmin)(c.y-ymin) = '#'
    myMatrix foreach { row => row foreach print; println }
  }

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

  val toad = List((-1,0),(0,0),(1,0),(-2,-1),(-1,-1),(0,-1))
  val blinker = List((-1,0),(0,0),(1,0))
  val blinkerOff = List((-10,20),(-11,20),(-12,20))
  val glider = List((-2,0),(-1,0),(0,0),(0,1),(-1,2))
  val gosperGliderGun = List((5,1),(5,2),(6,1),(6,2),(5,11),(6,11),(7,11),(4,12),(3,13),(3,14),(8,12),(9,13),(9,14),(6,15),(4,16),(5,17),(6,17),(7,17),(6,18),(8,16),(3,21),(4,21),(5,21),(3,22),(4,22),(5,22),(2,23),(6,23),(1,25),(2,25),(6,25),(7,25),(3,35),(4,35),(3,36),(4,36))

  GoL.set(glider)
  GoL.set(gosperGliderGun)
  for (i <- 1 to 40) {
    GoL.display()
    println(i)
    GoL.update()
  }
}

