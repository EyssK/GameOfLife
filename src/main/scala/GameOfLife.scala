object GameOfLife {

  class Board(cells: List[Cell]) {

    def append(c: Cell) = {
      this.cells :+ c
    }

    def update: Board = {
      val bNext: Board = new Board(Nil)
      val mTemp = Map[(Int,Int),Int]()
      for (c <- this.cells) {
        print(mTemp.get((c.x,c.y)))
      }
      return bNext
    }

    def display = ???


  }

  class Cell(x: Int, y: Int) {

  }

}

object Main extends App {
  println("Hello")

}



