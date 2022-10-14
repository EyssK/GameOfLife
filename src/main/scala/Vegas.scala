package MyVegas

import vegas._
import GameOfLife.GoL

object Main extends App{
  val toad = List((-1,0),(0,0),(1,0),(-2,-1),(-1,-1),(0,-1))
  GoL.set(toad)

  val xy = for (cell <- GoL.board.cells) yield (cell.x,cell.y)
  val testxy = List((1,2),(3,-4),(9,3))
  val plot = Vegas("GoL").
    withXY(
      xy.toList
      //testxy
    ).
    encodeX("x", Quant).
    encodeY("y", Quant).
    mark(Point).
    encodeDetail("Origin")
  
  plot.show


/*
  val SortColorPlot =
    Vegas("The Trellis display by Becker et al. helped establish small multiples as a “powerful mechanism for understanding interactions in studies of how a response depends on explanatory variables”. Here we reproduce a trellis of Barley yields from the 1930s, complete with main-effects ordering to facilitate comparison.").
      withURL("https://vega.github.io/vega-editor/app/data/barley.json").
      mark(Point).
      encodeRow("site", Ordinal).
      encodeX("yield", Quantitative, aggregate=AggOps.Mean).
      encodeY("variety", Ordinal, sortField=Sort("yield", AggOps.Mean), scale=Scale(bandSize = 12.0)).
      encodeColor(field="year", dataType=Nominal)

  SortColorPlot.show
*/

}
