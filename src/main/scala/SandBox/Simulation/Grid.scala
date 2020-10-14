package sandbox.simulation

import scala.collection.immutable

class Grid(val width: Int, val height: Int) {
  val container: Array[Array[Cell]] = Array.ofDim[Cell](width, height)
  for (y <- 0 until height; x <- 0 until width) {
    container(y)(x) = new Cell(Air)
  }
  val defaultElement: Cell = new Cell(NuclearPasta)

  def get(x: Int, y: Int): Cell = {
    if (validPos(x, y)) container(y)(x)
    else defaultElement
  }

  def set(x: Int, y: Int, cell: Cell): Unit = {
    if (validPos(x, y)) container(y)(x) = cell
  }

  def validPos(x: Int, y: Int): Boolean = {
    x >= 0 && x < width && y >= 0 && y < height
  }
}
