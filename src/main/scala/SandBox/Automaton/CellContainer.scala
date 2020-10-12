package SandBox

import com.badlogic.gdx.math.MathUtils.{floor, ceil}

class CellContainer(width: Int, height: Int) {
  val cells: Array[Cell] = Array.fill(width * height)(new Cell(Air, false))
  val indestructibleCell: Cell =
    new Cell(
      material = NuclearPasta,
      needsUpdate = false,
      mutable = false
    )

  def getCell(cellIndex: Int): Cell = {
    if (validIndex(cellIndex)) cells(cellIndex)
    else indestructibleCell
  }

  def swapCells(sourceIndex: Int, targetIndex: Int): Unit = {
    if (validIndex(sourceIndex) && validIndex(targetIndex)) {
      var sourceCell: Cell = getCell(sourceIndex)
      cells(sourceIndex) = cells(targetIndex)
      cells(targetIndex) = sourceCell
    }
  }

  def cellIndexByCardinalDir(cellIndex: Int, dir: CardinalDirection): Option[Int] = {
    cellIndexByXYOffset(cellIndex, dir.x, dir.y)
  }
  def cellIndexByXYOffset(cellIndex: Int, x: Int, y: Int): Option[Int] = {
    val targetIndex: Int = cellIndex + (-y * width) + x
    lazy val targetIndexInsideContainer: Boolean =
      ((indexToRow(cellIndex) - y) == indexToRow(targetIndex)) &&
        ((indexToColumn(cellIndex) + x) == indexToColumn(targetIndex))

    if (validIndex(targetIndex) && targetIndexInsideContainer) Some(targetIndex)
    else None
  }

  def cellByCardinalDir(cellIndex: Int, dir: CardinalDirection): Cell = {
    cellByXYOffset(cellIndex, dir.x, dir.y)
  }
  def cellByXYOffset(cellIndex: Int, x: Int, y: Int): Cell = {
    cellIndexByXYOffset(cellIndex, x, y) match {
      case Some(index) => cells(index)
      case None        => indestructibleCell
    }
  }

  def cellMoveByCardinalDir(cellIndex: Int, dir: CardinalDirection): Int = {
    cellMoveByXYOffset(cellIndex, dir.x, dir.y)
  }
  def cellMoveByXYOffset(cellIndex: Int, x: Int, y: Int): Int = {
    cellIndexByXYOffset(cellIndex, x, y) match {
      case Some(targetIndex) => {
        swapCells(cellIndex, targetIndex)
        targetIndex
      }
      case None => cellIndex
    }
    /* TODO: If x>1 and/or y>1 perform the move in multiple iterations
    swapping the adjacent cell at each iteration. */
  }

  private def validIndex(cellIndex: Int): Boolean = {
    cellIndex >= 0 && cellIndex < (width * height)
  }

  def indexToRow(cellIndex: Int): Int = {
    ceil(cellIndex / width)
  }

  def indexToColumn(cellIndex: Int): Int = {
    cellIndex % width
  }

}
