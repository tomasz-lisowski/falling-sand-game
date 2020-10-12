package SandBox

import scala.util.Random
import scala.collection.immutable._
import com.badlogic.gdx.math.MathUtils

class CellAutomaton(val width: Int, val height: Int) {
  private val cells: CellContainer = new CellContainer(width, height)
  /* Cells need to be updated in random order to avoid un-natural behaviour */
  private val cellIndices: Seq[Int] = Random.shuffle(Range(0, width * height - 1, 1))

  def placeMaterial(cellIndex: Int, matToPlace: Material): Unit = {
    val cell: Cell = cells.getCell(cellIndex)
    if (cell.getMutable) {
      cell.reset()
      cell.update(newMaterial = matToPlace)
    }
  }

  def getMaterial(cellIndex: Int): Material =
    cells.getCell(cellIndex).getMaterial

  def step(): Unit = {
    flagAllCellsForUpdate();
    simulateStep();
  }

  private def flagAllCellsForUpdate(): Unit = {
    cellIndices.foreach(cellIndex => {
      val cell: Cell = cells.getCell(cellIndex)
      cell.update(newNeedsUpdate = true)
    })
  }

  private def simulateStep(): Unit = {
    cellIndices.foreach(cellIndex => {
      val cell: Cell = cells.getCell(cellIndex)
      if (cell.getNeedsUpdate) cell.update(newNeedsUpdate = false)
      simulateCell(cellIndex)
    })
  }

  private def simulateCell(cellIndex: Int): Unit = {
    val cell: Cell = cells.getCell(cellIndex)
    val material: Material = cell.getMaterial

    def canDisplace(dir: CardinalDirection): Boolean = {
      val targetCell: Cell = cells.cellByCardinalDir(cellIndex, dir)
      val targetIsSolid: Boolean = targetCell.getMaterial.state == Solid
      !targetIsSolid
    }

    material.state match {
      case Gas => {
        moveCell(
          cellIndex = cellIndex,
          //
          canMoveNorth = canDisplace(North),
          canMoveNorthEast = canDisplace(NorthEast),
          canMoveNorthWest = canDisplace(NorthWest),
          //
          canMoveEast = canDisplace(East),
          canMoveWest = canDisplace(West)
        )
      }
      case Liquid => {
        moveCell(
          cellIndex = cellIndex,
          //
          canMoveSouth = canDisplace(South),
          canMoveSouthEast = canDisplace(SouthEast),
          canMoveSouthWest = canDisplace(SouthWest),
          //
          canMoveEast = canDisplace(East),
          canMoveWest = canDisplace(West)
        )
      }
      case Solid => {
        moveCell(
          cellIndex = cellIndex,
          //
          canMoveSouth = canDisplace(South),
          canMoveSouthEast = canDisplace(SouthEast),
          canMoveSouthWest = canDisplace(SouthWest)
        )
      }
    }
  }

  // Returns the cell index to which the cell moved to
  private def moveCell(
      cellIndex: Int,
      //
      canMoveNorth: Boolean = false,
      canMoveNorthEast: Boolean = false,
      canMoveNorthWest: Boolean = false,
      //
      canMoveSouth: Boolean = false,
      canMoveSouthEast: Boolean = false,
      canMoveSouthWest: Boolean = false,
      //
      canMoveEast: Boolean = false,
      canMoveWest: Boolean = false
  ): Int = {
    val rand: Int = MathUtils.random(0, 1)
    val cell: Cell = cells.getCell(cellIndex)

    val canMoveSouthEastAndWest: Boolean = canMoveSouthEast && canMoveSouthWest
    val canMoveNorthEastAndWest: Boolean = canMoveNorthEast && canMoveNorthWest
    val canMoveEastAndWest: Boolean = canMoveEast && canMoveWest

    val selectedDir: CardinalDirection =
      if (canMoveSouth) South
      //
      else if (canMoveSouthEastAndWest && rand == 0) SouthEast
      else if (canMoveSouthEastAndWest && rand == 1) SouthWest
      else if (canMoveSouthEast) SouthEast
      else if (canMoveSouthWest) SouthWest
      //
      else if (canMoveNorth) North
      //
      else if (canMoveNorthEastAndWest && rand == 0) NorthEast
      else if (canMoveNorthEastAndWest && rand == 1) NorthWest
      else if (canMoveNorthEast) NorthEast
      else if (canMoveNorthWest) NorthWest
      //
      else if (canMoveEastAndWest && rand == 0) East
      else if (canMoveEastAndWest && rand == 1) West
      else if (canMoveEast) East
      else if (canMoveWest) West
      //
      else Invalid

    cells.cellMoveByCardinalDir(cellIndex, selectedDir)
  }
}
