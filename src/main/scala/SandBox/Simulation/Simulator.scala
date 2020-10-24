package sandbox.simulation

import scala.util.Random
import com.badlogic.gdx.math.MathUtils

class Simulator(width: Int, height: Int) {
  val grid: Grid = new Grid(width, height)
  // Iterating over cells in random order helps to avoid un-natural behavior
  val randCellIterators =
    Seq(getRandomCellIterator(), getRandomCellIterator(), getRandomCellIterator())

  def step(): Unit = {
    val cellIteratorNum: Int = MathUtils.random(0, 2)

    randCellIterators(cellIteratorNum).foreach({
      case ((x, y), _) => grid.get(x, y).updated = 0
    })
    randCellIterators(cellIteratorNum).foreach({
      case ((x, y), (getFunc, setFunc)) =>
        if (grid.get(x, y).updated == 0) {
          grid.get(x, y).updated += 1
          grid.get(x, y).update(getFunc, setFunc)
        }
    })
  }

  private def get(x: Int, y: Int, grid: Grid)(dir: CardinalDir): Cell = {
    grid.get(x - dir.x, y - dir.y)
  }
  private def set(x: Int, y: Int, grid: Grid)(dir: CardinalDir, cell: Cell): Unit = {
    grid.set(x - dir.x, y - dir.y, cell)
  }

  private def getRandomCellIterator() = {
    Random.shuffle(
      for (y <- 0 until height; x <- 0 until width)
        yield ((x, y), (get(x, y, grid)(_), set(x, y, grid)(_, _)))
    )
  }
}
