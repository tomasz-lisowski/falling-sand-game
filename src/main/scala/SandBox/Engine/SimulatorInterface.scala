package sandbox.engine

import sandbox.simulation.Simulator
import sandbox.simulation.Material
import sandbox.simulation.Cell
import com.badlogic.gdx.math.MathUtils

class SimulatorInterface(simWidth: Int, simHeight: Int) extends EngineSystem {
  lazy val simulator: Simulator = new Simulator(simWidth, simHeight)

  def init(): Unit = {
    simulator
  }

  def placeMaterial(cellIndex: Int, matID: Int): Unit = {
    val x: Int = cellIndex % simWidth
    val y: Int = MathUtils.ceil(cellIndex / simHeight)
    val mat: Material = Material.all(matID)
    var cell: Cell = simulator.grid.get(x, y)
    cell.changeMaterial(mat)
  }

  def getMaterialColor(cellIndex: Int): Int = {
    val x: Int = cellIndex % simWidth
    val y: Int = MathUtils.ceil(cellIndex / simHeight)
    var cell: Cell = simulator.grid.get(x, y)
    cell.mat.color + cell.dataA
  }

  def step(): Unit = {
    simulator.step()
  }

}
