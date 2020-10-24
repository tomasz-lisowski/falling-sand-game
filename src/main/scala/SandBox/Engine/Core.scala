package sandbox.engine

import scala.util.Random
import scala.collection.mutable

import com.badlogic.gdx.{Gdx, Game}
import com.badlogic.gdx.math.MathUtils.{ceil, floor}
import com.badlogic.gdx.scenes.scene2d.{Stage}
import com.badlogic.gdx.graphics.{Pixmap}

class Engine(
    cellArea: Rect,
    uiArea: Rect
) extends Game {
  private lazy val renderer: Renderer = new Renderer(cellArea, uiArea)
  private lazy val scene: Scene = new Scene(renderer.viewport, renderer.batch, cellArea, uiArea)
  private lazy val inputHandler: InputHandler = new InputHandler
  private lazy val simulator: SimulatorInterface =
    new SimulatorInterface(cellArea.swidth, cellArea.sheight)

  override def create(): Unit = {
    renderer.init()
    scene.init()
    scene.registerInputHandlers(inputHandler)
    Gdx.input.setInputProcessor(inputHandler)
  }

  override def render(): Unit = {
    // Show FPS in title bar
    Gdx.graphics.setTitle(f"SandBox @ ${Gdx.graphics.getFramesPerSecond()}")

    placeMaterial(scene.mouseActive, scene.mouseX, scene.mouseY, scene.selectedMaterialID)
    simulator.step()
    updateCellAreaPixmap(scene.cellAreaPixmap)
    scene.update()
    renderer.render(scene.stage)
  }

  private val cellIndices: Range = Range(0, cellArea.swidth * cellArea.sheight, 1)
  private def updateCellAreaPixmap(pixmap: Pixmap): Unit = {
    pixmap.setBlending(Pixmap.Blending.None)
    cellIndices.foreach(cellIndex => {
      val cellX: Int = cellIndex % cellArea.swidth
      val cellY: Int = ceil(cellIndex / cellArea.sheight)
      val color: Int = simulator.getMaterialColor(cellIndex)
      pixmap.drawPixel(cellX, cellY, color)
    })
  }

  private def placeMaterial(
      shouldPlaceMaterial: Boolean,
      mouseX: mutable.Queue[Int],
      mouseY: mutable.Queue[Int],
      matID: Int
  ): Unit = {
    while (!mouseX.isEmpty && !mouseY.isEmpty) {
      val x: Int = mouseX.dequeue()
      val y: Int = mouseY.dequeue()

      val xInCellArea: Int = floor(x / cellArea.scale)
      val yInCellArea: Int = (cellArea.sheight - 1) - floor(y / cellArea.scale)

      if (shouldPlaceMaterial) {
        val targetCellIndex: Int = (yInCellArea * cellArea.swidth) + xInCellArea
        simulator.placeMaterial(targetCellIndex, matID)
      }
    }
  }
}
