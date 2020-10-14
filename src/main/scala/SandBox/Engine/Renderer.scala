package sandbox.engine

import com.badlogic.gdx.{Gdx}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.utils.viewport.{ExtendViewport}
import com.badlogic.gdx.graphics.g2d.{SpriteBatch}
import com.badlogic.gdx.scenes.scene2d.{Stage}

class Renderer(
    cellArea: Rect,
    uiArea: Rect
) extends EngineSystem {
  lazy val batch: SpriteBatch = new SpriteBatch

  lazy val viewport =
    new ExtendViewport(
      Gdx.graphics.getWidth(),
      Gdx.graphics.getHeight(),
      new OrthographicCamera(Gdx.graphics.getWidth(), Gdx.graphics.getHeight())
    )

  def init(): Unit = {}

  def render(stage: Stage): Unit = {
    // Clear screen
    Gdx.gl.glClearColor(0.0f, 0.0f, 0.0f, 1.0f)
    Gdx.gl20.glClear(GL20.GL_COLOR_BUFFER_BIT)

    stage.act()
    stage.draw()
  }
}
