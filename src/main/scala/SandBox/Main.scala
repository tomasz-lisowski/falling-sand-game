package SandBox

import com.badlogic.gdx.graphics.{Color}
import com.badlogic.gdx.Files.{FileType}
import com.badlogic.gdx.backends.lwjgl.{LwjglApplication, LwjglApplicationConfiguration}

object Main extends App {
  val config = new LwjglApplicationConfiguration
  config.title = "Sand Box"
  config.foregroundFPS = 80
  config.width = 512
  config.height = 512 + 64 + 32
  config.forceExit = false
  config.pauseWhenMinimized = true
  config.resizable = false
  config.initialBackgroundColor = Color.BLACK
  // config.undecorated = true
  config.vSyncEnabled = false
  config.useGL30 = true
  config.addIcon("icon.bmp", FileType.Internal)

  val cellAreaScale: Float = 4f
  val cellArea: Rect = new Rect(
    0,
    config.height - config.width,
    config.width,
    config.width,
    cellAreaScale
  )
  val uiArea: Rect = new Rect(
    0,
    0,
    config.width,
    config.height - config.width,
    1f
  )

  new LwjglApplication(
    new Engine(cellArea, uiArea),
    config
  )
}
