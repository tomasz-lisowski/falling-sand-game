package sandbox.engine

import com.badlogic.gdx.{Gdx}
import com.badlogic.gdx.utils.viewport.{Viewport}
import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureRegion}
import com.badlogic.gdx.scenes.scene2d.{Stage, InputEvent, InputListener, Actor}
import com.badlogic.gdx.scenes.scene2d.ui.{Image, Table, ImageButton, ButtonGroup, TextButton}
import com.badlogic.gdx.scenes.scene2d.utils.{TextureRegionDrawable, ClickListener}

class Scene(
    viewport: Viewport,
    batch: SpriteBatch,
    cellArea: Rect,
    uiArea: Rect
) extends EngineSystem {
  // Which material was selected using the UI
  var selectedMaterialID: Int = 4
  // Mouse pos inside cell area
  var mouseX: Int = 0
  var mouseY: Int = 0
  var mouseActive: Boolean = false

  lazy val stage: Stage = new Stage(viewport, batch)

  // Setup the cell area texture (allows postprocessing and rendering in 1 draw call)
  // This pixmap will be updated from outside of the renderer (renderer only handles its rendering)
  lazy val cellAreaPixmap: Pixmap = new Pixmap(
    cellArea.swidth,
    cellArea.sheight,
    Pixmap.Format.RGBA8888
  )

  private lazy val cellAreaTexture: Texture = new Texture(cellAreaPixmap)
  private lazy val cellAreaTextureRegion: TextureRegion = new TextureRegion(cellAreaTexture)

  private lazy val rootTable: Table = new Table()
  private lazy val uiTable: Table = new Table()

  private lazy val cellAreaImage: Image = new Image(cellAreaTextureRegion)
  private lazy val buttonWater =
    new ImageButton(
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/water_unchecked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/water_clicked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/water_checked.png")))
    )
  private lazy val buttonSand =
    new ImageButton(
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/sand_unchecked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/sand_clicked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/sand_checked.png")))
    )
  private lazy val buttonAir =
    new ImageButton(
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/air_unchecked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/air_clicked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/air_checked.png")))
    )
  private lazy val buttonOil =
    new ImageButton(
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/oil_unchecked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/oil_clicked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/oil_checked.png")))
    )
  private lazy val buttonFire =
    new ImageButton(
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/fire_unchecked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/fire_clicked.png"))),
      new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/fire_checked.png")))
    )

  private lazy val footer = new Image(
    new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/footer.png")))
  )

  private lazy val materialButtons: ButtonGroup[ImageButton] =
    new ButtonGroup[ImageButton](buttonWater, buttonSand, buttonAir, buttonOil, buttonFire)

  def registerInputHandlers(masterHandler: InputHandler): Unit = {
    cellAreaImage.addListener(new InputListener() {
      override def mouseMoved(
          event: InputEvent,
          x: Float,
          y: Float
      ): Boolean = {
        updateMouse(newX = x.toInt, newY = y.toInt)
        true
      }
      override def touchDown(
          event: InputEvent,
          x: Float,
          y: Float,
          pointer: Int,
          button: Int
      ): Boolean = {
        updateMouse(newActive = true)
        true
      }
      override def touchUp(
          event: InputEvent,
          x: Float,
          y: Float,
          pointer: Int,
          button: Int
      ): Unit = {
        updateMouse(newActive = false)
      }
      override def touchDragged(
          event: InputEvent,
          x: Float,
          y: Float,
          pointer: Int
      ): Unit = {
        updateMouse(newX = x.toInt, newY = y.toInt)
      }
    })

    buttonWater.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit =
        selectedMaterialID = 4
    })
    buttonSand.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit =
        selectedMaterialID = 3
    })
    buttonAir.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit =
        selectedMaterialID = 2
    })
    buttonOil.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit =
        selectedMaterialID = 5
    })
    buttonFire.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit =
        selectedMaterialID = 6
    })

    masterHandler.addProcessor(stage)
  }

  def init(): Unit = {
    // Default material
    buttonWater.setChecked(true)

    // Config material buttons group
    materialButtons.setMaxCheckCount(1)
    materialButtons.setMinCheckCount(1)
    materialButtons.setUncheckLast(true)

    // Add elements to the stage
    rootTable.top()
    rootTable.add(cellAreaImage).width(cellArea.width).height(cellArea.height)
    rootTable.row()
    rootTable.add(uiTable).left()
    rootTable.row()
    rootTable.add(footer)
    uiTable.left()
    uiTable.add(buttonWater)
    uiTable.add(buttonSand)
    uiTable.add(buttonAir)
    uiTable.add(buttonOil)
    uiTable.add(buttonFire)

    stage.addActor(rootTable)
    rootTable.setFillParent(true)
  }

  def update(): Unit = {
    // Update cell area texture
    cellAreaTexture.draw(cellAreaPixmap, 0, 0)
  }

  private def updateMouse(
      newActive: Boolean = mouseActive,
      newX: Int = mouseX,
      newY: Int = mouseY
  ): Unit = {
    mouseActive = newActive
    val validX: Boolean = newX >= 0 || newX < cellArea.width
    val validY: Boolean = newY >= 0 || newY < cellArea.height
    if (validX) mouseX = newX
    if (validY) mouseY = newY
  }
}
