package sandbox.engine

import com.badlogic.gdx.{Gdx}
import com.badlogic.gdx.utils.viewport.{Viewport}
import com.badlogic.gdx.graphics.{Pixmap, Texture, Color}
import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureRegion, Sprite}
import com.badlogic.gdx.scenes.scene2d.{Stage, InputEvent, InputListener, Actor}
import com.badlogic.gdx.scenes.scene2d.ui.{Image, Table, ImageButton, ButtonGroup, TextButton}
import com.badlogic.gdx.scenes.scene2d.utils.{SpriteDrawable, TextureRegionDrawable, ClickListener}

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

  /*== Material Select Buttons Start ==*/
  case class MatSelectButton(val imageButton: ImageButton, val matIDToSpawn: Int)

  private lazy val matSelectButtons: Seq[MatSelectButton] = Seq(
    createMatSelectButton("assets/water.png", 0x198ae6ff, 4),
    createMatSelectButton("assets/sand.png", 0xe6b619ff, 3),
    createMatSelectButton("assets/air.png", 0xfdf5ffff, 2),
    createMatSelectButton("assets/oil.png", 0xbd19e6ff, 5),
    createMatSelectButton("assets/fire.png", 0xe62419ff, 6),
    createMatSelectButton("assets/lava.png", 0xe64619ff, 10)
  )

  private lazy val matSelectButtonGroup: ButtonGroup[ImageButton] = new ButtonGroup[ImageButton]()

  private def createMatSelectButton(
      buttonImagePath: String,
      baseColorNum: Int,
      matIDToSpawn: Int
  ): MatSelectButton = {
    def baseColorToUncheckedColor(color: Color): Color = new Color(color.r, color.g, color.b, 0.2f)
    def baseColorToClickedColor(color: Color): Color = new Color(color.r, color.g, color.b, 0.5f)
    def baseColorToCheckedColor(color: Color): Color = color

    val baseColor = new Color(baseColorNum)
    val buttonTexture = new Texture(Gdx.files.internal(buttonImagePath))
    val buttonSpriteDrawable = new SpriteDrawable(new Sprite(buttonTexture))
    val imageButton = new ImageButton(
      buttonSpriteDrawable.tint(baseColorToUncheckedColor(baseColor)),
      buttonSpriteDrawable.tint(baseColorToClickedColor(baseColor)),
      buttonSpriteDrawable.tint(baseColorToCheckedColor(baseColor))
    )
    MatSelectButton(imageButton, matIDToSpawn)
  }
  /*== Material Select Buttons End ==*/

  private lazy val footer = new Image(
    new TextureRegionDrawable(new Texture(Gdx.files.internal("assets/footer.png")))
  )

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

    matSelectButtons.foreach(button => {
      button.imageButton.addListener(new ClickListener() {
        override def clicked(event: InputEvent, x: Float, y: Float): Unit =
          selectedMaterialID = button.matIDToSpawn
      })
    })

    masterHandler.addProcessor(stage)
  }

  def init(): Unit = {
    // Add material select buttons into a button group
    matSelectButtons.foreach(button => matSelectButtonGroup.add(button.imageButton))

    // Default material
    matSelectButtons(0).imageButton.setChecked(true)

    // Config material buttons group
    matSelectButtonGroup.setMaxCheckCount(1)
    matSelectButtonGroup.setMinCheckCount(1)
    matSelectButtonGroup.setUncheckLast(true)

    // Add elements to the stage
    rootTable.top()
    rootTable.add(cellAreaImage).width(cellArea.width).height(cellArea.height)
    rootTable.row()
    rootTable.add(uiTable).left()
    rootTable.row()
    rootTable.add(footer)
    uiTable.left()
    matSelectButtons.foreach(button => uiTable.add(button.imageButton))

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