package sandbox.engine

import scala.collection.mutable

import com.badlogic.gdx.{Gdx}
import com.badlogic.gdx.math.{Bresenham2, GridPoint2}
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
  var selectedMaterialID: Int = 3
  // Mouse pos inside cell area
  private var mousePrevX: Int = 0
  private var mousePrevY: Int = 0
  // All points between current and previous mouse pos
  var mouseX: mutable.Queue[Int] = mutable.Queue[Int](0)
  var mouseY: mutable.Queue[Int] = mutable.Queue[Int](0)
  // Is left mouse button down
  var mouseActive: Boolean = false

  lazy val stage: Stage = new Stage(viewport, batch)

  // Setup the cell area texture (allows postprocessing and rendering in 1 draw call)
  // This pixmap will be updated from outside of the Scene (Scene only handles its rendering)
  lazy val cellAreaPixmap: Pixmap = new Pixmap(
    cellArea.swidth,
    cellArea.sheight,
    Pixmap.Format.RGBA8888
  )
  private lazy val cellAreaTexture: Texture = new Texture(cellAreaPixmap)
  private lazy val cellAreaTextureRegion: TextureRegion = new TextureRegion(cellAreaTexture)
  private lazy val cellAreaImage: Image = new Image(cellAreaTextureRegion)

  private lazy val rootTable: Table = new Table()
  private lazy val uiTable: Table = new Table()

  /*== Material Select Buttons Start ==*/
  case class MatSelectButton(val imageButton: ImageButton, val matIDToSpawn: Int)

  // Group buttons such that only 1 can be active at any time (gets filled at init)
  private lazy val matSelectButtonGroup: ButtonGroup[ImageButton] = new ButtonGroup[ImageButton]()
  private lazy val matSelectButtons: Seq[MatSelectButton] = Seq(
    createMatSelectButton("assets/water.png", 0x198ae6ff, 4),
    createMatSelectButton("assets/sand.png", 0xe6b619ff, 3),
    createMatSelectButton("assets/air.png", 0xfdf5ffff, 2),
    createMatSelectButton("assets/oil.png", 0xbd19e6ff, 5),
    createMatSelectButton("assets/fire.png", 0xe62419ff, 6),
    createMatSelectButton("assets/lava.png", 0xe64619ff, 10),
    createMatSelectButton("assets/seed.png", 0x19e631ff, 12),
    createMatSelectButton("assets/wood.png", 0xe68619ff, 13),
    createMatSelectButton("assets/nuclear_pasta.png", 0xe6194cff, 0),
    createMatSelectButton("assets/smoke.png", 0xccccccff, 7),
    createMatSelectButton("assets/stone.png", 0x9c9d97ff, 1),
    createMatSelectButton("assets/vapor.png", 0xffffffff, 11),
    createMatSelectButton("assets/soot.png", 0x707070ff, 9),
    createMatSelectButton("assets/copper.png", 0xe66119ff, 14),
    createMatSelectButton("assets/copper_oxide.png", 0x19e2e6ff, 15),
    createMatSelectButton("assets/hcl_acid.png", 0xcee619ff, 16)
  )

  private def createMatSelectButton(
      buttonImagePath: String,
      baseColorNum: Int,
      matIDToSpawn: Int
  ): MatSelectButton = {
    def baseColorToUncheckedColor(color: Color): Color = new Color(color.r, color.g, color.b, 0.4f)
    def baseColorToClickedColor(color: Color): Color = new Color(color.r, color.g, color.b, 0.6f)
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

    // Set default material
    matSelectButtons(3).imageButton.setChecked(true)

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
    // Add material select buttons in rows of 8
    matSelectButtons
      .grouped(8)
      .foreach(row => {
        uiTable.row()
        row.foreach(button => uiTable.add(button.imageButton))
      })

    stage.addActor(rootTable)
    rootTable.setFillParent(true)
  }

  def update(): Unit = {
    // Mouse did not move so reuse last position
    if (mouseX.isEmpty) mouseX.enqueue(mousePrevX)
    if (mouseY.isEmpty) mouseY.enqueue(mousePrevY)

    // Update cell area texture
    cellAreaTexture.draw(cellAreaPixmap, 0, 0)
  }

  // Helper object to get a line of points between 2 points
  private lazy val bresenham: Bresenham2 = new Bresenham2()
  private def updateMouse(
      newActive: Boolean = mouseActive,
      newX: Int = mousePrevX,
      newY: Int = mousePrevY
  ): Unit = {
    mouseActive = newActive
    val validX: Boolean = newX >= 0 || newX < cellArea.width
    val validY: Boolean = newY >= 0 || newY < cellArea.height

    if (validX && validY) {
      val interpMousePos: com.badlogic.gdx.utils.Array[GridPoint2] =
        bresenham.line(mousePrevX, mousePrevY, newX, newY)
      val interpMousePosIter: com.badlogic.gdx.utils.Array.ArrayIterator[GridPoint2] =
        interpMousePos.iterator

      mouseX.enqueue(mousePrevX)
      mouseY.enqueue(mousePrevY)
      mousePrevX = newX
      mousePrevY = newY

      // Save all intermediate points (between current and prev frame mouse pos)
      while (interpMousePosIter.hasNext) {
        val e = interpMousePosIter.next()
        mouseX.enqueue(e.x)
        mouseY.enqueue(e.y)
      }
    }
  }
}
