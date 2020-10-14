package sandbox.simulation
import com.badlogic.gdx.math.MathUtils
import scala.util.Random

class Cell(
    var mat: Material = Air,
    var updated: Int = 0,
    var dataA: Int = Cell.randAlpha(),
    var dataB: Int = -1
) {
  def update(
      get: CardinalDir => Cell,
      set: (CardinalDir, Cell) => Unit
  ): Unit = {
    updated += 1
    val canDisplaceNorth: Boolean = mat.canDisplace(get(North).mat, North)
    val canDisplaceNorthEast: Boolean = mat.canDisplace(get(NorthEast).mat, NorthEast)
    val canDisplaceNorthWest: Boolean = mat.canDisplace(get(NorthWest).mat, NorthWest)
    val canDisplaceSouth: Boolean = mat.canDisplace(get(South).mat, South)
    val canDisplaceSouthEast: Boolean = mat.canDisplace(get(SouthEast).mat, SouthEast)
    val canDisplaceSouthWest: Boolean = mat.canDisplace(get(SouthWest).mat, SouthWest)
    val canDisplaceEast: Boolean = mat.canDisplace(get(East).mat, East)
    val canDisplaceWest: Boolean = mat.canDisplace(get(West).mat, West)

    val randSouthLatitude: CardinalDir =
      if (MathUtils.random(0, 1) == 0) SouthEast else SouthWest
    val randNorthLatitude: CardinalDir =
      if (MathUtils.random(0, 1) == 0) NorthEast else NorthWest
    val randLatitude: CardinalDir =
      if (MathUtils.random(0, 1) == 0) East else West

    mat match {
      case NuclearPasta => ()
      case Stone        => update_stone()
      case Air          => update_air()
      case Sand         => update_sand()
      case Water        => update_water()
      case Oil          => update_oil()
      case Fire         => update_fire()
      case Smoke        => update_smoke()
      case BurningOil   => update_oil_burning()
      case SmokeSoot    => update_smoke_soot()
      case Lava         => update_lava()
      case WaterVapor   => update_water_vapor()
      case Seed         => update_seed()
      case Wood         => update_wood()
    }

    def update_stone(): Unit = {
      val moveDir: CardinalDir = move_dest_solid()
      move(moveDir)
    }

    def update_air(): Unit = {
      if (dataA != 255) dataA = 255 // Uniform color
      val moveDir: CardinalDir = move_dest_gas()
      move(moveDir)
    }

    def update_sand(): Unit = {
      val moveDir: CardinalDir = move_dest_granular_solid()
      move(moveDir)
    }

    def update_water(): Unit = {
      // Sawtooth color lightness
      if (dataA > 200) dataA -= 1
      else dataA = 255

      val lavaInNeighborhood = matIsInNeighborhood(Lava)
      val stoneInNeighborhood = matIsInNeighborhood(Stone)
      if (lavaInNeighborhood._1) {
        set(Center, new Cell(WaterVapor))
        set(lavaInNeighborhood._2, new Cell(Stone))
      } else if (stoneInNeighborhood._1) {
        val chanceToCorrodeSand = 0.001f
        if (MathUtils.random(0f, 1f) <= chanceToCorrodeSand) {
          set(stoneInNeighborhood._2, new Cell(Sand))
        }
      } else {
        val moveDir: CardinalDir = move_dest_liquid()
        move(moveDir)
      }
    }

    def update_oil(): Unit = {
      // DataB = Lightness increasing (1) or decreasing (0)
      // Pulse color lightness
      if (dataA == 255) dataB = 0
      else if (dataA == 200) dataB = 1
      if (dataB == 0) dataA -= 1
      else if (dataB == 1) dataA += 1

      val fireInNeighborhood = matIsInNeighborhood(Fire)
      val burningOilInNeighborhood = matIsInNeighborhood(BurningOil)
      val lavaInNeighborhood = matIsInNeighborhood(Lava)
      if (fireInNeighborhood._1 || burningOilInNeighborhood._1 || lavaInNeighborhood._1) {
        set(Center, new Cell(BurningOil))
      } else {
        val moveDir: CardinalDir = move_dest_liquid()
        move(moveDir)
      }
    }

    def update_fire(): Unit = {
      // DataB = lifetime remaining
      if (dataB == -1) dataB = 16
      else if (dataB > 0) dataB -= 1
      if (matIsInNeighborhood(Water)._1) dataB = 0 // Water extinguishes fire

      if (dataB > 0) {
        if (dataB < 5 && mat.chanceEmit > MathUtils.random(0f, 1f)) try_emit_mat(mat.matToEmit)
        val moveDir: CardinalDir = move_dest_gas()
        move(moveDir)
        dataB -= 1
      } else {
        set(Center, new Cell(Air))
      }
    }

    def update_smoke(): Unit = {
      // DataB = lifetime remaining
      if (dataB == -1) dataB = 10
      else if (dataB > 0) dataB -= 1

      if (dataB > 0) {
        val moveDir: CardinalDir = move_dest_gas()
        move(moveDir)
        dataB -= 1
      } else {
        set(Center, new Cell(Air))
      }
    }

    def update_oil_burning(): Unit = {
      // DataB = lifetime remaining
      if (dataB == -1) dataB = 10
      else if (dataB > 0 && matIsInNeighborhood(Air)._1) dataB -= 1 // Can't burn without air

      if (mat.chanceEmit > MathUtils.random(0f, 1f)) try_emit_mat(mat.matToEmit)
      if (dataB > 0) {
        val moveDir: CardinalDir = move_dest_gas()
        move(moveDir)
      } else {
        set(Center, new Cell(mat.matToEmit))
      }
    }

    def update_smoke_soot(): Unit = {
      // DataB = lifetime remaining
      if (dataB == -1) dataB = 300
      else if (dataB > 0) dataB -= 1

      if (dataB > 0) {
        val moveDir: CardinalDir = move_dest_gas()
        move(moveDir)
      } else {
        set(Center, new Cell(Smoke))
      }
    }

    def update_lava(): Unit = {
      // Sawtooth color lightness
      if (dataA > 200) dataA -= 1
      else dataA = 255

      val moveDir: CardinalDir = move_dest_liquid()
      move(moveDir)
    }

    def update_water_vapor(): Unit = {
      // DataB = lifetime remaining
      if (dataB == -1) dataB = 800
      else if (dataB > 0) dataB -= 1

      if (dataA < 240) dataA = Cell.randAlpha()

      if (dataB > 0) {
        val moveDir: CardinalDir = move_dest_gas()
        move(moveDir)
      } else {
        set(Center, new Cell(Water))
      }
    }

    def update_seed(): Unit = {
      val fireInNeighborhood = matIsInNeighborhood(Fire)
      val lavaInNeighborhood = matIsInNeighborhood(Lava)
      if (fireInNeighborhood._1 || lavaInNeighborhood._1) {
        set(Center, new Cell(Fire))
      } else {
        val moveDir: CardinalDir = move_dest_granular_solid()
        move(moveDir)
      }
    }

    def update_wood(): Unit = {
      val fireInNeighborhood = matIsInNeighborhood(Fire)
      val lavaInNeighborhood = matIsInNeighborhood(Lava)
      if (fireInNeighborhood._1 || lavaInNeighborhood._1) {
        val chanceToBurn = 0.1f
        if (MathUtils.random(0f, 1f) <= chanceToBurn) {
          set(Center, new Cell(Fire))
        }
      }
    }

    def move(moveDir: CardinalDir): Unit = {
      // Limit number of times a cell can move in one step to 2
      if (moveDir != Center && get(moveDir).updated <= 1) {
        get(moveDir).updated += 1
        set(Center, get(moveDir))
        set(moveDir, this)
      }
    }

    def try_emit_mat(mat: Material): Unit = {
      val spaceToEmit = matIsInNeighborhood(Air)
      if (spaceToEmit._1) {
        val emitDir = spaceToEmit._2
        val emitCell = new Cell(mat)
        set(emitDir, emitCell)
      }
    }

    def move_dest_granular_solid(): CardinalDir = {
      if (canDisplaceSouth) South
      else if (canDisplaceSouthEast && canDisplaceSouthWest)
        randSouthLatitude
      else if (canDisplaceSouthEast) SouthEast
      else if (canDisplaceSouthWest) SouthWest
      else Center
    }

    def move_dest_solid(): CardinalDir = {
      if (canDisplaceSouth) South
      else Center
    }

    def move_dest_liquid(): CardinalDir = {
      def move_dest_horiz(): CardinalDir = {
        if (canDisplaceSouth) South
        else if (canDisplaceSouthEast && canDisplaceSouthWest)
          randSouthLatitude
        else if (canDisplaceSouthEast) SouthEast
        else if (canDisplaceSouthWest) SouthWest
        else Center
      }

      def move_dest_vert(): CardinalDir = {
        if (canDisplaceEast && canDisplaceWest)
          randLatitude
        else if (canDisplaceEast) East
        else if (canDisplaceWest) West
        else Center
      }

      lazy val moveVertOrHoriz: Float = MathUtils.random(0f, 1f)
      if (moveVertOrHoriz < 0.9f) {
        val destDir: CardinalDir = move_dest_horiz()
        if (destDir == Center) move_dest_vert()
        else destDir
      } else {
        val destDir: CardinalDir = move_dest_vert()
        if (destDir == Center) move_dest_horiz()
        else destDir
      }
    }

    def move_dest_gas(): CardinalDir = {
      lazy val moveVertOrHoriz: Int = MathUtils.random(0, 1)
      if (moveVertOrHoriz == 0) {
        if (canDisplaceSouth) South
        else if (canDisplaceSouthEast && canDisplaceSouthWest)
          randSouthLatitude
        else if (canDisplaceSouthEast) SouthEast
        else if (canDisplaceSouthWest) SouthWest
        else Center
      } else {
        if (canDisplaceEast && canDisplaceWest)
          randLatitude
        else if (canDisplaceEast) East
        else if (canDisplaceWest) West
        else Center
      }
    }

    // Returns (true, dir) if found and (false, _) if not
    def matIsInNeighborhood(mat: Material): (Boolean, CardinalDir) = {
      // TODO: Use filter
      CardinalDirection.all.foreach(dir => {
        if (get(dir).mat == mat) {
          return (true, dir)
        }
      })
      (false, Center)
    }
  }

}

object Cell {
  def randAlpha(): Int = 200 + MathUtils.random(0, 55)
}
