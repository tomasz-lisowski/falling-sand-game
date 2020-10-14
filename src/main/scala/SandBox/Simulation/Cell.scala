package sandbox.simulation
import com.badlogic.gdx.math.MathUtils
import scala.util.Random

class Cell(
    var mat: Material = Air,
    var updated: Int = 0,
    var dataA: Int = 100 + MathUtils.random(0, 50),
    var dataB: Int = -1
) {
  def update(
      get: CardinalDir => Cell,
      set: (CardinalDir, Cell) => Unit
  ): Unit = {
    lazy val canDisplaceNorth: Boolean = mat.canDisplace(get(North).mat, North)
    lazy val canDisplaceNorthEast: Boolean = mat.canDisplace(get(NorthEast).mat, NorthEast)
    lazy val canDisplaceNorthWest: Boolean = mat.canDisplace(get(NorthWest).mat, NorthWest)
    lazy val canDisplaceSouth: Boolean = mat.canDisplace(get(South).mat, South)
    lazy val canDisplaceSouthEast: Boolean = mat.canDisplace(get(SouthEast).mat, SouthEast)
    lazy val canDisplaceSouthWest: Boolean = mat.canDisplace(get(SouthWest).mat, SouthWest)
    lazy val canDisplaceEast: Boolean = mat.canDisplace(get(East).mat, East)
    lazy val canDisplaceWest: Boolean = mat.canDisplace(get(West).mat, West)

    lazy val randSouthLatitude: CardinalDir =
      if (MathUtils.random(0, 1) == 0) SouthEast else SouthWest
    lazy val randNorthLatitude: CardinalDir =
      if (MathUtils.random(0, 1) == 0) NorthEast else NorthWest
    lazy val randLatitude: CardinalDir =
      if (MathUtils.random(0, 1) == 0) East else West

    updated = 1
    mat match {
      case NuclearPasta => ()
      case Stone        => update_stone()
      case Air          => update_air()
      case Sand         => update_sand()
      case Water        => update_water()
      case Oil          => update_oil()
      case Fire         => update_fire()
      case Smoke        => update_smoke()
    }

    def update_stone(): Unit = {
      val moveDir: CardinalDir = move_dest_solid()
      move(moveDir)
    }

    def update_air(): Unit = {
      val moveDir: CardinalDir = move_dest_gas()
      move(moveDir)
    }

    def update_sand(): Unit = {
      val moveDir: CardinalDir = move_dest_granular_solid()
      move(moveDir)
    }

    def update_water(): Unit = {
      val moveDir: CardinalDir = move_dest_liquid()
      move(moveDir)
    }

    def update_oil(): Unit = {
      val moveDir: CardinalDir = move_dest_liquid()
      move(moveDir)
    }

    def update_fire(): Unit = {
      if (dataB == -1) dataB = 16 // Init lifetime
      else if (dataB > 0) {
        if (dataB < 5 && mat.chanceEmit > MathUtils.random(0f, 1f)) try_emit_mat(mat.matToEmit)
        val moveDir: CardinalDir = move_dest_gas()
        move(moveDir)
        dataB -= 1 // Decrease lifetime
      } else {
        mat = Air
        dataB = -1
      }
    }

    def update_smoke(): Unit = {
      if (dataB == -1) dataB = 20 // Init lifetime
      else if (dataB > 0) {
        val moveDir: CardinalDir = move_dest_gas()
        move(moveDir)
        dataB -= 1 // Decrease lifetime
      } else {
        mat = Air
        dataB = -1
      }
    }

    def move(moveDir: CardinalDir): Unit = {
      if (moveDir != Center && get(moveDir).updated <= 1) {
        get(moveDir).updated += 1
        set(Center, get(moveDir))
        set(moveDir, this)
      }
    }

    def try_emit_mat(mat: Material): Unit = {
      CardinalDirection.all.foreach(emitDir => {
        if (get(emitDir).mat == Air) {
          get(emitDir).mat = mat
          get(emitDir).dataB = -1
          return
        }
      })
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
      def move_horiz(): CardinalDir = {
        if (canDisplaceSouth) South
        else if (canDisplaceSouthEast && canDisplaceSouthWest)
          randSouthLatitude
        else if (canDisplaceSouthEast) SouthEast
        else if (canDisplaceSouthWest) SouthWest
        else Center
      }

      def move_vert(): CardinalDir = {
        if (canDisplaceEast && canDisplaceWest)
          randLatitude
        else if (canDisplaceEast) East
        else if (canDisplaceWest) West
        else Center
      }

      lazy val moveVertOrHoriz: Float = MathUtils.random(0f, 1f)
      if (moveVertOrHoriz < 0.8f) {
        val destDir: CardinalDir = move_horiz()
        if (destDir == Center) move_vert()
        else destDir
      } else {
        val destDir: CardinalDir = move_vert()
        if (destDir == Center) move_horiz()
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
  }

}
