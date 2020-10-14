package sandbox.simulation

import com.badlogic.gdx.math.MathUtils

sealed trait Material {
  val color: Int // 0xRRGGBBAA
  val state: MatterState
  val density: Float // kg/m^3
  val chanceEmit: Float = 0f
  val matToEmit: Material = Air

  def canDisplace(mat: Material, dir: CardinalDir): Boolean = {
    val targetCanGetDisplaced: Boolean = mat.state != Solid
    val targetIsLessDense: Boolean = mat.density < density

    if (this == mat || MathUtils.isEqual(density, mat.density, 0.01f)) {
      false
    } else {
      targetCanGetDisplaced && targetIsLessDense
    }
  }
}

// An indestructible material
object NuclearPasta extends Material {
  val color = 0xece4c9ff
  val state = Solid
  val density = Float.MaxValue
}

object Stone extends Material {
  val color = 0x9c9d97ff
  val state = Solid
  val density = 2400f
}

object Air extends Material {
  val color = 0x121212ff
  val state = Gas
  val density = 1.3f
}

object Sand extends Material {
  val color = 0xffff00ff
  val state = Solid
  val density = 1442f
}

object Water extends Material {
  val color = 0x0000ffff
  val state = Liquid
  val density = 997f
}

object Oil extends Material {
  val color = 0x752438ff
  val state = Liquid
  val density = 870f
}

object Fire extends Material {
  val color = 0xff1111ff
  val state = Gas // or Plasma
  val density = 0.3f

  override val chanceEmit = 0.2f
  override val matToEmit = Smoke
}

object Smoke extends Material {
  val color = 0xccccccff
  val state = Gas
  val density = 0.3f
}

object Material {
  val all: Map[Int, Material] =
    Map(
      (0, NuclearPasta),
      (1, Stone),
      (2, Air),
      (3, Sand),
      (4, Water),
      (5, Oil),
      (6, Fire),
      (7, Smoke)
    )
}
