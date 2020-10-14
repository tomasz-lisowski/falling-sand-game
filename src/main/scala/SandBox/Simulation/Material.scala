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
  val color = 0xffa3ba00
  val state = Solid
  val density = Float.MaxValue
}

object Stone extends Material {
  val color = 0x9c9d9700
  val state = Solid
  val density = 2400f
}

object Air extends Material {
  val color = 0xfdf5ff00
  val state = Gas
  val density = 1.3f
}

object Sand extends Material {
  val color = 0xffe89e00
  val state = Solid
  val density = 1442f
}

object Water extends Material {
  val color = 0x1574c100
  val state = Liquid
  val density = 997f
}

object Oil extends Material {
  val color = 0x120b1900
  val state = Liquid
  val density = 870f
}

object Fire extends Material {
  val color = 0xb1221b00
  val state = Gas // or Plasma
  val density = 0.3f

  override val chanceEmit = 0.2f
  override val matToEmit = Smoke
}

object Smoke extends Material {
  val color = 0x52525200
  val state = Gas
  val density = 0.3f
}

object BurningOil extends Material {
  val color = 0x38001700
  val state = Liquid
  val density = 870f

  override val chanceEmit = 0.5f
  override val matToEmit = SmokeSoot
}

object SmokeSoot extends Material {
  val color = 0x0f0f0f00
  val state = Gas
  val density = 0.8f
}

object Lava extends Material {
  val color = 0xdb300000
  val state = Liquid
  val density = 3100f
}

object WaterVapor extends Material {
  val color = 0xffffff00
  val state = Gas
  val density = 1.2f
}

object Seed extends Material {
  val color = 0x1f472400
  val state = Solid
  val density = 730f
}

object Wood extends Material {
  val color = 0x3b2a1700
  val state = Solid
  val density = 700f
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
      (7, Smoke),
      (8, BurningOil),
      (9, SmokeSoot),
      (10, Lava),
      (11, WaterVapor),
      (12, Seed),
      (13, Wood)
    )
}
