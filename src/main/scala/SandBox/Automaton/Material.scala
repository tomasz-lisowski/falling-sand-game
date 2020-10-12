package SandBox

/* Each cell is 2D but we give all a unit depth
to make the units hence simulation more clear.*/
sealed trait Material {
  val color: Int // 0xRRGGBBAA
  val state: MatterState // Solid, Liquid, Gas used to simplify thermodynamics
  val mass: Float // m^3 (per cell)
  val density: Float = mass // p = m/V but V = 1 (each cell is assumed to be 1 m^3) so p = m

  def probabilityMove: Float = density / 2000f // Chance that a cell moves, based on density
}

// An indestructible material
object NuclearPasta extends Material {
  val color = 0xece4c9ff
  val state = Solid
  val mass = Float.MaxValue
}

object Stone extends Material {
  val color = 0x9c9d97ff
  val state = Solid
  val mass = 2400f
}

object Air extends Material {
  val color = 0x121212ff
  val state = Gas
  val mass = 1.3f
}

object Sand extends Material {
  val color = 0xffff00ff
  val state = Solid
  val mass = 1442f
}

object Water extends Material {
  val color = 0x0000ffff
  val state = Liquid
  val mass = 997f
}

object Oil extends Material {
  val color = 0x752438ff
  val state = Liquid
  val mass = 870f
}

object Fire extends Material {
  val color = 0xff1111ff
  val state = Gas
  val mass = 0.3f // Doesn't really apply to fire but to the gasses
}

object Material {
  val ids: Seq[Material] = Seq(NuclearPasta, Stone, Air, Sand, Water, Oil, Fire)
}
