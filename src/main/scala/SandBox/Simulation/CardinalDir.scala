package sandbox.simulation

abstract class CardinalDir() {
  // Unit offset from center to a cardinal direction
  val x: Int
  val y: Int
}
case object North extends CardinalDir {
  val x = 0
  val y = 1
}
case object NorthEast extends CardinalDir {
  val x = 1
  val y = 1
}
case object East extends CardinalDir {
  val x = 1
  val y = 0
}
case object SouthEast extends CardinalDir {
  val x = 1
  val y = -1
}
case object South extends CardinalDir {
  val x = 0
  val y = -1
}
case object SouthWest extends CardinalDir {
  val x = -1
  val y = -1
}
case object West extends CardinalDir {
  val x = -1
  val y = 0
}
case object NorthWest extends CardinalDir {
  val x = -1
  val y = 1
}
case object Center extends CardinalDir {
  val x = 0
  val y = 0
}

object CardinalDir {
  val all: Seq[CardinalDir] =
    Seq(
      North,
      NorthEast,
      NorthWest,
      East,
      West,
      SouthEast,
      South,
      SouthWest
    )
}
