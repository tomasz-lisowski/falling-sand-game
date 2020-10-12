package SandBox

abstract class CardinalDirection() {
  // Unit offset from center to a cardinal direction
  val x: Int
  val y: Int
}
case object North extends CardinalDirection {
  val x = 0
  val y = 1
}
case object NorthEast extends CardinalDirection {
  val x = 1
  val y = 1
}
case object East extends CardinalDirection {
  val x = 1
  val y = 0
}
case object SouthEast extends CardinalDirection {
  val x = 1
  val y = -1
}
case object South extends CardinalDirection {
  val x = 0
  val y = -1
}
case object SouthWest extends CardinalDirection {
  val x = -1
  val y = -1
}
case object West extends CardinalDirection {
  val x = -1
  val y = 0
}
case object NorthWest extends CardinalDirection {
  val x = -1
  val y = 1
}
case object Invalid extends CardinalDirection {
  val x = 0
  val y = 0
}

object CardinalDirection {
  val all: Seq[CardinalDirection] =
    Seq(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
}
