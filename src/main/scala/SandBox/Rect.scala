package SandBox

import com.badlogic.gdx.math.MathUtils.{floor, ceil}

class Rect(
    val x: Int,
    val y: Int,
    val width: Int,
    val height: Int,
    val scale: Float
) {
  // Scaled width and height
  def swidth: Int = floor(width / scale)
  def sheight: Int = floor(height / scale)
}
