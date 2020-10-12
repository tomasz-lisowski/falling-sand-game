package SandBox

/* All cells are fully mutable otherwise the 512x512=262144 cells
cause a large memory leak. If a cell should not be modified,
the "mutable" flag should be set (not ideal). */
final class Cell(
    private var material: Material,
    private var needsUpdate: Boolean,
    private val mutable: Boolean = true
) {
  def reset(): Unit = {
    material = Air
  }

  def getMaterial: Material = material
  def getNeedsUpdate: Boolean = needsUpdate
  def getMutable: Boolean = mutable

  def update(
      newMaterial: Material = material,
      newNeedsUpdate: Boolean = needsUpdate
  ): Unit = {
    if (mutable) {
      material = newMaterial
      needsUpdate = newNeedsUpdate
    } else {
      throw new RuntimeException("Tried mutating an immutable cell.")
    }
  }
}
