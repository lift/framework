package net.liftweb.util

/**
 * A property that can be modified under limited conditions.
 */
abstract class PropertyWithModificationCondition[T](initialValue: T) {
  @volatile private var value = initialValue

  def get = value

  /**
   * Attemps to set the property to a new value.
   *
   * @return Whether the new property was installed. `false` means modification is no longer allowed.
   */
  def set(newValue: T): Boolean =
    if (allowModification) {
      value = newValue
      true
    } else {
      onModificationProhibited()
      false
    }

  def allowModification: Boolean

  def onModificationProhibited() {}
}
