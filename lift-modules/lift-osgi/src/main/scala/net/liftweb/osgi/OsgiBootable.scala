package net.liftweb {
package osgi {

import http.Bootable

/**
 * Special Bootable for lift-osgi bundle: Do nothing!
 */
class OsgiBootable extends Bootable {

  /** Do nothing! */
  override def boot() {}
}

}
}
