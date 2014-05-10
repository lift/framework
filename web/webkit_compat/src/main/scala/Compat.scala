// This file exists for compatilibity with non-2.11 versions of Scala. Certain
// aspects of 2.11 were only made available in 2.10, meaning we need a
// compatibility layer to properly build back through 2.9.x. This file provides
// those compatibility needs.
package net.liftweb.collection {
  package object concurrent {
    type Map[A,B] = scala.collection.mutable.ConcurrentMap[A,B]
  }

  object JavaConversions {
    def mapAsScalaConcurrentMap[A,B](thing: java.util.concurrent.ConcurrentMap[A,B]) = {
      scala.collection.JavaConversions.asScalaConcurrentMap(thing)
    }
  }
}
