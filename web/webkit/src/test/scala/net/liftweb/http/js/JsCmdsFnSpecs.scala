package net.liftweb.http.js

import org.specs2.mutable.Specification
import net.liftweb.http.js.JsCmds._

object JsCmdsFnSpecs extends Specification {
  sequential
  "JsCmds functions Specification".title

  "JsCmds.toList()" should {
    "return a single JsCmd in a List() if it is not a cmd pair" in {
      toList(Noop) must_== List(Noop)
    }

    "return a List of 2 JsCmd instances if joined as a pair" in {
      toList(Noop & Reload) must_== List(Noop, Reload)
    }

    "return a List of 3 JsCmd instances when left-associatively joining the triplet" in {
      toList((JsBreak & Noop) & Reload) must_== List(JsBreak, Noop, Reload)
    }

    "return a List of 3 JsCmd instances when right-associatively joining the triplet" in {
      toList(JsBreak & (Noop & Reload)) must_== List(JsBreak, Noop, Reload)
    }
  }

  "JsCmds.trimNoops()" should {
    "return Noop if the JsCmd is in fact a Noop" in {
      trimNoops(Noop) must_== Noop
    }

    "return Noop if all of the JsCmds are Noops" in {
      trimNoops(Noop & Noop & Noop) must_== Noop
    }

    "remove leading Noops" in {
      trimNoops(Noop & Noop & Reload) must_== Reload
    }

    "remove trailing Noops" in {
      trimNoops(Reload & Noop & Noop) must_== Reload
    }

    "remove Noops among multiple non-Noops" in {
      trimNoops(Noop & Reload & Noop & Noop & JsBreak & JsContinue & Noop) must_== (Reload & JsBreak & JsContinue)
    }
  }
}
