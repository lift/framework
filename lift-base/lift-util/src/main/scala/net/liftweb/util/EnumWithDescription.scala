/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package util {

/*
 A wrapper arround a Scala Enumeration Value that has a name, description for each object
 */
trait ValueWithDescription  {
    def description: String
    def name: String
}

abstract class EnumWithDescription  {
    type Value = enum.Value with ValueWithDescription

    private var _values: List[Value] = Nil
    def values = _values

    // possibly not a good idea using this directly
    val enum = new Enumeration {
        def Value(inName: String, inDescription: String): Value with ValueWithDescription = {
            new Val(nextId, inName) with ValueWithDescription {
                def description = inDescription
                def name = inName
            }
        }
    }

    def Value(name: String, description: String): Value = {
        val value = enum.Value(name, description)
        _values = _values ::: List(value)  // build in order
        value
    }

    def Value(name: String): Value = Value(name, name)

    def valueOf(name: String) = values find (_.name == name)

    def nameDescriptionList = values map(x => (x.name, x.description) )

}

}
}
