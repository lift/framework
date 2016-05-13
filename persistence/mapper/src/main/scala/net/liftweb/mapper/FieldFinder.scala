/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

package net.liftweb
package mapper

import scala.reflect.{ClassTag,classTag}

class FieldFinder[T: ClassTag](metaMapper: AnyRef, logger: net.liftweb.common.Logger) {
  import java.lang.reflect._

  logger.debug("Created FieldFinder for " + classTag[T].runtimeClass)

  def isMagicObject(m: Method) = m.getReturnType.getName.endsWith("$"+m.getName+"$") && m.getParameterTypes.length == 0

  def typeFilter: Class[_]=>Boolean = classTag[T].runtimeClass.isAssignableFrom

  /**
    * Find the magic mapper fields on the superclass
    */
  def findMagicFields(onMagic: AnyRef, startingClass: Class[_]): List[Method] = {
    // If a class name ends in $module, it's a subclass created for scala object instances
    def deMod(in: String): String =
      if (in.endsWith("$module")) in.substring(0, in.length - 7)
      else in

    // find the magic fields for the given superclass
    def findForClass(clz: Class[_]): List[Method] = clz match {
      case null => Nil
      case c =>
        // get the names of fields that represent the type we want

        val fields = Map(c.getDeclaredFields.
                          filter{f =>
                            val ret = typeFilter(f.getType)
                            logger.trace("typeFilter(" + f.getType + "); T=" + classTag[T].runtimeClass)
                            ret
                          }.
                          map(f => (deMod(f.getName), f)) :_*)

        logger.trace("fields: " + fields)

        // this method will find all the super classes and super-interfaces
        def getAllSupers(clz: Class[_]): List[Class[_]] = clz match {
          case null => Nil
          case c =>
            c :: c.getInterfaces.toList.flatMap(getAllSupers) :::
            getAllSupers(c.getSuperclass)
        }

        // does the method return an actual instance of an actual class that's
        // associated with this Mapper class
        def validActualType(meth: Method): Boolean = {
          try {
            // invoke the method
            meth.invoke(onMagic) match {
              case null =>
                logger.debug("Not a valid mapped field: %s".format(meth.getName))
                false
              case inst =>
                // do we get a T of some sort back?
                if (!typeFilter(inst.getClass)) false
                else {
                  // find out if the class name of the actual thing starts
                  // with the name of this class or some superclass...
                  // basically, is an inner class of this class
                  getAllSupers(clz).find{
                    c =>
                    inst.getClass.getName.startsWith(c.getName)}.isDefined
                }
            }

          } catch {
            case e: Exception =>
              logger.debug("Not a valid mapped field: %s, got exception: %s".format(meth.getName, e))
              false
          }
        }

        // find all the declared methods
        val meths = c.getDeclaredMethods.toList.
        filter(_.getParameterTypes.length == 0). // that take no parameters
        filter(m => Modifier.isPublic(m.getModifiers)). // that are public
        filter(m => fields.contains(m.getName) && // that are associated with private fields
                fields(m.getName).getType == m.getReturnType).
        filter(validActualType) // and have a validated type

        meths ::: findForClass(clz.getSuperclass)
    }

    findForClass(startingClass).distinct
  }

  lazy val accessorMethods = findMagicFields(metaMapper, metaMapper.getClass.getSuperclass)
}

