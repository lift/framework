/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package json

import java.util.concurrent.ConcurrentHashMap
import scala.util.{Try, Success, Failure}

private[json] object ScalaSigReader {

  def readConstructor(argName: String, clazz: Class[_], typeArgIndex: Int, argNames: List[String]): Class[_] = {
    // In Scala 3, we fall back to runtime inspection since compile-time type information
    // is not available in the same way as Scala 2's signature parsing.
    // For most use cases, this will return AnyRef and the JSON extraction will handle
    // type conversion at runtime based on the JSON structure.
    
    try {
      // Try to use standard Java reflection to get constructor parameter types
      val constructors = clazz.getDeclaredConstructors
      val matchingConstructor = constructors.find(_.getParameterCount == argNames.length)
      
      matchingConstructor match {
        case Some(constructor) =>
          val paramTypes = constructor.getGenericParameterTypes
          val argIndex = argNames.indexOf(argName)
          if (argIndex >= 0 && argIndex < paramTypes.length) {
            paramTypes(argIndex) match {
              case cls: Class[_] => 
                // Handle generic type arguments - for now, return the raw type
                if (typeArgIndex > 0) classOf[AnyRef] else cls
              case _ => classOf[AnyRef]
            }
          } else {
            classOf[AnyRef]
          }
        case None => classOf[AnyRef]
      }
    } catch {
      case _: Exception => classOf[AnyRef]
    }
  }

  def readField(name: String, clazz: Class[_], typeArgIndex: Int): Class[_] = {
    try {
      // Try to find the field using Java reflection
      val field = Try(clazz.getDeclaredField(name)).orElse(
        // If direct field lookup fails, try looking in superclasses
        Try(findFieldInHierarchy(clazz, name))
      )
      
      field match {
        case Success(f) =>
          f.getGenericType match {
            case cls: Class[_] => 
              // Handle generic type arguments - for now, return the raw type
              if (typeArgIndex > 0) classOf[AnyRef] else cls
            case _ => classOf[AnyRef]
          }
        case Failure(_) => classOf[AnyRef]
      }
    } catch {
      case _: Exception => classOf[AnyRef]
    }
  }
  
  private def findFieldInHierarchy(clazz: Class[_], name: String): java.lang.reflect.Field = {
    if (clazz == null) throw new NoSuchFieldException(name)
    
    try {
      clazz.getDeclaredField(name)
    } catch {
      case _: NoSuchFieldException =>
        findFieldInHierarchy(clazz.getSuperclass, name)
    }
  }
}