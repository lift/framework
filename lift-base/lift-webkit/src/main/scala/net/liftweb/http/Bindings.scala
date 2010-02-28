/*
 * Copyright 2006-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
import scala.xml.{NodeSeq, Text}

package net.liftweb {
import common.{Box,Full,Empty,Failure}
import util.Props

package http {

/**
 * A collection of types and implicit transformations used to allow composition
 * of page elements based upon the types of rendered objects.
 *
 * In Lift, a "snippet" is a function from NodeSeq => NodeSeq, where the argument
 * to the function is a template, and the result is a fragment of a page to be 
 * rendered. Of course, this is a bit of an abbreviation; the snippet function
 * also has an argument which is the application state made available from S.
 * A DataBinding[T] is very similar in this respect; it is a function from some
 * piece of information of type T to a function from NodeSeq =&gt; NodeSeq. Since
 * DataBinding is strongly typed with respect to the type of information being
 * rendered, DataBinding instances are ideal for the rendering of objects that
 * is used to build up snippets. For example:
 * 
 * <pre>
 * import net.liftweb.http.Bindings._

 * case class MyClass(str: String, i: Int, other: MyOtherClass)
 * case class MyOtherClass(foo: String)
 *
 * trait MyClassBinding extends DataBinding[MyClass] {
 *   implicit val otherBinding: DataBinding[MyOtherClass]
 *
 *   override def apply(entity: MyClass) = (xhtml: NodeSeq) =&gt; {
 *     val otherTemplate = chooseTemplate("myclass", "other", xhtml)
 *     bind(
 *       "myclass", xhtml, 
 *       "str" -&gt; Text("#" + entity.str + "#"),
 *       "i" -&gt; Text(entity.i.toString),
 *       "other" -&gt; entity.other.bind(otherTemplate)
 *     )
 *   }
 *
 * }
 * 
 * object myOtherClassBinding extends DataBinding[MyOtherClass] {
 *   override def apply(other: MyOtherClass) = (xhtml: NodeSeq) =&gt; {
 *     bind("other", xhtml, "foo" -&gt; Text("%" + other.foo + "%"))
 *   }
 * }
 *
 * object MyClassConcreteBinding extends MyClassBinding {
 *   override val otherBinding = myOtherClassBinding
 * }
 * </pre>
 *
 * In this example, two classes and their associated bindings are constructed;
 * the first binding for MyClass is abstract, needing a specific instance of
 * DataBinding[MyOtherClass] to enable the implicit conversion needed to render
 * the contained MyOtherClass instance. A subtemplate is selected, and the
 * call to other.bind both necessitates the implicit conversion to a Bindings.Binder
 * instance and applies the appropriate formatting. You can see how this
 * usage keeps the concerns of the view and the model nicely separated, while
 * allowing composition over object graphs.
 *
 * Please see the tests, as well as <a href="http://logji.blogspot.com/2009/09/composable-bindings-in-lift.html">this blog post</a> for additional details.
 */
object Bindings {
    type Binding = NodeSeq => NodeSeq

    type DataBinding[T] = T => NodeSeq => NodeSeq

    /**
     * Implicitly convert the specified object to a binder for that object if a DataBinding for
     * that object's type is available in implicit scope. This essentially adds a bind() method 
     * to an object if an appropriate implicit DataBinding is available.
     */
    implicit def binder[T](t: T)(implicit binding: DataBinding[T]): Binder = Binder(binding(t))

    /**
     * Wrap the specified Binding (a function from NodeSeq => NodeSeq) in a Binder so that
     * it can be applied using Binder's bind methods.
     */
    implicit def binder(binding: Binding): Binder = Binder(binding)

    /**
     * A decorator for a binding function that allows it to be called as bind() rather than apply().
     * This class also provides facilities for binding to a specific template
     */
    case class Binder(val binding: Binding) {
        /**
         * Apply this binder's binding function to the specified NodeSeq.
         */
        def bind(xhtml: NodeSeq): NodeSeq = binding.apply(xhtml)

        /**
         * Apply this binder's binding function to the specified templated
         * looked up using TemplateFinder.findAnyTemplate 
         */
        def bind(templatePath: List[String]): NodeSeq = {
            TemplateFinder.findAnyTemplate(templatePath) map binding match {
                case Full(xhtml) => xhtml
                case Failure(msg, ex, _) if Props.mode == Props.RunModes.Development => Text(ex.map(_.getMessage).openOr(msg))
                case Empty if Props.mode == Props.RunModes.Development => Text("Unable to find template with path " + templatePath.mkString("/", "/", ""))
                case _ => NodeSeq.Empty
            }
        }
    }

    /**
     * Bind any input value to the empty NodeSeq.
     */
    object EmptyBinding extends Binding {
        override def apply(xhtml : NodeSeq) : NodeSeq = NodeSeq.Empty
    }
}

}}
