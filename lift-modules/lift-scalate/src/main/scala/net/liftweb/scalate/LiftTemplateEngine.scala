package net.liftweb.scalate

import java.io.File
import tools.nsc.Global
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.util.ClassPathBuilder
import org.fusesource.scalate.support.FileResourceLoader
import net.liftweb.http.LiftRules
import org.fusesource.scalate.{DefaultRenderContext, ResourceNotFoundException, Binding, TemplateEngine}
import net.liftweb.http.provider.servlet.HTTPServletContext
import net.liftweb.common.Logger

/**
 * A TemplateEngine using the Lift web abstractions
 *
 * @version $Revision : 1.1 $
 */

class LiftTemplateEngine extends TemplateEngine {
  bindings = List(Binding("context", classOf[DefaultRenderContext].getName, true, isImplicit = true))

  if (useWebInfWorkingDirectory) {
    val path = realPath("WEB-INF")
    if (path != null) {
      workingDirectory = new File(path, "_scalate")
      workingDirectory.mkdirs
    }
  }
  classpath = buildClassPath
  resourceLoader = new LiftResourceLoader(this)
  layoutStrategy = new DefaultLayoutStrategy(this, "/WEB-INF/scalate/layouts/default.scaml", "/WEB-INF/scalate/layouts/default.ssp")

  private def buildClassPath(): String = {
    val builder = new ClassPathBuilder

    // Add containers class path
    builder.addPathFrom(getClass)
            .addPathFrom(classOf[TemplateEngine])
            .addPathFrom(classOf[Product])
            .addPathFrom(classOf[Global])

    // Always include WEB-INF/classes and all the JARs in WEB-INF/lib just in case
    builder.addClassesDir(realPath("/WEB-INF/classes"))
            .addLibDir(realPath("/WEB-INF/lib"))

    builder.classPath
  }

  def useWebInfWorkingDirectory = {
    val customWorkDir = System.getProperty("scalate.workingdir", "")
    val property = System.getProperty("scalate.temp.workingdir", "")
    println("using scalate.temp.workingdir: " + property)
    property.toLowerCase != "true" && customWorkDir.length <= 0
  }

  def realPath(uri: String): String = {
    LiftRules.context match {
      case http: HTTPServletContext => http.ctx.getRealPath(uri)
      case c => warn("Do not know how to get the real path of: " + uri + " for context: " + c); uri
    }
  }

  class LiftResourceLoader(context: LiftTemplateEngine) extends FileResourceLoader {
    override protected def toFile(uri: String) = {
      realFile(uri)
    }

    override protected def toFileOrFail(uri: String): File = {
      val file = realFile(uri)
      if (file == null) {
        throw new ResourceNotFoundException(resource = uri, root = context.realPath("/"))
      }
      file
    }

    /**
     * Returns the real path for the given uri
     */
    def realPath(uri: String): String = {
      val file = realFile(uri)
      if (file != null) file.getPath else null
    }

    /**
     * Returns the File for the given uri
     */
    def realFile(uri: String): File = {
      def findFile(uri: String): File = {
        /*
                val url = LiftRules.context.resource(uri)
                if (url != null) {
                  url.toFile
                }
                else {
                  null
                }
        */
        val path = context.realPath(uri)
        debug("realPath for: " + uri + " is: " + path)

        var answer: File = null
        if (path != null) {
          val file = new File(path)
          debug("file from realPath for: " + uri + " is: " + file)
          if (file.canRead) {answer = file}
        }
        answer
      }

      findFile(uri) match {
        case file: File => file
        case _ => if (uri.startsWith("/") && !uri.startsWith("/WEB-INF")) {
          findFile("/WEB-INF" + uri)
        }
        else {
          null
        }
      }
    }

  }

}

