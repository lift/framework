package net.liftweb.markdown

/*
 * Copyright 2013 WorldWide Conferencing, LLC
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
 *
 * Based on https://github.com/chenkelmann/actuarius originally developed by
 * Christoph Henkelmann http://henkelmann.eu/
 */

import java.io.{InputStreamReader, FileInputStream, StringWriter}

/**
 * Quick and dirty test for measuring the time of this Parser.
 * Contains hardcoded file paths, just ignore this, it will be removed soon.
 */

trait TimedTransformer {

    /**
     * Overwrite this method to return a custom decorator if you want modified output.
     */
    def deco():Decorator = Decorator

    private object lineTokenizer extends LineTokenizer {
        override def allowXmlBlocks() = TimedTransformer.this.deco().allowVerbatimXml()
    }
    private object blockParser extends BlockParsers {
        override def deco() = TimedTransformer.this.deco()
    }

    /**
     * This is the method that turns markdown source into xhtml.
     */
    def apply(s:String) = {

        //first, run the input through the line parser
        val (ms1,lineReader:MarkdownLineReader) = TimeTest.executionTime(()=>lineTokenizer.tokenize(s))

        //then, run it through the block parser
        val (ms2, result) = TimeTest.executionTime(()=>blockParser(lineReader))
        println("lines=" + ms1 + ", blocks=" + ms2)
        result
    }
}



object TimeTest {
    private object actuariusProcessor extends TimedTransformer()

    private def readFile(path:String):String  = {
        //read from system input stream
        val reader = new InputStreamReader(new FileInputStream(path))
        val writer = new StringWriter()
        val buffer = new Array[Char](1024)
		var read = reader.read(buffer)
		while (read != -1) {
			writer.write(buffer, 0, read)
			read = reader.read(buffer)
		}
        //turn read input into a string
        writer.toString
    }

    def executionTime[T](f:(()=>T)):(Long, T) = {
        val start = System.currentTimeMillis
        val t = f()
        val end = System.currentTimeMillis
        (end - start, t)
    }

    private def runActuarius(markdown:String, iterations:Int) {
        for (i <- 0 until iterations) actuariusProcessor(markdown)
    }


    def testRun(markdown:String, iterations:Int) {
        println("Running Actuarius " + iterations + " times...")
        println("... took " + (executionTime(() => runActuarius(markdown, iterations)))._1 + "ms")
    }

    object testParser extends BaseParsers {
        //def ws1:Parser[String] = """( |\t|\v)+""".r
        def ws2:Parser[String] = rep1(elem(' ') | elem('\t') | elem('\u000B')) ^^ {_.mkString}

        def runParser(s:String, p:Parser[String], iterations:Int) {
            for (i <- 0 until iterations) {
                apply(p, s)
            }
        }
    }

    def runActuarius = {
        val markdown = readFile("/home/chris/sbt_projects/markdown_race/test.txt").mkString*100
        val iterations = 10
        println("==== First run to warm up the VM: ====")
        testRun(markdown, iterations)
        println("==== Second run, JIT compiler should be done now: ====")
        testRun(markdown, iterations)
    }

    def runWs = {
        val wsString = " " * 1000
        val iterations = 100000
        println("Running ws...")
        println("...took " + executionTime (() => testParser.runParser(wsString, testParser.ws, iterations))._1 + "ms")
        //println("Running ws1...")
        //println("...took " + executionTime (() => testParser.runParser(wsString, testParser.ws, iterations)))
        println("Running ws2...")
        println("...took " + executionTime (() => testParser.runParser(wsString, testParser.ws2, iterations))._1 + "ms")

    }

    def main(args:Array[String]) {
        /*
        val markdown = readFile("/home/chris/sbt_projects/markdown_race/test.txt").mkString*100
        val iterations = 10
        println("==== First run to warm up the VM: ====")
        testRun(markdown, iterations)
        println("==== Second run, JIT compiler should be done now: ====")
        testRun(markdown, iterations)*/
        //runWs
        runActuarius
    }
}