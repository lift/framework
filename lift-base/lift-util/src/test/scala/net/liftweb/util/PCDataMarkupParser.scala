/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.scala.xml._
import common._

class PCDataMarkupTest extends JUnit4(PCDataMarkupSpecs)
object PCDataMarkupSpecs extends Specification {
val data1 = """


<html>dude</html>


"""

val data2 = """

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>dude</html>


"""

val data3 = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<title>Meow</title>
<meta name="generator" content="Bluefish 1.0.7"/>
<meta name="author" content="David Pollak"/>
<meta name="date" content="2010-10-31T05:40:15-0700"/>
<meta name="copyright" content=""/>
<meta name="keywords" content=""/>
<meta name="description" content=""/>
<meta name="ROBOTS" content="NOINDEX, NOFOLLOW"/>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
<meta http-equiv="content-type" content="application/xhtml+xml; charset=UTF-8"/>
<meta http-equiv="content-style-type" content="text/css"/>
</head>
<body>

</body>
</html>"""


   "PCDataMarkupParser" should {
     "Parse a document with whitespace" in {
       PCDataXmlParser(data1).open_! must ==/ (<html>dude</html>)
     }

     "Parse a document with doctype" in {
       PCDataXmlParser(data2).open_! must ==/ (<html>dude</html>)
     }

     "Parse a document with xml and doctype" in {
       PCDataXmlParser(data3).open_!.apply(0).label must_== "html"
     }

   }

}

}
}
