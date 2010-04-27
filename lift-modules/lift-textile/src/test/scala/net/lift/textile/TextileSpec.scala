/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package textile {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

import _root_.scala.xml._

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

class TextileSpecTest extends Runner(TextileSpec) with JUnit with Console
object TextileSpec extends Specification {

  val shouldRelax = (java.lang.Boolean.getBoolean("textile.relax"))

  import TextileParser._

  "A Textile Parser" can {
     "deal with italic" in {
      toHtml("foo __bar__") must ==/(<p>foo <i>bar</i></p> )
    }

    "multi-line div" in {
      val div =
"""<div class="vcard">
   <div class="fn">Joe Doe</div>
   <div class="org">The Example Company</div>
   <div class="tel">604-555-1234</div>
   http://example.com/
 </div>"""

      val res = toHtml(div)

      res must ==/(<p><div>
   <div>Joe Doe</div>
   <div>The Example Company</div>
   <div>604-555-1234</div>
   <a href="http://example.com/">http://example.com/</a>
 </div></p>)
    }



    "deal with bold and italic foo dog" in {
      toHtml("*foo* dog __bar__") must ==/(<p><strong>foo</strong> dog <i>bar</i></p> )
    }

    "deal with bold and italic" in {
      toHtml("*foo* __bar__") must ==/(<p><strong>foo</strong> <i>bar</i></p> )
    }

    "attributes in link quotes" in {
      val res = toHtml(""""(cms-woof) click here":# """)
     res must ==/(<p><a href="javascript://" class="cms-woof"> click here</a> </p>)
    }


    "Be a single line of text" in {
      toHtml("Hello World") must ==/(<p>Hello World</p>)
    }

    "Make things bold" in {
      toHtml("**Hello World**") must ==/(<p><b>Hello World</b></p>)
    }

    "Make other things bold" in {
      toHtml("Dude this is **Hello World** kind of stuff") must ==/(<p>Dude this is <b>Hello World</b> kind of stuff</p>)
    }

    "I am <i>Italic</i> <u>under</u> <strong>strong</strong>" in {
      toHtml("I am <i>Italic</i> <u>under</u> <strong>strong</strong>") must ==/(<p>I am <i>Italic</i> <u>under</u> <strong>strong</strong></p>)
    }

    "I am <em>very</em> serious" in {
      toHtml("I am <em>very</em> serious") must ==/(<p>I am <em>very</em> serious</p>)
    }

    "Observe -- very nice!" in {
      toHtml("Observe -- very nice!") must ==/(<p>Observe &#8212; very nice!</p>)
    }

    "Observe - tiny and brief." in {
      toHtml("Observe - tiny and brief.") must ==/(<p>Observe &#8211; tiny and brief.</p>)
    }

    "\"Observe!\"" in {
      val ret = toHtml("\"Observe!\"")

      ret must ==/(<p>&#8220;Observe!&#8221;</p>)
    }

    "A simple example." in {
      toHtml("A simple example.") must ==/(<p>A simple example.</p>)
    }

    "Multi-line with - signs" in {
      val res = toHtml(
        """
A simple example.
A hi-tech example.
A hi-tech lo-tech example.
A -deleted- example.
A -deleted hi-tech- example.
A regular example.
""")

      res must ==/(<p>A simple example.<br />
          A hi-tech example. <br />
          A hi-tech lo-tech example. <br />
          A <del>deleted</del> example.<br />
          A <del>deleted hi-tech</del> example. <br />
          A regular example.</p>)
    }

    "A question mark in an URL" in {
      val res = toHtml(""""video link":http://www.youtube.com/watch?v=FclcMfzjwug foo""")
      
      res must ==/(<p><a href="http://www.youtube.com/watch?v=FclcMfzjwug">video link</a> foo</p>)
    }

    "a link http://yahoo.com inside" in {
      toHtml("a link http://yahoo.com inside") must ==/(<p>a link <a href="http://yahoo.com">http://yahoo.com</a> inside</p>)
    }

    "deal with a very long line of text" in {
      val sb = new StringBuilder()
      (1 to 10000).foreach(i => sb.append(i.toString+" "))
      toHtml(sb.toString)
    }

    "h3" in {
      toHtml("h3. Dogs eat dogfood") must ==/(<h3>Dogs eat dogfood</h3>)
    }

    "h3 with whitespace" in {
      toHtml("   h3. Dogs eat dogfood") must ==/(<h3>Dogs eat dogfood</h3>)
    }

    "Single line delete " in {
      toHtml("This contains -deleted stuff-") must ==/(<p>This contains <del>deleted stuff</del></p>)
    }

    "3 bullets" in {
      val it = toHtml(
        """
* Hello
* Dude
* Dog
""")

      it must ==/(
        <ul><li> Hello</li>
        <li> Dude</li>
        <li> Dog</li>
        </ul>
      )
    }

    "3 bullets strong" in {
      val it = toHtml(
        """
* *Hello* moo
* Dude
* Dog
""")

      it must ==/(
        <ul><li> <strong>Hello</strong> moo</li>
        <li> Dude</li>
        <li> Dog</li>
        </ul>
      )
    }

    "3 bullets not strong" in {
      val it = toHtml(
        """
* *Hello moo
* Dude
* Dog
""")

      it must ==/(
        <ul><li> *Hello moo</li>
        <li> Dude</li>
        <li> Dog</li>
        </ul>
      )
    }

     "3 bullets after a line" in {
      val it = toHtml(
        """
I Like Yaks
* Hello moo
* Dude
* Dog
""")

      it must ==/(
        <p>I Like Yaks<br/><ul><li> Hello moo</li>
        <li> Dude</li>
        <li> Dog</li>
        </ul>
        </p>
      )
    }

    "a link http://yahoo.com not inside" in {
      toHtml("a link http://yahoo.com not inside", true).toString.trim must_== "<p>a link http://yahoo.com not inside</p>"
    }

  }

    "Fast" in {
      val s =   """VF-DE:
        IFC.ksh: 04/10/08 03:26:52: *************** start of IFC.ksh ***************
        IFC.ksh: 04/10/08 15:34:30: *************** end of IFC.ksh ***************
VF-ES:
        IFC.ksh: 04/11/08 03:11:48: *************** start of IFC.ksh ***************
        IFC.ksh: 04/11/08 04:48:24: *************** end of IFC.ksh ***************
VF-GR:
        IFC.ksh: 04/11/08 03:11:59: *************** start of IFC.ksh ***************
        IFC.ksh: 04/11/08 03:46:47: *************** end of IFC.ksh ***************
VF-IE:
        IFC.ksh: 04/11/08 03:15:07: *************** start of IFC.ksh ***************
        IFC.ksh: 04/11/08 05:36:57: *************** end of IFC.ksh ***************
VF-IT:
        IFC.ksh: 04/11/08 03:05:07: *************** start of IFC.ksh ***************
        IFC.ksh: 04/11/08 05:06:09: *************** end of IFC.ksh ***************
VF-NL:
        IFC.ksh: 04/11/08 03:55:05: *************** start of IFC.ksh ***************
        IFC.ksh: 04/11/08 04:28:57: *************** end of IFC.ksh ***************
VF-PT:
        IFC.ksh: 04/11/08 03:00:09: *************** start of IFC.ksh ***************
        IFC.ksh: 04/11/08 03:32:05: *************** end of IFC.ksh ***************
VF-UK:
        IFC.ksh: 04/11/08 03:38:41: *************** start of IFC.ksh ***************
        IFC.ksh: 04/11/08 05:26:49: *************** end of IFC.ksh ***************

I saved the entry by pressing "Yep" button, but because the symbol "-" caused the subsequent letters to be crossed out I've edited the text like this:
DE:
        IFC.ksh: 04/10/08 03:26:52: *************** start of """

      val (time, res) = Helpers.calcTime{
	toHtml(s)
      }
      val cmp: Long =  (if (shouldRelax) 10000L else 3000L)
      time must be_<( cmp)
    }

    "Fast 2" in {
      val s =     """I am <em>very</em> serious

Observe -- very nice!

Observe - tiny and brief.

"Observe!"

Hello Dude

**Bold * Not Strong**


my bold line **bold**

**strong* Not Bold


*strong*

This is a single paragraph

This is another paragraph

I am <b>very</b> serious.

This
is a paragraph

<pre>
I am <b>very</b> serious.

Oh, yes I am!!
</pre>

I spoke.
And none replied.




Observe...

Observe: 2 x 2.

one(TM), two(R), three(C).

h1. Header 1
second line of header 1

h2. Header 2

h3. Header 3

An old text

bq. A block quotation.

Any old text

This is covered elsewhere[1].

fn1. Down here, in fact.

I _believe_ every word.

And then? She *fell*!

I __know__.
I **really** __know__.

??Cat's Cradle?? by Vonnegut

Convert with @r.to_html@

I'm -sure- not sure.

You are a +pleasant+ child.

a ^2^ + b ^2^ = c ^2^

log ~2~ x

I'm %unaware% of most soft drinks.

I'm %{color:red}unaware%
of most soft drinks.

http://hobix.com/textile/#attributes

I searched "Google":http://google.com.

CamelCase

\\CamelCase

ThreeHumpCamel

Four4FourHumpCamel


I am crazy about "Hobix":hobix
and "it's":hobix "all":hobix I ever
"link to":hobix!

[hobix]http://hobix.com

# A first item
# A second item
# A third

# Fuel could be:
## Coal
## Gasoline
## Electricity
# Humans need only:
## Water
## Protein

* A first item
* A second item
* A third

* Fuel could be:
** Coal
** Gasoline
** Electricity
* Humans need only:
** Water
** Protein

| name | age | sex |
| joan | 24 | f |
| archie | 29 | m |
| bella | 45 | f |

|_. name |_. age |_. sex |
| joan | 24 | f |
| archie | 29 | m |
| bella | 45 | f |

|_. attribute list |
|<. align left |
|>. align right|
|=. center |
|<>. justify this block |
|^. valign top |
|~. bottom |

|\2. spans two cols |
| col 1 | col 2 |

|/3. spans 3 rows | a |
| b |
| c |

|{background:#ddd}. Grey cell|

table{border:1px solid black}.
|This|is|a|row|
|This|is|a|row|

|This|is|a|row|
{background:#ddd}. |This|is|grey|row|

p<. align left

p>. align right

p=. centered

p<>. justified

p(. left ident 1em

p((. left ident 2em

p))). right ident 3em

h2()>. Bingo.

h3()>[no]{color:red}. Bingo

<pre>
<code>
a.gsub!( /</, '' )
</code>
</pre>


<div style='float:right;'>
float right
</div>

<div style='float:right;'>

h3. Sidebar

"Hobix":http://hobix.com/
"Ruby":http://ruby-lang.org/

</div>

The main text of the
page goes here and will
stay to the left of the
sidebar.

!http://hobix.com/sample.jpg!

!http://hobix.com/sa.jpg(Bunny.)!

!http://hobix.com/sample.jpg!:http://hobix.com/

!>http://hobix.com/sample.jpg!

And others sat all round the small
machine and paid it to sing to them.

We use CSS(Cascading Style Sheets).


"""
      val (time, res) = Helpers.calcTime{
	toHtml(s)
      }

      time must be_< (if (shouldRelax) 10000L else 3000L)
    }


  """Hello:
* THis is a * on a line
* This is a *strong* line
* This is a **Bold** line
* This is a line with no markup
"""

}

}
}
