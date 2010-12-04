/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
import _root_.org.specs.specification._

import _root_.scala.xml.Elem

import common._

object Html5ParserSpec extends Specification with Html5Parser with Html5Writer {

"Htm5 Writer" should {
  "Write &" in {
    toString(<foo baz="&amp;dog"/>) must_== """<foo baz="&dog"></foo>"""
  }
}

"Html5 Parser" should {
  "parse page1" in {
    val parsed = parse(page1).open_!
    (parsed \\ "script").length must be >= 4
  }

  /*
  "change <lift:head> to <head>" in {
    val parsed = parse("<div><lift:head>123</lift:head></div>").open_!
    val heads = parsed \\ "head"
    heads.length must_== 1
    heads.text must_== "123"
    (heads(0).asInstanceOf[Elem].prefix == null) must_== true
  }*/

  "parse page2" in {
    val parsed = parse(page2).open_!
    (parsed \\ "script").length must be >= 4
  }

  /*
  "fail to parse page3" in {
    val parsed = parse(page3)
    parsed match {
      case f: Failure => 1 must_== 1
    }
  }*/

  "Parse stuff with lift: namespace" in {
    val parsed = parse("""<lift:surround with="dog"><div/></lift:surround>""")
    val e = parsed.open_!.asInstanceOf[scala.xml.Elem]
    e.prefix must_== "lift"
    e.label must_== "surround"
    (parsed.open_! \ "@with").text must_== "dog"
  }

  "Parse stuff without lift: namespace" in {
    val parsed = parse("""<div with="dog"><div/></div>""")
    val e = parsed.open_!.asInstanceOf[scala.xml.Elem]
    e.label must_== "div"
    (parsed.open_! \ "@with").text must_== "dog"
  }
}


val page1 =
"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset=utf-8 />
<meta name="viewport" content="width=620" />
<title>HTML5 Demo: data-*</title>
<link rel="stylesheet" href="/css/html5demos.css" type="text/css" />
<script src="/js/h5utils.js"></script></head>
<body>
<section id="wrapper">
    <header>
      <h1>data-*</h1>
    </header><style>
#test {
  padding: 10px;
  border: 1px solid #ccc;
  margin: 20px 0;
}
pre {
  overflow-x: auto;
  padding: 10px;
  border: 1px dashed #ccc;
  background: #fff;
  font-size: 12px;
}
</style>

<article>
  <section>
    <p>The <code>data-[name]</code> attribute on elements can now be accessed directly via the DOM using <code>element.dataset.[attr]</code>.</p>
    <p>Try openning the Web Console and editing <code>element.dataset</code> directly: <br /><code>element.dataset.foo = 'bar';</code></p>
  </section>
  <p id="status">Not connected</p>
  <section>
    <div id="test" data-name="rem" data-height="short">This element has data</div>
    <input type="button" value="Show data" id="show" />
    <input type="button" value="Change data via dataset" id="change1" />
    <input type="button" value="change data via setAttribute" id="change2" />
  </section>
  <pre><code id="element">[click buttons above to show element html]</code></pre>
</article>
<script>
(function () {

function show() {
  code.innerHTML = test.outerHTML.replace(/[<>]/g, function (m) {
    return { '<': '<', '>': '>' }[m];
  });
  
  for (var prop in test.dataset) {
    code.innerHTML += '\nel.dataset.' + prop + ' = "' + test.dataset[prop] + '"';
  }
}

var state = document.getElementById('status'),
    code = document.getElementById('element');
    
var test = window.element = document.getElementById('test');

if (test.dataset === undefined) {
  state.innerHTML = 'dataset not supported';
  state.className = 'fail';
} else {
  state.className = 'success';
  state.innerHTML = 'element.dataset supported';
}

addEvent(document.getElementById('show'), 'click', function () {
  show();
});

addEvent(document.getElementById('change1'), 'click', function () {
  test.dataset.name = 'via el.dataset';
  show();
});
  
addEvent(document.getElementById('change2'), 'click', function () {
  test.setAttribute('data-name', 'via setAttribute');
  show();  
});


})();
</script>
    <footer><a href="/">HTML5 demos</a>/<a id="built" href="http://twitter.com/rem">@rem built this</a>/<a href="#view-source">view source</a></footer> 
</section>
<a href="http://github.com/remy/html5demos"><img style="position: absolute; top: 0; left: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_left_darkblue_121621.png" alt="Fork me on GitHub" /></a>
<script src="/js/prettify.packed.js"></script>
<script>
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script>
try {
var pageTracker = _gat._getTracker("UA-1656750-18");
pageTracker._trackPageview();
} catch(err) {}</script>
</body>
</html>"""

val page2 =
"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset=utf-8 />
<meta name="viewport" content="width=620" />
<title>HTML5 Demo: HTML5 History API</title>
<link rel="stylesheet" href="/css/html5demos.css" type="text/css" />
<script src="/js/h5utils.js"></script></head>
<body>
<section id="wrapper">
    <header>
      <h1>HTML5 History API</h1>
    </header>
<style>
#examples {
  padding-left: 20px;
}
#examples li {
  list-style: square;
  padding: 0;
  margin: 0;
}
</style>
<article>
  <p id="status">HTML5 History API not supported</p>
  <p><em>Last event fired: <span id="lastevent">(none)</span></em></p>
  <p>To test the History API, click through the urls below. Note that none of these urls point to <em>real</em> pages. JavaScript will intercept these clicks, load data and the browser address bar will <em>appear</em> to change - but this is the History API in action!</p>
  <p>Use the back and forward buttons in your browser to navigate the history.</p>
  <ul id="examples">
    <li><a href="/history/first">first</a></li>
    <li><a href="/history/second">second</a></li>
    <li><a href="/history/third">third</a></li>
    <li><a href="/history/fourth">fourth</a></li>
  </ul>
  <p><small>Note: since these urls aren't real, refreshing the page will land on an invalid url.</small></p>
  <div id="output"></div>
</article>
<script>
var $ = function (s) { return document.getElementById(s); },
    state = $('status'),
    lastevent = $('lastevent'),
    urlhistory = $('urlhistory'),
    examples = $('examples'),
    output = $('output'),
    template = '<p>URL: <strong>{url}</strong>, name: <strong>{name}</strong>, location: <strong>{location}</strong></p>',
    data = { // imagine these are ajax requests :)
      first : {
        name: "Remy",
        location: "Brighton, UK"
      },
      second: {
        name: "John",
        location: "San Francisco, USA"
      },
      third: {
        name: "Jeff",
        location: "Vancover, Canada"
      },
      fourth: {
        name: "Simon",
        location: "London, UK"
      }
    };

function reportEvent(event) {
  lastevent.innerHTML = event.type;
}

function reportData(data) {
  output.innerHTML = template.replace(/(:?\{(.*?)\})/g, function (a,b,c) {
    return data[c];
  });
}

if (typeof history.pushState === 'undefined') {
  state.className = 'fail';
} else {
  state.className = 'success';
  state.innerHTML = 'HTML5 History API available';
}

addEvent(examples, 'click', function (event) {
  var title;
  
  event.preventDefault();
  if (event.target.nodeName == 'A') {
    title = event.target.innerHTML;
    data[title].url = event.target.getAttribute('href'); // slightly hacky (the setting), using getAttribute to keep it short
    history.pushState(data[title], title, event.target.href);
    reportData(data[title]);
  }
});

addEvent(window, 'popstate', function (event) {
  var data = event.state;
  reportEvent(event);
  reportData(event.state || { url: "unknown", name: "undefined", location: "undefined" });
});

addEvent(window, 'hashchange', function (event) {
  reportEvent(event);

  // we won't do this for now - let's stay focused on states
  /*
  if (event.newURL) {
    urlhistory.innerHTML = event.oldURL;
  } else {
    urlhistory.innerHTML = "no support for <code>event.newURL/oldURL</code>";
  }
  */
});

addEvent(window, 'pageshow', function (event) {
  reportEvent(event);
});

addEvent(window, 'pagehide', function (event) {
  reportEvent(event);
});

</script>    <footer><a href="/">HTML5 demos</a>/<a id="built" href="http://twitter.com/rem">@rem built this</a>/<a href="#view-source">view source</a></footer> 
</section>
<a href="http://github.com/remy/html5demos"><img style="position: absolute; top: 0; left: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_left_darkblue_121621.png" alt="Fork me on GitHub" /></a>
<script src="/js/prettify.packed.js"></script>
<script>
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script>
try {
var pageTracker = _gat._getTracker("UA-1656750-18");
pageTracker._trackPageview();
} catch(err) {}</script>
</body>
</html>"""

val page3 =
"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset=utf-8 />
<meta name="viewport" content="width=620" />
<title>HTML5 Demo: HTML5 History API</title>
<link rel="stylesheet" href="/css/html5demos.css" type="text/css" />
<script src="/js/h5utils.js"></script></head>
<body>
<section id="wrapper">
    <header>
      <h1>HTML5 History API</h1>
    </header>
<style>
#examples {
  padding-left: 20px;
}
#examples li {
  list-style: square;
  padding: 0;
  margin: 0;
}
</style>
<article>
  <p id="status">HTML5 History API not supported</p>
  <p><em>Last event fired: <span id="lastevent">(none)</span></em></p>
  <p>To test the History API, click through the urls below. Note that none of these urls point to <em>real</em> pages. JavaScript will intercept these clicks, load data and the browser address bar will <em>appear</em> to change - but this is the History API in action!</p>
  <p>Use the back and forward buttons in your browser to navigate the history.</p>
  <ul id="examples">
    <li><a href="/history/first">first</a></li>
    <li><a href="/history/second">second</a></li>
    <li><a href="/history/third">third</a></li>
    <li><a href="/history/fourth">fourth</a></li>
  </ul>
  <p><small>Note: since these urls aren't real, refreshing the page will land on an invalid url.</small></p>
  <div id="output"></div>
</article>
<div
<script>
var $ = function (s) { return document.getElementById(s); },
    state = $('status'),
    lastevent = $('lastevent'),
    urlhistory = $('urlhistory'),
    examples = $('examples'),
    output = $('output'),
    template = '<p>URL: <strong>{url}</strong>, name: <strong>{name}</strong>, location: <strong>{location}</strong></p>',
    data = { // imagine these are ajax requests :)
      first : {
        name: "Remy",
        location: "Brighton, UK"
      },
      second: {
        name: "John",
        location: "San Francisco, USA"
      },
      third: {
        name: "Jeff",
        location: "Vancover, Canada"
      },
      fourth: {
        name: "Simon",
        location: "London, UK"
      }
    };

function reportEvent(event) {
  lastevent.innerHTML = event.type;
}

function reportData(data) {
  output.innerHTML = template.replace(/(:?\{(.*?)\})/g, function (a,b,c) {
    return data[c];
  });
}

if (typeof history.pushState === 'undefined') {
  state.className = 'fail';
} else {
  state.className = 'success';
  state.innerHTML = 'HTML5 History API available';
}

addEvent(examples, 'click', function (event) {
  var title;
  
  event.preventDefault();
  if (event.target.nodeName == 'A') {
    title = event.target.innerHTML;
    data[title].url = event.target.getAttribute('href'); // slightly hacky (the setting), using getAttribute to keep it short
    history.pushState(data[title], title, event.target.href);
    reportData(data[title]);
  }
});

addEvent(window, 'popstate', function (event) {
  var data = event.state;
  reportEvent(event);
  reportData(event.state || { url: "unknown", name: "undefined", location: "undefined" });
});

addEvent(window, 'hashchange', function (event) {
  reportEvent(event);

  // we won't do this for now - let's stay focused on states
  /*
  if (event.newURL) {
    urlhistory.innerHTML = event.oldURL;
  } else {
    urlhistory.innerHTML = "no support for <code>event.newURL/oldURL</code>";
  }
  */
});

addEvent(window, 'pageshow', function (event) {
  reportEvent(event);
});

addEvent(window, 'pagehide', function (event) {
  reportEvent(event);
});

</script>    <footer><a href="/">HTML5 demos</a>/<a id="built" href="http://twitter.com/rem">@rem built this</a>/<a href="#view-source">view source</a></footer> 
</section>
<a href="http://github.com/remy/html5demos"><img style="position: absolute; top: 0; left: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_left_darkblue_121621.png" alt="Fork me on GitHub" /></a>
<script src="/js/prettify.packed.js"></script>
<script>
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script>
try {
var pageTracker = _gat._getTracker("UA-1656750-18");
pageTracker._trackPageview();
} catch(err) {}</script>
</body>
</html>"""



}
class Html5ParserSpecTest extends _root_.org.specs.runner.JUnit4(Html5ParserSpec)

}
}
