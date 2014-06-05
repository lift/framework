# Simply Lift

> David Pollak   
> February 19, 2013
> <hr>
> Copyright © 2010-2013 by David Pollak   
> <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/deed.en_US"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/3.0/88x31.png" /></a>
> <br >
> Simply Lift</span> by <a href="http://simply.liftweb.net" rel="cc:attributionURL">http://simply.liftweb.net</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/deed.en_US">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.<br >Based on a work at <a  href="https://github.com/lift/framework" rel="dct:source">https://github.com/lift/framework</a>.


## The Lift Web Framework

### Introduction

The Lift Web Framework provides web application developers tools to make writing security, interacting, scalable web applications easier than with any other web framework. After reading Part I of this book, you should understand Lift's core concepts and be able to write Lift applications. But with anything, practice is important. I have been writing Lift and Scala for 6 years, and even I learn new things about the language and the framework on a weekly basis. Please consider Lift an path and an exploration, rather than an end point.

“Yo, David, stop yer yappin'. I'm coming from Rails|Spring|Struts|Django and I want to get started super fast with Lift.” <a href="#From-MVC">See From MVC</a>.

Lift is built on top of the [Scala](http://scala-lang.org) programming language. Scala runs on the [Java Virtual Machine](http://www.oracle.com/technetwork/java/index.html). Lift applications are typically packaged as [http://en.wikipedia.org/wiki/WAR_(Sun_file_format)||WAR] files and run as a [http://www.oracle.com/technetwork/java/index-jsp-135475.html||J/EE Servlets] or Servlet Filters. This book will provide you with the core concepts you need to successfully write Lift web applications. The book assumes knowledge of Servlets and Servlet containers, the Scala Language (Chapters 1-6 of [http://apress.com/book/view/9781430219897||Beginning Scala] gives you a good grounding in the language), build tools, program editors, web development including HTML and JavaScript, etc. Further, this book will not explore persistence. Lift has additional modules for persisting to relational and non-relational data stores. Lift doesn't distinguish as to how an object is materialized into the address space... Lift can treat any object any old way you want. There are many resources (including [http://exploring.liftweb.net/||Exploring Lift]) that cover ways to persist data from a JVM.

Lift is different from most web frameworks and it's likely that Lift's differences will present a challenge and a friction if you are familiar with the MVCMVC school of web frameworksThis includes Ruby on Rails, Struts, Java Server Faces, Django, TurboGears, etc.. But Lift is different and Lift's differences give you more power to create interactive applications. Lift's differences lead to more concise web applications. Lift's differences result in more secure and scalable applications. Lift's differences let you be more productive and make maintaining applications easier for the future you or whoever is writing your applications. Please relax and work to understand Lift's differences... and see how you can make best use of Lift's features to build your web applications.

Lift creates abstractions that allow easier expression of business logic and then maps those abstractions to HTTP and HTML. This approach differs from traditional web frameworks which build abstractions on top of HTTP and HTML and require the developer to bridge between common business logic patterns and the underlying protocol. The difference means that you spend more time thinking about your application and less time thinking about the plumbing.

I am a “concept learner.” I learn concepts and then apply them over and over again as situations come up. This book focuses a lot on the concepts. If you're a concept learner and like my stream on conciousness style, this book will likely suit you well. On the other hand, it may not.

Up to date versions of this book are available in PDF form at http://simply.liftweb.net/Simply_Lift.pdf. The source code for this book is available at [https://github.com/dpp/simply_lift||https://github.com/dpp/simply_lift].

If you've got questions, feedback, or improvements to this document, please join the conversation on the [http://groups.google.com/group/liftweb||Lift Google Group].

I'm a “roll up your sleaves and get your hands dirty with code” kinda guy... so let's build a simple Chat application in Lift. This application will allow us to demonstrate some of Lift's core features as well as giving a “smack in the face” demonstration of how Lift is different.

### The ubiquitous Chat app

Writing a multi-user chat application in Lift is super-simple and illustrates many of Lift's core concepts.

The Source Code can be found at [https://github.com/dpp/simply_lift/tree/master/chat].

#### The View

When writing a Lift app, it's often best to start off with the user interface... build what the user will see and then add behavior to the HTML page. So, let's look at the Lift template that will make up our chat application.



It's a valid HTML page, but there are some hinky looking class attributes. The first one is <body class="lift:content_id=main">. The class in this case says “the actual page content is contained by the element with id='main'.” This allows you to have valid HTML pages for each of your templates, but dynamically add “chrome” around the content based on one or more chrome templates.

Let's look at the <div id="main">. It's got a funky class as well: lift:surround?with=default;at=content. This class invokes a snippet which surrounds the <div> with the default template and inserts the <div> and its children at the element with id “content” in the default template. Or, it wraps the default chrome around the <div>. For more on snippets, see [sec:Snippets].

Next, we define how we associate dynamic behavior with the list of chat elements: <div class="lift:comet?type=Chat">. The “comet” snippet looks for a class named Chat that extends CometActor and enables the mechanics of pushing content from the CometActor to the browser when the state of the CometActor changes. 

#### The Chat Comet component

The [http://en.wikipedia.org/wiki/Actor_model||Actor Model] provides state in functional languages include Erlang. Lift has an Actor library and LiftActors (see [sec:LiftActor]) provides a powerful state and concurrency model. This may all seem abstract, so let's look at the Chat class.



The Chat component has private state, registers with the ChatServer, handles incoming messages and can render itself. Let's look at each of those pieces.

The private state, like any private state in prototypical object oriented code, is the state that defines the object's behavior.

registerWith is a method that defines what component to register the Chat component with. Registration is a part of the Listener (or [http://en.wikipedia.org/wiki/Observer_pattern||Observer]) pattern. We'll look at the definition of the ChatServer in a minute.

The lowPriority method defines how to process incoming messages. In this case, we're Pattern Matching (see [sec:Pattern-Matching]) the incoming message and if it's a Vector[String], then we perform the action of setting our local state to the Vector and re-rendering the component. The re-rendering will force the changes out to any browser that is displaying the component.

We define how to render the component by defining the CSS to match and the replacement (See [sec:CSS-Selector-Transforms]). We match all the <li> tags of the template and for each message, create an <li> tag with the child nodes set to the message. Additionally, we clear all the elements that have the clearable in the class attribute.

That's it for the Chat CometActor component.

#### The ChatServer

The ChatServer code is:



The ChatServer is defined as an object rather than a class. This makes it a singleton which can be referenced by the name ChatServer anywhere in the application. Scala's singletons differ from Java's static in that the singleton is an instance of an object and that instance can be passed around like any other instance. This is why we can return the ChatServer instance from the registerWith method in that Chat component.

The ChatServer has private state, a Vector[String] representing the list of chat messages. Note that Scala's type inferencer infers the type of msgs so you do not have to explicitly define it.

The createUpdate method generates an update to send to listeners. This update is sent when a listener registers with the ChatServer or when the updateListeners() method is invoked.

Finally, the lowPriority method defines the messages that this component can handle. If the ChatServer receives a String as a message, it appends the String to the Vector of messages and updates listeners.

#### User Input

Let's go back to the view and see how the behavior is defined for adding lines to the chat.

<form class="lift:form.ajax"> defines an input form and the form.ajax snippet turns a form into an Ajax (see [sec:Ajax]) form that will be submitted back to the server without causing a full page load.

Next, we define the input form element: <input class="lift:ChatIn" id="chat_in"/>. It's a plain old input form, but we've told Lift to modify the <input>'s behavior by calling the ChatIn snippet.

#### Chat In

The ChatIn snippet (See [sec:Snippets]) is defined as:



The code is very simple. The snippet is defined as a method that associates a function with form element submission, onSubmit. When the element is submitted, be that normal form submission, Ajax, or whatever, the function is applied to the value of the form. In English, when the user submits the form, the function is called with the user's input.

The function sends the input as a message to the ChatServer and returns JavaScript that sets the value of the input box to a blank string.

#### Running it

Running the application is easy. Make sure you've got Java 1.6 or better installed on your machine. Change directories into the chat directory and type sbt update ~jetty-run. The Simple Build Tool will download all necessary dependencies, compile the program and run it.

You can point a couple of browsers to http://localhost:8080 and start chatting.

Oh, and for fun, try entering <script>alert('I ownz your browser');<script> and see what happens. You'll note it's what you want to happen.

#### What you don't see

Excluding imports and comments, there are about 20 lines of Scala code to implement a multi-threaded, multi-user chat application. That's not a lot.

The first thing that's missing is synchronization or other explicit forms of thread locking. The application takes advantage of Actors and immutable data structures, thus the developer can focus on the business logic rather than the threading and locking primatives.

The next thing that's missing is routing and controllers and other stuff that you might have to do to wire up Ajax calls and polling for server-side changes (long or otherwise). In our application, we associated behavior with display and Lift took care of the rest (see [sec:Page-rewriting]).

We didn't do anything to explicitly to avoid cross-site scripting in our application. Because Lift takes advantage of Scala's strong typing and type safety (see [sec:Type-safety]), Lift knows the difference between a String that must be HTML encoded and an HTML element that's already properly encoded. By default, Lift applications are resistant to many of the OWASP top 10 security vulnerabilities (see [sec:Security]).

This example shows many of Lift's strengths. Let's expand the application and see how Lift's strengths continue to support the development of the application.









Chapter 7 Core Concepts



7.2 Box/Option

Scala has a ton of nice features. One of the features that I was slow to adopt, until Burak Emir gently reminded me a bunch of times, is "Options". Read on about Options, Boxes, and how Lift makes good use of them to make clean, error resistant code. If you come from an imperative (Java, Ruby) background, you'll probably recognize the following code:

x = someOperation 

Okay, so that's pseudo-code, but there are tons of operation, guard, operation, guard, blah blah constructs.

Further, null/nil are passed around as failures. This is especially bad when it's null, but it's pretty bad when it's nil because it's not clear to the consumer of the API that there can be a "call failed" return value.

In Java, null is a non-object. It has no methods. It is the exception to the statically typed rule (null has no class, but any reference of any class can be set to null.) Invoking a method on null has one and only one result: an exception is thrown. null is often returned from methods as a flag indicating that the method ran successfully, but yielded no meaningful value. For example, CardHolder.findByCreditCardNumber("2222222222") In fact, the guy who invented null called it a [http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare||billion dollar mistake].

Ruby has nil which is marginally better than null. nil is a real, singleton object. There's only one instance of nil in the whole system. It has methods. It is a subclass of Object. Object has a method called "nil?" which returns false, except the nil singleton overrides this method to return true. nil is returned much like null in Java. It's the "no valid answer" answer.

Scala does something different.

There's an abstract class, called Option. Options are strongly typed. They are declared Option[T]. This means an Option can be of any type, but once its type is defined, it does not change. There are two subclasses of Option: Some and None. None is a singleton (like nil). Some is a container around the actual answer. So, you might have a method that looks like:

def findUser(name: String): Option[User] = {  

Some, you've got a findUser method that returns either Some(User) or None. So far, it doesn't look a lot different than our example above. So, to confuse everyone, I'm going to talk about collections for a minute.

A really nice thing in Scala (yes, Ruby has this too) is rich list operations. Rather than creating a counter and pulling list (array) elements out one by one, you write a little function and pass that function to the list. The list calls the function with each element and returns a new list with the values returned from each call. It's easier to see it in code:

    scala> List(1,2,3).map(x => x * 2)

The above code multiplies each list item by two and "map" returns the resulting list. Oh, and you can be more terse, if you want:

    scala> List(1,2,3).map(_ * 2)

You can nest map operations:

    scala> List(1,2,3).map(x => List(4,5,6).map(y => x * y))

And, you can "flatten" the inner list:

scala> List(1,2,3).flatMap(x => List(4,5,6).map(y => x * y))Finally, you can "filter" only the even numbers from the first list:

scala> List(1,2,3).filter(_ % 2 == 0). flatMap(x => List(4,5,6).map(y => x * y))But, as you can see, the map/flatMap/filter stuff gets pretty verbose. Scala introduced a "for" comprehension to make the code more readable:

scala> for { 

Okay, but what does this have to do with Option[T]?

Turns out that Option implements map, flatMap, and filter (the methods necessary for the Scala compiler to use in the 'for' comprehension). Just as a side note, when I first encountered the phrase "'for' comprehension", I got scared. I've been doing programming for years and never heard of a "comprenhension" let alone a 'for' one. Turns out, that there's nothing fancy going on, but "'for' comprehension" is just a term of art for the above construct.

So, the cool thing is that you can use this construct very effectively. The first example is simple:

scala> for {x <- Some(3); y <- Some(4)} yield x * y 

"That's nice, you just wrote a lot of code to multiply 3 by 4."

Let's see what happens if we have a "None" in there:

scala> val yOpt: Option[Int] = None

So, we get a "None" back. How do we turn this into a default value?

scala> (for {x <- Some(3); y <- yOpt} yield x * y) getOrElse -1 

scala> (for {x <- Some(3); y <- Some(4)} yield x * y) getOrElse -1

Note that the "getOrElse" code is "passed by name". Put another way, that code is only executed if the "else" clause is valid.

Lift has an analogous construct called Box.

A Box can be Full or not. A non-Full Box can be the Empty singleton or a Failure. A Failure carries around information about why the Box contains no value.

Failure is very helpful because you can carry around information to display an error... an HTTP response code, a message, what have you.

In Lift, I put this all together in the following way:

• methods that return request parameters return Box[String] 

• finder methods on models (not find all, just the ones that return a single instance) return Box[Model]

• any method that would have returned a null if I was writing in Java returns a Box[T] in Lift 

That means you get code that looks like:

scala> for {id <- S.param("id") ?~ "id param missing"

There's no explicit guard/test to see if the "id" parameter was passed in and there's no explicit test to see if the user was found.

Note also that this code is completely type-safe. While there was no explicit type declarations, the compiler was able to figure out what types the various objects were.

So, let's look at the code inside a REST handler:

serve { 

If the id parameter is missing, present a nice error message and return a 401 (okay... this is random, but you get the point). And by default, if the user isn't found, return a 404 with the error that the user isn't found.

Here's what it looks like using wget:

dpp@bison:~/lift_sbt_prototype$ wget http://localhost:8080/user/info.xml

One more thing about Box and Option... they lead to less complex, more maintainable code. Even if you didn't know anything about Scala or Lift, you can read the XML serving code and the console exchange and figure out what happened any why it happened. This is a lot more readable than deeply nested if statements. And if it's readable, it's maintainable.

I hope this is an understandable introduction to Scala's Option class and 'for' comprehension and how Lift makes use of these tools.

7.3 S/SHtml

7.4 Boot

7.5 SiteMap

7.6 GUIDs

A core concept in Lift is GUIDs. GUIDs are globally unique identifiers used to associate something in the browser with a function on the server. GUIDs make Lift more secure because they make replay attacks very difficult and GUIDs make it easier to develop complex, stateful, interactive applications because the developer spends more time on business logic and less time on the plumbing of it.

7.6.1 How GUIDs are generated

7.6.2 Where they are used 

7.7 LiftRules

7.8 SessionVars and RequestVars

7.9 Helpers



7.11 Client-side behavior invoking server-side functions

7.12 Ajax

7.13 Comet

7.14 LiftActor

7.15 Pattern Matching

7.16 Type safety

7.17 Page rewriting

7.18 Security

Chapter 8 Common Patterns





8.2 Dependency Injection

Dependency injection is an important topic in the Java world. It's important because Java lacks certain basic features (e.g., functions) that tend to bind abstract interfaces to concrete implementations. Basically, it's so much easier to do MyInterface thing = new MyInterfaceImpl(), so most developers do just that. 

Scala's [http://scala.sygneca.com/patterns/component-mixins||cake pattern] goes a long way to help developers compose complex behaviors by combining Scala traits. Jonas Bonér wrote an excellent piece on [http://jonasboner.com/2008/10/06/real-world-scala-dependency-injection-di.html||Dependency Injection].

The cake pattern only goes half way to giving a Java developer complete dependency injection functionality. The cake pattern allows you to compose the complex classes out of Scala traits, but the cake pattern is less helpful in terms of allowing you to make dynamic choices about which combination of cake to vend in a given situation. Lift provides extra features that complete the dependency injection puzzle. 

8.2.1 Lift Libraries and Injector

Lift is both a web framework and a set of Scala libraries. Lift's common, actor, json, and util packages provide common libraries for Scala developers to build their application. Lift's libraries are well tested, widely used, well supported, and released on a well defined schedule (montly milestones, quarterly releases). 

Lift's Injector trait forms the basis of dependency injection: 

/** 

You can use this trait as follows: 

object MyInjector extends Injector {...}

The reason that the instance of MyThing is in a Box is because we're not guaranteed that MyInjector knows how to create an instance of Thing. Lift provides an implementation of Injector called SimpleInjector that allows you to register (and re-register) functions for injection:

object MyInjector extends SimpleInjector

This isn't bad... it allows us to define a function that makes the injection-time decision, and we can change the function out during runtime (or test-time.) However, there are two problems: getting Boxes for each injection is less than optimal. Further, globally scoped functions mean you have to put a whole bunch of logic (test vs. production vs. xxx) into the function. SimpleInjector has lots of ways to help out.

object MyInjector extends SimpleInjector {

Inject has a futher trick up its sleave... with Inject, you can scope the function... this is helpful for testing and if you need to change behavior for a particular call scope:

MyInjector.thing.doWith(new Thing with SpecialThing {}) { 

Within the scope of the doWith call, MyInjector.thing will vend instances of SpecialThing. This is useful for testing as well as changing behavior within the scope of the call or globally. This gives us much of the functionality we get with dependency injection packages for Java. But within Lift WebKit, it gets better.

8.2.2 Lift WebKit and enhanced injection scoping 

Lift's WebKit offers broad ranging tools for handling HTTP requests as well as HTML manipulation.

Lift WebKit's Factory extends SimpleInjector, but adds the ability to scope the function based on current HTTP request or the current container session:

object MyInjector extends Factory {  

WebKit's LiftRules is a Factory and many of the properties that LiftRules contains are FactoryMakers. This means that you can change behavior during call scope (useful for testing):

LiftRules.convertToEntity.doWith(true) { ... test that we convert certain characters to entities} 

Or based on the current request (for example you can change the rules for calculating the docType during the current request):

if (isMobileReqest) LiftRules.docType.request.set((r: Req) => Full(DocType.xhtmlMobile))

Or based on the current session (for example, changing maxConcurrentRequests based on some rules when a session is created):

if (browserIsSomethingElse) LiftRules.maxConcurrentRequests.session.set((r: Req) => 32) 

8.2.3 Conclusion 

Lift's SimpleInjector/Factory facilities provide a powerful and flexible mechanism for vending instances based on a global function, call stack scoping, request and session scoping and provides more flexible features than most Java-based dependency injection frameworks without resorting to XML for configuration or byte-code rewriting magic.

8.3 Modules

Lift has supported modules from the first version of the project in 2007. Lift's entire handling of the HTTP request/response cycle is open to hooks. Further, Lift's templating mechanism where resulting HTML pages are composed by transforming page content via snippets (See [sec:Snippets]) which are simply functions that take HTML and return HTML: NodeSeq => NodeSeq. Because Lift's snippet resolution mechanism is open and any code referenced in Boot (See [sec:Boot]), any code can be a Lift “module” by virtue of registering its snippets and other resources in LiftRules. Many Lift modules already exist including PayPal, OAuth, OpenID, LDAP, and even a module containing many jQuery widgets.

The most difficult issue relating to integration of external modules into Lift is how to properly insert the module's menu items into a SiteMap (See [sec:SiteMap-1]) menu hierarchy. Lift 2.2 introduces a more flexible mechanism for mutating the SiteMap: SiteMap mutators. SiteMap mutators are functions that rewrite the SiteMap based on rules for where to insert the module's menus in the menu hierarchy. Each module may publish markers. For example, here are the markers for ProtoUser:

/**

The module also makes a SiteMap mutator available, this can either be returned from the module's init method or via some other method on the module. ProtoUser makes the sitemapMutator method available which returns a SiteMap => SiteMap.

The application can add the marker to the appropriate menu item:

Menu("Home") / "index" >> User.AddUserMenusAfter

And when the application registers the SiteMap with LiftRules, it applies the mutator:

LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

Because the mutators are composable:

val allMutators = User.sitemapMutator andThen FruitBat.sitemapMutator

For each module, the implementation of the mutators is pretty simple:

  private lazy val AfterUnapply = SiteMap.buildMenuMatcher(_ == AddUserMenusAfter)

We've defined some extractors that help with pattern matching. SiteMap.buildMenuMatcher is a helper method to make building the extractors super-simple. Then we supply a PartialFunction[Menu, List[Menu]] which looks for the marker LocParam and re-writes the menu based on the marker. If there are no matches, the additional rule is fired, in this case, we append the menus at the end of the SiteMap.



Chapter 9 Built-in Snippets

9.1 CSS

9.2 Msgs

9.3 Msg

9.4 Menu

9.5 A

9.6 Children

9.7 Comet

9.8 Form

9.9 Ignore

9.10 Loc

9.11 Surround

9.12 TestCond

9.13 Embed

9.14 Tail

9.15 WithParam

9.16 VersionInfo

9.17 SkipDocType

9.18 XmlGroup

9.19 LazyLoad

9.20 WithResourceId

Chapter 10 SiteMap

Chapter 11 REST

Lift makes providing REST-style web services very simple.

First, create an object that extends RestHelper:

import net.liftweb.http._

And hook your changes up to Lift in Boot.scala:

LiftRules.dispatch.append(MyRest) // stateful -- associated with a servlet container session

Within your MyRest object, you can define which URLs to serve:

serve {  

The above code uses the suffix of the request to determine the response type. Lift supports testing the Accept header for a response type:

serve {  

The above can also be written:

serve { 

Note: If you want to navigate your Web Service, you must remember to add a *.xml or *.json (depending in what you have implemented) at the end of the URL: http://localhost:8080/XXX/api/static/call.json http://localhost:8080/XXX/api/static/call.xml 

Because the REST dispatch code is based on Scala’s pattern matching, we can extract elements from the request (in this case the third element will be extracted into the id variable which is a String:

serve { And with extractors, we convert an element to a particular type and only succeed with the pattern match (and the dispatch) if the parameter can be converted. For example:

serve { 

In the above example, id is extracted if it can be converted to a Long.

Lift’s REST helper can also extract XML or JSON from a POST or PUT request and only dispatch the request if the XML or JSON is valid:

serve { 

There may be cases when you want to have a single piece of business logic to calculate a value, but then convert the value to a result based on the request type. That’s where serveJx comes in … it’ll serve a response for JSON and XML requests. If you define a trait called Convertable:

trait Convertable {

Then define a pattern that will convert from a Convertable to a JSON or XML:

implicit def cvt: JxCvtPF[Convertable] = { case (JsonSelect, c, _) => c.toJson case (XmlSelect, c, _) => c.toXml } 

And anywhere you use serveJx and your pattern results in a Box[Convertable], the cvt pattern is used to generate the appropriate response:

serveJx { 

Or:

// extract the parameters, create a user 

In the above example, if the firstname parameter is missing, the response will be a 400 with the response body “firstname parameter missing”. If the lastname parameter is missing, the response will be a 404 with the response body “lastname parameter missing”.

Chapter 12 MVC (If you really want it)



Part II Recipes





Chapter 19 Embedding JavaScript in an HTML page

19.1 Problem

What am I doing wrong? I'm trying to output a javascript object into the page (so my front end guy can do some stuff with the data without parsing it out of elements by id) but it's replacing all the double quotes with &quot; (only in view source - if I inspect it then firebug converts them to double quotes again)

I've copied the example from Exploring Lift, but it still does the same:

& ".data_as_object *" #> {

Becomes:

<div class="data_as_object" style="display: none;">var myObject =

I've noticed that if what I'm outputting is a number rather than a string then it's fine.

19.2 Solution

Try:

& ".data_as_object *" #> {

JsExp are also Nodes, so they render out, but they render out escaped. Putting Script() around them turns them into:

<script>

Part III Questions and Answers

Chapter 20 Scaling

Lift is a web framework built on the Scala programming language. Lift takes advantage of many of Scala's features that allow developers to very concisely code secure, scalable, highly interactive web applications. Lift provides a full set of layered abstractions on top of HTTP and HTML from "close to the metal" REST abstractions up to transportation agnostic server push (Comet) support. Scala compiles to JVM byte-code and is compatible with Java libraries and the Java object model. Lift applications are typically deployed as WAR files in J/EE web containers... Lift apps run in Tomcat, Jetty, Glassfish, etc. just like any other J/EE web application. Lift apps can generally be monitored and managed just like any Java web app. Web Applications, Sessions, and State. All web applications are stateful in one way or another. Even a "static" web site is made up of the files that are served... the application's state is defined in those files. The site content may be served out of a database, but the content served does not depend on identity of the user or anything about the HTTP request except the contents of the HTTP request. These contents can include the URI, parameters, and headers. The complete value of the response can be calculated from the request without referencing any resources except the content resources. For the purpose of this discussion, I will refer to these as session-less requests. News sites like the UK Guardian, MSNBC, and others are prototypical examples of this kind of site. Sessions. Some applications are customized on a user-by-user basis. These applications include the likes of Foursquare and others where many HTTP requests make up a "session" in which the results of previous HTTP requests change the behavior of future HTTP requests. Put in concrete terms, a user can log into a site and for some duration, the responses are specific to that user. There are many mechanisms for managing sessions, but the most common and secure method is creating a cryptographically unique token (a session id), and putting that token in the Set-Cookie response header such that the browser will present that Cookie in subsequent HTTP requests for a certain period of time. The server-side state is referenced by the Cookie and the state is made available to the web application during the scope of servicing the request and any mutations the web app makes to session state during the request are kept on the server and are available to the application in subsequent requests. Another available technique for managing state is to serialize application state in the Cookie and deliver it to the browser such that the server is not responsible for managing state across requests. As we've recently discovered, this is a tremendously insecure way to manage application state. Further, for any moderately complex application, the amount of data the needs to be transferred as part of each request and response is huge. Migratory Sessions. Many web application managers allow for server-managed sessions to migrate across a cluster of web application servers. In some environments such as Ruby on Rails, this is a hard requirement because only one request at a time can be served per process, thus for any moderate traffic site, there must be multiple processes serving pages. There are many strategies for migrating state across processes: storing state on disk, in memcached, in a database (relational or NoSQL), or having some proprietary cluster communications protocol. In any of these scenarios sessions can migrate across the grid of processes serving requests for a given web application. Web applications that support migratory state are often referred to as "stateless" because the session state does not reside in the same process as the web application. Session Affinity. Some applications require that all requests related to a particular session are routed to the same process and that process keeps session-related content in local memory. In a cluster, there are multiple mechanisms for achieving session affinity... the two most popular being HAProxy and Nginx. Availability, Scalability, Security, Performance, and User Experience. There are many vectors on which to measure the overall-quality of a web application. Let's take a quick peek at each one. Availability. Availability of an application is the amount of time it gives a meaningful response to a request. Highly available applications generally span multiple pieces of hardware and often multiple data centers. Highly available applications are also typically available during upgrades of part of the system that makes up the application. Highly available applications have very few single points of failure and those single points of failure are usually deployed on very reliable hardware. Scalability. A scalable application can, within certain bounds, respond with similar performance to increased load by adding hardware to process more load. No system is infinitely or linearly scalable. However, many systems have grossly disproportionate load demands such that, for example, you can add a lot of web application front-ends to a Rails application before there's enough load on the back-end RDBMS such that scaling is impaired.

Security. The Internet is a dangerous place and no request that is received from the Internet can be trusted. Applications, frameworks, systems and everything else must be designed to be secure and resist attacks. The most common attacks on web application are listed in the OWASP Top Ten. Performance. Web application performance can be measured on two vectors: response time to a request and system resources required to service the request. These two vectors are inter-dependent. User Experience. The user experience of a web app is an important measure of its quality. User experience can be measured on many different vectors including perceived responsiveness, visual design, interactivity, lack of "hicups", etc. Ultimately, because we're building applications for users, the user experience is very important. Lift's trade-offs. Given the number and complexity related to the quality of a web application, there are a lot of trade-offs, implicit and explicit, to building a framework that allows developers and business people to deliver a great user experience. Let's talk for a minute about what Lift is and what it isn't. Lift is a web framework. It provides a set of abstractions over HTTP and HTML such that developers can write excellent web applications. Lift is persistence agnostic. You can use Lift with relational databases, file systems, NoSQL data stores, mule carts, etc. As long as you can materialize an object into the JVM where Lift is running, Lift can make use of that object. Lift sits on top of the JVM. Lift applications execute in the Java Virtual Machine. The JVM is a very high performance computing system. There are raging debates as to the relative performance of JVM code and native machine code. No matter which benchmarks you look at, the JVM is a very fast performer. Lift apps take advantage of the JVM's performance characteristics. Moderately complex Lift apps that access the database can serve 1,000+ requests per second on quad-core Intel hardware. Even very complex Lift apps that make many back-end calls per request can serve hundreds of requests per second on EC2 large instances. Lift as proxy. Many web applications, typically REST applications, provide a very thin layer on top of a backing data store. The web application serves a few basic functions to broker between the HTTP request and the backing store. These functions include: request and parameter validation, authentication, parameter unpacking, back-end service request, and translation of response data to wire format (typically XML or JSON). Lift can service these kinds of requests within the scope of a session or without any session at all, depending on application design. For more information on Lift's REST features, see Lift RestHelper. When running these kinds of services, Lift apps can be treated without regard for session affinity. Lift as HTML generator. Lift has a powerful and secure templating mechanism. All Lift templates are expressed as valid XML and during the rendering process, Lift keeps the page in XML format. Pages rendered via Lift's templating mechanism are generally resistant to cross site scripting attacks and other attacks that insert malicious content in rendered pages. Lift's templating mechanism is designer friendly yet supports complex and powerful substitution rules. Further, the rendered page can be evaluated and transformed during the final rendering phase to ensure that all script tags are at the bottom of the page, all CSS tags are at the top, etc. Lift's templating mechanism can be used to serve sessionless requests or serve requests within the context of a session. Further, pages can be marked as not requiring a session, yet will make session state available if the request was made in the context of a container session. Lift page rendering can even be done in parallel such that if there are long off-process components on the page (e.g., advertising servers), those components can be Sessionless Lift, forms and Ajax Lift applications can process HTML forms and process Ajax requests even if there's no session associated with the request. Such forms and Ajax requests have to have stable field names and stable URLs, but this is the same requirement as most web frameworks including Struts, Rails, and Django impose on their applications. In such a mode, Lift apps have the similar characteristics to web apps written on tops of Struts, Play, JSF and other popular Java web frameworks. Lift as Secure, Interactive App Platform Lift features require session affinity: GUID to function mapping, type-safe SessionVars and Comet. Applications that take advantage of these features need to have requests associated with the JVM that stores the session. I'll discuss the reason for this limitation, the down-side to the limitation, the downside to migratory session, and the benefits of these features. Application servers that support migratory sessions (sessions that are available to application servers running in multiple address spaces/processes) require a mechanism for transferring the state information between processes. This is typically (with the exception of Terracotta) done by serializing the stored data. Serialization is the process of converting rich data structures into a stream of bytes. Some of Scala's constructs are hard or impossible to serialize. For example, local variables that are mutated within a closure are promoted from stack variables to heap variables. When those variables are serialized at different times, the application winds up with two references even though the references are logically the same. Lift makes use of many of these constructs (I'll explain why next) and Lift's use of these constructs makes session serialization and migration impossible. It also means that Lift's type-safe SessionVars are not guaranteed to be serialized. One of the key Lift constructs is to map a cryptographically unique identifier in the browser to a function on the server. Lift uses Scala functions which close over scope, including all of the variables referenced by the function. This means that it's not necessary to expose primary keys to the client when editing a record in the database because the primary key of the record or the record itself is known to the function on the server. This guards against OWASP Vulnerability A4, Insecure Object References as well as Replay Attacks. From the developer's standpoint, writing Lift applications is like writing a VisualBasic application... the developer associates the user action with a function. Lift supplies the plumbing to bridge between the two. Lift's GUID to function mapping extends to Lift's Ajax support. Associating a button, checkbox, or other HTML element with an Ajax call is literally a single line: SHtml.ajaxButton(<b>PressMe</b>, () => Alert("You pressed a button at "+Helpers.currentTimeFormatted) Lift's Ajax support is simple, maintainable, and secure. There's no need to build and maintain routing. Lift has the most advanced server-push/Comet support of any web framework or any other system currently available. Lift's comet support relies on session affinity. Lift's comet support associates an Actor with a section of screen real estate. A single browser window may have many pieces of screen real estate associated with many of Lift's CometActors. When state changes in the Actor, the state change is pushed to the browser. Lift takes care of multiplexing a single HTTP connection to handle all the comet items on a given page, the versioning of the change deltas (if the HTTP connection is dropped while 3 changes become available, all 3 of those changes are pushed when the next HTTP request is made.) Further, Lift's comet support will work the same way once web sockets are available to the client and server... there will be no application code changes necessary for web sockets support. Lift's comet support requires that the connect is made from the browser back to the same JVM in which the CometActors are resident... the same JVM where the session is located.

The downside to Lift's session affinity requirement mainly falls on the operations team. They must use a session aware load balancer or other mechanism to route incoming requests to the server that the session is associated with. This is easily accomplished with HAProxy and Nginx. Further, if the server running a given session goes down, the information associated with that session is lost (note that any information distributed off-session [into a database, into a cluster of Akka actors, etc.] is preserved.) But, Lift has extended session facilities that support re-creation of session information in the event of session lost. Lift also has heart-beat functionality so that sessions are kept alive as long as a browser page is open to the application, so user inactivity will not result in session timeouts.

Compared to the operational cost of a session aware load balancer, there are many costs associated with migratory sessions. First, there must be a persistence mechanism for those sessions. Memcached is an unreliable mechanism as memcached instances have no more stability than the JVM which hosts the application and being a cache, some sessions may get expired. Putting session data in backing store such as MySQL or Cassandra increases the latency of requests. Further, the costs of serializing state, transmitting the state across the network, storing it, retrieving it, transmitting it across the network, and deserializing it all costs a lot of cycles and bandwidth. When your Lift application scales beyond a single server, beyond 100 requests per second, the costs of migrating state on every request becomes a significant operational issue.

Session serialization can cause session information loss in the case of multiple requests being executed in multiple processes. It's common to have multiple tabs/windows open to the same application. If session data is serialized as a blob and two different requests from the same server are being executed at the same time, the last request to write session data into the store will over-write the prior session data. This is a concurrency problem and can lead to hard to debug issues in production because reproducing this kind of problem is non-trivial and this kind of problem is not expected by developers.

The third issue with migratory sessions and session serialization is that the inability to store complex information in the session (e.g., a function that closes over scope) means that the developer has to write imperative code to serialize session state to implement complex user interactions like multi-screen wizards (which is a 400 line implementation in Lift). These complex, hand written serializations are error prone, can introduce security problems and are non-trivial to maintain.

The operational costs of supporting session affinity are not materially different from the operational costs of providing backing store for migratory sessions. On the other hand, there are many significant downsides to migratory sessions. Let's explore the advantages of Lift's design.

Lift's use of GUIDs associated with functions on the server: Increase the security of the application by guarding against cross site request forgeries, replay attacks, and insecure object references Decrease application development and maintenance time and costs Increase application interactivity, thus a much better user experience Increase in application richness because of simpler Ajax, multi-page Wizards, and Comet Improved application performance because fewer cycles are spent serializing and transmitting session information No difference in scalability... just add more servers to the front end to scale the front end of your application The positive attributes of Lift's design decisions are evident at Foursquare which handles thousands of requests per second all served by Lift. There are very few sites that have more traffic than Foursquare. They have scaled their web front end successfully and securely with Lift. Other high volume sites including Novell are successfully scaling with Lift. If you are scaling your site, there are also commercial Lift Cloud manager tools that can help manage clusters of Lift's session requirements. Conclusion Lift provides a lot of choices for developing and deploying complex web applications. Lift can operate in a web container like any other Java web framework. If you choose to use certain Lift features and you are deploying across multiple servers, you need to have a session aware load balancer. Even when using Lift's session-affinity dependent features, Lift applications have higher performance, identical availability, identical scalability, better security, and better user experience than applications written with web frameworks such as Ruby on Rails, Struts, and GWT.

Chapter 21 How Lift does function/GUID mapping

Chapter 22 How Lift does Comet

I can speak to Lift's Comet Architecture which was selected by Novell to power their Pulse product after they evaluated a number of different technologies.

Lift's Comet implementation uses a single HTTP connection to poll for changes to an arbitrary number of components on the page. Each component has a version number. The long poll includes the version number and the component GUID. On the server side, a listener is attached to all of the GUIDs listed in the long poll requests. If any of the components has a higher version number (or the version number increases during the period of the long poll), the deltas (a set of JavaScript describing the change from each version) is sent to the client. The deltas are applied and the version number on the client is set to the highest version number for the change set.

Lift integrates long polling with session management so that if a second request comes into the same URL during a long poll, the long poll is terminated to avoid connection starvation (most browsers have a maximum of 2 HTTP connections per named server). Lift also supports DNS wild-carded servers for long poll requests such that each tab in the browser can do long polling against a different DNS wildcarded server. This avoids the connection starvation issues.

Lift dynamically detects the container the Servlet is running in and on Jetty 6 & 7 and (soon) Glassfish, Lift will use the platform's "continuations" implementation to avoid using a thread during the long poll.

Lift's JavaScript can sit on top of jQuery and YUI (and could sit on top of Prototype/Scriptaculous as well.) The actual polling code includes back-off on connection failures and other "graceful" ways of dealing with transient connection failures.

I've looked at Atmosphere and CometD (both JVM-oriented Comet technologies). Neither had (at the time I evaluated them) support for multiple components per page or connection starvation avoidance.



Part IV Misc

Chapter 24 Releases





