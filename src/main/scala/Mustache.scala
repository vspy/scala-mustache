import scala.annotation.tailrec
import scala.io.Source
import scala.collection.MapLike
import java.lang.reflect.{Field=>F,Method=>M}
import scala.concurrent.{Await, Awaitable}
import scala.concurrent.duration._

package mustache {

  case class MustacheParseException(line:Int, msg:String) 
    extends Exception("Line "+line+": "+msg)

  /**
   * view helper trait 
   **/
  trait MustacheHelperSupport {
    private val contextLocal = new java.lang.ThreadLocal[Any]()
    private val renderLocal = new java.lang.ThreadLocal[Function1[String,String]]()

    protected def context:Any = contextLocal.get
    protected def render(template:String):Any =
      (renderLocal.get())(template)

    def withContextAndRenderFn[A](context:Any, render:(String)=>String)(fn: =>A):A = {
      contextLocal.set(context)
      renderLocal.set(render)
      try { fn }
      finally { 
        contextLocal.set(null)
        renderLocal.set(null)
      }
    }
  }

  /**
   * template
   **/
  class Mustache(
    root: Token
  ) extends MustacheHelperSupport 
  {

    def this (source:Source
              , open:String = "{{"
              , close:String = "}}") =
      this((new Parser{
        val src = source
        var otag = open
        var ctag = close
      }).parse())

    def this(str:String) = this(Source.fromString(str))

    def this(
        str:String
      , open:String 
      , close:String
      ) = this(Source.fromString(str), open, close)

    private val compiledTemplate = root

    val globals:Map[String,Any] = 
      {
        val excludedGlobals = List("wait","toString","hashCode","getClass","notify","notifyAll")
        Map( 
          (this.getClass().getMethods
            .filter(x => {
              val name = x.getName 
              val pt = x.getParameterTypes
              ( !name.startsWith("render$default")
              ) && (
                !name.startsWith("product$default")
              ) && (
                !name.startsWith("init$default")
              ) && (
                !excludedGlobals.contains(name)
              ) && ((
                pt.length == 0
              ) || (
                pt.length == 1 
                && pt(0) == classOf[String]
              ))
            })
            .map( x=>{x.getName-> 
              (if(x.getParameterTypes.length == 0) ()=>{ x.invoke(this) }
              else (str:String)=>{ x.invoke(this, str) })
            })) : _*
          )
        }

    def render(
      context : Any = null
      , partials : Map[String,Mustache] = Map()
      , callstack : List[Any] = List(this)
    ) : String = product(context, partials, callstack).toString

    def product(
      context : Any = null
      , partials : Map[String,Mustache] = Map()
      , callstack : List[Any] = List(this)
    ) : TokenProduct = compiledTemplate.render(context, partials, callstack)

  }

  private class ParserState
  private object Text extends ParserState
  private object OTag extends ParserState
  private object Tag  extends ParserState
  private object CTag extends ParserState

  private abstract class Parser {
    val src:Source

    var state:ParserState = Text
    var otag:String
    var ctag:String
    var tagPosition:Int = 0
    var line:Int = 1
    var prev:Char = '\uffff'
    var cur:Char = '\uffff'
    var curlyBraceTag:Boolean = false
    var stack:List[Token] = List()

    val buf = new StringBuilder(8192)

    def parse():Token = {
      while(consume) {
        state match {
          case Text =>
            if (cur == otag.charAt(0))
              if (otag.length > 1) { tagPosition = 1; state = OTag } 
              else { staticText; state = Tag }
            else buf.append(cur)

          case OTag =>
            if(cur == otag.charAt(tagPosition))
              if (tagPosition == otag.length-1) { staticText; state = Tag }
              else { tagPosition = tagPosition+1 }
            else { notOTag; buf.append(cur) }

          case Tag =>
            if (buf.isEmpty && cur == '{') {
              curlyBraceTag = true
              buf.append(cur)
            } else if (curlyBraceTag && cur == '}') {
              curlyBraceTag = false
              buf.append(cur)
            } else if (cur == ctag.charAt(0)) {
              if (ctag.length > 1) { tagPosition = 1; state = CTag }
              else tag
            } else buf.append(cur)

          case CTag =>
            if (cur == ctag.charAt(tagPosition)) {
              if (tagPosition == ctag.length-1) tag
              else { tagPosition = tagPosition+1 }
            } else { notCTag; buf.append(cur) }
        }
      }
      state match {
        case Text => staticText
        case OTag => { notOTag; staticText }
        case Tag => fail("Unclosed tag \""+buf.toString+"\"")
        case CTag => { notCTag; staticText }
      }
      stack.foreach {
        case IncompleteSection(key,_,_,_) => fail("Unclosed mustache section \""+key+"\"")
        case _ =>
      }
      val result = stack.reverse

      if(result.size == 1) result(0)
      else RootToken(result)
    }

    private def fail[A](msg:String):A = throw MustacheParseException(line,msg)

    private def consume = {
      prev = cur
      
      if (src.hasNext) {
        cur = src.next
        // \n, \r\n, \r
        if ( cur == '\r' ||
              ( cur == '\n' && prev != '\r' ) 
            ) line = line+1
        true
      } else false
    }

    private def notOTag = { 
      buf.append(otag.substring(0,tagPosition))
      state = Text 
    }
    private def notCTag = { 
      buf.append(ctag.substring(0,tagPosition))
      state = Tag
    }
    private def reduce:String = { val r = buf.toString; buf.clear; r }

    private def staticText:Unit = { 
      val r = reduce
      if (r.length>0) stack = StaticTextToken(r)::stack
    }

    private def checkContent(content:String):String = {
      val trimmed = content.trim
      if (trimmed.length == 0) fail("Empty tag")
      else trimmed
    }

    private def tag:Unit = {
      state = Text
      val content = checkContent(reduce)
      def skipFirst = checkContent(content substring 1)
      def skipBoth = checkContent(content substring(1, content.length-1))

      content.charAt(0) match {
        case '!' => // ignore comments
        case '&' =>
          stack = UnescapedToken(skipFirst,otag,ctag)::stack
        case '{' =>
          if (content endsWith "}")
            stack = UnescapedToken(skipBoth,otag,ctag)::stack
          else fail("Unbalanced \"{\" in tag \""+content+"\"")
        case '^' =>
          stack = IncompleteSection(skipFirst, true, otag, ctag)::stack
        case '#' =>
          stack = IncompleteSection(skipFirst, false, otag, ctag)::stack
        case '/' => {
          val name = skipFirst

          @tailrec
          def addSection(
            children:List[Token]
            ,s:List[Token]
          ) : List[Token] = s.headOption match {
            case None => fail("Closing unopened section \""+name+"\"")

            case Some(IncompleteSection(key, inverted,startOTag,startCTag)) 
              if (key == name) => 
                SectionToken(
                  inverted
                  , name
                  , children
                  , startOTag
                  , startCTag
                  , otag
                  , ctag)::s.tail

            case Some(IncompleteSection(key, inverted,_,_)) 
              if (key != name) => fail("Unclosed section \""+key+"\"")

            case Some(other) => 
              addSection(other::children, s.tail)
          }
          stack = addSection(List[Token](), stack)
        }
        case '>' | '<' =>
          stack = PartialToken(skipFirst,otag,ctag)::stack
        case '=' =>
          if (content.size>2 && content.endsWith("=")) {
            val changeDelimiter = skipBoth
            changeDelimiter.split("""\s+""",-1).toSeq match {
              case Seq(o,c) => { 
                stack = ChangeDelimitersToken(o,c,otag,ctag)::stack
                otag = o; ctag = c 
              }
              case _ => fail("Invalid change delimiter tag content: \""+changeDelimiter+"\"")
            }
          } else 
              fail("Invalid change delimiter tag content: \""+content+"\"")
        case _ => stack = EscapedToken(content, otag, ctag)::stack
      }
    }
  }

  // mustache tokens ------------------------------------------
  trait TokenProduct {
    val maxLength:Int
    def write(out:StringBuilder):Unit

    override def toString = {
      val b = new StringBuilder(maxLength)
      write(b)
      b.toString
    }
  }

  object EmptyProduct extends TokenProduct {
    val maxLength = 0 
    def write(out:StringBuilder):Unit = {}
  }

  case class StringProduct(str:String) extends TokenProduct {
    val maxLength = str.length 
    def write(out:StringBuilder):Unit = out.append(str)
  }


  trait Token {
    def render(context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct
    def templateSource:String
  }

  trait CompositeToken {
    def composite(
          tokens:List[Token]
          , context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct = 
      composite(tokens.map{(_,context)},partials, callstack)

    def composite(
          tasks:Seq[Tuple2[Token,Any]]
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct = {
      val result = tasks.map(t=>{t._1.render(t._2, partials, callstack)})
      val len = result.foldLeft(0)({_+_.maxLength})
      new TokenProduct {
        val maxLength = len
        def write(out:StringBuilder) = result.map{_.write(out)}
      }
    }
  }

  case class RootToken(children:List[Token]) extends Token with CompositeToken {
    private val childrenSource = children.map(_.templateSource).mkString

    def render(context:Any, partials:Map[String,Mustache], callstack:List[Any]):TokenProduct =
      composite(children, context, partials, callstack)

    def templateSource:String = childrenSource
  }

  case class IncompleteSection(key:String, inverted:Boolean, otag:String, ctag:String) extends Token {
    def render(context:Any, partials:Map[String,Mustache], callstack:List[Any]):TokenProduct = fail
    def templateSource:String = fail

    private def fail = 
      throw new Exception("Weird thing happened. There is incomplete section in compiled template.")

  }

  case class StaticTextToken(staticText:String) extends Token {
    private val product = StringProduct(staticText)

    def render(context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct = product

    def templateSource:String = staticText

  }

  case class ChangeDelimitersToken(
    newOTag:String, newCTag:String, otag:String, ctag:String
  ) extends Token {
    private val source = otag + "=" + newOTag + " " + newCTag + "=" + ctag

    def render(context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct = EmptyProduct 

    def templateSource:String = source

  }

  case class PartialToken(key:String, otag:String, ctag:String) extends Token {
    def render(context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct =
      partials.get(key) match {
        case Some(template) => template.product(context, partials, template::callstack)
        case _ => throw new IllegalArgumentException("Partial \""+key+"\" is not defined.")
      }
    def templateSource:String = otag+">"+key+ctag
  }

  trait ContextHandler {

    protected def defaultRender(
                    otag:String
                    , ctag:String 
    ):(Any,Map[String,Mustache],List[Any])=>(String)=>String = 
      (context:Any, partials:Map[String,Mustache],callstack:List[Any])=>(str:String)=>{
        val t = new Mustache(str, otag, ctag)
        t.render(context, partials, callstack)
      }

    def valueOf(key:String
        , context:Any
        , partials:Map[String,Mustache]
        , callstack:List[Any]
        , childrenString:String
        , render: (Any, Map[String, Mustache],List[Any])=>(String)=>String
    ):Any = 
    {
      val r = render(context, partials, callstack)

      val wrappedEval = 
        callstack.filter(_.isInstanceOf[Mustache]).asInstanceOf[List[Mustache]].foldLeft(()=>{ eval(findInContext(context::callstack, key)
              , childrenString, r) })( (f,e)=>{ ()=>{e.withContextAndRenderFn(context,r)(f())} } )
      wrappedEval() match {
        case None if (key == ".") => context
        case other => other
      }
    }
    

    @tailrec
    private def eval(
                  value:Any
                  , childrenString:String
                  , render:(String)=>String 
    ):Any =
      value match {
        case Some(someValue) => eval(someValue, childrenString, render)

        case a:Awaitable[_] =>
          eval(Await.result(a, Duration.Inf), childrenString, render)

        case f:Function0[_] => 
          eval(f(), childrenString, render)

        case s:Seq[_] => s

        case m:MapLike[_, _, _] => m

        case f:Function1[String, _] => 
          eval(f(childrenString), childrenString, render)

        case f:Function2[String, Function1[String,String], _] => 
          eval(f(childrenString, render), childrenString, render)

        case other => other
      }

    @tailrec
    private def findInContext(stack:List[Any], key:String):Any =
      stack.headOption match {
        case None => None
        case Some(head) =>
          (head match {
            case null => None
            case m : MapLike[String,_,_] =>
              m.get(key) match {
                case Some(v) => v
                case None => None
              }
            case m:Mustache =>
              m.globals.get(key) match {
                case Some(v) => v
                case None => None
              }
            case any => reflection(any, key)
          }) match {
            case None => findInContext(stack.tail, key)
            case x => x
          }
      }

    private def reflection(x:Any, key:String):Any = {
      val w = wrapped(x)
      (methods(w).get(key), fields(w).get(key)) match {
        case (Some(m), _) => m.invoke(w)
        case (None, Some(f)) => f.get(w)
        case _ => None
      }
    }

    private def fields(w:AnyRef) = Map( 
      w.getClass().getFields.map(x => {x.getName -> x}):_*
    )

    private def methods(w:AnyRef) = Map(
      w.getClass().getMethods
        .filter(x => { x.getParameterTypes.length == 0 })
        .map(x => { x.getName -> x }) :_*
    )

    private def wrapped(x:Any):AnyRef =
      x match {
        case x: Byte => byte2Byte(x)
        case x: Short => short2Short(x)
        case x: Char => char2Character(x)
        case x: Int => int2Integer(x)
        case x: Long => long2Long(x)
        case x: Float => float2Float(x)
        case x: Double => double2Double(x)
        case x: Boolean => boolean2Boolean(x)
        case _ => x.asInstanceOf[AnyRef]
      }
  }

  trait ValuesFormatter {
    @tailrec
    final def format(value:Any):String =
      value match {
        case null => ""
        case None => ""
        case Some(v) => format(v)
        case x => x.toString
      }
  }

  case class SectionToken(
     inverted:Boolean
    ,key:String
    ,children:List[Token]
    ,startOTag:String
    ,startCTag:String
    ,endOTag:String
    ,endCTag:String
  ) extends Token with ContextHandler with CompositeToken {

    private val childrenSource = children.map(_.templateSource).mkString

    private val source = startOTag + (if(inverted) "^" else "#") + key + 
      startCTag + childrenSource + endOTag + "/" + key + endCTag

    private val childrenTemplate = {
      val root = if(children.size == 1) children(0)
                  else RootToken(children)
      new Mustache( root )
    }

    def render(context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct =
      valueOf(key
              , context
              , partials
              , callstack
              , childrenSource
              , renderContent
      ) match {
        case null => 
          if (inverted) composite(children, context, partials, context::callstack)
          else EmptyProduct
        case None => 
          if (inverted) composite(children, context, partials, context::callstack)
          else EmptyProduct
        case b:Boolean => 
          if (b^inverted) composite(children, context, partials, context::callstack)
          else EmptyProduct
        case s:Seq[_] if(inverted) => 
          if (s.isEmpty) composite(children, context, partials, context::callstack)
          else EmptyProduct
        case s:Seq[_] if(!inverted) => {
          val tasks = for (element<-s;token<-children) yield (token, element)
          composite(tasks, partials, context::callstack)
        }
        case str:String => 
          if (!inverted) StringProduct(str)
          else EmptyProduct

        case other => 
          if (!inverted) composite(children, other, partials, context::callstack)
          else EmptyProduct
      }

    private def renderContent(context:Any
                  , partials:Map[String,Mustache]
                  , callstack:List[Any]
                ) ( template:String ) : String =
      // it will be children nodes in most cases
      // TODO: some cache for dynamically generated templates?
      if (template == childrenSource)
        childrenTemplate.render(context, partials, context::callstack)
      else {
        val t = new Mustache(template, startOTag, startCTag)
        t.render(context, partials, context::callstack)
      }

    def templateSource:String = source
  }

  case class UnescapedToken(key:String, otag:String, ctag:String) 
    extends Token 
    with ContextHandler 
    with ValuesFormatter {
    private val source = otag + "&" + key + ctag

    def render(context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct = {
      val v = format(valueOf(key,context,partials,callstack,"",defaultRender(otag,ctag)))
      new TokenProduct {
        val maxLength = v.length
        def write(out:StringBuilder):Unit = {out.append(v)}
      }
    }

    def templateSource:String = source
  }

  case class EscapedToken(key:String, otag:String, ctag:String) 
    extends Token 
    with ContextHandler 
    with ValuesFormatter {
    private val source = otag + key + ctag

    def render(context:Any
          , partials:Map[String,Mustache]
          , callstack:List[Any]):TokenProduct = { 
      val v = format(valueOf(key,context,partials,callstack,"",defaultRender(otag,ctag)))
      new TokenProduct {
        val maxLength = (v.length*1.2).toInt
        def write(out:StringBuilder):Unit =
          v.foreach {
            case '<' => out.append("&lt;")
            case '>' => out.append("&gt;")
            case '&' => out.append("&amp;")
            case '"' => out.append("&quot;")
            case c => out.append(c)
          }
      }
    }

    def templateSource:String = source
  }

}
