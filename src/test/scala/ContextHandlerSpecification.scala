import scala.concurrent.{future}
import scala.concurrent.ExecutionContext.Implicits.global

import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object ContextHandlerSpecification extends SpecificationWithJUnit {

  "context handler" should {
    object T extends ContextHandler
    object SampleTemplate extends Mustache("") {
      def globalFn0 = "global value"
      def globalFn1(str:String) = ">"+str+"<"
    }

    def render(context:Any,partials:Map[String,Mustache],callstack:List[Any])(str:String) = ""

    "return None for null context" in {
      T.valueOf("testKey", null, Map(), List(SampleTemplate),"", render) must be equalTo(None)
    }

    "extract values out of the map" in {
      T.valueOf("n/a", Map(), Map(), List(SampleTemplate),"", render) must be equalTo(None)
      T.valueOf("foo", Map("foo"->"bar"), Map(), List(SampleTemplate),"", render) must be equalTo("bar")
    }

    object SampleObject {
      val sampleField = "foo"
      def sampleMethod = "bar"
    }

    "extract values out of the singletone" in {
      T.valueOf("n/a", SampleObject, Map(), List(SampleTemplate),"", render) must be equalTo(None)
      T.valueOf("sampleField", SampleObject, Map(), List(SampleTemplate),"", render) must be equalTo("foo")
      T.valueOf("sampleMethod", SampleObject, Map(), List(SampleTemplate),"", render) must be equalTo("bar")
    }    

    case class SampleCaseClass(foo:String, bar:String)

    "extract values out of the case class" in {
      val ctx = SampleCaseClass("fooValue", "barValue")
      T.valueOf("n/a", ctx, Map(), List(SampleTemplate),"", render) must be equalTo(None)
      T.valueOf("foo", ctx, Map(), List(SampleTemplate),"", render) must be equalTo("fooValue")
      T.valueOf("bar", ctx, Map(), List(SampleTemplate),"", render) must be equalTo("barValue")
    }

    trait SampleTrait { 
      val foo:String 
      def bar = "barValue"
    }
    object SampleTraitObject extends SampleTrait { 
      val foo = "42" 
    }
    "extract values out of the traits" in {
      T.valueOf("n/a", SampleTraitObject, Map(), List(SampleTemplate),"", render) must be equalTo(None)
      T.valueOf("foo", SampleTraitObject, Map(), List(SampleTemplate),"", render) must be equalTo("42")
      T.valueOf("bar", SampleTraitObject, Map(), List(SampleTemplate),"", render) must be equalTo("barValue")
    }

    "extract values out of the closures" in {
      T.valueOf("test", Map( "test"-> (()=>{42}) ), Map(), List(SampleTemplate),"", render) must be equalTo(42)
    }

    "extract values out of the futures" in {
      T.valueOf("test", Map( "test"-> future{ 42 } ), Map(), List(SampleTemplate),"", render) must be equalTo(42)
    }

    "extract values out of the nested closures / futures" in {
      T.valueOf("test", Map( "test"-> future{ ()=>{ 42 } } ), Map(), List(SampleTemplate),"", render) must be equalTo(42)
    }

    "extract simple globals" in {
      T.valueOf("globalFn0", null, Map(), List(SampleTemplate),"", render) must be equalTo("global value")
    }

  }

}
}
