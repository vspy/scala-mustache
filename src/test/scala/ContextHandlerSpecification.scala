import org.specs._
import org.specs.runner._
import scala.actors.Futures._

package mustache {
object ContextHandlerSpecification extends SpecificationWithJUnit {

  "context handler" should {
    object T extends ContextHandler

    "return None for null context" in {
      T.valueOf("testKey", null) must be equalTo(None)
    }

    "extract values out of the map" in {
      T.valueOf("n/a", Map()) must be equalTo(None)
      T.valueOf("foo", Map("foo"->"bar")) must be equalTo("bar")
    }

    object SampleObject {
      val sampleField = "foo"
      def sampleMethod = "bar"
    }

    "extract values out of the singletone" in {
      T.valueOf("n/a", SampleObject) must be equalTo(None)
      T.valueOf("sampleField", SampleObject) must be equalTo("foo")
      T.valueOf("sampleMethod", SampleObject) must be equalTo("bar")
    }    

    case class SampleCaseClass(foo:String, bar:String)

    "extract values out of the case class" in {
      val ctx = SampleCaseClass("fooValue", "barValue")
      T.valueOf("n/a", ctx) must be equalTo(None)
      T.valueOf("foo", ctx) must be equalTo("fooValue")
      T.valueOf("bar", ctx) must be equalTo("barValue")
    }

    trait SampleTrait { 
      val foo:String 
      def bar = "barValue"
    }
    object SampleTraitObject extends SampleTrait { 
      val foo = "42" 
    }
    "extract values out of the traits" in {
      T.valueOf("n/a", SampleTraitObject) must be equalTo(None)
      T.valueOf("foo", SampleTraitObject) must be equalTo("42")
      T.valueOf("bar", SampleTraitObject) must be equalTo("barValue")
    }

    "extract values out of the closures" in {
      T.valueOf("test", Map( "test"-> (()=>{42}) )) must be equalTo(42)
    }

    "extract values out of the futures" in {
      T.valueOf("test", Map( "test"-> future{ 42 } )) must be equalTo(42)
    }

    "extract values out of the nested closures / futures" in {
      T.valueOf("test", Map( "test"-> future{ ()=>{ 42 } } )) must be equalTo(42)
    }

  }

}
}
