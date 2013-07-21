import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object ValuesFormatterSpecification extends SpecificationWithJUnit {

  "values formatter" should {
    object T extends ValuesFormatter

    "render empty string for null and none" in {
      T.format(null) must be equalTo("")
      T.format(None) must be equalTo("")
    }
    "render options properly" in {
      T.format(Some("abc")) must be equalTo("abc")
      T.format(Some(Some("abc"))) must be equalTo("abc")
    }
    "render integers" in {
      T.format(42) must be equalTo("42")
    }
  }

}
}
