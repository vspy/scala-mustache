import org.specs._
import org.specs.runner._

package mustache {
object EscapedTokenSpecification extends SpecificationWithJUnit {

  object SampleTemplate extends Mustache("")

  "escaped text token" should {
    "render escaped text" in {
      EscapedToken("foo","{{","}}").render(
        Map("foo"->"\"<>&test\""), Map(), List(SampleTemplate)
      ).toString must be equalTo("&quot;&lt;&gt;&amp;test&quot;")
    }
  }

}
}

