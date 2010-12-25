import org.specs._
import org.specs.runner._

package mustache {
object StaticTextTokenSpecification extends SpecificationWithJUnit {

  object SampleTemplate extends Mustache("")

  "static text token" should {
    "render static text" in {
      StaticTextToken("Hey!").render(
        null, Map(), SampleTemplate
      ).toString must be equalTo("Hey!")
    }
  }

}
}
