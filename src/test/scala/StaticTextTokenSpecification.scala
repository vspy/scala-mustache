import org.specs._
import org.specs.runner._

package mustache {
object StaticTextTokenSpecification extends SpecificationWithJUnit {

  "static text token" should {
    "render static text" in {
      StaticTextToken("Hey!").render(
        null, Map()
      ).toString must be equalTo("Hey!")
    }
  }

}
}
