import org.specs._
import org.specs.runner._

package mustache {
object Issue1Specification extends SpecificationWithJUnit {

  "mustache" should {

    "render values from previous context levels" in {
      new Mustache(
"""{{#organization}}{{header}}
id: {{id}}
name: {{name}}
{{/organization}}"""
      ).render(Map(
        "header"->"Hello"
        ,"organization"->Map(
                          "id"->1
                          ,"name"->"My Organization"
                        )
      )).toString must be equalTo(
"""Hello
id: 1
name: My Organization
"""
      )
    }

  }

}
}
