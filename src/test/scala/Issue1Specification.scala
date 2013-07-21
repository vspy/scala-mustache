import org.specs2.mutable._
import org.specs2.runner._

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
