import org.specs._
import org.specs.runner._


package mustache {
object MustacheSpecification extends SpecificationWithJUnit {

  "mutache should" should {

    "render typical template from man page" in {
      Mustache(
        "Hello {{name}}\nYou have just won ${{value}}!\n"+
        "{{#in_ca}}\nWell, ${{taxed_value}}, after taxes.\n"+
        "{{/in_ca}}")
      .render(Map(
        "name"->"Chris"
        ,"value"->10000
        ,"taxed_value" -> { 10000.0 - (10000.0 * 0.4) }
        ,"in_ca" -> true)
      ).toString must be equalTo(
        "Hello Chris\nYou have just won $10000!\n\n"+
        "Well, $6000, after taxes."
      )
    }

    "render variables example from man page" in {
      Mustache(
        "* {{name}}\n* {{age}}\n* {{company}}\n* {{{company}}}"
      ).render(Map(
        "name"->"Chris"
        ,"company"->"<b>GitHub</b>"
      )).toString must be equalTo(
        "* Chris\n* \n* &lt;b&gt;GitHub&lt;/b&gt;\n* <b>GitHub</b>"
      )
    }

    "render section example from man page" in {
      Mustache(
        "Shown.\n{{#nothin}}\nNever shown!\n{{/nothin}}"
      ).render(Map(
        "person" -> true
      )).toString must be equalTo(
        "Shown.\n"
      )
    }

    "render section list example from man page" in {
      Mustache(
        "{{#repo}}\n  <b>{{name}}</b>\n{{/repo}}"
      ).render(Map(
        "repo" -> List(
                    Map("name"->"resque")
                    ,Map("name"->"hub")
                    ,Map("name"->"rip")
                  )
      )).toString must be equalTo(
        "\n<b>resque</b>\n\n<b>hub</b>\n\n<b>rip</b>\n"
      )
    }

    "render section non-false example from man page" in {
      Mustache(
        "{{#person?}}Hi {{name}}!{{/person?}}"
      ).render(Map(
        "person?" -> Map("name"->"John")
      )).toString must be equalTo(
        "Hi John!"
      )
    }

  }

}
}
