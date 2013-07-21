import org.specs2.mutable._
import org.specs2.runner._


package mustache {
object ManPageExamplesSpecification extends SpecificationWithJUnit {

  "mustache" should {

    "render typical template from man page" in {
      new Mustache(
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
        "Well, $6000.0, after taxes.\n"
      )
    }

    "render variables example from man page" in {
      new Mustache(
        "* {{name}}\n* {{age}}\n* {{company}}\n* {{{company}}}"
      ).render(Map(
        "name"->"Chris"
        ,"company"->"<b>GitHub</b>"
      )).toString must be equalTo(
        "* Chris\n* \n* &lt;b&gt;GitHub&lt;/b&gt;\n* <b>GitHub</b>"
      )
    }

    "render section example from man page" in {
      new Mustache(
        "Shown.\n{{#nothin}}\nNever shown!\n{{/nothin}}"
      ).render(Map(
        "person" -> true
      )).toString must be equalTo(
        "Shown.\n"
      )
    }

    "render section list example from man page" in {
      new Mustache(
        "{{#repo}}\n  <b>{{name}}</b>\n{{/repo}}"
      ).render(Map(
        "repo" -> List(
                    Map("name"->"resque")
                    ,Map("name"->"hub")
                    ,Map("name"->"rip")
                  )
      )).toString must be equalTo(
        "\n  <b>resque</b>\n\n  <b>hub</b>\n\n  <b>rip</b>\n"
      )
    }

    "render section non-false example from man page" in {
      new Mustache(
        "{{#person?}}Hi {{name}}!{{/person?}}"
      ).render(Map(
        "person?" -> Map("name"->"John")
      )).toString must be equalTo(
        "Hi John!"
      )
    }

    "render inverted section example from man page" in {
      new Mustache(
        "{{#repo}}<b>{{name}}</b>{{/repo}}{{^repo}}No repos :({{/repo}}"
      ).render(Map(
        "repo" -> Nil
      )).toString must be equalTo(
        "No repos :("
      )
    }

    "render comments example from man page" in {
      new Mustache(
        "<h1>Today{{! ignore me }}.</h1>"
      ).render().toString must be equalTo(
        "<h1>Today.</h1>"
      )
    }

    "render partials example from man page" in {
      val userTemplate = new Mustache("<strong>{{name}}</strong>")
      val baseTemplate = new Mustache(
        "<h2>Names</h2>\n{{#names}}\n  {{> user}}\n{{/names}}"
      )
      val ctx = Map("names"->List(
                       Map("name"->"Alice")
                      ,Map("name"->"Bob")
                ))
      val partials = Map("user" -> userTemplate)

      baseTemplate.render(ctx, partials).toString must be equalTo(
        "<h2>Names</h2>\n\n  <strong>Alice</strong>\n\n  <strong>Bob</strong>\n"
      )
    }

    "render delimiters example from man page" in {
      new Mustache(
        "* {{default_tags}}\n{{=<% %>=}}\n* <% erb_style_tags %>\n"+
        "<%={{ }}=%>\n* {{ default_tags_again }}"
      ).render(Map(
        "default_tags" -> "Line one"
        ,"erb_style_tags" -> "Line two"
        ,"default_tags_again" -> "Line three"
      )).toString must be equalTo(
        "* Line one\n\n* Line two\n\n* Line three"
      )
    }

  }

}
}
