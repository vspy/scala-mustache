import scala.io.Source

package mustache {
object PerformanceTest {

  def main(args: Array[String]) = 
    for(i<-0 until 3) {
      test("/small.mustache",Map ("name"->"world"))
      test("/medium.mustache", Map(
        "projects"->List(
          Map(
            "name"->"resque"
            ,"url"->"http://github.com/defunkt/resque"
            ,"description"->"A Redis-backed Ruby library for queueing and working."
          ), Map(
            "name"->"mustache"
            ,"url"->"http://defunkt.github.com/mustache"
            ,"description"->"Logic-less templates."
          ), Map(
            "name"->"pystache"
            ,"url"->"http://github.com/defunkt/pystache"
            ,"description"->"Mustache in Python"
          ), Map(
            "name"->"hub"
            ,"url"->"http://github.com/defunkt/hub"
            ,"description"->"hub introduces git to GitHub"
          ), Map(
            "name"->"repl"
            ,"url"->"http://github.com/defunkt/repl"
            ,"description"->"sometimes you need a repl"
          ), Map(
            "name"->"gist"
            ,"url"->"http://github.com/defunkt/gist"
            ,"description"->"Absolutely the best command line gister."
          ), Map(
            "name"->"gem-man"
            ,"url"->"http://defunkt.github.com/gem-man/"
            ,"description"->"View a RubyGem's man page"
          ), Map(
            "name"->"hurl"
            ,"url"->"http://hurl.it/"
            ,"description"->"Hurl makes HTTP requests"
          ))
        ,"width"->4968
      ))
    }

  private def test(filename:String, ctx:Any) = {
    val template = new Mustache(Source.fromInputStream(getClass().getResourceAsStream(filename)))
    val t1 = System.currentTimeMillis
    for (i <- 0 until 100000) { template.render(ctx) }
    val t2 = System.currentTimeMillis
    println(filename+" : "+(t2-t1))
  }

}
}


