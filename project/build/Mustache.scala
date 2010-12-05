import sbt._

class MustacheProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def libraryDependencies = Set(
    //tests
    "junit" % "junit" % "4.8.1" % "test->default"
    ,"org.scala-tools.testing" % "specs_2.8.1" % "1.6.6" % "test->default"
  ) ++ super.libraryDependencies

  def performanceClasspath = testClasspath +++ ("src" / "test" / "resources")
  lazy val performance = 
    runTask(
        Some("mustache.PerformanceTest"), performanceClasspath
    ).dependsOn(testCompile) describedAs "Runs the performance test."
}


