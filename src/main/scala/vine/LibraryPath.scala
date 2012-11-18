package vine

object LibraryPath {

  private val PROPERTY = "java.library.path"

  def getLibraryPath: Set[String] = System.getProperty(PROPERTY).split(":").toSet

  def setLibraryPath(xs: Iterable[String]) { System.setProperty(PROPERTY, xs.mkString(":")) }

  def addToLibraryPath(xs: String*) {
    setLibraryPath(getLibraryPath ++ xs.toSet)
  }

}
