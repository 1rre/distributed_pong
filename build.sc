import mill._, scalalib._

object Client extends ScalaModule {

  def scalaVersion = "2.13.5"

  //def scalacOptions = Seq("-deprecation")

  def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
}

object Local extends ScalaModule {
  def scalaVersion = "2.13.5"
}
