import mill._, scalalib._

object Client extends ScalaModule {
  def scalaVersion = "2.13.3"
  def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
}
