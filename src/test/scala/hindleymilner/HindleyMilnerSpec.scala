package hindleymilner

import hindleymilner.TypeInfer._
import hindleymilner.ast._

object HindleyMilnerSpec {
  def main(args: Array[String]): Unit = {
    val idcall = App(Abs("x", Var("x")), N(5))
    val compose =
      Abs("f",
        Abs("g",
          Abs("x",
            App(Var("f"),
              App(Var("g"), Var("x"))
            ))))
    println(displayType(inferType(idcall)))
    println(displayType(inferType(compose)))
  }
}
