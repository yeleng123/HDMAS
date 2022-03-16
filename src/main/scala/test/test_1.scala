package test

object test_1 {
  def main(args: Array[String]): Unit = {

    val fun = (x:Int, y:String, z:Char)=> {
      if (x == 0 && y == "" && z == '0') {
        false
      } else {
        true
      }
    }

    println(fun(0,"",'0'))

    def z(m:Char) = {
      if (m == '0') {
        true
      }
      else {false}
    }
    println(z('0'))




  }


}
