/**
 * Testing For comprehensions and their desugaring. Examples obtained from
 * https://www.baeldung.com/scala/for-comprehension and from the Scala Cookbook
 * by Alvin Alexander.
 *
 * Ninad Jog
 * August 4, 2021
 */
object DesugForComp {

  def main (args: Array [String]) {

    println ("Hello, desugared for comprehensions!")

    //-------------------------
    // 1. foreach

    val result: Result [Int] = Result (42)

    for {
      res <- result
    } println (res) // prints 42

    // Desugared version:
    result
      .foreach (res => println (res))

    println

    //--------------------------
    // 2. yield is desugared by 'map'

    val answer: Result [Int] = Result (50)

    val x = for {
      ans <- answer
    } yield ans * 3
    println (x) // prints Result(150)

    // Desugared version:
    val xx: Result [Int] =
      answer
        .map (ans => ans * 3)

    println (xx)
    println

    //--------------------------
    // 3. Continued generators are desugared by 'flatMap' (bind >>= in Haskell)

    val anotherResult: Result [Int] = Result (100)
    val y = for {
      res     <- result
      another <- anotherResult
    } yield res + another

    println (y) // prints Result(142)

    // Desugared version
    val yy: Result [Int] =
      result
        .flatMap (res     => anotherResult
        .map     (another => res + another))

    println (yy)
    println

    //--------------------------
    // 4. Similar to the previous example but with 3 generators instead of 2

    val p = Result (1)
    val q = Result (2)
    val r = Result (3)

    val m = for {
      i <- p
      j <- q
      k <- r
    } yield i + j + k

    println (m) // prints Result(6)

    //---------------
    // Desugared version
    val mm = p
      .flatMap (i => q
      .flatMap (j => r
      .map     (k => i + j + k)))

    println (mm) // prints Result(6)
    println

    //--------------------------
    // 5. Guard (MonadPlus in Haskell)

    val c = for {
      res <- result
      if res == 42
    } yield res

    println (c) // prints Result(42)

    // Desugared version
    val cc =
      result
        .withFilter (res => res == 42)
        .map        (res => res)  // Identity function

    println (cc) // prints Result(42)
    println

    //--------------------------
    // 6. Loop with two guards (from Scala Cookbook, pg 59)

    val d = for {
      i <- 1 to 10
      if i != 2         // exclude 2
      if i % 2 == 0     // include only even numbers
    } yield i

    println (d)         // prints Vector(4, 6, 8, 10)

    //------------
    // Desugared version with two guards

    val dd =
      (1 to 10)
        .withFilter (i => i != 2)
        .withFilter (i => i % 2 == 0)
        .map        (i => i)

    println (dd)  // prints Vector(4, 6, 8, 10)
    println

    //-----------------------
    // 7. Pattern matching in the left side of a for comprehension

    val cities = List ("New York", "Washington", "London", "Toronto", "Sydney")
    val zipCities: List [(String, Int)] = cities.zipWithIndex // List [("New York", 0), ("Washington", 1),...]

    val e: List [String] =
      for {
        (city, idx) <- zipCities
        if idx % 2 == 0
      } yield city

    println (e) // prints List(New York, London, Sydney)

    //--------------
    // Desugared version

    val ee =
      zipCities
        .withFilter { case (_, idx)  => (idx % 2) == 0 }
        .map        { case (city, _) => city           }

    println (ee) // prints List(New York, London, Sydney)

  } // main

  //---------------------------------------------------------------------------
  /*
   * 'map' desugars the body of yield
   * 'flatMap' is equivalent to a monad's bind operator
   * 'withFilter' desugars the 'if' guard
   */
  case class Result [A] (result: A) {
    def foreach     (f: A => Unit):       Unit       = f (result)
    def map [B]     (f: A => B):          Result [B] = Result (f (result))
    def flatMap [B] (f: A => Result [B]): Result [B] = f (result)
    def withFilter  (f: A => Boolean)   : Result [_] = if (f(result)) this else EmptyResult
  }

  // Identity (empty object) representing the empty result for filter
  object EmptyResult extends Result [Null](null)

}
