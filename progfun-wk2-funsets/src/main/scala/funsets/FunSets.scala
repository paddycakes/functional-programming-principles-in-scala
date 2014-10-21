package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  // NOTE: The following are higher-order functions because
  // they either take a function as a parameter or return a
  // function. The function in this case is:
  //      Int => Boolean
  // which has a type alias 'Set'
  
  
  // NOTE: The substitution model is very important here. 
  // Essentially there is only one Set definition - singletonSet
  // and Sets with more elements are just combinations (ie. unions)
  // of singletonSets. So every operations resolves to trees of 
  // singletonSets with substitution at the lowest level of each 
  // singletonSet to a functional comparison:
  //     Int => Boolean
  //     (x: Int) => x == elem

  
  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
  
  // Without type alias - for parameter type:
  // def contains(s: Int => Boolean, elem: Int): Boolean = s(elem)

  
  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem
  
  // Without type alias - for return type:
  // def singletonSet(elem: Int): Int => Boolean = (x: Int) => x == elem
 
  // Could also have used compiler inference to implement 
  // the function without an explicit parameter type, ie:
  // x => x == elem

  
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)
  
  // Without type alias - for parameter types and return type:
  // def union(s: Int => Boolean, t: Int => Boolean): Int => Boolean = x => s(x) || t(x)
  

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)
  
  // Without type alias - for parameter types and return type:
  // def intersect(s: Int => Boolean, t: Int => Boolean): Int => Boolean = x => s(x) && t(x)
  

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)
  
  // Without type alias - for parameter types and return type:
  // def diff(s: Int => Boolean, t: Int => Boolean): Int => Boolean = x => s(x) && !t(x)
  
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)
  
  // You are returning a function (ie. Set) which can then be used to determine if an 
  // arbitrary Integer is both in the parameter Set and meets the specified predicate.
  // p is actually itself, in fact, a 'Set' according to the type alias. It is the set 
  // of all Integers that are divisible by 2.
  
  // Without type alias - for parameter types and return type:
  // def filter(s: Int => Boolean, p: Int => Boolean): Int => Boolean = x => s(x) && p(x)

  
  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  // Without type alias - for parameter types and return type:
  // def forall(s: Int => Boolean, p: Int => Boolean): Boolean
  
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, negate(p))
  
  def negate(pred: Int => Boolean): Int => Boolean =
  	(x: Int) => !pred(x)
  	
  // NOTE:
  // The key to this solution is that we want to short-circuit the
  // evaluation somehow by triggering this return:
  //    else if (s(a) && !p(a)) false
  // How do we do this?
  // i. We need to negate the parameter function, for eg:
  //   (x > 10) 
  // evaluates to true, then we need to negate it to false so that the code 
  // in forall will in turn evaluate to true by:
  //   !(!(x > 10))
  // which evaluate from inwards out to
  //       (true)    = true
  //      !(true)    = false
  //    !(!(true))   = true
  // So we have now short-circuited a match. However, the return value from 
  // forall to exists is false so we again need to negate that:
  //     !forall(s, negate(p))
  //     !(false)     = true
  // and hence the exists function works as required.
 

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = 
    // x => exists(s, f(s) == x)
    x => exists(s, y => f(y) == x)
    
    // Need 2 anonymous functions
    
    

  
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
