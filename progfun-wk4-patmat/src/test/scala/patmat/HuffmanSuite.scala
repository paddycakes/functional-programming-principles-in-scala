package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Leaf('a',2)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
  
  test("weight of a single leaf tree") {
    new TestTrees {
      assert(weight(t3) === 2)
    }
  }  

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("chars of a single leaf tree") {
    new TestTrees {
      assert(chars(t3) === List('a'))
    }
  }  

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times") {
    val l = times(string2Chars("hello, world"))
    println(l)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("makeOrderedLeafList for some frequency table with leaves of same weight") {
    assert(makeOrderedLeafList(List(('x', 2), ('e', 2), ('t', 2))) === List(Leaf('e',2), Leaf('t',2), Leaf('x',2)))
  }
  
  test("singleton of nil") {
    assert(singleton(Nil) == false)
  }
  
  test("singleton of a list with one tree") {
    new TestTrees {    
      assert(singleton(List(t1)) == true)
    }
  }
  
  test("singleton of a longer list") {
    new TestTrees {    
      assert(singleton(List(t1, t2, t3)) == false)
    }
  }  
  
  // TODO: More tests for combine of longer lists (include Fork elements as well)

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine of a singleton") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e', 1)))
  }
  
  test("combine of empty list") {
    val leaflist = List()
    assert(combine(leaflist) === List())
  }
  
  test("create code tree") {
    val codeTree = createCodeTree(string2Chars("aaaaaaaabbbcdefgh"))
    println(codeTree)
  }
  
  test("decoded secret") {
    val secret = decodedSecret
    println(secret)
  }
  
  test("encode aab should give 001") {
    new TestTrees {    
      val bits = encode(t1)("aab".toList)
      assert(bits == List(0, 0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("convert code tree to code table") {
    new TestTrees {    
      val codeTable = convert(t2)
      println(codeTable)
    }    
  }
}
