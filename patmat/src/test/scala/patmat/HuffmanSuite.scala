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
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("singleton correctly identifies single tree") {
    assert(singleton(List(Leaf('a', 2))) === true)
    assert(singleton(List(Leaf('a', 2), Leaf('b', 3))) === false)
  }

  test("convert generates code table") {
    val tree = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val codeTable = convert(tree)
    assert(codeTable === List(('a', List(0)), ('b', List(1))))
  }

  test("mergeCodeTables correctly merges code tables") {
    val table1 = List(('a', List(0)), ('b', List(1)))
    val table2 = List(('c', List(0, 1)), ('d', List(1, 0)))
    val mergedTable = mergeCodeTables(table1, table2)
    assert(mergedTable === List(('a', List(0)), ('b', List(1)), ('c', List(0, 1)), ('d', List(1, 0))))
  }

  test("quickEncode encodes text") {
    val tree = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    assert(quickEncode(tree)("ab".toList) === List(0, 1))
  }

  test("createCodeTree generates the correct Huffman tree") {
    val inputText = "abracadabra"
    // Expected Huffman code tree for the input text
    // Note: The expected tree structure depends on the character frequencies.
    val expectedTree: CodeTree = Fork(
      Leaf('a',5),
      Fork(
        Leaf('r',2),
        Fork(
          Fork(
            Leaf('c',1),
            Leaf('d',1),List('c', 'd'),2),
          Leaf('b',2),
          List('c', 'd', 'b'),4),
        List('r', 'c', 'd', 'b'),6),
      List('a', 'r', 'c', 'd', 'b'),11)

    val generatedTree = createCodeTree(string2Chars("abracadabra"))

    assert(generatedTree === expectedTree)
  }

}

