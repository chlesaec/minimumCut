package fibonacciheap

import scala.collection.mutable.ListBuffer

class Heap[K, N](using ev: Numeric[K]) extends Element[K, N] {

  def insert(key: K, value: N): Node[K, N] = {
    Node(key, value, this)
  }

  def min(): Node[K, N] | Null = child


  override def getKey: String = "Heap"

  def deleteMin(): Node[K, N] | Null = {
    val currentChild: Node[K, N] | Null = this.child
    if (currentChild == null) {
      null
    }
    else {
      this.removeMin(currentChild)
      this.child
    }
  }

  private def removeMin(currentMin: Node[K, N]): Node[K, N] | Null = {
    val nextSibling: Node[K, N] | Null = currentMin.removeFromSibling()
    val childMin: Node[K, N] | Null = currentMin.child
    if (childMin != null) {
      if (nextSibling != null) {
        this.rank += currentMin.rank
        val unionNode = childMin.addSibling(nextSibling).findMinSibling()
        this.child = unionNode
        this.child.parent = this
        this.child.forEachSibling[Unit] { (_, s: Node[K, N]) => s.parent = this }

        unionNode.balance(this)
      }
      else {
        this.child = childMin
        this.child.parent = this
        this.child.forEachSibling[Unit] { (_, s: Node[K, N]) => s.parent = this }
        this.rank = currentMin.rank

        childMin.balance(this)
      }
    }
    else if (nextSibling != null) {
      nextSibling.balance(this)
    }
    else {
      null
    }
  }

  def delete(key: K) = {

  }
}

abstract class Element[K, N](using ev: Numeric[K]) {
  var rank: Int = 0

  var child: Node[K, N] | Null = null

  def addChild(node: Node[K, N],
               nbSibling: Int): Node[K, N] = {
    val currentRoot: Node[K, N] | Null = this.child
    node.forEachSibling[Unit] { (_, s: Node[K, N]) => s.parent = this }

    if (currentRoot == null) {
      this.child = node
    }
    else {
      node.addSibling(currentRoot)
      if (ev.lt(node.key, currentRoot.key)) {
        this.child = node
      }
    }
    this.rank += nbSibling + 1
    node
  }

  def getKey: String = ""
}


case class Node[K, N](var key: K,
                      value: N,
                      var parent: Element[K, N] | Null)(implicit ev: Numeric[K])
  extends Element[K, N] {

  var marked: Boolean = false

  var rightSibling: Node[K, N] = this

  var leftSibling: Node[K, N] = this

  {
    if (parent != null) {
      parent.addChild(this, 0)
    }
  }


  override def getKey: String = this.key.toString

  /**
   * ... => [ this ] => [A1] =>  ...
   * ... => [ node ] => [B1] => ...
   * to
   * ... => [ this ] =>  [B1] => ... => [ node ] =>  [A1] => ...
   *
   * @param node
   */
  def addSibling(node: Node[K, N]): Node[K, N] = {
    node.parent = this.parent
    node.forEachSibling[Unit] { (* : Unit | Null, s: Node[K, N]) =>
      s.parent = this.parent
      null
    }
    val currentRight = this.rightSibling
    this.linkToRight(node.rightSibling)
    node.linkToRight(currentRight)
    if (ev.lt(this.key, node.key)) {
      this
    }
    else {
      node
    }
  }

  def removeFromSibling(): Node[K, N] | Null = {
    val right = this.rightSibling
    if (this.parent != null && this.parent.rank == 0) {
      println("Pr rank")
    }
    val currentParent = this.parent
    if (currentParent != null) {
      currentParent.rank -= 1
    }
    if (right == this) {
      if (currentParent != null) {
        currentParent.child = null
      }
      this.parent = null
      null
    }
    else {
      right.leftSibling = this.leftSibling
      this.leftSibling.rightSibling = right

      if (currentParent != null && currentParent.child == this) {
        currentParent.child = right.findMinSibling()
      }
      this.rightSibling = this
      this.leftSibling = this

      this.parent = null
      right
    }

  }

  private def linkToRight(newRight: Node[K, N]): Unit = {
    this.rightSibling = newRight
    newRight.leftSibling = this
  }

  // mix all node with same rank
  def balance(currentParent: Element[K, N]): Node[K, N] = {
    // search max rank sibling
    val (maxRank, counter): (Int, Int) = this.forEachSibling[(Int, Int)] {
      (input: (Int, Int) | Null, node: Node[K, N]) =>
        input match
          case null => (node.rank, 1)
          case x: (Int, Int) => (Math.max(node.rank, x._1), x._2 + 1)
    }

    // ranges this node + right sibling by rank
    val nodes: Array[ListBuffer[Node[K, N]]] = Array.tabulate[ListBuffer[Node[K, N]]](maxRank + counter)(_ => ListBuffer.empty)
    nodes.apply(this.rank).append(this)
    var next: Node[K, N] = this.rightSibling
    while (next != this) {
      nodes.apply(next.rank).append(next)
      next = next.rightSibling
    }

    for (x <- 0 until nodes.length - 1) {
      val list = nodes.apply(x)
      while (list.length >= 2) {
        val n1: Node[K, N] = list.remove(list.length - 1)
        val n2: Node[K, N] = list.remove(list.length - 1)

        n1.removeFromSibling()
        n2.removeFromSibling()

        val unionNode: Node[K, N] = Node.union[K, N](n1, n2, null, null)
        currentParent.addChild(unionNode, 0)

        val nextList = nodes.apply(x + 1)
        nextList.append(unionNode)
      }
    }

    val min: Node[K, N] | Null = currentParent.child match
      case null => null
      case n: Node[K, N] => n.findMinSibling()

    min
  }

  def findMinSibling(): Node[K, N] = {
    this.forEachSibling {
      (n1: Node[K, N] | Null, n2: Node[K, N]) =>
        if (n1 == null) {
          n2
        }
        else if (ev.lt(n1.key, n2.key)) {
          n1
        }
        else {
          n2
        }
    }
  }

  def forEachSibling[T](f: (T | Null, Node[K, N]) => T): T = {
    var result: T = f(null, this)
    var next: Node[K, N] = this.rightSibling
    while (next != this) {
      result = f(result, next)
      next = next.rightSibling
    }
    result
  }


  def decreaseKey(heap: Heap[K, N], delta: K) = {
    this.key = ev.minus(this.key, delta)
    this.removeFromSibling();
    heap.addChild(this, 0)
  }

  def delete() = {

  }
}

object Node {
  def union[K, N]
  (n1: Node[K, N],
   n2: Node[K, N],
   parent1: Node[K, N] | Null,
   parent2: Node[K, N] | Null)(using ev: Ordering[K]): Node[K, N] = {

    if (ev.lt(n1.key, n2.key)) {
      val nbSibling: Int = if parent2 == null then 0 else parent2.rank - 1
      n1.addChild(n2, nbSibling)
      n1
    }
    else {
      n2.addChild(n1, if parent1 == null then 0 else parent1.rank - 1)
      n2
    }
  }

  def checkNode[K, N](element: Element[K, N])(implicit ev: Ordering[K]): String = {
    var err: String = ""
    var nbeChild: Int = 0
    val firstChild: Node[K, N] | Null = element.child
    if (element.rank < 0) {
      err += s"\n Node ${element.getKey} rank  ${element.rank} < 0"
    }
    if (element.rank > 0 && firstChild == null) {
      println(s"Node ${element.getKey} : pb child null, rank > 0 (${element.rank})")
    }
    if (firstChild != null) {
      val rankTheory = firstChild.forEachSibling[Int] {
        (nbe: Int | Null, sibling: Node[K, N]) =>
          nbe match
            case null => 1
            case n: Int => n + 1
      }
      if (rankTheory != element.rank) {
        err += s" \nNode ${element.getKey}  rank ${element.rank} should be $rankTheory"
      }

      val minFound: K = firstChild.forEachSibling[K] {
        (minKey: Null | K, sibling: Node[K, N]) =>
          minKey match
            case null => sibling.key
            case value: K => if ev.lt(value, sibling.key) then value else sibling.key
      }
      if (ev.lt(minFound, firstChild.key)) {
        err += s"\n ${firstChild.key} should be min, but this is ${minFound}"
      }
      var nextChild: Node[K, N] = firstChild.rightSibling
      while (nextChild != firstChild) {
        if (nextChild.parent != firstChild.parent) {
          err += s"\n parent ${nextChild.key} != ${firstChild.key}"
        }
        nextChild = nextChild.rightSibling
      }

      if (element.isInstanceOf[Node[K, N]]) {
        val node = element.asInstanceOf[Node[K, N]]
        nbeChild = nbeChild + 1
        err += this.checkNode(firstChild)
        var nextChild: Node[K, N] = firstChild.rightSibling
        while (nextChild != firstChild) {
          if (ev.lteq(nextChild.key, node.key)) {
            err += s"\n Next ${nextChild.key} <= ${node.key}"
            nextChild = firstChild
          }
          else {
            err += this.checkNode(nextChild)
            nextChild = nextChild.rightSibling
          }
          nbeChild = nbeChild + 1
        }
        if (element.rank != nbeChild) {
          err += s"\n Node ${node.key} ${node.rank} != nbe child ${nbeChild}"
        }
      }

      err += firstChild.forEachSibling[String] {
        (error: Null | String, sibling: Node[K, N]) =>
          val newErr = Node.checkNode(sibling)
          if (!newErr.isBlank) {
            error match
              case null => newErr
              case e: String => e + "\n" + newErr
          }
          else {
            error match
              case null => ""
              case e: String => e
          }
      }
    }
    err
  }
}
