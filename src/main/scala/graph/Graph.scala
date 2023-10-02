package graph

import scala.collection.mutable.ArrayBuffer


class Vertex[N, E](val identifier: Long, val n: N) {

  var index: Int = 0

  val successors: Edges[N, E] = Edges[N, E]()
  val predecessors: Edges[N, E] = Edges[N, E]()

  def addSuccessor(vertex: Vertex[N, E], e: E): Edge[N, E] = {
    val edge = Edge[N, E](e, this, vertex)
    this.successors.add(edge)
    vertex.predecessors.add(edge)

    edge
  }

  def addPredecessor(node: Vertex[N, E], e: E): Edge[N, E] = {
    val edge = Edge[N, E](e, node, this)
    this.successors.add(edge)
    node.predecessors.add(edge)

    edge
  }

  def countSibling() : Int = this.successors.count + this.predecessors.count

  def getSibling(index: Int) : Edge[N, E] = {
    if (index >= 0 && index < this.successors.count) {
      this.successors.get(index)
    }
    else if (index >= 0 && index < this.countSibling()) {
      this.predecessors.get(index - this.successors.count)
    }
    else {
      throw new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${this.countSibling() - 1})")
    }
  }

  override def toString = s"Node($identifier)"
}

class Vertices[N, E]() {
  protected[graph] val vertices = ArrayBuffer[Vertex[N, E]]()

  def add(node: Vertex[N, E]): Unit = {
    node.index = this.vertices.length
    this.vertices.append(node)
  }

  def remove(node: Vertex[N, E]): Unit = {
    if (node.index < this.vertices.length - 1) {
      val lastNode = this.vertices.apply(this.vertices.length - 1)
      this.vertices.update(node.index, lastNode)
      lastNode.index = node.index
    }
    this.vertices.remove(this.vertices.length - 1)
  }

  def iterator(): Iterator[Vertex[N, E]] = {
    this.vertices.iterator
  }

  def count: Int = this.vertices.length

  def find(identifier: Long) : Option[Vertex[N, E]] = {
    this.vertices.find{ (p: Vertex[N, E]) => identifier == p.identifier }
  }
}

case class Edge[N, E](e: E,
                      start: Vertex[N, E],
                      end: Vertex[N, E]) {
  var indexStart: Int = 0
  var indexEnd: Int = 0

  def remove(): Unit = {
    this.start.successors.remove(this)
  }

  def otherNode(node: Vertex[N, E]) : Vertex[N, E] =  if this.start == node then this.end else this.start
}

class Edges[N, E]() {
  private val edges = ArrayBuffer[Edge[N, E]]()

  protected[graph] def add(edge: Edge[N, E]): Unit = {
    this.edges.append(edge)
    if (edge.start.successors == this) {
      edge.indexStart = this.edges.length - 1
    }
    else {
      edge.indexEnd = this.edges.length - 1
    }
  }

  def remove(edge: Edge[N, E]): Unit = {
    if (edge.start.successors == this) {
      this.removeEdgeStart(edge)
      edge.end.predecessors.removeEdgeEnd(edge)
    }
    else {
      this.removeEdgeEnd(edge)
      edge.start.successors.removeEdgeStart(edge)
    }
  }

  protected[graph] def removeEdgeStart(edge: Edge[N, E]): Unit = {
    require(this.edges.apply(edge.indexStart) == edge)
    val index = edge.indexStart
    val lastEdge = this.edges.remove(this.edges.length - 1)
    if (index < this.edges.length) {
      this.edges.update(index, lastEdge)
      lastEdge.indexStart = index
    }
  }

  protected[graph] def removeEdgeEnd(edge: Edge[N, E]): Unit = {
    require(this.edges.apply(edge.indexEnd) == edge)
    val index = edge.indexEnd
    val lastEdge = this.edges.remove(this.edges.length - 1)
    if (index < this.edges.length) {
      this.edges.update(index, lastEdge)
      lastEdge.indexEnd = index
    }
  }

  def iterator(): Iterator[Edge[N, E]] = {
    this.edges.iterator
  }

  def get(index: Int) : Edge[N, E] = {
    this.edges.apply(index)
  }

  def count : Int = this.edges.length
}

case class Graph[N, E]() {

  val nodes: Vertices[N, E] = Vertices[N, E]()

  var lastIdentifier : Long = 0

  def addNode(n: N): Vertex[N, E] = {
    lastIdentifier += 1
    val node = Vertex[N, E](lastIdentifier, n)
    this.nodes.add(node)
    node
  }

  def nodesCount : Int = this.nodes.count


  def check(): Boolean = {
    !this.nodes.iterator()
      .flatMap{ (n: Vertex[N, E]) => n.successors.iterator() }
      .map{ (succ: Edge[N, E]) => this.nodes.find(succ.end.identifier) }
      .contains { Option.empty }
  }
}
