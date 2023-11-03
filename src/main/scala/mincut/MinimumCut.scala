package mincut

import scala.collection.mutable.{ArrayBuffer, ArraySeq, ListBuffer, Seq}
import graph.{Edge, Graph, Vertex, Vertices}

import scala.collection.mutable

class VertexData(val vertex: Vertex[Null | VertexData, Int],
                 var queueItem: QueueItem[Vertex[Null | VertexData, Int]],
                 var mark: Boolean = false) {

}

type MinCutVertex = Vertex[Null | VertexData, Int]
type MinCutGraph = Graph[Null | VertexData, Int]
type MinCutEdge = Edge[Null | VertexData, Int]

abstract class QueueItem[T](val vertex: T,
                            var weight: Int) {
  def increaseWeight(value: Int): Unit
}

trait PriorityQueue[T] {
  def insert(vertex: T, weight: Int): QueueItem[T]

  def findMaxWeight(): QueueItem[T] | Null

  def deleteMaxWeight(): QueueItem[T] | Null

  def count(): Int
}

class GroupVertices(override val identifier: Long,
                    n: Null | VertexData,
                    val node1: MinCutVertex,
                    val node2: MinCutVertex)
  extends MinCutVertex(identifier, n) {

  def contains(n: MinCutVertex): Boolean = n == node1 || n == node2

  override def toString: String = {
    s"GroupVertices<${identifier}>(${node1}, ${node2})"
  }
}

object GroupVertices {
  def build(g: MinCutGraph, node1: MinCutVertex, node2: MinCutVertex): GroupVertices = {
    g.lastIdentifier += 1
    val group = GroupVertices(g.lastIdentifier, node1.n, node1, node2)
    g.nodes.add(group)
    updateEdges(group, node1, node2)
    updateEdges(group, node2, node1)
    g.nodes.remove(node1)
    g.nodes.remove(node2)
    group
  }

  private def updateEdges(group: MinCutVertex,
                          node1: MinCutVertex,
                          node2: MinCutVertex): Unit = {

    while (node1.countSibling() > 0) {
      val e = node1.getSibling(0)
      val otherNode: MinCutVertex = e.otherNode(node1)

      if (otherNode == node2) {
        e.remove()
      }
      else {
        val duplicateEdge = this.findEdgeToNode(otherNode, node2)
        duplicateEdge match
          case edge: MinCutEdge => {
            edge.remove()
            e.remove()
            group.addSuccessor(otherNode, e.edgeData + edge.edgeData)
          }
          case null => {
            e.remove()
            group.addSuccessor(otherNode, e.edgeData)
          }
      }
    }
  }

  private def findEdgeToNode(node1: MinCutVertex, node2: MinCutVertex): MinCutEdge | Null = {
    var index: Int = 0
    while (index < node1.countSibling()) {
      val e = node1.getSibling(index)
      if (e.end == node2 || e.start == node2) {
        return e
      }
      index += 1
    }
    null
  }
}


class MinimumCutPhaseResult(val g: MinCutGraph,
                            val last: MinCutVertex,
                            val before: MinCutVertex,
                            val value: Int) {
  private val gn: GroupVertices = {
    g.lastIdentifier += 1
    GroupVertices.build(g, last, before)
  }

  def allVertices(): List[MinCutVertex] = {
    val vertices = ListBuffer[MinCutVertex]()
    this.addVertices(vertices, last)
    vertices.toList
  }

  private def addVertices(vertices: ListBuffer[MinCutVertex], v: MinCutVertex): Unit = {
    v match
      case gv: GroupVertices => {
        this.addVertices(vertices, gv.node1)
        this.addVertices(vertices, gv.node2)
      }
      case v: MinCutVertex => vertices += v
  }
}

object MinimumCutPhaseResult {
  def build(g: MinCutGraph, last: MinCutVertex, before: MinCutVertex): MinimumCutPhaseResult = {
    val lastWeight = last.siblings().map(e => e.edgeData).sum
    val beforeWeight = before.siblings().map(e => e.edgeData).sum

    var n1 = last
    var n2 = before
    val min = math.min(lastWeight, beforeWeight)
    if (beforeWeight < lastWeight) {
      n1 = before
      n2 = last
    }

    MinimumCutPhaseResult(g, n1, n2, min)
  }
}

class MincutResult(val value: Int,
                   val vertices: List[MinCutVertex]) {
  def this(phaseResult: MinimumCutPhaseResult) = {
    this(phaseResult.value, phaseResult.allVertices())
  }
}

class MinimumCut(val priorityQueue: () => PriorityQueue[MinCutVertex]) {

  private def updateQueue(queue: PriorityQueue[MinCutVertex],
                          removedVertex: MinCutVertex) = {
    removedVertex.siblings()
      .foreach {
        (e: MinCutEdge) =>
          val vertex = e.otherNode(removedVertex)
          val data = vertex.n match {
            case null => {
              val item: QueueItem[MinCutVertex] = queue.insert(vertex, e.edgeData)
              vertex.n = new VertexData(vertex, item)
            }
            case v: VertexData => {
              if (!v.mark) {
                v.queueItem.increaseWeight(e.edgeData)
              }
            }
          }
      }

  }

  def minimumCutPhase(g: MinCutGraph): MinimumCutPhaseResult = {
    val nodes = ArrayBuffer[MinCutVertex]()

    val current: MinCutVertex = g.nodes.iterator().next()
    nodes += current

    val queue = this.priorityQueue()
    val item: QueueItem[MinCutVertex] = queue.insert(current, 0)
    current.n = new VertexData(current, item, true)

    queue.deleteMaxWeight() // To mark current vertex.
    this.updateQueue(queue, current)

    var last: QueueItem[MinCutVertex] = queue.findMaxWeight()
    var beforeLast: QueueItem[MinCutVertex] = last
    var lastMaxWeight: Int = 0

    while (queue.count() > 0) {
      // Find max connected nodes
      val item: QueueItem[MinCutVertex] | Null = queue.deleteMaxWeight()
      if (item != null) {
        item.vertex.n.mark = true

        nodes += item.vertex
        this.updateQueue(queue, item.vertex)

        beforeLast = last
        last = item
      }
    }

    MinimumCutPhaseResult.build(g, last.vertex, beforeLast.vertex)
  }

  def minimumCut(g: MinCutGraph): MincutResult = {
    val result = this.minimumCutPhase(g)

    var currentResult = result
    var minCutResult = result

    var continue = currentResult.g.nodesCount >= 2
    while (continue) {
      currentResult.g.nodes.iterator().foreach {
        vertex => if (vertex.n != null) vertex.n = null
      }
      currentResult = this.minimumCutPhase(currentResult.g)

      if (currentResult.value < minCutResult.value) {
        minCutResult = currentResult
      }
      if (currentResult.g.nodesCount <= 2) {
        continue = false
      }
    }
    MincutResult(minCutResult)
  }
}
