package mincut

import scala.collection.mutable.{ArrayBuffer, ArraySeq, Seq}
import graph.{Edge, Graph, Vertex, Vertices}

import scala.collection.mutable

class VertexData(val vertex: Vertex[VertexData, Int],
                 var connectionWeight: Int) {
  def getKey(): Int = connectionWeight
}

type MinCutVertex = Vertex[Null|VertexData, Int]
type MinCutGraph = Graph[Null|VertexData, Int]
type MinCutEdge = Edge[Null|VertexData, Int]

abstract class QueueItem[T](val vertex: T) {
  def increaseWeight(value: Int) : Unit
}

trait PriorityQueue[T] {
  def insert(vertex: T, weight: Int) : QueueItem[T]

  def findMaxWeight() : QueueItem[T]

  def deleteMaxWeight(): Unit
}

class GroupVertices(override val identifier: Long,
                    n: Null|VertexData,
                    val node1: MinCutVertex,
                    val node2: MinCutVertex)
  extends MinCutVertex(identifier, n) {

  def contains(n: MinCutVertex): Boolean = n == node1 || n == node2
}

object GroupVertices {
  def build(g: MinCutGraph, node1: MinCutVertex, node2: MinCutVertex) : GroupVertices = {
    g.lastIdentifier += 1
    val group = GroupVertices(g.lastIdentifier, node1.n, node1, node2)
    g.nodes.add(group)
    updateEdges(group, node1, node2)
    updateEdges(group, node2, node1)
    g.nodes.remove(node1)
    g.nodes.remove(node2)
    group
  }

  private def updateEdges(group: GroupVertices,
                          node1: MinCutVertex,
                          node2: MinCutVertex): Unit = {
    while (node1.countSibling() > 0) {
      val e = node1.getSibling(0)
      if (e.end == this || e.start == this) {
        e.remove()
      }
      else {
        val otherNode: MinCutVertex = e.otherNode(node1)
        if (otherNode == node2) {
          e.remove()
        }
        else {
          val duplicateEdge = this.findEdgeToNode(otherNode, node2)
          duplicateEdge match
            case edge: MinCutEdge => {
              e.remove()
              edge.remove()
              group.addSuccessor(otherNode, e.e + edge.e)
            }
            case null => {
              e.remove()
              group.addSuccessor(otherNode, e.e)
            }
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
}


class Minimumcut(val priorityQueue: () => PriorityQueue[MinCutVertex]) {

  def weight(node: MinCutVertex, seq: Seq[MinCutVertex]): Int = {
    val predSum = node.predecessors.iterator()
      .filter {
        (e: MinCutEdge) => seq.contains(e.start)
      }
      .foldLeft(0) {
        (v: Int, edge: MinCutEdge) => edge.e + v
      }
    val succSum = node.successors.iterator()
      .filter {
        (e: MinCutEdge) => seq.contains(e.end)
      }
      .foldLeft(0) {
        (v: Int, edge: MinCutEdge) => edge.e + v
      }
    predSum + succSum
  }

  private def updateQueue(queue: PriorityQueue[MinCutVertex],
                          addedVertex: MinCutVertex) = {
    addedVertex.successors.iterator()
      .map { (e: MinCutEdge) => e.end }
    val data = new VertexData(addedVertex, 1)
    addedVertex.n = data
  }

  def minimumCutPhase(g: MinCutGraph): MinimumCutPhaseResult = {
    val nodes = ArrayBuffer[MinCutVertex]()
    val leave = ArrayBuffer[MinCutVertex]()
    leave ++= g.nodes.iterator()
    val current: MinCutVertex = leave.remove(leave.length - 1)
    nodes += current

    val queue = this.priorityQueue()

    var last: MinCutVertex = current
    var beforeLast: MinCutVertex = current
    var lastMaxWeight: Int = 0


    while (leave.length > 0) {
      val maxWeight: (MinCutVertex, Int, Int) = leave.zipWithIndex.map {
        (node: MinCutVertex, index: Int) => (node, this.weight(node, nodes), index)
      }
        .reduce {
          (n1: (MinCutVertex, Int, Int),
           n2: (MinCutVertex, Int, Int)) =>
            if (n1._2 > n2._2) n1 else n2
        }
      nodes += maxWeight._1
      beforeLast = last
      last = maxWeight._1
      leave.remove(maxWeight._3)
      lastMaxWeight = maxWeight._2
    }
    println(s"Phase end with last:${last.identifier} + ${beforeLast.identifier}; weight ${lastMaxWeight}")
    MinimumCutPhaseResult(g, last, beforeLast, lastMaxWeight)
  }

  def minimumCut(g: MinCutGraph): MinimumCutPhaseResult = {
    val result = this.minimumCutPhase(g)
    println(s"check graph ${g.check()}")

    var currentResult = result
    var minCutResult = result

    var continue = currentResult.g.nodesCount >= 2
    while (continue) {
      currentResult = this.minimumCutPhase(currentResult.g)
      println(s"check graph ${g.check()}")
      if (currentResult.value < minCutResult.value) {
        minCutResult = currentResult
      }
      if (currentResult.g.nodesCount <= 2) {
        continue = false
      }
    }
    minCutResult
  }
}
