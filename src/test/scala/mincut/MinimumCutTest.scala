package mincut

import graph.Graph
import org.junit.jupiter.api.{Assertions, Test}

class MinimumCutTest {

  @Test
  def min(): Unit = {
    //val queue = PriorityQueueForMinCut[MinCutVertex]()
    val min = MinimumCut(() => PriorityQueueForMinCut[MinCutVertex]())
    val graph = Graph[Null | VertexData, Int]()
    val n1 = graph.addNode(null)
    val n2 = graph.addNode(null)
    val n3 = graph.addNode(null)
    val n4 = graph.addNode(null)
    val n5 = graph.addNode(null)
    val n6 = graph.addNode(null)
    val n7 = graph.addNode(null)
    val n8 = graph.addNode(null)

    n1.addSuccessor(n2, 2)
    n1.addSuccessor(n5, 3)

    n2.addSuccessor(n3, 3)
    n2.addSuccessor(n5, 2)
    n2.addSuccessor(n6, 2)

    n3.addSuccessor(n4, 4)
    n3.addSuccessor(n7, 2)

    n4.addSuccessor(n7, 2)
    n4.addSuccessor(n8, 2)

    n5.addSuccessor(n6, 3)

    n6.addSuccessor(n7, 1)

    n7.addSuccessor(n8, 3)

    val result: MincutResult = min.minimumCut(graph)
    Assertions.assertEquals(4, result.value)
  }
}
