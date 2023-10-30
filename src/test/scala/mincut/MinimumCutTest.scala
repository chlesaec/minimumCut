package mincut

import graph.Graph
import org.junit.jupiter.api.{Assertions, Test}

class MinimumCutTest {

  @Test
  def min(): Unit = {
    //val queue = PriorityQueueForMinCut[MinCutVertex]()
    val min = Minimumcut(() => PriorityQueueForMinCut[MinCutVertex]())
    val graph = Graph[Int, Int]()
    val n1 = graph.addNode(1)
    val n2 = graph.addNode(2)
    val n3 = graph.addNode(3)
    val n4 = graph.addNode(4)
    val n5 = graph.addNode(5)
    val n6 = graph.addNode(6)
    val n7 = graph.addNode(7)
    val n8 = graph.addNode(8)

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

   // val result : MinimumCutPhaseResult = min.minimumCut(graph)
  //  Assertions.assertEquals(4, result.value)
  }
}
