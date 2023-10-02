package mincut

import org.junit.jupiter.api.{Assertions, Test}

class MinCutPriorityQueueTest {

  @Test
  def weight() = {
    val queue = PriorityQueueForMinCut[String]()

    val item1 = queue.insert("W1", 10)
    Assertions.assertSame(item1, queue.findMaxWeight())

    val item2 = queue.insert("W2", 20)
    Assertions.assertSame(item2, queue.findMaxWeight())

    val item3 = queue.insert("W3", 15)
    Assertions.assertSame(item2, queue.findMaxWeight())

    item1.increaseWeight(15)
    Assertions.assertSame(item1, queue.findMaxWeight())
  }
}
