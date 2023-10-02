package mincut

import fibonacciheap.Node

class QueueItemForMinCut[T](override val vertex: T,
                         val queue: PriorityQueueForMinCut[T]) extends QueueItem[T](vertex) {
  var node: Null|Node[Int, QueueItem[T]] = null

  override def increaseWeight(value: Int) : Unit = {
    node match
      case n: Node[Int, QueueItem[T]] => n.decreaseKey(queue.heap, value)
      case null => ()
  }

}

class PriorityQueueForMinCut[T] extends PriorityQueue[T] {

  val heap = fibonacciheap.Heap[Int, QueueItem[T]]()

  override def insert(vertex: T, weigth: Int): QueueItem[T] = {
    val item = QueueItemForMinCut(vertex, this)
    val node = heap.insert(-weigth, item)
    item.node = node
    item
  }

  override def findMaxWeight(): QueueItem[T]|Null = {
    heap.min() match
      case null => null
      case node : Node[Int, QueueItem[T]] => node.value
  }

  override def deleteMaxWeight(): Unit = this.heap.deleteMin()
}
