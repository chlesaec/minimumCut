package mincut

import fibonacciheap.Node

class QueueItemForMinCut[T](override val vertex: T,
                            weight: Int,
                            val queue: PriorityQueueForMinCut[T]) extends QueueItem[T](vertex, weight) {
  var node: Null|Node[Int, QueueItem[T]] = null

  override def increaseWeight(value: Int) : Unit = {
    this.weight += value
    node match
      case n: Node[Int, QueueItem[T]] => n.decreaseKey(queue.heap, value)
      case null => ()
  }

}

class PriorityQueueForMinCut[T] extends PriorityQueue[T] {

  val heap = fibonacciheap.Heap[Int, QueueItem[T]]()

  var counter: Int = 0

  override def insert(vertex: T, weigth: Int): QueueItem[T] = {
    this.counter += 1
    val item = QueueItemForMinCut(vertex, weigth, this)
    val node = heap.insert(-weigth, item)
    item.node = node
    item
  }

  override def findMaxWeight(): QueueItem[T]|Null = {
    heap.min() match
      case null => null
      case node : Node[Int, QueueItem[T]] => node.value
  }

  override def deleteMaxWeight(): QueueItem[T]|Null = {
    this.counter -= 1
    val min = this.heap.deleteMin()
    min match
      case null => null
      case node: Node[Int, QueueItem[T]] => node.value
  }

  override def count(): Int = this.counter
}
