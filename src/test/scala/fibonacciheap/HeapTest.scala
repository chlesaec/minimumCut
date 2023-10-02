package fibonacciheap

import org.junit.jupiter.api.{Assertions, Test}
import fibonacciheap.{ Node, Heap }

class HeapTest {

    @Test
    def heap() = {
        val heap : Heap[Int, Int] = Heap()
        heap.insert(20, 20)
        this.checkMin(heap, 20)

        heap.insert(30, 30)
        this.checkMin(heap, 20)

        heap.insert(10, 10)
        this.checkMin(heap, 10)

        val node15 = heap.insert(15, 15)
        this.checkMin(heap, 10)

        node15.decreaseKey(heap, 6)
        this.checkMin(heap, 9)

        heap.deleteMin()
        this.checkMin(heap, 10)

        heap.deleteMin()
        this.checkMin(heap, 20)
    }

    @Test
    def bigheap() = {
        val heap : Heap[Int, Int] = Heap()
        var heapTemp : Heap[Int, Int] = Heap()
        for (index: Int <- 1 to 103) {
            val key = ((index * 17) % 103) + 1
            //println(s"insert ${index} key=${key}")
            heapTemp.insert(key, key)
            checkStructure(heapTemp)
            if ((index % 10) == 0 || index == 103) {
                val min : Node[Int, Int]|Null = heapTemp.min()
                min match {
                    case null => Assertions.fail(s"no min for index ${index}")
                    case n: Node[Int, Int] => heap.addChild(n, heapTemp.rank - 1)
                }
                checkStructure(heap)
                heapTemp = Heap()
            }
        }
        this.checkStructure(heap)
        for (index: Int <- 1 to 103) {
            this.checkMin(heap, index)
            heap.deleteMin()
            this.checkStructure(heap)
        }
    }

    def checkMin(heap : Heap[Int, Int], expected: Int) = {
        val min: Node[Int, Int]|Null = heap.min()
        if (min != null && expected != min.key) {
            println("Err")
        }
        Assertions.assertEquals(expected, min match {
            case n: Node[Int, Int] => n.key
            case null => 0
        })
    }

    def checkStructure(heap : Heap[Int, Int]) = {
        val err = Node.checkNode(heap)
        if (!err.isBlank) {
            Assertions.fail(err)
        }
        val firstChild: Node[Int, Int]|Null = heap.min()
        if (firstChild != null) {
            this.checkNode(firstChild)
        }
    }

    def checkNode(node: Node[Int, Int]): Unit = {
        var nbeChild : Int = 0
        val firstChild: Node[Int, Int]|Null = node.child
        if (firstChild != null) {
            Assertions.assertTrue(firstChild.key > node.key, s"pb ${firstChild.key} <= ${node.key}")

            nbeChild = nbeChild + 1
            this.checkNode(firstChild)
            var nextChild :Node[Int, Int] = firstChild.rightSibling
            while (nextChild != firstChild) {
                Assertions.assertSame(firstChild.parent, nextChild.parent)
                Assertions.assertTrue(nextChild.key > node.key, s"pb ${nextChild.key} <= ${node.key}")

                this.checkNode(nextChild)
                nextChild = nextChild.rightSibling
                nbeChild = nbeChild + 1
            }
        }
        Assertions.assertEquals(node.rank, nbeChild, s"node ${node.key} has rank ${node.rank} but ${nbeChild} childs")
    }

}
