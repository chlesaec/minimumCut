package fibonacciheap

import org.junit.jupiter.api.{Assertions, Test}

import scala.collection.mutable;



class NodeTest {

    @Test
    def addSibling() = {
        val heap = Heap[Int, String]()
        val n1 = Node(1, "A1", heap)
        this.checkNode(n1, 1);

        val n2 = Node(2, "A2", heap);
        this.checkNode(n1, 2);
        this.show(n1)

        val n3 = Node(3, "A3", heap);
        this.checkNode(n1, 3);
        this.show(n1)

        val n4 = Node(4, "A4", heap);
        this.checkNode(n1, 4);
        this.show(n1)

        val n5 = Node(5, "A5", heap);
        this.checkNode(n1, 5);
        this.show(n1)

        val heap2 = Heap[Int, String]()
        val m1 = Node(1, "B1", heap2);
        val m2 = Node(2, "B2", heap2);
        val m3 = Node(3, "B3", heap2);
        val m4 = Node(4, "B4", heap2);
        val m5 = Node(5, "B5", heap2);

        n1.addSibling(m1)
        this.show(n1)
        this.checkNode(n1, 10);
    }

    private def checkNode(n : Node[Int, String], size : Int) = {
        var current = n
        val labels :  mutable.HashSet[String] = mutable.HashSet()
        var i : Int = 0
        while (i < size) {
            labels.add(current.value)
            Assertions.assertSame(current, current.leftSibling.rightSibling)
            Assertions.assertSame(current, current.rightSibling.leftSibling)
            if (size >= 2) {
                Assertions.assertNotSame(current, current.rightSibling)
                Assertions.assertNotSame(current, current.leftSibling)
            }
            current = current.rightSibling
            i += 1
        }
        // back to first
        Assertions.assertEquals(size, labels.size)
        Assertions.assertSame(current, n, s"Not back to current for ${size}")
    }

    private def show(n: Node[Int, String]) : Unit = {
        print(s"${n.value}");
        var current : Node[Int, String] = n.rightSibling;
        while (current != n) {
            print(s"-->${current.value}");
            current = current.rightSibling;
        }

        println("")
    }
}
