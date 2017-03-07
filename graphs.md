Breadth First Search
```Scala
/**
  * Distance will be shortest
  */
def bfs(gg: Graph, startId: String): BFS_Result = {
  val start = gg.getVertexById(startId)
  val vertices = gg.vertices
  val colorMap = mutable.HashMap.empty[String, String]
  val distanceMap = mutable.HashMap.empty[String, Int]
  val parentMap = mutable.HashMap.empty[String, String]

  implicit class RichVertex(vv: Node) {
    def color: String = colorMap(vv.id)
    def color_=(color: String): Unit = {
      colorMap(vv.id) = color
    }

    def distance = distanceMap(vv.id)
    def distance_=(distance: Int) = distanceMap(vv.id) = distance

    def parent = parentMap(vv.id)
    def parent_=(parent: String) = parentMap(vv.id) = parent
  }

  vertices.foreach { vv =>
    vv.color = "WHITE"
    vv.distance = Int.MaxValue
    vv.parent = ""
  }

  start.color = "GRAY"
  start.distance = 0
  start.parent = ""

  val qq = mutable.Queue.empty[String]
  qq.enqueue(start.id)

  while(qq.nonEmpty) {
    val uId = qq.dequeue()
    val uu = gg.getVertexById(uId)
    gg.getAdjacentVertices(uu.id).foreach { vv =>
      if (vv.color == "WHITE") {
        vv.color = "GRAY"
        vv.distance = uu.distance + 1
        vv.parent = uu.id
        qq.enqueue(vv.id)
      }
      uu.color = "BLACK"
    }
  }
  BFS_Result(colorMap.toMap, distanceMap.toMap, parentMap.toMap)
}
```

Depth First Search
```Scala
def dfs(gg: Graph): DFS_Result = {
  val vertices = gg.vertices
  val colorMap = mutable.HashMap.empty[String, String]
  val parentMap = mutable.HashMap.empty[String, String]
  val discoverTimeMap = mutable.HashMap.empty[String, Int]
  val finishTimeMap = mutable.HashMap.empty[String, Int]
  var time = 0

  implicit class RichVertex(vv: Node) {
    def color: String = colorMap(vv.id)
    def color_=(color: String): Unit = {
      colorMap(vv.id) = color
    }

    def parent = parentMap(vv.id)
    def parent_=(parent: String) = parentMap(vv.id) = parent

    def discoverTime = discoverTimeMap(vv.id)
    def discoverTime_=(time: Int) = discoverTimeMap(vv.id) = time

    def finishTime = finishTimeMap(vv.id)
    def finishTime_=(time: Int) = finishTimeMap(vv.id) = time
  }

  vertices.foreach { vv =>
    vv.color = "WHITE"
    vv.parent = ""
  }

  def dfs_visit(gg: Graph, uu: Node): Unit = {
    time += 1
    uu.discoverTime = time
    uu.color = "GRAY"
    gg.getAdjacentVertices(uu.id).foreach { vv =>
      if (vv.color == "WHITE") {
        vv.parent = uu.id
        dfs_visit(gg, vv)
      }
    }
    uu.color = "BLACK"
    time += 1
    uu.finishTime = time
  }

  vertices.foreach { vv =>
    if (vv.color == "WHITE") {
      dfs_visit(gg, vv)
    }
  }
  DFS_Result(colorMap.toMap)
}
```

Minimum Spanning Tree Kruskal
```Scala
def mst_Kruskal(gg: Graph, weightMap: Edge => Int): Seq[Edge] = {
  var edgesResult = mutable.ArrayBuffer.empty[Edge]
  var sets = mutable.HashMap(gg.vertices.map(vv => vv.id -> mutable.HashSet(vv.id)): _*)

  def findSet(x: String): (String, mutable.HashSet[String]) = {
    sets.find { case (key, set) =>
      set.contains(x)
    }.get
  }

  def union(aa: String, bb: String) = {
    val (aaKey, aaValues) = findSet(aa)
    val (bbKey, bbValues) = findSet(bb)
    sets -= bbKey
    sets(aaKey) = aaValues.union(bbValues)
  }

  gg.edges.sortBy(weightMap).foreach { ee =>
    if (findSet(ee.from)._1 != findSet(ee.to)._1) {
      edgesResult += ee
      union(ee.from, ee.to)
    }
  }
  edgesResult.toList
}
```

Minimum Spanning Tree Prim
```Scala
def mst_Prim(gg: Graph, weightMap: Edge => Int, rootId: String): Seq[Edge] = {
  val parentMap = mutable.HashMap.empty[String, String]
  val keyMap = mutable.HashMap.empty[String, Int] // Minimum weight connecting v to vertex in tree

  object qq {
    private val data = ArrayBuffer.empty[String]

    def addAll(vs: Seq[String]): Unit = {
      data.appendAll(vs)
    }

    def nonEmpty: Boolean = data.nonEmpty

    def dequeue(): String = {
      val (minV, index) = data.zipWithIndex.minBy {
        case (vv, _) =>
          keyMap(vv)
      }
      data.remove(index)
      minV
    }

    def contains(v: String): Boolean = {
      data.contains(v)
    }
  }

  implicit def node2Id(node: Node): String = node.id

  gg.vertices.foreach { vv =>
    parentMap(vv) = ""
    keyMap(vv) = Int.MaxValue
  }
  keyMap(rootId) = 0

  qq.addAll(gg.vertices.map(vv => vv.id))

  while (qq.nonEmpty) {
    val uu = qq.dequeue()
    gg.getAdjacentVertices(uu).foreach { vv =>
      if (qq.contains(vv) && weightMap(EdgeImpl(uu, vv)) < keyMap(vv)) {
        parentMap(vv) = uu
        keyMap(vv) = weightMap(EdgeImpl(uu, vv))
      }
    }
  }

  val result = gg.vertices.filter(_.id != rootId).map { vv =>
    EdgeImpl(vv, parentMap(vv))
  }.toList
  result
}
```