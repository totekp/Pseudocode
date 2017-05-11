
Breadth First Search
```Scala
/**
  * Distance will be shortest
  */
def bfs(gg: Graph,
        startId: String,
        adjProcessor: Option[Edge => Seq[Edge]] = None,
        processVertexEarly: Option[Node => Unit] = None,
        processVertexLate: Option[Node => Unit] = None,
        processEdge: Option[Edge => Unit] = None
       ): BFSOutput = {

  val start = gg.getVertexById(startId)

  // Use external versions for custom parameters
  val statusMap = mutable.HashMap.empty[String, String].withDefaultValue(UNDISCOVERED)
  val distanceMap = mutable.HashMap.empty[String, BigInt].withDefaultValue(INF)
  val parentMap = mutable.HashMap.empty[String, String].withDefaultValue("")

  implicit class RichVertex(vv: Node) {
    def status: String = statusMap(vv.id)

    def status_=(status: String): Unit = {
      statusMap(vv.id) = status
    }

    def distance = distanceMap(vv.id)

    def distance_=(distance: BigInt) = distanceMap(vv.id) = distance

    def parent = parentMap(vv.id)

    def parent_=(parent: String) = parentMap(vv.id) = parent
  }

  start.status = DISCOVERED
  start.distance = 0

  val qq = mutable.Queue.empty[String]
  qq.enqueue(start.id)

  while (qq.nonEmpty) {
    val uId = qq.dequeue()
    val uu = gg.getVertexById(uId)
    processVertexEarly.foreach(_(uu))
    val adjVertices = {
      adjProcessor match {
        case Some(processor) =>

          gg.getAdjacentVertices(uu.id)
            .flatMap(aa => EdgeImpl(uu.id, aa.id) |> processor)
            .map(ee => gg.getVertexById(ee.to))

        case _ =>
          gg.getAdjacentVertices(uu.id)
      }
    }
    adjVertices.foreach { vv =>
      if (vv.status == UNDISCOVERED) {
        vv.status = DISCOVERED
        processEdge.foreach(_(EdgeImpl(uu.id, vv.id)))
        vv.distance = uu.distance + 1
        vv.parent = uu.id
        qq.enqueue(vv.id)
      }
      uu.status = PROCESSED
    }
    processVertexLate.foreach(_(uu))
  }
  BFSOutput(statusMap.toMap, distanceMap.toMap, parentMap.toMap)
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
    keyMap(vv) = INF
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

Bellman Ford Single Source Shortest Path
```Scala
def bellmanFord(gg: Graph, weightMap: Edge => Int, startId: String): Option[BellmanFord_Result] = {
  implicit def node2Id(node: Node): String = node.id

  val distanceMap = mutable.HashMap.empty[String, BigInt]
  val parentMap = mutable.HashMap.empty[String, String]

  def initializeSingleSource(): Unit = {
    gg.vertices.foreach { vv =>
      distanceMap(vv) = INF
    }
    distanceMap(startId) = 0
  }

  def relax(u: String, v: String): Unit = {
    val alternateDistance = distanceMap(u) + weightMap(EdgeImpl(u, v))
    if (distanceMap(v) > alternateDistance) {
      distanceMap(v) = alternateDistance
      parentMap(v) = u
    }
  }

  initializeSingleSource()
  (1 to gg.vertices.length).foreach { ii =>
    gg.edges.foreach { ee =>
      relax(ee.from, ee.to)
    }
  }

  gg.edges.foreach { ee =>
    if (distanceMap(ee.to) > distanceMap(ee.from) + weightMap(EdgeImpl(ee.from, ee.to))) {
      return None
    }
  }
  Some(BellmanFord_Result(distanceMap.toMap, parentMap.toMap))
}
```

Floyd-Warshall All Pairs Shortest Paths
```Scala
def floydWarshall(gg: Graph, weightMap: Edge => Int): FloydWarshall_Result = {
  implicit def node2Id(node: Node): String = node.id

  val vertices = gg.vertices.toVector.sortBy(_.id)
  val n = vertices.length
  object dd {
    private val data = Array.fill(n + 1, n, n)(BigInt(INF))

    def apply(k: Int, i: Int, j: Int): BigInt = {
      data(k)(i)(j)
    }

    def update(k: Int, i: Int, j: Int, value: BigInt): Unit = {
      data(k)(i)(j) = value
    }

    def getData: Array[Array[Array[BigInt]]] = {
      data
    }

    def updateKTable(k: Int, weightFn: (Int, Int) => BigInt): Unit = {
      for {
        i <- 0 until n
        j <- 0 until n
      } {
        dd(k, i, j) = weightFn(i, j)
      }
    }
  }

  dd.updateKTable(0, (i, j) => {
    weightMap(EdgeImpl(vertices(i), vertices(j)))
  })

  for {
    k <- 1 to n
  } {
    dd.updateKTable(k, (i, j) => dd(k - 1, i, j).min(dd(k - 1, i, k - 1) + dd(k - 1, k - 1, j)))
  }

  FloydWarshall_Result(vertices.map(_.id), dd.getData.map(_.toVector.map(_.toVector)).toVector)
}
```

Ford-Fulkerson Edmond-Karp Maximum Flow
```Scala
def fordFulkerson(gg: Graph, capacities: Edge => Int, s: String, t: String) = {
  var parentMap = Map.empty[String, String]
  val residualMap = mutable.HashMap.empty[Edge, Double].withDefaultValue(0)
  var maxFlow = 0d
  gg.edges.foreach { ee =>
    residualMap(ee) = capacities(ee)
  }

  // Edmond-Karp bfs shortest strategy
  while({
    val result = bfs(gg, s, adjPipe = Some {
      case ee =>
        if (residualMap(ee) > 0) List(ee) else Nil
    })
    parentMap = result.parentMap
    result.colorMap.get(t).exists(_ != "WHITE")
  }) {
    var pathFlow = Double.PositiveInfinity
    var tTemp = t
    while (tTemp != s) {
      val alternateFlow = residualMap(EdgeImpl(parentMap(tTemp), tTemp))
      pathFlow = pathFlow.min(alternateFlow)
      tTemp = parentMap(tTemp)
    }
    maxFlow += pathFlow

    var v = t
    while (v != s) {
      val u = parentMap(v)
      residualMap(EdgeImpl(u, v)) -= pathFlow
      residualMap(EdgeImpl(v, u)) += pathFlow
      v = parentMap(v)
    }

  }
  FordFulterson_Result(maxFlow, residualMap.toMap)
} // end Ford Fulkerson
 ```
 
 Minimum Cut
 ```Scala
def minCut(gg: Graph, capacities: Edge => Int, s: String, t: String): Seq[Edge] = {
 val maxFlowResult = fordFulkerson(gg, capacities, s, t)
 gg.edges.filter { ee =>
   maxFlowResult.residualMap(ee) == 0 && capacities(ee) > 0
 }
} // end Min Cut
 ```
 
 Vertex Coloring
 ```Scala
def vertexColoring(gg: Graph,
                  _existingColors: Map[String, Int] = Map.empty,
                  maxSolutions: Option[Int] = Some(1),
                  maxColors: Option[Int] = None
                 ): Seq[VertexColoring_Result] = {
 // Welsh-Powell algorithm
 val existingColors = _existingColors.withDefaultValue(-1)
 val vertices = gg.vertices
 if (vertices.isEmpty) {
   return Seq(VertexColoring_Result(Map.empty))
 }

 // Found a solution
 if (existingColors.size == gg.vertices.size) {
   return Seq(VertexColoring_Result(existingColors))
 }

 object remainingVertices {
   private val data = ArrayBuffer.empty[String]

   def addAll(vs: Seq[String]): Unit = {
     data.appendAll(vs)
   }

   def nonEmpty: Boolean = data.nonEmpty

   def dequeue(): String = {
     val (maxV, index) = data.zipWithIndex.maxBy {
       case (vv, _) =>
         val adjVertices = gg.getAdjacentVertices(vv)
         val degree = adjVertices.length
         val coloredVertices = adjVertices.map(aa => existingColors(aa.id)).filter(_ != -1)
         val saturatedDegree = coloredVertices.distinct.length
         (saturatedDegree, degree, vv)
     }(Ordering.Tuple3(Ordering.Int, Ordering.Int, Ordering.String))
     data.remove(index)
     maxV
   }

   def contains(v: String): Boolean = {
     data.contains(v)
   }
 }

 val solns = ArrayBuffer.empty[VertexColoring_Result]

 remainingVertices.addAll(gg.vertices.filter(tt => !existingColors.contains(tt.id)).map(_.id))
 val uu = remainingVertices.dequeue()

 val neighborColors = gg.getAdjacentVertices(uu).map(v => existingColors(v.id)).filter(_ != -1).distinct
 val allColors = maxColors.map(aa => 0 until aa).getOrElse(gg.vertices.indices)
 val availableColors = allColors.diff(neighborColors)
 availableColors.foreach { color =>
   if (maxSolutions.isEmpty || maxSolutions.exists(_ > solns.length)) {
     val solns_cur = vertexColoring(gg, existingColors + (uu -> color), maxSolutions = maxSolutions, maxColors = maxColors)
     solns.appendAll(solns_cur)
   }
 }
 solns.toVector
}
```

Bipartite Matching
```Scala
def bipartiteMatching(
                       matches: Seq[(String, String)],
                       initialCapacities: Map[(String, String), Int],
                       superSourceFlow: Int = INF,
                       superSinkFlow: Int = INF
                     ): BipartiteMatchingResult = {
  val superSource = "SuperSource"
  val superSink = "SuperSink"
  val edges = matches.map(ee => EdgeImpl(ee._1, ee._2))
  val sources = matches.map(_._1)
  val sinks = matches.map(_._2)

  val gg = GraphFactory.createGraphFromEdges[Int](edges)
    .addParent(superSource, sources)
    .addChild(superSink, sinks)
    .addReverseEdges

  def initialCapacity(aa: String, bb: String): Int = {
    (aa, bb) match {
      case (`superSource`, _) =>
        superSourceFlow
      case (_, `superSink`) =>
        superSinkFlow
      case _ =>
        try {
          initialCapacities((aa, bb))
        } catch {
          case NonFatal(t) =>
            0
        }
    }
  }
  val result = fordFulkerson(gg, ee => {
    initialCapacity(ee.from, ee.to)
  }, superSource, superSink)
  def getAssignments(sources: Set[String], sinks: Set[String], initialCapcities: Edge => Int, residualMap: Map[Edge, Double]): Vector[(Edge, Double)] = {
    residualMap.filter { case (ee, flow) =>
      flow > 0 &&
        (sources.contains(ee.to) && sinks.contains(ee.from)) &&
        initialCapcities(ee.reverse) > 0
    }.map { case (ee, flow) =>
      (ee.reverse, flow)
    }.toVector
  }

  val assignments = getAssignments(sources.toSet, sinks.toSet, ee => initialCapacities(ee.from, ee.to), result.residualMap)
  BipartiteMatchingResult(result.maxFlow, assignments)
}
```

Articulation Vertices
```Scala
def articulationVertices(gg: Graph): ArticulationVerticesOutput = {
  val discoveryTime  = mutable.HashMap.empty[String, Int].withDefaultValue(Int.MaxValue)
  val lowTime = mutable.HashMap.empty[String, Int].withDefaultValue(Int.MaxValue)
  val visited = mutable.HashSet.empty[String]
  val articulationPoints = mutable.HashSet.empty[String]
  val parents = mutable.HashMap.empty[String, String].withDefaultValue("")
  var time = 0

  gg.vertices.map(_.id).foreach { uu =>
    if (!visited.contains(uu)) {
      var children = 0
      visited(uu) = true
      discoveryTime(uu) = time
      lowTime(uu) = time
      time += 1
      dfs(
        gg,
        optStartId = Some(uu),
        processEdge = Some({edge =>
          val (uu, vv) = (edge.from, edge.to)
          if (!visited(vv)) {
            parents(vv) = uu
            children += 1

            lowTime(uu) = math.min(lowTime(uu), lowTime(vv))

            if (parents(uu).isEmpty && children > 1) {
              articulationPoints += uu
            }

            if (parents(uu).nonEmpty && lowTime(vv) >= discoveryTime(uu)) {
              articulationPoints += uu
            }
          } else if (vv != parents(uu)) {
            lowTime(uu) = math.min(lowTime(uu), discoveryTime(vv))
          }
        })
      )
    }
  }
  ArticulationVerticesOutput(articulationPoints.toSet)
}
 ```
Graph Construction
- Adjacency list string

gVplnngmM3i9az7BA6oYULBSDQ0YGp7OSQFYQiODg8/lMggWzE25qwDYLgOKYZ3ZpAwkM2vTSS03
OhY0X0vv55y5dduM6X7BpPZZxVt/0orHdbhbp6esRamasZHJYXwAupvlwledQhpjgwLo6FVBkSWu
l3yh2uw+amtx0T7v/rf/OFFtp35GeklGIEEdCo+FQt6PuOSuM3shCbB1wCnSvuI6H77mm0ff2B2J
93KeObTFUPx9pkRjT5ftcx0zVivvggEXTElly/rSjAzpS7kyZ+xsONaCuGypBFvLf+LR4ipIkraH
hb5W7aWtisZT0akPpKrbqIMoe33n6REc3JQcj6hhwByWtKM+00kKyZeJjj+9Qbf06lGURTY02ZQw
8TnsKabCp+WZ31x27rcdSjAXoYlHO7IIDA5gNCXu9/uOLU85PLQESGgNIm11aWxfWY/LCF4OrB/f
qG3UzKoFK+dE/Yuht949WAVyyb8f6t7KO/3coevNN+JQuKo9K8h0VkgRa9czQKNV+odevU1RzRXe
obF8mwwUiPnhH3MAjO33VKo7jiKQG24Dtvli9Ibi7NDWHrG3scbifRjd2CwYmxevivXjsD7s2Mgl
crq6EedALrq4CzKMRG+6L2fXm9c9KF/LsLky6qzI2h8Ljyc5uPa7iNCvkmO179S1uz7Uzc65dmx6
yqRTgia7m4lxJOh+2zpNqSqHxaLnnfNsrDSX6tn39KskX0e3tiDrRis5sN8sq5j9KMdVkUXU53yF
CW5pldEg1hZm3S9RDXBHPchh9xG2CTIOB9XsfDTh4ZBfMevlfVGfm6++JGezUdAnMSXIFTXx02S2
kdIFycCr2wfF4SJ++PVC7Hv4sbC2pgu4NrvUWTO7jr3qntE4GyaL0rpncaRhOnEoYC4qQhUK2Nk3
hmiYQ/C7OLabav/ajRyixS8BQjaxJd4JV/n8lsgrdoyJirEc

- Table

qKQprksOiGBaYPWZjb61OgskASnuzZBMB/9dCpAKylIc7Rb8zSyxiwZ37TYuxxHAWzR+RqQnEdHQ
ddfMC9kB4XR7/0/r+yJthNJazGD0G6j8Af2sFWOIPoC9ttpsqZZp+z6wZD/VhIKqX+VcolIOuxBD
nAtKaE4lf5hzcyuT75dP4EigKBzDhHw1N5mP+Xu4Yj7vKJFcq5M/lO5SrqUNqfVEPbIe7yyXUmi1
L/Kl5wwJ9cNC/j6Ka1u3JVml1XKmqI81AaH3YR0fDjkI2DhsdAtXKkkaDTytvoREUrFMS8ri/T+a
znU0P/wGQmNmApoYma/fbj33M6q6zLLkX6Sgplx0VmkWhPcZanEpKTPhLCmhVGZUpRRkY2dVKzRc
Dgz1/wnaR0UMugwhmJkRoSLY5tocOf/Ev3dIdAZAyoG2VoSr66mqZEEWhgP0vmCbeg1KAVkcUahY
9sQlSvCwfYFnGmzZsQLgVQEn0nC3H+Ua6etQq0qZtuKaFE6wrPuBWWjaZvhqiIfimXhv8uW26qPC
g2iC+2sr79XYrfZB64SxYjhK3xp8qu+DLIgczugFNNbVixa1e4+bKzxLEusVLOVsBxxjkfxBdY7s
feORiziTOZga5ZrUD7hqtphWU8c+WB3fmm159eBgKJdSaP0lz7DBwAfcl488JOBGQ70vjNFVkR97
d1FnvFJd1NP6GDR3XVgIc5QAXNsGaC0DX/+qJfOoW9Pm0WPc8+Dca1chOo0omb9Bdz1oSKxivM0M
FRuQW3DE5jO6PXSqR+Nn0g7EkIDEL26WLYVxyrRH8YIkCUA4//mHgmxuARxr1stPEGadetKx2L/A
5UCYYp8DUmatSwDwh33bDTwcaYSCRd4QmlLDTeAJW9EHF5N9By2pZX+Gy8PqZZhReuYAiHnXv8xr
H1iOvo7heZs9eL7KoHKDd5zvY1npGBWoybzsQTQAeiIIjjZYN4CeQIR6vK8dz04GXHeG4PIq9Y0g
EFMQjOZuwFc/W+4BP2XV5ZqZcSoR4gJDixHZfCUUxEglyGEzRXY+XwbKTQAgC0m7eWOOUkSLjC8a
fYrs2HYruKCgLZdOpahDmetcCTiNSBlZZtvgnIwGax6I1IYB6cyNJKj1IqB0q7tFYOgUIW3oo+8M
fnW/3kZi3iC/7AdUMUr+MGhrY4/Z0p0+i5mP2L9xLR+hG1dNCRA+SATZhNzM1tKbDkEdCFDof2FK
aBo7NYgT0CM7I2e5p0KKd3WLNBLdZZ2Z2iWCf/8KBoVwtzREJIEyn9ICrNqffJFVvp/u0OLREJbw
zrdcSgqLLdhOnPvGjUAe5uqffh3StxK57jwDfZbFRzDVO043XTAoqZyiESISlpWXiVyq8HryU+rH
ssTzB5TUAnUPIt3cKqe4y0BpEjWZtAnd+81vvf7lr45kp+cfWDinvHnsBKjgiE2a/PnKwBiTlVTM
mr0e5TkOc213E1l74T4N6cL1P5miboc5WaUWz7ZljO/9v5gilw==
