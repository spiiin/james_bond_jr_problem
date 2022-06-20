object james_bond_jr {
  type State = Vector[Int]
  val initialState: State = Vector(1,2,2,1, 3,4,4,3, 3,4,4,3, 2,4,4,2)
  val endState: State = Vector(4,3,4,2, 3,1,2,4, 4,2,4,3, 2,4,3,1)

  trait Move {
    def act(state: State): State
  }

  case class MoveLeft(row: Int) extends Move {
    def act(state: State) = state.updated(0 + row * 4, state(1 + row * 4)).updated(1 + row * 4, state(2 + row * 4)).updated(2 + row * 4, state(3 + row * 4)).updated(3 + row * 4, state(0 + row * 4))
  }

  case class MoveRight(row: Int) extends Move {
    def act(state: State) = state.updated(1 + row * 4, state(0 + row * 4)).updated(2 + row * 4, state(1 + row * 4)).updated(3 + row * 4, state(2 + row * 4)).updated(0 + row * 4, state(3 + row * 4))
  }

  case class MoveUp(col: Int) extends Move {
    def act(state: State) = state.updated(0 + col, state(4 + col)).updated(4 + col, state(8 + col)).updated(8 + col, state(12 + col)).updated(12 + col, state(0 + col))
  }

  case class MoveDown(col: Int) extends Move {
    def act(state: State) = state.updated(4 + col, state(0 + col)).updated(8 + col, state(4 + col)).updated(12 + col, state(8 + col)).updated(0 + col, state(12 + col))
  }

  val moves =
      (for (i <- 0 until 4) yield MoveLeft(i)) ++
      (for (i <- 0 until 4) yield MoveRight(i)) ++
      (for (i <- 0 until 4) yield MoveUp(i)) ++
      (for (i <- 0 until 4) yield MoveDown(i))
      
  class Path(path: List[Move], val endState: State) extends Ordered[Path]{
    def extend(move: Move) = new Path(move :: path, move act endState)
    override def toString = (path.reverse mkString " ") + "-->" + endState
    def compare(that: Path): Int = {
      def diffFromEnd(x:Path) :Int = (x.endState zip endState).count( {case (x,y) => x!=y} )
      diffFromEnd(this) - diffFromEnd(that)
    }
  }

  val initialPath = new Path(Nil, initialState)
  
  def from(paths: Vector[Path], explored:Set[State]): Stream[Vector[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val more= for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      //val a = println("pathSet.size =" + more.size + "explored.size = " + explored.size)
      val sortedMore = (more take 50000).sorted
      paths #:: from(sortedMore, explored ++ (sortedMore map (_.endState)))
    }
  }

  val pathSets = from(Vector(initialPath), Set(initialState))

  def solution = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState == endState
    } yield path
  }

  def printSolution(p: Stream[Path]): Unit = {
    val l = (solution take 1).toList
    println(l mkString "\n")
    return
  }
  
  printSolution(solution)
}