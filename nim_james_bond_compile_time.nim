import sugar, sequtils, algorithm, sets, lists
import random
#randomize()

type
    Field = array[16, int]
    Dir = enum →, ←, ↑, ↓, NOP
    OperationCode = tuple[dir: Dir, line: int]
    FieldPathInfo = tuple[rate: int, v: Field, prev: Field, level: int, operation: OperationCode]
    OpenList = object
        opened : DoublyLinkedList[FieldPathInfo]
        openedLen : int
        hashes : HashSet[Field]

proc initOpenList(a: var OpenList) =
    a.opened = initDoublyLinkedList[FieldPathInfo]()
    a.openedLen = 0
    a.hashes = toHashSet[Field](@[])

proc append(a: var OpenList, pathInfo : FieldPathInfo) =
    a.opened.append(pathInfo)
    a.openedLen += 1

proc popHead(a: var OpenList) : FieldPathInfo = 
    result = a.opened.head.value
    a.opened.remove(a.opened.head)
    a.openedLen -= 1

proc extend(a: var OpenList, b: seq[FieldPathInfo]) = 
    for element in b:
        if element.v notin a.hashes:
            a.opened.append(element)
            a.hashes.incl(element.v)
            a.openedLen += 1

proc sort(a: var OpenList) =
    proc sorted(list: DoublyLinkedList[FieldPathInfo]) : DoublyLinkedList[FieldPathInfo] =
        var tempSeq = newSeq[DoublyLinkedNode[FieldPathInfo]]()
        var curNode = list.head
        while curNode != nil:
            tempSeq.add(curNode)
            curNode = curNode.next
        tempSeq.sort((x, y) => cmp(-x.value.rate, -y.value.rate))
        for node in tempSeq:
            result.append(node)
    a.opened = a.opened.sorted()

proc crop(a : var OpenList, count:int) =
    proc crop(list: DoublyLinkedList[FieldPathInfo], len : int) : int =
        var curNode = list.head
        while result < len and curNode.next != nil:
            curNode = curNode.next
            result += 1
        curNode.next = nil 
    if a.openedLen > count:
        a.openedLen = a.opened.crop(count)

proc empty(a: OpenList): bool = a.opened.head == nil

const source: Field = [1,2,2,1, 3,4,4,3, 3,4,4,3, 2,4,4,2]
const target: Field = [4,3,4,2, 3,1,2,4, 4,2,4,3, 2,4,3,1]

proc UpdateArray(m: Field, line: int, updateFunc: proc(m: var Field, line:int)): Field = 
    (var n = m; updateFunc(n, line); n)

template DeclareFunction(PROC_NAME: untyped, updateFunc: proc(n: var Field, line: int)) = 
    proc PROC_NAME(m : Field, line: int) : Field = UpdateArray(m, line, updateFunc)

DeclareFunction Right, proc(n: var Field, line: int) =
    let plus = line * 4
    (n[0+plus],n[1+plus],n[2+plus],n[3+plus]) = (n[3+plus],n[0+plus],n[1+plus],n[2+plus])

DeclareFunction Left, proc(n: var Field, line: int) =
    let plus = line * 4
    (n[0+plus],n[1+plus],n[2+plus],n[3+plus]) = (n[1+plus],n[2+plus],n[3+plus],n[0+plus])

DeclareFunction Up, proc(n: var Field, line: int) =
    (n[0+line],n[4+line],n[8+line],n[12+line]) = (n[12+line],n[0+line],n[4+line],n[8+line])

DeclareFunction Down, proc(n: var Field, line: int) =
    (n[0+line],n[4+line],n[8+line],n[12+line]) = (n[4+line],n[8+line],n[12+line],n[0+line])

proc ShiftX(operation : proc, line: int): auto = (x: Field) => operation(x, line)

#there is no product in standart library
iterator product[T, U](s1: openArray[T], s2: openArray[U]): tuple[a: T, b: U] =
    for a in s1:
        for b in s2:
            yield (a, b)

#sort of magic - iterator can be used only with `for`, but it can be wrapped to var sequence
proc generateProduct[T,U] (aa:openArray[T], bb:openArray[U]) : seq[tuple[a:T, b:U]] = 
    (var x = newSeq[tuple[a:T, b:U]](); for pair in product(aa, bb) : x.add(pair); x)
    
const fieldOperations = generateProduct([(Right,→), (Left,←), (Up,↑), (Down,↓)], [0, 1, 2, 3]).mapIt(
    (ShiftX(it.a[0], it.b), (it.a[1], it.b))   #tuple[function, tuple[Dir, int]]#
);
proc callFunction[T, U] (operation : T, param: U) : auto = operation[0] param
proc operationName[T] (operation : T): auto = operation[1] #inplace rtti

proc dist(a: Field, b: Field) : auto = zip(a,b).countIt(it[0]==it[1])

proc distAdvanced(a:Field, b: Field) : auto =
    let children = fieldOperations.mapIt (it.callFunction a)
    let nextStepMax = children.mapIt(zip(it, b).countIt(it[0]==it[1])).max
    let curStepMax = dist(a, b)
    if curStepMax==16: curStepMax else: nextStepMax

proc reconstructPath(closed : seq[FieldPathInfo]) : auto =
    result = newSeq[(Field, OperationCode)]()
    var curPoint = target
    var foundNext = true
    var minVertex = closed[0]
    while foundNext:
        var minIndex = 0
        minVertex.level = 999
        foundNext = false
        for i, t in closed.pairs:
            if t.v == curPoint and minVertex.level > t.level:
                minVertex = t
                minIndex = i
                foundNext = true
        if foundNext:
            result.add((minVertex.v, minVertex.operation))
            curPoint = minVertex.prev
        else:
            break
    return result.reversed

var rng {.compileTime.} = initRand(0x1337DEADBEEF)
  
proc compileTimeShuffle[T](x: var openArray[T]) =
  for i in countdown(x.high, 1):
    let j = rng.rand(i)
    swap(x[i], x[j])

proc solve(): seq[FieldPathInfo] =
    let empty : Field = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    var openList : OpenList
    openList.initOpenList()
    openList.append (distAdvanced(source, target), source, empty, 0, (NOP, 0))

    result = newSeq[FieldPathInfo]()
    var head : FieldPathInfo;
    var iteration: int;
    while not openList.empty:
        head = openList.popHead()
        result.add(head)
        if head.v == target:
            echo "Solved"
            return
        var children = fieldOperations.mapIt (
            let newV = it.callFunction(head.v); 
            (distAdvanced(newV, target), newV, head.v, head.level + 1, it.operationName)
        )
        children.compileTimeShuffle()
        openList.extend(children)

        inc(iteration)
        if iteration mod 500 == 0:
            #echo("sorted: ", openList.openedLen, " close:", len(result), " hashes:", len(openList.hashes))
            openList.sort()
            openList.crop(50000) 
    return

proc `$`(op: OperationCode) : auto = "(" & $op.dir & "," & $op.line & ")"

proc main() : Field =
    const answer = solve().reconstructPath()
    for (field, operation) in answer:
        echo operation, ": ", field
    return answer[0][0]

discard main()

#nim c --maxLoopIterationsVM:100000000 nim_james_bond_compile_time.nim