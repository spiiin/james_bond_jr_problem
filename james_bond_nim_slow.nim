import sugar, sequtils, itertools, algorithm, sets
import random
randomize()

type
    Field = array[16, int]
    Dir = enum →, ←, ↑, ↓, NOP
    OperationCode = tuple[dir: Dir, line: int]
    FieldPathInfo = tuple[rate: int, v:Field, prev:Field, level:int, operation: OperationCode]

let source: Field = [1,2,2,1, 3,4,4,3, 3,4,4,3, 2,4,4,2]
let target: Field = [4,3,4,2, 3,1,2,4, 4,2,4,3, 2,4,3,1]

var hashes = toHashSet[Field](@[])

proc `<`(a : FieldPathInfo, b : FieldPathInfo) : auto = a.rate > b.rate

proc UpdateArray(m: Field, line: int, updateFunc: proc(m: var Field, line:int)): Field = (var n = m; updateFunc(n, line); n)

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

proc ShiftX[T] (operation : T, line: int): auto = (x: Field) => operation(x, line)

#sort of magic - iterator can be used only with `for`, but it can be wrapped to var sequence, and after calculation
proc generateProduct[T,U] (aa:T, bb:U) : auto = (var x = newSeq[tuple[a:type(aa[0]), b:type(bb[0])]](); for pair in product(aa, bb) : x.add(pair); x)
var fieldOperations = generateProduct([(Right,→), (Left,←), (Up,↑), (Down,↓)], [0, 1, 2, 3]).map(
    (x) => (ShiftX(x.a[0], x.b), (x.a[1], x.b))   #tuple[function, tuple[Dir, int]]#
);
proc callFunction[T, U] (operation : T, param: U) : auto = operation[0] param
proc operationName[T] (operation : T): auto = operation[1] #inplace rtti

proc dist(a: Field, b: Field) : auto = zip(a,b).countIt(it[0]==it[1])

proc distAdvanced(a:Field, b: Field) : auto =
    let children = fieldOperations.map (x => x.callFunction a)
    let nextStepMax = children.map( x => zip(x,b).countIt(it[0]==it[1])).max
    let curStepMax = dist(a, b)
    if curStepMax==16: curStepMax else: nextStepMax

proc extend(a: var seq[FieldPathInfo], b: seq[FieldPathInfo]) = 
    for element in b:
        if element.v notin hashes:
            a.add(element)
            hashes.incl(element.v)

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

proc solve(): seq[FieldPathInfo] =
    let empty : Field = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    var opened = newSeq[FieldPathInfo]()
    opened.add((distAdvanced(source, target), source, empty, 0, (NOP, 0)))

    result = newSeq[FieldPathInfo]()
    var head : FieldPathInfo;
    var iteration: int;
    while opened.len != 0:
        head = opened[0]
        opened.delete(0)
        result.add(head)
        if head.v == target:
            echo "Solved"
            return
        var children = fieldOperations.map (x) => (let newV = x.callFunction(head.v); (distAdvanced(newV, target), newV, head.v, head.level + 1, x.operationName))
        children.shuffle()

        opened.extend(children)
        inc(iteration)
        if iteration mod 500 == 0:
            echo("sorted: ", len(opened), " close:", len(result), " hashes:", len(hashes))
            opened.sort()
            if opened.len > 50000:
                opened = opened[0..50000]
    return

for (field, operation) in solve().reconstructPath():
    echo "(", operation.dir, ",", operation.line, ")", ": ", field
