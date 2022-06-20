require daslib/apply
require daslib/algorithm

typedef FieldPathInfo = tuple<field:int[16]; fieldFrom:int[16];  rate:int>
struct Field
    field: int[16]

//1
//var source=[[int[16] 1; 2; 3; 4; 4; 1; 2; 3; 3; 4; 1; 2; 2; 3; 4; 1]]
//var target=[[int[16] 1; 2; 3; 4; 1; 2; 3; 4; 1; 2; 3; 4; 1; 2; 3; 4]]
//2
//var source=[[int[16] 1;1;1;1; 1;2;3;1; 1;3;2;1; 1;1;1;1]]
//var target=[[int[16] 1;1;1;1; 1;3;3;1; 1;2;2;1; 1;1;1;1]]
//3
//var source=[[int[16] 1;2;1;3; 3;2;1;2; 1;1;3;1; 1;2;1;3]]
//var target=[[int[16] 3;1;2;3; 2;1;1;1; 1;1;1;2; 3;2;1;3]]
//5
//var source=[[int[16] 1;1;2;2; 1;1;2;2; 3;3;4;4; 3;3;4;4]]
//var target=[[int[16] 1;1;2;2; 1;3;4;2; 3;3;4;4; 1;3;4;2]]
//4
var source=[[int[16] 1;2;2;1; 3;4;4;3; 3;4;4;3; 2;4;4;2]]
var target=[[int[16] 4;3;4;2; 3;1;2;4; 4;2;4;3; 2;4;3;1]]

var zeros = [[int[16] 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]]

var openHash : table<int; bool>

def shift(var v:int[16]; i1,i2,i3,i4:int)
    var temp = v[i1]
    v[i1] = v[i2]
    v[i2] = v[i3]
    v[i3] = v[i4]
    v[i4] = temp

def right(var v:int[16]; line: int)
    var ans = v
    var plus = line*4
    shift(ans, 0+plus, 3+plus, 2+plus, 1+plus)
    return <- ans

def left(var v:int[16]; line: int)
    var ans = v
    var plus = line*4
    shift(ans, 0+plus, 1+plus, 2+plus, 3+plus)
    return <- ans

def up(var v:int[16]; line: int)
    var ans = v
    shift(ans, 0+line, 12+line, 8+line, 4+line)
    return <- ans

def down(var v:int[16]; line: int)
    var ans = v
    shift(ans, 0+line, 4+line, 8+line, 12+line)
    return <- ans

var fieldOperations <- [[auto
    @(var v:int[16]) => left(v,0);
    @(var v:int[16]) => left(v,1);
    @(var v:int[16]) => left(v,2);
    @(var v:int[16]) => left(v,3);

    @(var v:int[16]) => right(v,0);
    @(var v:int[16]) => right(v,1);
    @(var v:int[16]) => right(v,2);
    @(var v:int[16]) => right(v,3);

    @(var v:int[16]) => up(v,0);
    @(var v:int[16]) => up(v,1);
    @(var v:int[16]) => up(v,2);
    @(var v:int[16]) => up(v,3);

    @(var v:int[16]) => down(v,0);
    @(var v:int[16]) => down(v,1);
    @(var v:int[16]) => down(v,2);
    @(var v:int[16]) => down(v,3)
]]

def hash(var v:int[16])
    var res = 0
    for e in v
        res += (e-1)
        res <<= 2
    return res

def rate(var v:int[16])
    var sum = 0
    for vi, ti in v, target
        if vi == ti
            sum++
    return sum

def same(var a,b: int[16])
    for ai, bi in a, b
        if ai != bi
            return false
    return true

var opStartIndex = 0
def openv(v: FieldPathInfo?; var open:array<FieldPathInfo?>; var closed:array<FieldPathInfo?>)
    var vert = v.field
    var res: array<FieldPathInfo?>
    opStartIndex++   //shuffle(fieldOperations)
    for i in range(16)
        var val = invoke(fieldOperations[(opStartIndex + i)%16], vert)
        var childHash = hash(val)
        if !key_exists(openHash, childHash)
            res |> push <| new [[FieldPathInfo val, vert, rate(val)]]
            openHash[childHash] = true
    return <-res

def search()
    var open, closed: array<FieldPathInfo?>
    var i = 0
    openHash[hash(source)] = true
    open |> push <| new [[FieldPathInfo source, zeros, rate(source)]]
    while length(open) > 0
        i++
        //sort every n-th iterations
        if i % 500 == 0
            sort(open, $(a,b) => (a.rate > b.rate))
            if length(open) > 50000
                open |> resize <| 50000
            //print("open={length(open)}, close={length(closed)} hash={length(openHash)}\n")
        var head = open[0]
        if same(head.field, target)
            closed |> push <| head
            return <- closed
        var children <- openv(head, open, closed)
        open |> erase <| 0
        closed |> push <| head
        open |> push <| children         
    return <- closed

def extract(var closed: array<FieldPathInfo?>)
    var last = target
    var result : array<Field>
    result |> emplace <| [[Field field=target]]
    while !same(last, zeros)
        for x in closed
            var el = x.field
            var next = x.fieldFrom
            if same(el, last)
                result |> emplace <| [[Field field=next]]
                last = next
                //closed |> erase <| x
                break
    result |> pop
    reverse(result)
    return <- result

[export]
def main
    var dif <- extract(search())
    for i in range(length(dif)-1)
        print("{dif[i].field} -> {dif[i+1].field}\n")