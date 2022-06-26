require daslib/apply
require daslib/algorithm

typedef Field = int4[4]
typedef FieldPathInfo = tuple<field:Field; fieldFrom:Field; rate:int>

//1
//var source=[[Field 1; 2; 3; 4; 4; 1; 2; 3; 3; 4; 1; 2; 2; 3; 4; 1]]
//var target=[[Field 1; 2; 3; 4; 1; 2; 3; 4; 1; 2; 3; 4; 1; 2; 3; 4]]
//2
//var source=[[Field 1;1;1;1; 1;2;3;1; 1;3;2;1; 1;1;1;1]]
//var target=[[Field 1;1;1;1; 1;3;3;1; 1;2;2;1; 1;1;1;1]]
//3
//var source=[[Field 1;2;1;3; 3;2;1;2; 1;1;3;1; 1;2;1;3]]
//var target=[[Field 3;1;2;3; 2;1;1;1; 1;1;1;2; 3;2;1;3]]
//5
//var source=[[Field 1;1;2;2; 1;1;2;2; 3;3;4;4; 3;3;4;4]]
//var target=[[Field 1;1;2;2; 1;3;4;2; 3;3;4;4; 1;3;4;2]]
//4
var source = [[Field int4(1,2,2,1); int4(3,4,4,3); int4(3,4,4,3); int4(2,4,4,2) ]]
var target = [[Field int4(4,3,4,2); int4(3,1,2,4); int4(4,2,4,3); int4(2,4,3,1) ]]

var zeros = [[Field int4(0,0,0,0); int4(0,0,0,0); int4(0,0,0,0); int4(0,0,0,0) ]]

var openHash : table<uint64>

def right(var v:Field; line: int)
    var ans = v
    ans[line] = ans[line].yzwx
    return <- ans

def left(var v:Field; line: int)
    var ans = v
    ans[line] = ans[line].wxyz
    return <- ans

def up(var v:Field; line: int)
    var ans = v
    var temp = ans[0][line]
    ans[0][line] = ans[1][line]
    ans[1][line] = ans[2][line]
    ans[2][line] = ans[3][line]
    ans[3][line] = temp
    return <- ans

def down(var v:Field; line: int)
    var ans = v
    var temp = ans[0][line]
    ans[0][line] = ans[3][line]
    ans[3][line] = ans[2][line]
    ans[2][line] = ans[1][line]
    ans[1][line] = temp
    return <- ans

var fieldOperations <- [[auto
    @(var v:Field) => left(v,0);
    @(var v:Field) => left(v,1);
    @(var v:Field) => left(v,2);
    @(var v:Field) => left(v,3);

    @(var v:Field) => right(v,0);
    @(var v:Field) => right(v,1);
    @(var v:Field) => right(v,2);
    @(var v:Field) => right(v,3);

    @(var v:Field) => up(v,0);
    @(var v:Field) => up(v,1);
    @(var v:Field) => up(v,2);
    @(var v:Field) => up(v,3);

    @(var v:Field) => down(v,0);
    @(var v:Field) => down(v,1);
    @(var v:Field) => down(v,2);
    @(var v:Field) => down(v,3)
]]

def rate(var v:Field)
    var sum = 0
    for vi, ti in v, target
        if vi == ti
            sum++
    return sum

def same(var a,b: Field)
    return a[0]==a[0] && a[1]==b[1] && a[2]==b[2] && a[3]==b[3]

var opStartIndex = 0
[unsafe_deref]
def openv(v: FieldPathInfo?; var open:array<FieldPathInfo?>; var closed:array<FieldPathInfo?>)
    var vert = v.field
    var res: array<FieldPathInfo?>
    opStartIndex++   //shuffle(fieldOperations)
    for i in range(16)
        var val = invoke(fieldOperations[(opStartIndex + i)%16], vert)
        var childHash = hash(val)
        if !key_exists(openHash, childHash)
            res |> push <| new [[FieldPathInfo val, vert, rate(val)]]
            openHash |> insert <| childHash
    return <-res

[unsafe_deref]
def search()
    var open, closed: array<FieldPathInfo?>
    var i = 0
    openHash |> insert <| hash(source)
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

[unsafe_deref]
def extract(var closed: array<FieldPathInfo?>)
    var last = target
    var result : array<Field>
    result |> emplace <| target
    while !same(last, zeros)
        for x in closed
            var el = x.field
            var next = x.fieldFrom
            if same(el, last)
                result |> emplace <| last
                last = next
                //closed |> erase <| x
                break
    result |> pop
    return <- result

[export]
def main
    profile(100, "JamesBondProblemFast") <|
        var dif <- extract(search())
    //for i in range(length(dif)-1)
    //    print("{dif[length(dif)-1-i]}\n")