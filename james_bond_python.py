from random import shuffle

def Right(m,line):
    n = m[:]
    plus = line*4
    n[0+plus],n[1+plus],n[2+plus],n[3+plus] = n[3+plus],n[0+plus],n[1+plus],n[2+plus]
    return n
   
def Left(m,line):
    n = m[:]
    plus = line*4
    n[0+plus],n[1+plus],n[2+plus],n[3+plus] = n[1+plus],n[2+plus],n[3+plus],n[0+plus]
    return n
   
def Up(m,line):
    n = m[:]
    n[0+line],n[4+line],n[8+line],n[12+line] = n[12+line],n[0+line],n[4+line],n[8+line]
    return n
   
def Down(m,line):
    n = m[:]
    n[0+line],n[4+line],n[8+line],n[12+line] = n[4+line],n[8+line],n[12+line],n[0+line]
    return n
   
fun = [lambda m : Left(m,0),
       lambda m : Left(m,1),
       lambda m : Left(m,2),
       lambda m : Left(m,3),
       lambda m : Right(m,0),
       lambda m : Right(m,1),
       lambda m : Right(m,2),
       lambda m : Right(m,3),
       lambda m : Up(m,0),
       lambda m : Up(m,1),
       lambda m : Up(m,2),
       lambda m : Up(m,3),
       lambda m : Down(m,0),
       lambda m : Down(m,1),
       lambda m : Down(m,2),
       lambda m : Down(m,3)
      ]
   
def openv (v,open,closed,open_hash):
  vert = v[0]
  res = []
  shuffle(fun)
  for f in fun:
    val = f(vert)
    if hash(val) not in open_hash:
      res.append([val,vert,rate(val)])
  return res
 
 
def extract(closed):
  last = target
  result=[target]
  while last!=None:
    for x in closed:
      el,next,rate = x
      if el == last:
        result.append(next)
        last = next
        closed.remove(x)
        break
    #last=None
  result.pop()
  result.reverse()
  return result
 
 
def find_dif(m1,m2):
  last_dif_x,last_dif_y, count_dif_x,count_dif_y = 0,0,0,0
  for x in range(0,4):
    plus = x*4
    b = (m1[0+plus],m1[1+plus],m1[2+plus],m1[3+plus]) == (m2[0+plus],m2[1+plus],m2[2+plus],m2[3+plus])
    if not b:
      last_dif_x = x
      count_dif_x = count_dif_x+1
      if count_dif_x>1:
        break
  if count_dif_x==1:
    plus = last_dif_x*4
    if [m1[0+plus],m1[1+plus],m1[2+plus],m1[3+plus]] ==[m2[1+plus],m2[2+plus],m2[3+plus],m2[0+plus]]:
      text = "right"
    else:
      text = "left"
    return [text,last_dif_x]     
   
  for y in range(0,4):
    plus = y
    b = (m1[0+plus],m1[4+plus],m1[8+plus],m1[12+plus]) == (m2[0+plus],m2[4+plus],m2[8+plus],m2[12+plus])
    if not b:
      last_dif_y = y
      count_dif_y = count_dif_y+1
      if count_dif_y==1:
        break
  if count_dif_y==1:
    plus = last_dif_y
    if [m1[0+plus],m1[4+plus],m1[8+plus],m1[12+plus]] ==[m2[12+plus],m2[0+plus],m2[4+plus],m2[8+plus]]:
      text = "up"
    else:
      text = "down"
    return [text,last_dif_y]
   
 
 
def extract_changes(closed):
  res = []
  for i in range(0,len(closed)-1):
      m1,m2 = closed[i],closed[i+1]
      dif = find_dif(m1,m2)
      res.append(dif)
  return res
   
 
 
def search2():
  open = [[source,None,rate(source)]]
  open_hash = {hash(source):1}
  closed = []
  i=0
  while open!=[]:
    if i%500 ==0:
      #print("open = ", len(open))
      #print("close= ", len(closed))
      #print("hash= ", len(open_hash))
      open.sort(key=lambda x:x[2])
    i=i+1
    head = open[0]
    if head[0] == target:
      closed.append(open[0])
      return closed
    children = openv(head,open,closed,open_hash)
    ##print "children= ",len(children)
    open.remove(head)
    ##print "del element"
    closed.append(head)
    open_hash[hash(head[0])]=1
    open.extend(children)
    for x,p,r in children:
      open_hash[hash(x)]=1
  return None 
 
 
def hash(v):
  res=0
  for e in v:
    res+=e-1
    res<<=2
  ##print "hash ",v," = ",res
  return res
 
def rate(v):
  res=0
  for x,y in zip(v,target):
    if x==y:
      res+=1
  return 16-res
 
source = [1,2,2,1, 3,4,4,3, 3,4,4,3, 2,4,4,2]
target = [4,3,4,2, 3,1,2,4, 4,2,4,3, 2,4,3,1] ##4
    
#source=[1, 1, 2, 2,  1, 1, 2, 2,  3, 3, 4, 4,  3, 3, 4, 4] ###5
#target=[1, 1, 2, 2,  1, 3, 4, 2,  3, 3, 4, 4,  1, 3, 4, 2]
 
i=0
def my_search():
 return search2()
 
 
if __name__ == "__main__":
  res = extract(my_search())
  res_dif = extract_changes(res)
  #print('source:')
  #print(source)
  #print('target:')
  #print(target)
  #print("")
 
  #print('result:')
  #print("steps :" ,len(res_dif))
  #for x, y in zip(res,res_dif):
  #  print("state:",x)
  #  print("dif  :",y)
   
  #print("")
  print("only difs:")
  for x in res_dif:
    print(x)