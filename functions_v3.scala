//function that filters the positives in a list of integers... recursively
def filterPositives(list: List[Int]): List[Int] =
  if list.tail.isEmpty then
    if list.head > 0 then
      return List(list.head)
    else
      return List.empty
  if list.head > 0 then list.head :: filterPositives(list.tail)
  else filterPositives(list.tail)
  
//function that appends two lists together... recursively 
def append(list1: List[Int], list2: List[Int]): List[Int] =
  if list1.isEmpty then return list2
  if list2.isEmpty then return list1
  if list1.tail.isEmpty then return list1.head :: list2
  else list1.head :: append(list1.tail,list2)
 
//function that flattens a list of lists... recursively 
def flatten1(lol: List[List[Int]]): List[Int] =
  if lol.tail.isEmpty then return lol.head
  else
    append(lol.head,flatten1(lol.tail))

//function that returns the index of an element within a list... recursively 
def indexOf(target: Int, list: List[Int]): Int =
  if list.tail.isEmpty && list.head != target then return -1
  else if list.tail.isEmpty && list.head == target then return 0
  else if list.head != target && list.tail.isEmpty == false then
    if indexOf(target, list.tail) == -1 then return -1
    else return 1 + indexOf(target, list.tail)
  else if list.head == target && list.tail.isEmpty == false then return 0
  else return -1
