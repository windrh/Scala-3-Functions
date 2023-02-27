//uses pattern matching in order to filter positives from a list of integers, returns the list but with only pos. numbers 
def filterPositives(list: List[Int]): List[Int] =
   list match
     case a if (list.tail.isEmpty && list.head > 0) => List(list.head)
     case b if (list.tail.isEmpty && list.head == 0) => List.empty
     case c if (list.tail.isEmpty && list.head < 0) => List.empty
     case d if (list.head > 0) => list.head :: filterPositives(list.tail)
     case e if (list.head < 0) => filterPositives(list.tail)
     case f if (list.head == 0) => filterPositives(list.tail)
