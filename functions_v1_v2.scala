  // takes an integer and provides the square root wrapped in Some or None 
  def squareRoot(num: Int): Option[Double] =
    if num < 0 then
      None
    else if num > 0 then
      Some(sqrt(num))
    else
      Some(0)
  
  // takes an array and returns the size iteratively 
  def size(array: Array[String]): Int =
    var count = 0
    for item <- array do count += 1
    count
 
 // takes an array and target and returns t or f if the target INT is in the array 
 def search(target: Int, array: Array[Int]): Boolean =
   var truecount = 0
    for value <- array do
      if target == value then truecount += 1
    if truecount > 0 then return true
    else return false
    
 // counts vowels in a string 
 def countVowels(str: String): Int =
   var vowelcount = 0
   val vowellist = Array('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')
   for letter <- str do
     for vowel <- vowellist do if letter == vowel then vowelcount += 1
   vowelcount
   
// takes a price and discount num and returns the new price 
def discount(price: Int, discountnum: Int): Int =
  val newprice = (price).toFloat
  val newdiscount = (discountnum).toFloat
  val multiplier = 100.00 - newdiscount
  return ((multiplier * newprice).toInt) / 100
    
// takes a string and returns true if # of x == # of o 
def xsEqualos(str: String): Boolean =
  var xvalue = 0
  var ovalue = 0
  for letter <- str do
    if letter == 'x' then xvalue += 1
    else if letter == 'o' then ovalue += 1
  if xvalue == ovalue then return true
  else return false
    
// takes a credit card number in a string and returns everything x'd out excpet last 5 
def maskCreditCardNum(str: String): String =
  var starter = "*****"
  var strcount = 0
  for letter <- str do
    strcount += 1
    if strcount > 5 then starter += letter
  return starter
    
// finds the maximum value of an array iteratively 
def findMax(array: Array[Int]): Int =
  var maxvalue = 0
  for number <- array do
    if number > maxvalue then
      maxvalue = number
  maxvalue
    
// takes an array and counts the amount of duplicate values 
def countDupes(array: Array[Int]): Int =
  var uniquecounter = 0
  var gencount = 0
  for element <- array do
    var localcount = 0
    for value <- 0 until gencount do
      if array(value) == array(gencount) then
        localcount += 1   
      if localcount == 1 then
      uniquecounter += 1
    gencount += 1
  uniquecounter
  
// takes an array and reverses it iteratively, returns array 
def reverseArray(arr: Array[Int]): Array[Int] =
  var evenoddboolean = 0
  if arr.length % 2 == 1 then
    evenoddboolean += 1
  if evenoddboolean == 0 then
    for i <- 0 until arr.length / 2 do
      val leftbound = arr(i)
      val rightbound = arr(arr.length - i - 1)
      arr(i) = rightbound
      arr(arr.length - i - 1) = leftbound
  else
    for j <- 0 until -1 + arr.length / 2 do
      val leftbound = arr(j)
      val rightbound = arr(arr.length - j - 1)
      arr(j) = rightbound
      arr(arr.length - j - 1) = leftbound
  arr
    
// iteratively searches an array for a target using bianary search! BIG O OF LOG(N) LETS GOO 
def binarySearch(target: Int, array: Array[Int]): Int =
  val x = 0
  var leftlimit = 0
  var rightlimit = array.length - 1
  while leftlimit <= rightlimit do
    var index = rightlimit + leftlimit
    if index % 2 == 1 then
      index -= 1
    if target > array(index / 2) then
      leftlimit = 0
      leftlimit += index / 2
    if target < array(index / 2) then
      rightlimit = index / 2
    if target == array(index / 2) then
      return index / 2
    if rightlimit - leftlimit == 1 then
      if array(leftlimit) == target then
        return leftlimit
      if array(rightlimit) == target then
        return rightlimit
      else
        return -1
  return -1
   
  
  
  
