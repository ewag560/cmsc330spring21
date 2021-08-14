$nums = Array.new()
def fib(n)
  if n <= 0 then
    ns = $nums
    $nums = Array.new()
    return ns
  end

  $nums.unshift(fibo(n-1))
  fib(n-1)
end

def fibo(n)
  if n <= 1 then
    return n
  end

  return fibo(n-1) + fibo(n-2)
end


def ispalindrome(n)
  r = n.to_s.reverse.to_i

  if r == n then
    return true
  else return false
  end
end


def nthmax(n, a)
  if n > 0 then
    nthmax(n-1,a)
    a.delete(a.max)
  end

  return a.max
end


$str = ""
$max =0
def freq(s)
  if s[0] != nil && s.count(s[0]) > $max  then
    $max = s.count(s[0])
    $str = s[0]
  end
  s[0] = ''
  if s != '' then
    freq(s)
  end
  $max = 0
  return $str
end

$h = Hash.new
def zipHash(arr1, arr2)
  if arr1.length != arr2.length then
    return nil
  end
  
  if arr1.length == 0 then
    temphash = Hash.new
    temphash = $h
    $h = Hash.new
    return temphash
  end
  

  $h[arr1[0]] = arr2[0]
  arr1 = arr1.drop(1)
  arr2 = arr2.drop(1)
  if (arr1.length > 0) then
    $h = zipHash(arr1, arr2)
  end
  temphash = Hash.new
  temphash = $h
  $h = Hash.new
  return temphash
end

def hashToArray(hash)
  arr = Array.new
  hash.each{
    |key, value|
    ar = Array.new
    ar.push(key)
    ar.push(value)
    arr.push(ar)
  }
  return arr
end
