val unionFind = Array.fill(n+1)(0)
for (i <- 0 to n) unionFind(i) = i

def find(i: Int): Int = {
  if (unionFind(i) == i) i
  else {
    val r = find(unionFind(i))
    unionFind(i) = r
    r
  }
}

def union(i: Int, l: Int): Unit = {
  val iRoot = find(i)
  val lRoot = find(l)
  unionFind(iRoot) = math.min(iRoot, lRoot)
  unionFind(lRoot) = math.min(iRoot, lRoot)
}
