/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * based on C program by Kevin Carson
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "flag"
   "fmt"
   "strconv"
)

var n = 0

type Node struct {
     item   int
     left, right   *Node
}

func  bottomUpTree(item, depth int) *Node {
   if depth <= 0 {
      return &Node{item: item}
   }
   return &Node{ item, bottomUpTree(2*item-1, depth-1), bottomUpTree(2*item, depth-1) }
}

func (n *Node) itemCheck() int {
   if n.left == nil {
      return n.item
   }
   return n.item + n.left.itemCheck() - n.right.itemCheck()
}

const minDepth = 4

func main() {
   flag.Parse()
   if flag.NArg() > 0 { n,_ = strconv.Atoi( flag.Arg(0) ) }

   maxDepth := n
   if minDepth + 2 > n {
      maxDepth = minDepth + 2
   }
   stretchDepth := maxDepth + 1

   check := bottomUpTree(0, stretchDepth).itemCheck()
   fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check)

   longLivedTree := bottomUpTree(0, maxDepth)

   for depth := minDepth; depth <= maxDepth; depth+=2 {
      iterations := 1 << uint(maxDepth - depth + minDepth)
      check = 0

      for i := 1; i <= iterations; i++ {
         check += bottomUpTree(i,depth).itemCheck()
         check += bottomUpTree(-i,depth).itemCheck()
      }
      fmt.Printf("%d\t trees of depth %d\t check: %d\n", iterations*2, depth, check)
   }
   fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, longLivedTree.itemCheck())
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * based on C program by Kevin Carson
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "flag"
   "fmt"
   "strconv"
)

var n = 0    // var n = flag.Int("n", 2000, "count")

type Node struct {
     item   int
     left, right   *Node
}

type Arena struct {
   head   *Node
}

var arena Arena

func (n *Node) free() {
   if n.left != nil {
      n.left.free()
   }
   if n.right != nil {
      n.right.free()
   }
   n.left = arena.head
   arena.head = n
}

func (a *Arena) New(item int, left, right *Node) *Node {
   if a.head == nil {
      nodes := make([]Node, 3 << uint(n))
      for i := 0; i < len(nodes)-1; i++ {
         nodes[i].left = &nodes[i+1]
      }
      a.head = &nodes[0]
   }
   n := a.head
   a.head = a.head.left
   n.item = item
   n.left = left
   n.right = right
   return n
}

func  bottomUpTree(item, depth int) *Node {
   if depth <= 0 {
      return arena.New(item, nil, nil)
   }
   return arena.New(item, bottomUpTree(2*item-1, depth-1), bottomUpTree(2*item, depth-1))
}

func (n *Node) itemCheck() int {
   if n.left == nil {
      return n.item
   }
   return n.item + n.left.itemCheck() - n.right.itemCheck()
}

const minDepth = 4

func main() {
   flag.Parse()
   if flag.NArg() > 0 { n,_ = strconv.Atoi( flag.Arg(0) ) }

   maxDepth := n
   if minDepth + 2 > n {
      maxDepth = minDepth + 2
   }
   stretchDepth := maxDepth + 1

   check := bottomUpTree(0, stretchDepth).itemCheck()
   fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check)

   longLivedTree := bottomUpTree(0, maxDepth)

   for depth := minDepth; depth <= maxDepth; depth+=2 {
      iterations := 1 << uint(maxDepth - depth + minDepth)
      check = 0

      for i := 1; i <= iterations; i++ {
         t := bottomUpTree(i,depth)
         check += t.itemCheck()
         t.free()
         t = bottomUpTree(-i,depth)
         check += t.itemCheck()
         t.free()
      }
      fmt.Printf("%d\t trees of depth %d\t check: %d\n", iterations*2, depth, check)
   }
   fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, longLivedTree.itemCheck())
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * based on C program by Kevin Carson
 * flag.Arg hack by Isaac Gouy
 * custom pool and parallel loops by JONNALAGADDA Srinivas
 */

package main

import (
   "flag"
   "fmt"
   "runtime"
   "strconv"
)

type NodeStore struct {
   brk   int
   idx   int
   store []Node
}

func (s *NodeStore) Init(depth int) {
   s.brk = 1 << uint(depth+1)
   s.idx = -1
   s.store = make([]Node, s.brk)
}

func (s *NodeStore) ReInit() {
   s.idx = -1
}

func (s *NodeStore) Alloc(i int, l, r *Node) *Node {
   s.idx++
   p := &(s.store[s.idx])
   (*p).item = i
   (*p).left = l
   (*p).right = r
   return p
}

var n = 0

type Node struct {
   item        int
   left, right *Node
}

func bottomUpTree(item, depth int, store *NodeStore) *Node {
   if depth <= 0 {
      return store.Alloc(item, nil, nil)
   }
   return store.Alloc(item,
      bottomUpTree(2*item-1, depth-1, store),
      bottomUpTree(2*item, depth-1, store))
}

func (n *Node) itemCheck() int {
   if n.left == nil {
      return n.item
   }
   return n.item + n.left.itemCheck() - n.right.itemCheck()
}

const minDepth = 4
const MAXPROCS = 4

func main() {
   flag.Parse()
   if flag.NArg() > 0 {
      n, _ = strconv.Atoi(flag.Arg(0))
   }

   runtime.GOMAXPROCS(MAXPROCS)

   maxDepth := n
   if minDepth+2 > n {
      maxDepth = minDepth + 2
   }
   stretchDepth := maxDepth + 1

   store := new(NodeStore)
   store.Init(stretchDepth)
   check := bottomUpTree(0, stretchDepth, store).itemCheck()
   fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check)

   longLivedStore := new(NodeStore)
   longLivedStore.Init(maxDepth)
   longLivedTree := bottomUpTree(0, maxDepth, longLivedStore)

   ss := make([]string, maxDepth+1)
   fn := func(min, max int, ch chan int) {
      for depth := min; depth <= max; depth += (2 * MAXPROCS) {
         iterations := 1 << uint(maxDepth-depth+minDepth)
         check := 0

         store := new(NodeStore)
         store.Init(depth)
         for i := 1; i <= iterations; i++ {
            store.ReInit()
            check += bottomUpTree(i, depth, store).itemCheck()
            store.ReInit()
            check += bottomUpTree(-i, depth, store).itemCheck()
         }
         ss[depth] = fmt.Sprintf("%d\t trees of depth %d\t check: %d\n",
            iterations*2, depth, check)
      }
      ch <- 0
   }

   ch := make(chan int, MAXPROCS)
   for i := 0; i < MAXPROCS; i++ {
      go fn(minDepth+(i*2), maxDepth, ch)
   }
   for i := 0; i < MAXPROCS; i++ {
      <-ch
   }
   for i := minDepth; i <= maxDepth; i += 2 {
      fmt.Print(ss[i])
   }

   fmt.Printf("long lived tree of depth %d\t check: %d\n",
      maxDepth, longLivedTree.itemCheck())
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * modified by roger peppe
 * 
 */

package main

import (
   "flag"
   "fmt"
   "strconv"
   "sync"
)

const (
   blue = iota
   red
   yellow
   ncol
)

var complement = [...]int{
   red | red<<2: red,
   red | yellow<<2: blue,
   red | blue<<2: yellow,
   yellow | red<<2: blue,
   yellow | yellow<<2: yellow,
   yellow | blue<<2: red,
   blue | red<<2: yellow,
   blue | yellow<<2: red,
   blue | blue<<2: blue,
}

var colname = [...]string{
   blue: "blue",
   red: "red",
   yellow: "yellow",
}

// information about the current state of a creature.
type info struct {
   colour int // creature's current colour.
   name   int // creature's name.
}

// if mate is nil, it indicates there's no creature currently waiting
// otherwise the creature's info is stored in info, and
// it is waiting to receive its mate's information on the mate channel.
type Place struct {
   sync.Mutex
   n    int         // current number of encounters.
   mate chan<- info // creature waiting when non-nil.
   info info        // info about creature waiting.
}

// result sent by each creature at the end of processing.
type result struct {
   met  int
   same int
}

var n = 600

func main() {
   flag.Parse()
   if flag.NArg() > 0 {
      n, _ = strconv.Atoi(flag.Arg(0))
   }

   for c0 := 0; c0 < ncol; c0++ {
      for c1 := 0; c1 < ncol; c1++ {
         fmt.Printf("%s + %s -> %s\n", colname[c0], colname[c1], colname[complement[c0|c1<<2]])
      }
   }
   fmt.Print("\n")

   pallmall([]int{blue, red, yellow})
   pallmall([]int{blue, red, yellow, red, yellow, blue, red, yellow, red, blue})
}

func pallmall(cols []int) {

   // invariant: meetingplace always contains a value unless a creature
   // is currently dealing with it (whereupon it must put it back).
   meetingplace := new(Place)
   ended := make(chan result)
   msg := ""
   for i, col := range cols {
      go creature(info{col, i}, meetingplace, ended)
      msg += " " + colname[col]
   }
   fmt.Println(msg)
   tot := 0
   // wait for all results
   for _ = range (cols) {
      result := <-ended
      tot += result.met
      fmt.Printf("%v%v\n", result.met, spell(result.same, true))
   }
   fmt.Printf("%v\n\n", spell(tot, true))
}

// in this function, variables ending in 0 refer to the local creature,
// variables ending in 1 to the creature we've met.
func creature(info0 info, m *Place, ended chan result) {
   c0 := make(chan info)
   met := 0
   same := 0
   for {
      var othername int
      // get access to rendez data and decide what to do.
      m.Lock()
      switch {
      case m.n >= n:
         // if no more meetings left, then send our result data and exit.
         m.Unlock()
         ended <- result{met, same}
         return

      case m.mate == nil:
         // no creature waiting wait for someone to meet us,
         // get their info and send our info in reply.
         m.info = info0
         m.mate = c0
         m.Unlock()
         info1 := <-c0
         othername = info1.name
         info0.colour = complement[info0.colour|info1.colour<<2]

      default:
         // another creature is waiting for us with its info
         // increment meeting count,
         // send them our info in reply.
         mate := m.mate
         m.n++
         m.mate = nil
         info1 := m.info
         m.Unlock()
         mate <- info0
         othername = info1.name
         info0.colour = complement[info0.colour|info1.colour<<2]
      }
      if othername == info0.name {
         same++
      }
      met++
   }
}

var digits = [...]string{"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}

func spell(n int, required bool) string {
   if n == 0 && !required {
      return ""
   }
   return spell(n/10, false) + " " + digits[n%10]
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * Based on fannkuch.c by Heiner Marxen
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "flag"
   "fmt"
   "strconv"
)

var n = 0

func fannkuch(n int) int {
   if n < 1 {
      return 0
   }

   n1 := n - 1
   perm := make([]int, n)
   perm1 := make([]int, n)
   count := make([]int, n)

   for i := 0; i < n; i++ {
      perm1[i] = i   // initial (trivial) permutation
   }

   r := n
   didpr := 0
   flipsMax := 0
   for {
      if didpr < 30  {
         for i := 0; i < n; i++ {
            fmt.Printf("%d", 1+perm1[i])
         }
         fmt.Printf("\n")
         didpr++
      }
      for ; r != 1; r-- {
         count[r-1] = r
      }

      if perm1[0] != 0 && perm1[n1] != n1 {
         flips := 0
         for i := 1; i < n; i++ {   // perm = perm1
            perm[i] = perm1[i]
         }
         k := perm1[0]      // cache perm[0] in k
         for {         // k!=0 ==> k>0
            for i, j := 1, k-1; i < j; i, j = i+1, j-1 {
               perm[i], perm[j] = perm[j], perm[i]
            }
            flips++
            // Now exchange k (caching perm[0]) and perm[k]... with care!
            j := perm[k]; perm[k] = k; k = j;
            if k == 0 {
               break
            }
         }
         if flipsMax < flips {
            flipsMax = flips
         }
      }

      for ; r < n; r++ {
         // rotate down perm[0..r] by one
         perm0 := perm1[0]
         for i := 0; i < r; i++ {
            perm1[i] = perm1[i+1]
         }
         perm1[r] = perm0
         count[r]--
         if count[r] > 0 {
            break
         }
      }
      if r == n {
         return flipsMax
      }
   }
   return 0
}

func main() {
   flag.Parse()
   if flag.NArg() > 0 { n,_ = strconv.Atoi( flag.Arg(0) ) }
   fmt.Printf("Pfannkuchen(%d) = %d\n", n, fannkuch(n))
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Based on fannkuch.scala by Rex Kerr
 * contributed by The Go Authors.
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "flag"
   "fmt"
   "runtime"
   "strconv"
)

var N = 0
var nCPU = 4

type Job struct {
   start []int
   n     int
}

type Found struct {
   who *Kucher
   k   int
}

type Kucher struct {
   perm []int
   temp []int
   flip []int
   in   chan Job
}

func NewKucher(length int) *Kucher {
   return &Kucher{
      perm: make([]int, length),
      temp: make([]int, length),
      flip: make([]int, length),
      in:   make(chan Job),
   }
}

func (k *Kucher) permute(n int) bool {
   i := 0
   for ; i < n-1 && k.flip[i] == 0; i++ {
      t := k.perm[0]
      j := 0
      for ; j <= i; j++ {
         k.perm[j] = k.perm[j+1]
      }
      k.perm[j] = t
   }
   k.flip[i]--
   for i > 0 {
      i--
      k.flip[i] = i
   }
   return k.flip[n-1] >= 0
}

func (k *Kucher) count() int {
   K := 0
   copy(k.temp, k.perm)
   for k.temp[0] != 0 {
      m := k.temp[0]
      for i := 0; i < m; i++ {
         k.temp[i], k.temp[m] = k.temp[m], k.temp[i]
         m--
      }
      K++
   }
   return K
}

func (k *Kucher) Run(foreman chan<- Found) {
   for job := range k.in {
      verbose := 30
      copy(k.perm, job.start)
      for i, v := range k.perm {
         if v != i {
            verbose = 0
         }
         k.flip[i] = i
      }
      K := 0
      for {
         if verbose > 0 {
            for _, p := range k.perm {
               fmt.Print(p + 1)
            }
            fmt.Println()
            verbose--
         }
         count := k.count()
         if count > K {
            K = count
         }
         if !k.permute(job.n) {
            break
         }
      }
      foreman <- Found{k, K}
   }
}

type Fanner struct {
   jobind   int
   jobsdone int
   k        int
   jobs     []Job
   workers  []*Kucher
   in       chan Found
   result   chan int
}

func NewFanner(jobs []Job, workers []*Kucher) *Fanner {
   return &Fanner{
      jobs: jobs, workers: workers,
      in:     make(chan Found),
      result: make(chan int),
   }
}

func (f *Fanner) Run(N int) {
   for msg := range f.in {
      if msg.k > f.k {
         f.k = msg.k
      }
      if msg.k >= 0 {
         f.jobsdone++
      }
      if f.jobind < len(f.jobs) {
         msg.who.in <- f.jobs[f.jobind]
         f.jobind++
      } else if f.jobsdone == len(f.jobs) {
         f.result <- f.k
         return
      }
   }
}

func swapped(a []int, i, j int) []int {
   b := make([]int, len(a))
   copy(b, a)
   b[i], b[j] = a[j], a[i]
   return b
}

func main() {
   flag.Parse()
   runtime.GOMAXPROCS(nCPU)
   if flag.NArg() > 0 { N,_ = strconv.Atoi( flag.Arg(0) ) }
   base := make([]int, N)
   for i := range base {
      base[i] = i
   }

   njobs := 1
   if N > 8 {
      njobs += (N*(N-1))/2 - 28 // njobs = 1 + sum(8..N-1) = 1 + sum(1..N-1) - sum(1..7)
   }
   jobs := make([]Job, njobs)
   jobsind := 0

   firstN := N
   if firstN > 8 {
      firstN = 8
   }
   jobs[jobsind] = Job{base, firstN}
   jobsind++
   for i := N - 1; i >= 8; i-- {
      for j := 0; j < i; j++ {
         jobs[jobsind] = Job{swapped(base, i, j), i}
         jobsind++
      }
   }

   nworkers := nCPU
   if njobs < nworkers {
      nworkers = njobs
   }
   workers := make([]*Kucher, nworkers)
   foreman := NewFanner(jobs, workers)
   go foreman.Run(N)
   for i := range workers {
      k := NewKucher(N)
      workers[i] = k
      go k.Run(foreman.in)
      foreman.in <- Found{k, -1}
   }
   fmt.Printf("Pfannkuchen(%d) = %d\n", N, <-foreman.result)
}

/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by Oleg Mazurov, June 2010
 *
 */

package main

import (
    "fmt"
    "os"
    "runtime"
    "strconv"
)

type Result struct {
    maxFlips int
    checkSum int
}

var (
    NCHUNKS = 720
    CHUNKSZ = 0
    NTASKS  = 0
)
var n = 12
var Fact []int

func fannkuch( idxMin int, ch chan Result ) {

    idxMax := idxMin + CHUNKSZ
    if idxMax < Fact[n] {
        go fannkuch( idxMax, ch )
    } else {
        idxMax = Fact[n]
    }

    p     := make([]int, n)
    pp    := make([]int, n)
    count := make([]int, n)

    // first permutation
    for i := 0; i<n; i++ {
        p[i] = i
    }
    for i, idx := n-1, idxMin; i>0; i-- {
        d := idx / Fact[i]
        count[i] = d
        idx = idx % Fact[i]

        copy( pp, p )
        for j := 0; j <= i; j++ {
	    if j+d <= i {
                p[j] = pp[j+d]
	    } else {
                p[j] = pp[j+d-i-1]
	    }
        }
    }

    maxFlips := 1
    checkSum := 0

    for idx, sign := idxMin, true; ; sign = !sign {

        // count flips
        first := p[0]
	if first != 0 {
	    flips := 1
	    if p[first] != 0 {
		copy( pp, p )
		p0 := first
	        for {
		    flips++
		    for i, j := 1, p0-1; i < j; i, j = i+1, j-1 {
		        pp[i], pp[j] = pp[j], pp[i]
		    }
		    t := pp[p0]
		    pp[p0] = p0
		    p0 = t
		    if pp[p0] == 0 {
		        break
		    }
	        }
	    }
	    if maxFlips < flips {
		maxFlips = flips
	    }
	    if sign {
		checkSum += flips
	    } else {
		checkSum -= flips
	    }
	}

	if idx++; idx == idxMax {
	    break
	}

	// next permutation
	if sign {
	    p[0], p[1] = p[1], first
	} else {
	    p[1], p[2] = p[2], p[1]
	    for k := 2;; k++ {
	        if count[k]++; count[k] <= k {
		    break
		}
	        count[k] = 0
		for j:=0; j<=k; j++ {
		    p[j] = p[j+1]
		}
		p[k+1] = first
		first = p[0]
	    }
	}
    }

    ch <- Result{ maxFlips, checkSum }
}

func printResult( n int, res int, chk int ) {
    fmt.Printf("%d\nPfannkuchen(%d) = %d\n", chk, n, res)
}

func main() {

    if len(os.Args) > 1 {
	v, err := strconv.Atoi(os.Args[1])
	if err != nil {
	    fmt.Printf( "error: %s\n", err.String() )
	    return
	}
	n = v
    }
    if n <= 0 || n > 12 {
	printResult( n, -1, -1 )
	return
    }

    runtime.GOMAXPROCS(4)

    Fact = make([]int, n+1)
    Fact[0] = 1
    for i := 1; i<len(Fact); i++ {
        Fact[i] = Fact[i-1] * i
    }

    CHUNKSZ = (Fact[n] + NCHUNKS - 1) / NCHUNKS
    CHUNKSZ += CHUNKSZ%2
    NTASKS = (Fact[n] + CHUNKSZ - 1) / CHUNKSZ

    ch := make(chan Result, NTASKS)

    go fannkuch(0, ch)
    
    res := 0
    chk := 0
    for i := 0; i<NTASKS; i++ {
	r := <-ch
	if res < r.maxFlips {
            res = r.maxFlips
	}
	chk += r.checkSum
    }

    printResult( n, res, chk )
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * Based on C program by by Petr Prokhorenkov.
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "flag"
   "os"
   "strconv"
)

var out = make(buffer, 0, 32768)

// var n = flag.Int("n", 1000, "length of result")
var n = 0

const Line = 60

func Repeat(alu []byte, n int) {
   buf := append(alu, alu...)
   off := 0
   for n > 0 {
      m := n
      if m > Line {
         m = Line
      }
      buf1 := out.NextWrite(m + 1)
      copy(buf1, buf[off:])
      buf1[m] = '\n'
      if off += m; off >= len(alu) {
         off -= len(alu)
      }
      n -= m
   }
}

const (
   IM = 139968
   IA = 3877
   IC = 29573

   LookupSize  = 4096
   LookupScale float64 = LookupSize - 1
)

var rand uint32 = 42

type Acid struct {
   sym   byte
   prob  float64
   cprob float64
   next  *Acid
}

func computeLookup(acid []Acid) *[LookupSize]*Acid {
   var lookup [LookupSize]*Acid
   var p float64
   for i := range acid {
      p += acid[i].prob
      acid[i].cprob = p * LookupScale
      if i > 0 {
         acid[i-1].next = &acid[i]
      }
   }
   acid[len(acid)-1].cprob = 1.0 * LookupScale

   j := 0
   for i := range lookup {
      for acid[j].cprob < float64(i) {
         j++
      }
      lookup[i] = &acid[j]
   }

   return &lookup
}

func Random(acid []Acid, n int) {
   lookup := computeLookup(acid)
   for n > 0 {
      m := n
      if m > Line {
         m = Line
      }
      buf := out.NextWrite(m + 1)
      f := LookupScale / IM
      myrand := rand
      for i := 0; i < m; i++ {
         myrand = (myrand*IA + IC) % IM
         r := float64(int(myrand)) * f
         a := lookup[int(r)]
         for a.cprob < r {
            a = a.next
         }
         buf[i] = a.sym
      }
      rand = myrand
      buf[m] = '\n'
      n -= m
   }
}

func main() {
   defer out.Flush()

   flag.Parse()
   if flag.NArg() > 0 { n,_ = strconv.Atoi( flag.Arg(0) ) }

   iub := []Acid{
      {prob: 0.27, sym: 'a'},
      {prob: 0.12, sym: 'c'},
      {prob: 0.12, sym: 'g'},
      {prob: 0.27, sym: 't'},
      {prob: 0.02, sym: 'B'},
      {prob: 0.02, sym: 'D'},
      {prob: 0.02, sym: 'H'},
      {prob: 0.02, sym: 'K'},
      {prob: 0.02, sym: 'M'},
      {prob: 0.02, sym: 'N'},
      {prob: 0.02, sym: 'R'},
      {prob: 0.02, sym: 'S'},
      {prob: 0.02, sym: 'V'},
      {prob: 0.02, sym: 'W'},
      {prob: 0.02, sym: 'Y'},
   }

   homosapiens := []Acid{
      {prob: 0.3029549426680, sym: 'a'},
      {prob: 0.1979883004921, sym: 'c'},
      {prob: 0.1975473066391, sym: 'g'},
      {prob: 0.3015094502008, sym: 't'},
   }

   alu := []byte(
      "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
         "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
         "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
         "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
         "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
         "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
         "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")

   out.WriteString(">ONE Homo sapiens alu\n")
   Repeat(alu, 2*n)
   out.WriteString(">TWO IUB ambiguity codes\n")
   Random(iub, 3*n)
   out.WriteString(">THREE Homo sapiens frequency\n")
   Random(homosapiens, 5*n)
}


type buffer []byte

func (b *buffer) Flush() {
   p := *b
   if len(p) > 0 {
      os.Stdout.Write(p)
   }
   *b = p[0:0]
}

func (b *buffer) WriteString(s string) {
   p := b.NextWrite(len(s))
   copy(p, s)
}

func (b *buffer) NextWrite(n int) []byte {
   p := *b
   if len(p)+n > cap(p) {
      b.Flush()
      p = *b
   }
   out := p[len(p) : len(p)+n]
   *b = p[:len(p)+n]
   return out
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 */

package main

import (
   "bufio"
   "bytes"
   "fmt"
   "io/ioutil"
   "os"
   "runtime"
   "sort"
)

func count(data string, n int) map[string]int {
   counts := make(map[string]int)
   top := len(data) - n
   for i := 0; i <= top; i++ {
      s := data[i : i+n]
      counts[s]++
   }
   return counts
}

func countOne(data string, s string) int {
   return count(data, len(s))[s]
}

type kNuc struct {
   name  string
   count int
}

type kNucArray []kNuc

func (kn kNucArray) Len() int      { return len(kn) }
func (kn kNucArray) Swap(i, j int) { kn[i], kn[j] = kn[j], kn[i] }
func (kn kNucArray) Less(i, j int) bool {
   if kn[i].count == kn[j].count {
      return kn[i].name > kn[j].name // sort down
   }
   return kn[i].count > kn[j].count
}

func sortedArray(m map[string]int) kNucArray {
   kn := make(kNucArray, len(m))
   i := 0
   for k, v := range m {
      kn[i] = kNuc{k, v}
      i++
   }
   sort.Sort(kn)
   return kn
}

func printKnucs(a kNucArray) {
   sum := 0
   for _, kn := range a {
      sum += kn.count
   }
   for _, kn := range a {
      fmt.Printf("%s %.3f\n", kn.name, 100*float64(kn.count)/float64(sum))
   }
   fmt.Print("\n")
}

func main() {
   runtime.GOMAXPROCS(4)
   in := bufio.NewReader(os.Stdin)
   three := []byte(">THREE ")
   for {
      line, err := in.ReadSlice('\n')
      if err != nil {
         fmt.Fprintln(os.Stderr, "ReadLine err:", err)
         os.Exit(2)
      }
      if line[0] == '>' && bytes.Equal(line[0:len(three)], three) {
         break
      }
   }
   data, err := ioutil.ReadAll(in)
   if err != nil {
      fmt.Fprintln(os.Stderr, "ReadAll err:", err)
      os.Exit(2)
   }
   // delete the newlines and convert to upper case
   j := 0
   for i := 0; i < len(data); i++ {
      if data[i] != '\n' {
         data[j] = data[i] &^ ' ' // upper case
         j++
      }
   }
   str := string(data[0:j])

   var arr1, arr2 kNucArray
   countsdone := make(chan bool)
   go func() {
      arr1 = sortedArray(count(str, 1))
      countsdone <- true
   }()
   go func() {
      arr2 = sortedArray(count(str, 2))
      countsdone <- true
   }()

   interests := []string{"GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"}
   results := make([]chan string, len(interests))
   for i, s := range interests {
      ch := make(chan string)
      results[i] = ch
      go func(result chan string, ss string) {
         result <- fmt.Sprintf("%d\t%s\n", countOne(str, ss), ss)
      }(ch, s)
   }
   <-countsdone
   <-countsdone
   printKnucs(arr1)
   printKnucs(arr2)
   for _, rc := range results {
      fmt.Print(<-rc)
   }

}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by K P anonymous
 */

package main

import (
   "bufio"
   "bytes"
   "sync"
   "fmt"
   "io/ioutil"
   "os"
   "sort"
   "runtime"
)

type byteseg struct {
   hash  uint
   slice []byte
}

type entry struct {
   bs    []byte
   value int
   next  *entry
}

const tabSize = 2 << 16

type Table struct {
   count int
   items [tabSize]*entry
}

func (bt *Table) Dump() []kNuc {
   res := make([]kNuc, bt.count)
   ind := 0
   for _, e := range bt.items {
      for e != nil {
         res[ind] = kNuc{name: e.bs, count: e.value}
         ind++
         e = e.next
      }
   }
   return res
}

func hashbytes(seg []byte) uint {
   l := len(seg)
   h := uint(l)
   for i := 0; i < l; i++ {
      h = h*131 + uint(seg[i])
   }
   return h
}

func eqbytes(a, b []byte) bool {
   l := len(a)
   if l != len(b) {
      return false
   }
   i := 0
   for ; i < l && a[i] == b[i]; i++ {
   }
   return i == l
}


func (bt *Table) get(bs []byte) *entry {
   ind := hashbytes(bs) % uint(tabSize)
   e := bt.items[ind]
   if e == nil {
      r := &entry{bs, 0, nil}
      bt.count++
      bt.items[ind] = r
      return r
   }
   for {
      if eqbytes(e.bs, bs) {
         return e
      }
      if e.next == nil {
         r := &entry{bs, 0, nil}
         bt.count++
         e.next = r
         return r
      }
      e = e.next
   }
   return nil
}

func (bt *Table) Increment(bs []byte) {
   bt.get(bs).value++
}

func (bt *Table) Lookup(bs []byte) int {
   return bt.get(bs).value
}

var tables [8]Table
var ti = 0
var tmux sync.Mutex

func newTable() *Table {
   tmux.Lock()
   t := &tables[ti]
   ti++
   tmux.Unlock()
   return t
}

func count(data []byte, n int) *Table {
   counts := newTable()
   top := len(data) - n
   for i := 0; i <= top; i++ {
      counts.Increment(data[i : i+n])
   }
   return counts
}

func countOne(data []byte, s []byte) int {
   return count(data, len(s)).Lookup(s)
}

type kNuc struct {
   name  []byte
   count int
}

type kNucArray []kNuc

func (kn kNucArray) Len() int      { return len(kn) }
func (kn kNucArray) Swap(i, j int) { kn[i], kn[j] = kn[j], kn[i] }
func (kn kNucArray) Less(i, j int) bool {
   if kn[i].count == kn[j].count {
      return bytes.Compare(kn[i].name, kn[j].name) < 0 // sort down
   }
   return kn[i].count > kn[j].count
}

func sortedArray(m *Table) kNucArray {
   kn := kNucArray(m.Dump())
   sort.Sort(kn)
   return kn
}

func printKnucs(a kNucArray) {
   sum := 0
   for _, kn := range a {
      sum += kn.count
   }
   for _, kn := range a {
      fmt.Printf("%s %.3f\n", kn.name, 100*float64(kn.count)/float64(sum))
   }
   fmt.Print("\n")
}

func main() {
   runtime.GOMAXPROCS(4)
   in := bufio.NewReader(os.Stdin)
   three := []byte(">THREE ")
   for {
      line, err := in.ReadSlice('\n')
      if err != nil {
         fmt.Fprintln(os.Stderr, "ReadLine err:", err)
         os.Exit(2)
      }
      if line[0] == '>' && bytes.Equal(line[0:len(three)], three) {
         break
      }
   }
   data, err := ioutil.ReadAll(in)
   if err != nil {
      fmt.Fprintln(os.Stderr, "ReadAll err:", err)
      os.Exit(2)
   }
   // delete the newlines and convert to upper case
   j := 0
   for i := 0; i < len(data); i++ {
      if data[i] != '\n' {
         data[j] = data[i] &^ ' ' // upper case
         j++
      }
   }
   str := data[0:j]

   var arr1, arr2 kNucArray
   countsdone := make(chan bool, 2)
   go func() {
      arr1 = sortedArray(count(str, 1))
      countsdone <- true
   }()
   go func() {
      arr2 = sortedArray(count(str, 2))
      countsdone <- true
   }()

   interests := []string{"GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"}
   results := make([]chan string, len(interests))
   for i, s := range interests {
      ch := make(chan string, 1)
      results[i] = ch
      go func(result chan string, ss string) {
         result <- fmt.Sprintf("%d\t%s\n", countOne(str, []byte(ss)), ss)
      }(ch, s)
   }
   <-countsdone
   <-countsdone
   printKnucs(arr1)
   printKnucs(arr2)
   for _, rc := range results {
      fmt.Print(<-rc)
   }
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by K P anonymous
 */

package main

import (
   "bufio"
   "bytes"
   "fmt"
   "io/ioutil"
   "os"
   "runtime"
   "sort"
   "sync"
)

type entry struct {
   bs    []byte
   value int
   next  *entry
}

const tabSize = 2 << 16

type Table struct {
   count int
   items [tabSize]*entry
}

func (bt *Table) Dump() []kNuc {
   res := make([]kNuc, bt.count)
   i := 0
   for _, e := range bt.items {
      for e != nil {
         res[i] = kNuc{name: e.bs, count: e.value}
         i++
         e = e.next
      }
   }
   return res
}

func hashbytes(seg []byte) uint {
   l := len(seg)
   h := uint(l)
   for i := 0; i < l; i++ {
      h = h*131 + uint(seg[i])
   }
   return h
}

func (bt *Table) get(bs []byte) *entry {
   ind := hashbytes(bs) % uint(tabSize)
   e := bt.items[ind]
   if e == nil {
      r := &entry{bs, 0, nil}
      bt.count++
      bt.items[ind] = r
      return r
   }
   for {
      if bytes.Equal(e.bs, bs) {
         return e
      }
      if e.next == nil {
         r := &entry{bs, 0, nil}
         bt.count++
         e.next = r
         return r
      }
      e = e.next
   }
   return nil
}

var (
   tables [8]Table
   ti     = 0
   tmux   sync.Mutex
)

func newTable() *Table {
   tmux.Lock()
   t := &tables[ti]
   ti++
   tmux.Unlock()
   return t
}

func count(data []byte, n int) *Table {
   counts := newTable()
   top := len(data) - n
   for i := 0; i <= top; i++ {
      counts.get(data[i:i+n]).value++
   }
   return counts
}

func countOne(data []byte, s []byte) int {
   return count(data, len(s)).get(s).value
}

type kNuc struct {
   name  []byte
   count int
}

type kNucArray []kNuc

func (kn kNucArray) Len() int      { return len(kn) }
func (kn kNucArray) Swap(i, j int) { kn[i], kn[j] = kn[j], kn[i] }
func (kn kNucArray) Less(i, j int) bool {
   if kn[i].count == kn[j].count {
      return bytes.Compare(kn[i].name, kn[j].name) < 0 // sort down
   }
   return kn[i].count > kn[j].count
}

func printKnucs(a kNucArray) {
   sum := 0
   for _, kn := range a {
      sum += kn.count
   }
   for _, kn := range a {
      fmt.Printf("%s %.3f\n", kn.name, 100*float64(kn.count)/float64(sum))
   }
   fmt.Print("\n")
}

func main() {
   runtime.GOMAXPROCS(4)
   in := bufio.NewReader(os.Stdin)
   three := []byte(">THREE ")
   for {
      line, err := in.ReadSlice('\n')
      if err != nil {
         panic(err)
      }
      if bytes.HasPrefix(line, three) {
         break
      }
   }
   data, err := ioutil.ReadAll(in)
   if err != nil {
      panic(err)
   }
   // delete the newlines and convert to upper case
   j := 0
   for i := 0; i < len(data); i++ {
      if data[i] != '\n' {
         data[j] = data[i] &^ ' ' // upper case
         j++
      }
   }
   str := data[0:j]

   var wg sync.WaitGroup
   async := func(fn func()) {
      wg.Add(1)
      go func() {
         fn()
         wg.Done()
      }()
   }

   var arrs [2]kNucArray
   async(func() {
      arrs[0] = count(str, 1).Dump()
      sort.Sort(arrs[0])
   })
   async(func() {
      arrs[1] = count(str, 2).Dump()
      sort.Sort(arrs[1])
   })

   interests := []string{"GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"}
   results := make([]string, len(interests))
   for i, s := range interests {
      s, i := s, i
      async(func() {
         results[i] = fmt.Sprintf("%d\t%s", countOne(str, []byte(s)), s)
      })
   }
   wg.Wait()
   printKnucs(arrs[0])
   printKnucs(arrs[1])
   for _, rc := range results {
      fmt.Println(rc)
   }
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Martin Koistinen
 * Based on mandelbrot.c contributed by Greg Buchholz and The Go Authors
 * flag.Arg hack by Isaac Gouy
 *
 * Large changes by Bill Broadley, including:
 * 1) Switching the one goroutine per line to one per CPU
 * 2) Replacing gorouting calls with channels
 * 3) Handling out of order results in the file writer.
 */

package main

import (
   "bufio"
   "flag"
   "fmt"
   "os"
   "strconv"
   "runtime"
)

/* targeting a q6600 system, one cpu worker per core */
const pool = 4

const ZERO float64 = 0
const LIMIT = 2.0
const ITER = 50   // Benchmark parameter
const SIZE = 16000

var rows []byte
var bytesPerRow int

// This func is responsible for rendering a row of pixels,
// and when complete writing it out to the file.

func renderRow(w, h, bytes int, workChan chan int,iter int, finishChan chan bool) {

   var Zr, Zi, Tr, Ti, Cr float64
   var x,i int

   for y := range workChan {

      offset := bytesPerRow * y
      Ci := (2*float64(y)/float64(h) - 1.0)

      for x = 0; x < w; x++ {
         Zr, Zi, Tr, Ti = ZERO, ZERO, ZERO, ZERO
         Cr = (2*float64(x)/float64(w) - 1.5)

         for i = 0; i < iter && Tr+Ti <= LIMIT*LIMIT; i++ {
            Zi = 2*Zr*Zi + Ci
            Zr = Tr - Ti + Cr
            Tr = Zr * Zr
            Ti = Zi * Zi
         }

         // Store the value in the array of ints
         if Tr+Ti <= LIMIT*LIMIT {
            rows[offset+x/8] |= (byte(1) << uint(7-(x%8)))
         }
      }
   }
   /* tell master I'm finished */
   finishChan <- true
}

func main() {
   runtime.GOMAXPROCS(pool) 

   size := SIZE   // Contest settings
   iter := ITER

   // Get input, if any...
   flag.Parse()
   if flag.NArg() > 0 {
      size, _ = strconv.Atoi(flag.Arg(0))
   }
   w, h := size, size
   bytesPerRow =  w / 8

   out := bufio.NewWriter(os.Stdout)
   defer out.Flush()
   fmt.Fprintf(out, "P4\n%d %d\n", w, h)

   rows = make([]byte, bytesPerRow*h)

   /* global buffer of work for workers, ideally never runs dry */
   workChan := make(chan int, pool*2+1)
   /* global buffer of results for output, ideally never blocks */
   finishChan := make(chan bool)
   // start pool workers, and assign all work
   for y := 0; y < size; y++ {
      if y < pool {
         go renderRow(w, h, bytesPerRow, workChan, iter,finishChan)
      }
      workChan <- y
   }
   /* tell the workers all done */
   close(workChan)
   /* write for the file workers to finish */
   for i:=0;i<pool;i++ {
      <- finishChan
   }
   out.Write(rows)
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * based on meteor-contest.c by Christian Vosteen
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "flag"
   "fmt"
   "strconv"
)

var max_solutions = 0


func boolInt(b bool) int8 {
   if b {
      return 1
   }
   return 0
}

/* The board is a 50 cell hexagonal pattern.  For    . . . . .
 * maximum speed the board will be implemented as     . . . . .
 * 50 bits, which will fit into a 64 bit long long   . . . . .
 * int.                                               . . . . .
 *                                                   . . . . .
 * I will represent 0's as empty cells and 1's        . . . . .
 * as full cells.                                    . . . . .
 *                                                    . . . . .
 *                                                   . . . . .
 *                                                    . . . . .
 */

var board uint64 = 0xFFFC000000000000

/* The puzzle pieces must be specified by the path followed
 * from one end to the other along 12 hexagonal directions.
 *
 *   Piece 0   Piece 1   Piece 2   Piece 3   Piece 4
 *
 *  O O O O    O   O O   O O O     O O O     O   O
 *         O    O O           O       O       O O
 *                           O         O         O
 *
 *   Piece 5   Piece 6   Piece 7   Piece 8   Piece 9
 *
 *    O O O     O O       O O     O O        O O O O
 *       O O       O O       O       O O O        O
 *                  O       O O
 *
 * I had to make it 12 directions because I wanted all of the
 * piece definitions to fit into the same size arrays.  It is
 * not possible to define piece 4 in terms of the 6 cardinal
 * directions in 4 moves.
 */

const (
   E = iota
   ESE
   SE
   S
   SW
   WSW
   W
   WNW
   NW
   N
   NE
   ENE
   PIVOT
)

var piece_def = [10][4]int8{
   [4]int8{E, E, E, SE},
   [4]int8{SE, E, NE, E},
   [4]int8{E, E, SE, SW},
   [4]int8{E, E, SW, SE},
   [4]int8{SE, E, NE, S},
   [4]int8{E, E, SW, E},
   [4]int8{E, SE, SE, NE},
   [4]int8{E, SE, SE, W},
   [4]int8{E, SE, E, E},
   [4]int8{E, E, E, SW},
}


/* To minimize the amount of work done in the recursive solve function below,
 * I'm going to allocate enough space for all legal rotations of each piece
 * at each position on the board. That's 10 pieces x 50 board positions x
 * 12 rotations.  However, not all 12 rotations will fit on every cell, so
 * I'll have to keep count of the actual number that do.
 * The pieces are going to be unsigned long long ints just like the board so
 * they can be bitwise-anded with the board to determine if they fit.
 * I'm also going to record the next possible open cell for each piece and
 * location to reduce the burden on the solve function.
 */
var (
   pieces       [10][50][12]uint64
   piece_counts [10][50]int
   next_cell    [10][50][12]int8
)

/* Returns the direction rotated 60 degrees clockwise */
func rotate(dir int8) int8 { return (dir + 2) % PIVOT }

/* Returns the direction flipped on the horizontal axis */
func flip(dir int8) int8 { return (PIVOT - dir) % PIVOT }


/* Returns the new cell index from the specified cell in the
 * specified direction.  The index is only valid if the
 * starting cell and direction have been checked by the
 * out_of_bounds function first.
 */
func shift(cell, dir int8) int8 {
   switch dir {
   case E:
      return cell + 1
   case ESE:
      if ((cell / 5) % 2) != 0 {
         return cell + 7
      } else {
         return cell + 6
      }
   case SE:
      if ((cell / 5) % 2) != 0 {
         return cell + 6
      } else {
         return cell + 5
      }
   case S:
      return cell + 10
   case SW:
      if ((cell / 5) % 2) != 0 {
         return cell + 5
      } else {
         return cell + 4
      }
   case WSW:
      if ((cell / 5) % 2) != 0 {
         return cell + 4
      } else {
         return cell + 3
      }
   case W:
      return cell - 1
   case WNW:
      if ((cell / 5) % 2) != 0 {
         return cell - 6
      } else {
         return cell - 7
      }
   case NW:
      if ((cell / 5) % 2) != 0 {
         return cell - 5
      } else {
         return cell - 6
      }
   case N:
      return cell - 10
   case NE:
      if ((cell / 5) % 2) != 0 {
         return cell - 4
      } else {
         return cell - 5
      }
   case ENE:
      if ((cell / 5) % 2) != 0 {
         return cell - 3
      } else {
         return cell - 4
      }
   }
   return cell
}

/* Returns wether the specified cell and direction will land outside
 * of the board.  Used to determine if a piece is at a legal board
 * location or not.
 */
func out_of_bounds(cell, dir int8) bool {
   switch dir {
   case E:
      return cell%5 == 4
   case ESE:
      i := cell % 10
      return i == 4 || i == 8 || i == 9 || cell >= 45
   case SE:
      return cell%10 == 9 || cell >= 45
   case S:
      return cell >= 40
   case SW:
      return cell%10 == 0 || cell >= 45
   case WSW:
      i := cell % 10
      return i == 0 || i == 1 || i == 5 || cell >= 45
   case W:
      return cell%5 == 0
   case WNW:
      i := cell % 10
      return i == 0 || i == 1 || i == 5 || cell < 5
   case NW:
      return cell%10 == 0 || cell < 5
   case N:
      return cell < 10
   case NE:
      return cell%10 == 9 || cell < 5
   case ENE:
      i := cell % 10
      return i == 4 || i == 8 || i == 9 || cell < 5
   }
   return false
}

/* Rotate a piece 60 degrees clockwise */
func rotate_piece(piece int) {
   for i := 0; i < 4; i++ {
      piece_def[piece][i] = rotate(piece_def[piece][i])
   }
}

/* Flip a piece along the horizontal axis */
func flip_piece(piece int) {
   for i := 0; i < 4; i++ {
      piece_def[piece][i] = flip(piece_def[piece][i])
   }
}

/* Convenience function to quickly calculate all of the indices for a piece */
func calc_cell_indices(cell []int8, piece int, index int8) {
   cell[0] = index
   for i := 1; i < 5; i++ {
      cell[i] = shift(cell[i-1], piece_def[piece][i-1])
   }
}

/* Convenience function to quickly calculate if a piece fits on the board */
func cells_fit_on_board(cell []int8, piece int) bool {
   return !out_of_bounds(cell[0], piece_def[piece][0]) &&
      !out_of_bounds(cell[1], piece_def[piece][1]) &&
      !out_of_bounds(cell[2], piece_def[piece][2]) &&
      !out_of_bounds(cell[3], piece_def[piece][3])
}

/* Returns the lowest index of the cells of a piece.
 * I use the lowest index that a piece occupies as the index for looking up
 * the piece in the solve function.
 */
func minimum_of_cells(cell []int8) int8 {
   minimum := cell[0]
   for i := 1; i < 5; i++ {
      if cell[i] < minimum {
         minimum = cell[i]
      }
   }
   return minimum
}

/* Calculate the lowest possible open cell if the piece is placed on the board.
 * Used to later reduce the amount of time searching for open cells in the
 * solve function.
 */
func first_empty_cell(cell []int8, minimum int8) int8 {
   first_empty := minimum
   for first_empty == cell[0] || first_empty == cell[1] ||
      first_empty == cell[2] || first_empty == cell[3] ||
      first_empty == cell[4] {
      first_empty++
   }
   return first_empty
}

/* Generate the unsigned long long int that will later be anded with the
 * board to determine if it fits.
 */
func bitmask_from_cells(cell []int8) uint64 {
   var piece_mask uint64
   for i := 0; i < 5; i++ {
      piece_mask |= 1 << uint(cell[i])
   }
   return piece_mask
}

/* Record the piece and other important information in arrays that will
 * later be used by the solve function.
 */
func record_piece(piece int, minimum int8, first_empty int8, piece_mask uint64) {
   pieces[piece][minimum][piece_counts[piece][minimum]] = piece_mask
   next_cell[piece][minimum][piece_counts[piece][minimum]] = first_empty
   piece_counts[piece][minimum]++
}


/* Fill the entire board going cell by cell.  If any cells are "trapped"
 * they will be left alone.
 */
func fill_contiguous_space(board []int8, index int8) {
   if board[index] == 1 {
      return
   }
   board[index] = 1
   if !out_of_bounds(index, E) {
      fill_contiguous_space(board, shift(index, E))
   }
   if !out_of_bounds(index, SE) {
      fill_contiguous_space(board, shift(index, SE))
   }
   if !out_of_bounds(index, SW) {
      fill_contiguous_space(board, shift(index, SW))
   }
   if !out_of_bounds(index, W) {
      fill_contiguous_space(board, shift(index, W))
   }
   if !out_of_bounds(index, NW) {
      fill_contiguous_space(board, shift(index, NW))
   }
   if !out_of_bounds(index, NE) {
      fill_contiguous_space(board, shift(index, NE))
   }
}


/* To thin the number of pieces, I calculate if any of them trap any empty
 * cells at the edges.  There are only a handful of exceptions where the
 * the board can be solved with the trapped cells.  For example:  piece 8 can
 * trap 5 cells in the corner, but piece 3 can fit in those cells, or piece 0
 * can split the board in half where both halves are viable.
 */
func has_island(cell []int8, piece int) bool {
   temp_board := make([]int8, 50)
   var i int
   for i = 0; i < 5; i++ {
      temp_board[cell[i]] = 1
   }
   i = 49
   for temp_board[i] == 1 {
      i--
   }
   fill_contiguous_space(temp_board, int8(i))
   c := 0
   for i = 0; i < 50; i++ {
      if temp_board[i] == 0 {
         c++
      }
   }
   if c == 0 || (c == 5 && piece == 8) || (c == 40 && piece == 8) ||
      (c%5 == 0 && piece == 0) {
      return false
   }
   return true
}


/* Calculate all six rotations of the specified piece at the specified index.
 * We calculate only half of piece 3's rotations.  This is because any solution
 * found has an identical solution rotated 180 degrees.  Thus we can reduce the
 * number of attempted pieces in the solve algorithm by not including the 180-
 * degree-rotated pieces of ONE of the pieces.  I chose piece 3 because it gave
 * me the best time ;)
 */
func calc_six_rotations(piece, index int) {
   cell := make([]int8, 5)
   for rotation := 0; rotation < 6; rotation++ {
      if piece != 3 || rotation < 3 {
         calc_cell_indices(cell, piece, int8(index))
         if cells_fit_on_board(cell, piece) && !has_island(cell, piece) {
            minimum := minimum_of_cells(cell)
            first_empty := first_empty_cell(cell, minimum)
            piece_mask := bitmask_from_cells(cell)
            record_piece(piece, minimum, first_empty, piece_mask)
         }
      }
      rotate_piece(piece)
   }
}

/* Calculate every legal rotation for each piece at each board location. */
func calc_pieces() {
   for piece := 0; piece < 10; piece++ {
      for index := 0; index < 50; index++ {
         calc_six_rotations(piece, index)
         flip_piece(piece)
         calc_six_rotations(piece, index)
      }
   }
}


/* Calculate all 32 possible states for a 5-bit row and all rows that will
 * create islands that follow any of the 32 possible rows.  These pre-
 * calculated 5-bit rows will be used to find islands in a partially solved
 * board in the solve function.
 */
const (
   ROW_MASK    = 0x1F
   TRIPLE_MASK = 0x7FFF
)

var (
   all_rows = [32]int8{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
   }
   bad_even_rows   [32][32]int8
   bad_odd_rows    [32][32]int8
   bad_even_triple [32768]int8
   bad_odd_triple  [32768]int8
)

func rows_bad(row1, row2 int8, even bool) int8 {
   /* even is referring to row1 */
   var row2_shift int8
   /* Test for blockages at same index and shifted index */
   if even {
      row2_shift = ((row2 << 1) & ROW_MASK) | 0x01
   } else {
      row2_shift = (row2 >> 1) | 0x10
   }
   block := ((row1 ^ row2) & row2) & ((row1 ^ row2_shift) & row2_shift)
   /* Test for groups of 0's */
   in_zeroes := false
   group_okay := false
   for i := uint8(0); i < 5; i++ {
      if row1&(1<<i) != 0 {
         if in_zeroes {
            if !group_okay {
               return 1
            }
            in_zeroes = false
            group_okay = false
         }
      } else {
         if !in_zeroes {
            in_zeroes = true
         }
         if (block & (1 << i)) == 0 {
            group_okay = true
         }
      }
   }
   if in_zeroes {
      return boolInt(!group_okay)
   }
   return 0
}

/* Check for cases where three rows checked sequentially cause a false
 * positive.  One scenario is when 5 cells may be surrounded where piece 5
 * or 7 can fit.  The other scenario is when piece 2 creates a hook shape.
 */
func triple_is_okay(row1, row2, row3 int, even bool) bool {
   if even {
      /* There are four cases:
       * row1: 00011  00001  11001  10101
       * row2: 01011  00101  10001  10001
       * row3: 011??  00110  ?????  ?????
       */
      return ((row1 == 0x03) && (row2 == 0x0B) && ((row3 & 0x1C) == 0x0C)) ||
         ((row1 == 0x01) && (row2 == 0x05) && (row3 == 0x06)) ||
         ((row1 == 0x19) && (row2 == 0x11)) ||
         ((row1 == 0x15) && (row2 == 0x11))
   }
   /* There are two cases:
    * row1: 10011  10101
    * row2: 10001  10001
    * row3: ?????  ?????
    */
   return ((row1 == 0x13) && (row2 == 0x11)) ||
      ((row1 == 0x15) && (row2 == 0x11))
}

func calc_rows() {
   for row1 := int8(0); row1 < 32; row1++ {
      for row2 := int8(0); row2 < 32; row2++ {
         bad_even_rows[row1][row2] = rows_bad(row1, row2, true)
         bad_odd_rows[row1][row2] = rows_bad(row1, row2, false)
      }
   }
   for row1 := 0; row1 < 32; row1++ {
      for row2 := 0; row2 < 32; row2++ {
         for row3 := 0; row3 < 32; row3++ {
            result1 := bad_even_rows[row1][row2]
            result2 := bad_odd_rows[row2][row3]
            if result1 == 0 && result2 != 0 && triple_is_okay(row1, row2, row3, true) {
               bad_even_triple[row1+(row2*32)+(row3*1024)] = 0
            } else {
               bad_even_triple[row1+(row2*32)+(row3*1024)] = boolInt(result1 != 0 || result2 != 0)
            }

            result1 = bad_odd_rows[row1][row2]
            result2 = bad_even_rows[row2][row3]
            if result1 == 0 && result2 != 0 && triple_is_okay(row1, row2, row3, false) {
               bad_odd_triple[row1+(row2*32)+(row3*1024)] = 0
            } else {
               bad_odd_triple[row1+(row2*32)+(row3*1024)] = boolInt(result1 != 0 || result2 != 0)
            }
         }
      }
   }
}


/* Calculate islands while solving the board.
*/
func boardHasIslands(cell int8) int8 {
   /* Too low on board, don't bother checking */
   if cell >= 40 {
      return 0
   }
   current_triple := (board >> uint((cell/5)*5)) & TRIPLE_MASK
   if (cell/5)%2 != 0 {
      return bad_odd_triple[current_triple]
   }
   return bad_even_triple[current_triple]
}


/* The recursive solve algorithm.  Try to place each permutation in the upper-
 * leftmost empty cell.  Mark off available pieces as it goes along.
 * Because the board is a bit mask, the piece number and bit mask must be saved
 * at each successful piece placement.  This data is used to create a 50 char
 * array if a solution is found.
 */
var (
   avail          uint16 = 0x03FF
   sol_nums       [10]int8
   sol_masks      [10]uint64
   solutions      [2100][50]int8
   solution_count = 0
)

func record_solution() {
   for sol_no := 0; sol_no < 10; sol_no++ {
      sol_mask := sol_masks[sol_no]
      for index := 0; index < 50; index++ {
         if sol_mask&1 == 1 {
            solutions[solution_count][index] = sol_nums[sol_no]
            /* Board rotated 180 degrees is a solution too! */
            solutions[solution_count+1][49-index] = sol_nums[sol_no]
         }
         sol_mask = sol_mask >> 1
      }
   }
   solution_count += 2
}

func solve(depth, cell int8) {
   if solution_count >= max_solutions {
      return
   }

   for board&(1<<uint(cell)) != 0 {
      cell++
   }

   for piece := int8(0); piece < 10; piece++ {
      var piece_no_mask uint16 = 1 << uint(piece)
      if avail&piece_no_mask == 0 {
         continue
      }
      avail ^= piece_no_mask
      max_rots := piece_counts[piece][cell]
      piece_mask := pieces[piece][cell]
      for rotation := 0; rotation < max_rots; rotation++ {
         if board&piece_mask[rotation] == 0 {
            sol_nums[depth] = piece
            sol_masks[depth] = piece_mask[rotation]
            if depth == 9 {
               /* Solution found!!!!!11!!ONE! */
               record_solution()
               avail ^= piece_no_mask
               return
            }
            board |= piece_mask[rotation]
            if boardHasIslands(next_cell[piece][cell][rotation]) == 0 {
               solve(depth+1, next_cell[piece][cell][rotation])
            }
            board ^= piece_mask[rotation]
         }
      }
      avail ^= piece_no_mask
   }
}

/* pretty print a board in the specified hexagonal format */
func pretty(b *[50]int8) {
   for i := 0; i < 50; i += 10 {
      fmt.Printf("%c %c %c %c %c \n %c %c %c %c %c \n", b[i]+'0', b[i+1]+'0',
         b[i+2]+'0', b[i+3]+'0', b[i+4]+'0', b[i+5]+'0', b[i+6]+'0',
         b[i+7]+'0', b[i+8]+'0', b[i+9]+'0')
   }
   fmt.Printf("\n")
}

/* Find smallest and largest solutions */
func smallest_largest() (smallest, largest *[50]int8) {
   smallest = &solutions[0]
   largest = &solutions[0]
   for i := 1; i < solution_count; i++ {
      candidate := &solutions[i]
      for j, s := range *smallest {
         c := candidate[j]
         if c == s {
            continue
         }
         if c < s {
            smallest = candidate
         }
         break
      }
      for j, s := range *largest {
         c := candidate[j]
         if c == s {
            continue
         }
         if c > s {
            largest = candidate
         }
         break
      }
   }
   return
}

func main() {
   flag.Parse()
   if flag.NArg() > 0 { max_solutions,_ = strconv.Atoi( flag.Arg(0) ) }

   calc_pieces()
   calc_rows()
   solve(0, 0)
   fmt.Printf("%d solutions found\n\n", solution_count)
   smallest, largest := smallest_largest()
   pretty(smallest)
   pretty(largest)
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * based on C program by Christoph Bauer
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "flag"
   "fmt"
   "math"
   "strconv"
)

var n = 0

type Body struct {
   x, y, z, vx, vy, vz, mass float64
}

const (
   solarMass   = 4 * math.Pi * math.Pi
   daysPerYear = 365.24
)

func (b *Body) offsetMomentum(px, py, pz float64) {
   b.vx = -px / solarMass
   b.vy = -py / solarMass
   b.vz = -pz / solarMass
}

type System []*Body

func NewSystem(body []Body) System {
   n := make(System, len(body))
   for i := 0; i < len(body); i++ {
      n[i] = new(Body) // copy to avoid overwriting the inputs
      *n[i] = body[i]
   }
   var px, py, pz float64
   for _, body := range n {
      px += body.vx * body.mass
      py += body.vy * body.mass
      pz += body.vz * body.mass
   }
   n[0].offsetMomentum(px, py, pz)
   return n
}

func (sys System) energy() float64 {
   var e float64
   for i, body := range sys {
      e += 0.5 * body.mass *
         (body.vx*body.vx + body.vy*body.vy + body.vz*body.vz)
      for j := i + 1; j < len(sys); j++ {
         body2 := sys[j]
         dx := body.x - body2.x
         dy := body.y - body2.y
         dz := body.z - body2.z
         distance := math.Sqrt(dx*dx + dy*dy + dz*dz)
         e -= (body.mass * body2.mass) / distance
      }
   }
   return e
}

func (sys System) advance(dt float64) {
   for i, body := range sys {
      for j := i + 1; j < len(sys); j++ {
         body2 := sys[j]
         dx := body.x - body2.x
         dy := body.y - body2.y
         dz := body.z - body2.z

         dSquared := dx*dx + dy*dy + dz*dz
         distance := math.Sqrt(dSquared)
         mag := dt / (dSquared * distance)

         body.vx -= dx * body2.mass * mag
         body.vy -= dy * body2.mass * mag
         body.vz -= dz * body2.mass * mag

         body2.vx += dx * body.mass * mag
         body2.vy += dy * body.mass * mag
         body2.vz += dz * body.mass * mag
      }
   }

   for _, body := range sys {
      body.x += dt * body.vx
      body.y += dt * body.vy
      body.z += dt * body.vz
   }
}

var (
   jupiter = Body{
      x: 4.84143144246472090e+00,
      y: -1.16032004402742839e+00,
      z: -1.03622044471123109e-01,
      vx: 1.66007664274403694e-03 * daysPerYear,
      vy: 7.69901118419740425e-03 * daysPerYear,
      vz: -6.90460016972063023e-05 * daysPerYear,
      mass: 9.54791938424326609e-04 * solarMass,
   }
   saturn = Body{
      x: 8.34336671824457987e+00,
      y: 4.12479856412430479e+00,
      z: -4.03523417114321381e-01,
      vx: -2.76742510726862411e-03 * daysPerYear,
      vy: 4.99852801234917238e-03 * daysPerYear,
      vz: 2.30417297573763929e-05 * daysPerYear,
      mass: 2.85885980666130812e-04 * solarMass,
   }
   uranus = Body{
      x: 1.28943695621391310e+01,
      y: -1.51111514016986312e+01,
      z: -2.23307578892655734e-01,
      vx: 2.96460137564761618e-03 * daysPerYear,
      vy: 2.37847173959480950e-03 * daysPerYear,
      vz: -2.96589568540237556e-05 * daysPerYear,
      mass: 4.36624404335156298e-05 * solarMass,
   }
   neptune = Body{
      x: 1.53796971148509165e+01,
      y: -2.59193146099879641e+01,
      z: 1.79258772950371181e-01,
      vx: 2.68067772490389322e-03 * daysPerYear,
      vy: 1.62824170038242295e-03 * daysPerYear,
      vz: -9.51592254519715870e-05 * daysPerYear,
      mass: 5.15138902046611451e-05 * solarMass,
   }
   sun = Body{
      mass: solarMass,
   }
)

func main() {
   flag.Parse()
   if flag.NArg() > 0 { n,_ = strconv.Atoi( flag.Arg(0) ) }

   system := NewSystem([]Body{sun, jupiter, saturn, uranus, neptune})
   fmt.Printf("%.9f\n", system.energy())
   for i := 0; i < n; i++ {
      system.advance(0.01)
   }
   fmt.Printf("%.9f\n", system.energy())
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * based on pidigits.c (by Paolo Bonzini & Sean Bartlett,
 *                      modified by Michael Mellor)
 *
 * contributed by The Go Authors.
 * flag.Arg hack by Isaac Gouy
 */

package main

import (
   "big"
   "flag"
   "fmt"
   "strconv"
)

var n = 0
var silent = false

var (
	tmp1  = big.NewInt(0)
	tmp2  = big.NewInt(0)
	y2    = big.NewInt(0)
	bigk  = big.NewInt(0)
	numer = big.NewInt(1)
	accum = big.NewInt(0)
	denom = big.NewInt(1)
	ten   = big.NewInt(10)
)

func extract_digit() int64 {
	if numer.Cmp(accum) > 0 {
		return -1
	}

	// Compute (numer * 3 + accum) / denom
	tmp1.Lsh(numer, 1)
	tmp1.Add(tmp1, numer)
	tmp1.Add(tmp1, accum)
	tmp1.DivMod(tmp1, denom, tmp2)

	// Now, if (numer * 4 + accum) % denom...
	tmp2.Add(tmp2, numer)

	// ... is normalized, then the two divisions have the same result.
	if tmp2.Cmp(denom) >= 0 {
		return -1
	}

	return tmp1.Int64()
}

func next_term(k int64) {
	// TODO(eds) If big.Int ever gets a Scale method, y2 and bigk could be int64
	y2.SetInt64(k*2 + 1)
	bigk.SetInt64(k)

	tmp1.Lsh(numer, 1)
	accum.Add(accum, tmp1)
	accum.Mul(accum, y2)
	numer.Mul(numer, bigk)
	denom.Mul(denom, y2)
}

func eliminate_digit(d int64) {
	tmp := big.NewInt(0).Set(denom)
	accum.Sub(accum, tmp.Mul(tmp, big.NewInt(d)))
	accum.Mul(accum, ten)
	numer.Mul(numer, ten)
}

func printf(s string, arg ...interface{}) {
   if !silent {
      fmt.Printf(s, arg...)
   }
}

func main() {
   flag.Parse()
   if flag.NArg() > 0 { n,_ = strconv.Atoi( flag.Arg(0) ) }

   var m int // 0 <= m < 10
   for i, k := 0, int64(0); ; {
      d := int64(-1)
      for d < 0 {
         k++
         next_term(k)
         d = extract_digit()
      }

      printf("%c", d+'0')

      i++
      m = i % 10
      if m == 0 {
         printf("\t:%d\n", i)
      }
      if i >= n {
         break
      }
      eliminate_digit(d)
   }

   if m > 0 {
      printf("%s\t:%d\n", "          "[m:10], n)
   }
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 */

package main

import (
   "fmt"
   "io/ioutil"
   "os"
   "runtime"
   "regexp"
)

var variants = []string{
   "agggtaaa|tttaccct",
   "[cgt]gggtaaa|tttaccc[acg]",
   "a[act]ggtaaa|tttacc[agt]t",
   "ag[act]gtaaa|tttac[agt]ct",
   "agg[act]taaa|ttta[agt]cct",
   "aggg[acg]aaa|ttt[cgt]ccct",
   "agggt[cgt]aa|tt[acg]accct",
   "agggta[cgt]a|t[acg]taccct",
   "agggtaa[cgt]|[acg]ttaccct",
}

type Subst struct {
   pat, repl string
}

var substs = []Subst{
   Subst{"B", "(c|g|t)"},
   Subst{"D", "(a|g|t)"},
   Subst{"H", "(a|c|t)"},
   Subst{"K", "(g|t)"},
   Subst{"M", "(a|c)"},
   Subst{"N", "(a|c|g|t)"},
   Subst{"R", "(a|g)"},
   Subst{"S", "(c|g)"},
   Subst{"V", "(a|c|g)"},
   Subst{"W", "(a|t)"},
   Subst{"Y", "(c|t)"},
}

func countMatches(pat string, bytes []byte) int {
   re := regexp.MustCompile(pat)
   n := 0
   for {
      e := re.FindIndex(bytes)
      if e == nil {
         break
      }
      n++
      bytes = bytes[e[1]:]
   }
   return n
}

func main() {
   runtime.GOMAXPROCS(4)
   bytes, err := ioutil.ReadFile("/dev/stdin")
   if err != nil {
      fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
      os.Exit(2)
   }
   ilen := len(bytes)
   // Delete the comment lines and newlines
   bytes = regexp.MustCompile("(>[^\n]+)?\n").ReplaceAll(bytes, []byte{})
   clen := len(bytes)

   mresults := make([]chan int, len(variants))
   for i, s := range variants {
      ch := make(chan int)
      mresults[i] = ch
      go func(ss string) {
         ch <- countMatches(ss, bytes)
      }(s)
   }

   lenresult := make(chan int)
   bb := bytes
   go func() {
      for _, sub := range substs {
         bb = regexp.MustCompile(sub.pat).ReplaceAll(bb, []byte(sub.repl))
      }
      lenresult <- len(bb)
   }()

   for i, s := range variants {
      fmt.Printf("%s %d\n", s, <-mresults[i])
   }
   fmt.Printf("\n%d\n%d\n%d\n", ilen, clen, <-lenresult)
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 */

package main

import (
   "fmt"
   "io/ioutil"
   "os"
   "regexp"
)

var variants = []string{
   "agggtaaa|tttaccct",
   "[cgt]gggtaaa|tttaccc[acg]",
   "a[act]ggtaaa|tttacc[agt]t",
   "ag[act]gtaaa|tttac[agt]ct",
   "agg[act]taaa|ttta[agt]cct",
   "aggg[acg]aaa|ttt[cgt]ccct",
   "agggt[cgt]aa|tt[acg]accct",
   "agggta[cgt]a|t[acg]taccct",
   "agggtaa[cgt]|[acg]ttaccct",
}

type Subst struct {
   pat, repl string
}

var substs = []Subst{
   Subst{"B", "(c|g|t)"},
   Subst{"D", "(a|g|t)"},
   Subst{"H", "(a|c|t)"},
   Subst{"K", "(g|t)"},
   Subst{"M", "(a|c)"},
   Subst{"N", "(a|c|g|t)"},
   Subst{"R", "(a|g)"},
   Subst{"S", "(c|g)"},
   Subst{"V", "(a|c|g)"},
   Subst{"W", "(a|t)"},
   Subst{"Y", "(c|t)"},
}

func countMatches(pat string, bytes []byte) int {
   re := regexp.MustCompile(pat)
   n := 0
   for {
      e := re.FindIndex(bytes)
      if len(e) == 0 {
         break
      }
      n++
      bytes = bytes[e[1]:]
   }
   return n
}

func main() {
   bytes, err := ioutil.ReadFile("/dev/stdin")
   if err != nil {
      fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
      os.Exit(2)
   }
   ilen := len(bytes)
   // Delete the comment lines and newlines
   bytes = regexp.MustCompile("(>[^\n]+)?\n").ReplaceAll(bytes, []byte{})
   clen := len(bytes)
   for _, s := range variants {
      fmt.Printf("%s %d\n", s, countMatches(s, bytes))
   }
   for _, sub := range substs {
      bytes = regexp.MustCompile(sub.pat).ReplaceAll(bytes, []byte(sub.repl))
   }
   fmt.Printf("\n%d\n%d\n%d\n", ilen, clen, len(bytes))
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 */

package main

import (
   "fmt"
   "io/ioutil"
   "os"
   "regexp"
)

var variants = []string{
   "agggtaaa|tttaccct",
   "[cgt]gggtaaa|tttaccc[acg]",
   "a[act]ggtaaa|tttacc[agt]t",
   "ag[act]gtaaa|tttac[agt]ct",
   "agg[act]taaa|ttta[agt]cct",
   "aggg[acg]aaa|ttt[cgt]ccct",
   "agggt[cgt]aa|tt[acg]accct",
   "agggta[cgt]a|t[acg]taccct",
   "agggtaa[cgt]|[acg]ttaccct",
}

type Subst struct {
   pat, repl string
}

var substs = []Subst{
   Subst{"B", "(c|g|t)"},
   Subst{"D", "(a|g|t)"},
   Subst{"H", "(a|c|t)"},
   Subst{"K", "(g|t)"},
   Subst{"M", "(a|c)"},
   Subst{"N", "(a|c|g|t)"},
   Subst{"R", "(a|g)"},
   Subst{"S", "(c|g)"},
   Subst{"V", "(a|c|g)"},
   Subst{"W", "(a|t)"},
   Subst{"Y", "(c|t)"},
}

func countMatches(pat string, bytes []byte) int {
   re := regexp.MustCompile(pat)
   n := 0
   for {
      e := re.FindIndex(bytes)
      if len(e) == 0 {
         break
      }
      n++
      bytes = bytes[e[1]:]
   }
   return n
}

func main() {
   bytes, err := ioutil.ReadAll(os.Stdin)
   if err != nil {
      fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
      os.Exit(2)
   }
   ilen := len(bytes)
   // Delete the comment lines and newlines
   bytes = regexp.MustCompile("(>[^\n]+)?\n").ReplaceAll(bytes, []byte{})
   clen := len(bytes)
   for _, s := range variants {
      fmt.Printf("%s %d\n", s, countMatches(s, bytes))
   }
   for _, sub := range substs {
      bytes = regexp.MustCompile(sub.pat).ReplaceAll(bytes, []byte(sub.repl))
   }
   fmt.Printf("\n%d\n%d\n%d\n", ilen, clen, len(bytes))
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 */

package main

import (
   "fmt"
   "io/ioutil"
   "os"
   "runtime"
   "regexp"
)

var variants = []string{
   "agggtaaa|tttaccct",
   "[cgt]gggtaaa|tttaccc[acg]",
   "a[act]ggtaaa|tttacc[agt]t",
   "ag[act]gtaaa|tttac[agt]ct",
   "agg[act]taaa|ttta[agt]cct",
   "aggg[acg]aaa|ttt[cgt]ccct",
   "agggt[cgt]aa|tt[acg]accct",
   "agggta[cgt]a|t[acg]taccct",
   "agggtaa[cgt]|[acg]ttaccct",
}

type Subst struct {
   pat, repl string
}

var substs = []Subst{
   Subst{"B", "(c|g|t)"},
   Subst{"D", "(a|g|t)"},
   Subst{"H", "(a|c|t)"},
   Subst{"K", "(g|t)"},
   Subst{"M", "(a|c)"},
   Subst{"N", "(a|c|g|t)"},
   Subst{"R", "(a|g)"},
   Subst{"S", "(c|g)"},
   Subst{"V", "(a|c|g)"},
   Subst{"W", "(a|t)"},
   Subst{"Y", "(c|t)"},
}

func countMatches(pat string, bytes []byte) int {
   re := regexp.MustCompile(pat)
   n := 0
   for {
      e := re.FindIndex(bytes)
      if e == nil {
         break
      }
      n++
      bytes = bytes[e[1]:]
   }
   return n
}

func main() {
   runtime.GOMAXPROCS(4)
   bytes, err := ioutil.ReadAll(os.Stdin)
   if err != nil {
      fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
      os.Exit(2)
   }
   ilen := len(bytes)
   // Delete the comment lines and newlines
   bytes = regexp.MustCompile("(>[^\n]+)?\n").ReplaceAll(bytes, []byte{})
   clen := len(bytes)

   mresults := make([]chan int, len(variants))
   for i, s := range variants {
      ch := make(chan int)
      mresults[i] = ch
      go func(ss string) {
         ch <- countMatches(ss, bytes)
      }(s)
   }

   lenresult := make(chan int)
   bb := bytes
   go func() {
      for _, sub := range substs {
         bb = regexp.MustCompile(sub.pat).ReplaceAll(bb, []byte(sub.repl))
      }
      lenresult <- len(bb)
   }()

   for i, s := range variants {
      fmt.Printf("%s %d\n", s, <-mresults[i])
   }
   fmt.Printf("\n%d\n%d\n%d\n", ilen, clen, <-lenresult)
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by K P anonymous
 */

package main

import (
   "bufio"
   "os"
)

const lineSize = 60

var complement = [256]uint8{
   'A': 'T', 'a': 'T',
   'C': 'G', 'c': 'G',
   'G': 'C', 'g': 'C',
   'T': 'A', 't': 'A',
   'U': 'A', 'u': 'A',
   'M': 'K', 'm': 'K',
   'R': 'Y', 'r': 'Y',
   'W': 'W', 'w': 'W',
   'S': 'S', 's': 'S',
   'Y': 'R', 'y': 'R',
   'K': 'M', 'k': 'M',
   'V': 'B', 'v': 'B',
   'H': 'D', 'h': 'D',
   'D': 'H', 'd': 'H',
   'B': 'V', 'b': 'V',
   'N': 'N', 'n': 'N',
}

func main() {
   in, _ := bufio.NewReaderSize(os.Stdin, 1<<18)
   buf := make([]byte, 1<<20)
   line, err := in.ReadSlice('\n')
   for err == nil {
      os.Stdout.Write(line)

      // Accumulate reversed complement in buf[w:]
      nchar := 0
      w := len(buf)
      for {
         line, err = in.ReadSlice('\n')
         if err != nil || line[0] == '>' {
            break
         }
         line = line[0 : len(line)-1]
         nchar += len(line)
         if len(line)+nchar/lineSize+128 >= w {
            nbuf := make([]byte, len(buf)*5)
            copy(nbuf[len(nbuf)-len(buf):], buf)
            w += len(nbuf) - len(buf)
            buf = nbuf
         }

         for i, c := range line {
            buf[w-i-1] = complement[c]
         }
         w -= len(line)
      }

      // Copy down to beginning of buffer, inserting newlines.
      // The loop left room for the newlines and 128 bytes of padding.
      i := 0
      for j := w; j < len(buf); j += lineSize {
         i += copy(buf[i:i+lineSize], buf[j:])
         buf[i] = '\n'
         i++
      }
      os.Stdout.Write(buf[0:i])
   }
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by K P anonymous
 */

package main

import (
   "flag"
   "fmt"
   "math"
   "runtime"
   "strconv"
)

var n = 0    // var n = flag.Int("n", 2000, "count")
var nCPU = 4 // var nCPU = flag.Int("ncpu", 4, "number of cpus")

type Vec []float64

func (v Vec) Times(ii, n int, u Vec, c chan int) {
   ul := len(u)
   for i := ii; i < n; i++ {
      var vi float64
      for j := 0; j < ul; j++ {
         vi += u[j] / float64(((i+j)*(i+j+1)/2 + i + 1))
      }
      v[i] = vi
   }
   c <- 1
}

func (v Vec) TimesTransp(ii, n int, u Vec, c chan int) {
   ul := len(u)
   for i := ii; i < n; i++ {
      var vi float64
      for j := 0; j < ul; j++ {
         vi += u[j] / float64(((j+i)*(j+i+1)/2 + j + 1))
      }
      v[i] = vi
   }
   c <- 1
}

func wait(c chan int) {
   for i := 0; i < nCPU; i++ {
      <-c
   }
}

func (v Vec) ATimesTransp(u Vec) {
   x := make(Vec, len(u))
   c := make(chan int, nCPU)
   for i := 0; i < nCPU; i++ {
      go x.Times(i*len(v)/nCPU, (i+1)*len(v)/nCPU, u, c)
   }
   wait(c)
   for i := 0; i < nCPU; i++ {
      go v.TimesTransp(i*len(v)/nCPU, (i+1)*len(v)/nCPU, x, c)
   }
   wait(c)
}


func main() {
   flag.Parse()
   if flag.NArg() > 0 {
      n, _ = strconv.Atoi(flag.Arg(0))
   }
   runtime.GOMAXPROCS(nCPU)

   u := make(Vec, n)
   for i := range u {
      u[i] = 1
   }
   v := make(Vec, n)
   for i := 0; i < 10; i++ {
      v.ATimesTransp(u)
      u.ATimesTransp(v)
   }
   var vBv, vv float64
   for i, vi := range v {
      vBv += u[i] * vi
      vv += vi * vi
   }
   fmt.Printf("%0.9f\n", math.Sqrt(vBv/vv))
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Maxim Razin
*/

package main

import (
    "fmt"
    "os"
    "strconv"
)

type Token int

func worker(id int, in <-chan Token, out chan<- Token, res chan<- int) {
    for {
        t := <- in
        if t==0 {
            res <- id
        } else {
            out <- t-1
        }
    }
}

const NThreads = 503

func main() {
    n := 1000
    if len(os.Args)>1 { n,_ = strconv.Atoi(os.Args[1]) }

    var channels [NThreads] chan Token
    for i:=0; i<NThreads; i++ { channels[i] = make(chan Token) }
    res := make(chan int)

    for i:=0; i<NThreads; i++ {
        go worker(i+1, channels[i], channels[(i+1)%NThreads], res)
    }

    channels[0] <- Token(n)
    r := <- res
    fmt.Printf("%d\n",r)
    os.Exit(0)
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Maxim Razin
*/

package main

import (
    "fmt"
    "os"
    "runtime"
    "strconv"
)

type Token int

func worker(id int, in <-chan Token, out chan<- Token, res chan<- int) {
    for {
        t := <- in
        if t==0 {
            res <- id
        } else {
            out <- t-1
        }
    }
}

const NThreads = 503

func main() {
    runtime.GOMAXPROCS(4)
    n := 1000
    if len(os.Args)>1 { n,_ = strconv.Atoi(os.Args[1]) }

    var channels [NThreads] chan Token
    for i:=0; i<NThreads; i++ { channels[i] = make(chan Token) }
    res := make(chan int)

    for i:=0; i<NThreads; i++ {
        go worker(i+1, channels[i], channels[(i+1)%NThreads], res)
    }

    channels[0] <- Token(n)
    r := <- res
    fmt.Printf("%d\n",r)
    os.Exit(0)
}
