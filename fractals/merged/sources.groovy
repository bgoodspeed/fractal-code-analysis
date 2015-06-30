/*
The Computer Language Shootout
http://shootout.alioth.debian.org/

contributed by Jochen Hinrichsen
*/

def A(x, y) {
   // TODO: return statement is stated optional, but does not work w/o
   if (x == 0) return y+1
   if (y == 0) return A(x-1, 1)
   return A(x-1, A(x, y-1))
}

def n = this.args[0].toInteger()
def result = A(3, n)
println("Ack(3,${n}): ${result}")

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Jochen Hinrichsen
   modified by Marko Kocic
*/

final class TreeNode {
   private final left, right, item

   TreeNode(item){
      this.item = item
   }

   private static bottomUpTree(item, depth) {
      if (depth>0) {
      return new TreeNode(
           bottomUpTree(2*item-1, depth-1)
         , bottomUpTree(2*item, depth-1)
         , item
         )
      } else {
      	return new TreeNode(item)
      }
   }

   TreeNode(left, right, item){
      this.left = left
      this.right = right
      this.item = item
   }

   private itemCheck(){
      // if necessary deallocate here
      if (left==null) return item
      else return item + left.itemCheck() - right.itemCheck()
   }
}


def n = (args.length == 0) ? 10 : args[0].toInteger()
def minDepth = 4
def maxDepth = [ minDepth + 2, n].max()
def stretchDepth = maxDepth + 1

def check = (TreeNode.bottomUpTree(0,stretchDepth)).itemCheck()
println "stretch tree of depth ${stretchDepth}\t check: ${check}"

def longLivedTree = TreeNode.bottomUpTree(0,maxDepth)

def depth=minDepth
while (depth<=maxDepth) {
   def iterations = 1 << (maxDepth - depth + minDepth)
   check = 0
   for (i in 1..iterations) {
      check += (TreeNode.bottomUpTree(i,depth)).itemCheck()
      check += (TreeNode.bottomUpTree(-i,depth)).itemCheck()
   }

   println "${iterations*2}\t trees of depth ${depth}\t check: ${check}"
   depth+=2
}

println "long lived tree of depth ${maxDepth}\t check: ${longLivedTree.itemCheck()}"
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Brian Schlining
*/

def n = 7
if (args.length > 0) {
    n = Integer.parseInt(args[0])
}
println("Pfannkuchen(" + n + ") = " + fannkuch(n))

    
def fannkuch(int n) {
    int check = 0
    int[] perm = new int[n]
    int[] perm1 = new int[n]
    int[] count = new int[n]
    int[] maxPerm = new int[n]
    int maxFlipsCount = 0
    int m = n - 1

    for (i in 0..<n) {
        perm1[i] = i
    }
    int r = n

    while (true) {
        // write-out the first 30 permutations
        if (check < 30){
            for (i in 0..<n) {
                print(perm1[i] + 1)
            }
            print("\n")
            check++
        }

        while (r != 1) { 
            count[r - 1] = r
            r-- 
        }
        if (!(perm1[0] == 0 || perm1[m] == m)) {
            for (i in 0..<n) {
                perm[i] = perm1[i]
            }
            
            int flipsCount = 0
            int k

            while (!((k = perm[0]) == 0)) {
                int k2 = (k + 1) >> 1
                for (i in 0..<k2) {
                    int temp = perm[i] 
                    perm[i] = perm[k - i] 
                    perm[k - i] = temp
                }
                flipsCount++
            }

            if (flipsCount > maxFlipsCount) {
                maxFlipsCount = flipsCount
                for (i in 0..<n) {
                    maxPerm[i] = perm1[i]
                }
            }
        }

        while (true) {
            if (r == n) {
                return maxFlipsCount
            }
            int perm0 = perm1[0]
            int i = 0
            while (i < r) {
                int j = i + 1
                perm1[i] = perm1[j]
                i = j
            }
            perm1[r] = perm0

            count[r] = count[r] - 1
            if (count[r] > 0) {
                break
            }
            r++
        }
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   originally written by Jochen Hinrichsen
   fixed for Groovy 1.5 and refactored by Cedric Hurst 
*/

public class fasta {
   static final def NEWLINE = "\n".getBytes()[0]
   
   static final def LINE_LENGTH = 60
   
   // Weighted selection from alphabet
   static final String ALU = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"
   static final byte[] ALUB = ALU.getBytes()
   
   static def frequency(String s, double p) {
      return [p, (byte)s]
   }
   
   static final def IUB = [
           frequency('a', 0.27d),
           frequency('c', 0.12d),
           frequency('g', 0.12d),
           frequency('t', 0.27d),
   
           frequency('B', 0.02d),
           frequency('D', 0.02d),
           frequency('H', 0.02d),
           frequency('K', 0.02d),
           frequency('M', 0.02d),
           frequency('N', 0.02d),
           frequency('R', 0.02d),
           frequency('S', 0.02d),
           frequency('V', 0.02d),
           frequency('W', 0.02d),
           frequency('Y', 0.02d)
   ]
   
   static final def HomoSapiens = [
           frequency('a', 0.3029549426680d),
           frequency('c', 0.1979883004921d),
           frequency('g', 0.1975473066391d),
           frequency('t', 0.3015094502008d)
   ]
   
   static final def BUFFER_SIZE = 8192
   
   // pseudo-random number generator
   static def IM = 139968
   static def IA = 3877
   static def IC = 29573
   static def last = 42
   
   static def random(def max) {
       last = (last * IA + IC) % IM
       max * last / IM
   }
   
   static def makeCumulative(a) {
        def cp = 0.0d
        for (i in 0..<a.size()) {
            cp += a[i][0]
            a[i][0] = cp
        }
   }
   
   // select a random frequency.c
   def selectRandom() {
       def len = a.size()
       def r = random(1.0d)
       for (i in 0..<len)
           if (r < a[i][0])
               return a[i][1]
       return a[len - 1][1]
   }
   
   String id, desc
   int n, index
   PrintStream writer
   byte[] bbuffer
   def a
   
   public fasta()
   {
      this.bbuffer = new byte[BUFFER_SIZE]
      this.index = 0
   }
   
   def makeRepeat() {
      byte[] bbuffer = new byte[BUFFER_SIZE]
      index = 0
      int m = 0
      int k = 0
      int kn = a.length()
      writer << ">" + id + " " + desc + "\n"
      while (n > 0) {
            m = (n < LINE_LENGTH) ? n : LINE_LENGTH
            if (BUFFER_SIZE - index < m){
               writer.write(bbuffer, 0, index)
               index = 0
            }
            for (i in 0..<m) {
               if (k == kn) k = 0
               bbuffer[index++] = ALUB[k++]
            }
            bbuffer[index++] = NEWLINE
            n -= LINE_LENGTH
      }
      if(index != 0) writer.write(bbuffer, 0, index)
   }
   
   def makeRandom() {
      index = 0
      int m = 0
      writer << ">" + id + " " + desc + "\n"
      while (n > 0) {
            m = (n < LINE_LENGTH) ? n : LINE_LENGTH
            if (BUFFER_SIZE - index <= m){
                  writer.write(bbuffer, 0, index)
                  index = 0
            }
            for (i in 0..<m) {
               bbuffer[index++] = selectRandom()
            }
            bbuffer[index++] = NEWLINE
            n -= LINE_LENGTH
      }
      if(index != 0) writer.write(bbuffer, 0, index)
   }
   
   public static void main(args)
   {
      makeCumulative(HomoSapiens)
      makeCumulative(IUB)
      
      def n = !args ? 2500000 : args[0].toInteger()

      new fasta(id: "ONE", desc: "Homo sapiens alu", 
                 a: ALU, n: n * 2, writer: System.out).makeRepeat()

      new fasta(id: "TWO", desc: "IUB ambiguity codes",
                 a: IUB, n: n * 3, writer: System.out).makeRandom()

      new fasta(id: "THREE", desc: "Homo sapiens frequency", 
                 a: HomoSapiens, n: n * 5, writer: System.out).makeRandom()
   }
}
/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/
 
	contributed by Jochen Hinrichsen
*/

def n = (args.length == 0) ? 10**7 : args[0].toInteger()

def partialSum = 0d
for (i in 1..n) {
    partialSum += 1.0d / i
}
def f = new java.text.DecimalFormat("#." + "0" * 9)
println f.format(partialSum)

// EOF
/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/
    public static final long IM = 139968
    public static final long IA =   3877
    public static final long IC =  29573

    public static long last = 42
    def gen_random(double max) {
		max * (last = (last * IA + IC) % IM) / IM
    }

    def heapsort(int n, double[] ra) {
		int l, j, ir, i
		double rra

		l = (n >> 1) + 1
		ir = n
		while (true) {
		    if (l > 1) {
				rra = ra[--l]
		    } else {
				rra = ra[ir]
				ra[ir] = ra[1]
				if (--ir == 1) {
				    ra[1] = rra
				    return
				}
		    }
		    i = l
		    j = l << 1
		    while (j <= ir) {
				if (j < ir && ra[j] < ra[j+1]) { ++j }
				if (rra < ra[j]) {
				    ra[i] = ra[j]
				    j += (i = j)
				} else {
				    j = ir + 1
				}
		    }
		    ra[i] = rra
		}
    }

def N = (args.length == 0) ? 1000 : args[0].toInteger()
def nf = java.text.NumberFormat.getInstance()
nf.setMaximumFractionDigits(10)
nf.setMinimumFractionDigits(10)
nf.setGroupingUsed(false)
double []ary = (double[]) java.lang.reflect.Array.newInstance(double.class, N+1)
for (i in 0..<N) {
    ary[i] = gen_random(1)
}
heapsort(N, ary)
println nf.format(ary[N])

// EOF

/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

println "hello world"

// EOF

/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

double Cr, Ci, Tr, Ti, Zr=0, Zi=0, limit_sq = 4.0
int res, i=0, x=0, y=0, pos=0, acc=1, iter = 50
res = (args.length >= 1) ? Integer.parseInt(args[0], 10) : 200
int max = (res * res) >>> 3
def pbm_data = new byte[ max ]
String pbm_header = new String("P4" + ((char) 012) + res + " " + res + ((char) 012))

System.out.write(pbm_header.getBytes(), 0, pbm_header.length())

// for ( ; pos < max; x%=res, Zr=Zi=i=0) {
while (pos < max) {
	Cr = (2*((double)x++)/res - 1.5);
	Ci=(2*((double)y)/res - 1)

	// for(acc<<=1; (acc&1)==0 && i++ < iter; acc |= Zr*Zr+Zi*Zi > limit_sq ? 1 : 0) {
	acc<<=1
	while (((acc&1)==0) && (i++ < iter)) {
		Tr = Zr*Zr - Zi*Zi + Cr
		Ti = 2*Zr*Zi + Ci
		Zr = Tr
		Zi = Ti

		// println "Zr^2 + Zi^2 = ${Zr*Zr+Zi*Zi}"
		acc |= (Zr*Zr+Zi*Zi > limit_sq) ? 1 : 0
		// println "acc = ${acc}"
	}
			
	if (x==res) {
		y++
		if (acc<256) acc <<= (8-res%8)
	}
	if (acc>255) { 
		pbm_data [ pos++ ] = (byte) (acc^=255)
		acc = 1
	}

	x%=res
	Zr=Zi=i=0

	// println "acc = ${acc}"
	// println "pos = ${pos}"
	// println "--------------------------------------"
}

System.out.write( pbm_data, 0, pos);

// EOF

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Brian Schlining
*/

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;

def NUMBER_OF_THREADS = 500
def QUEUE_BUFFER_RATIO = Integer.getInteger("bufferPct", 100) / 100.0

int messagesCount = Integer.parseInt(args[0])
int queueSize = messagesCount * QUEUE_BUFFER_RATIO

MyMessage first = new MyMessage(null, queueSize);
MyMessage last = first;
for (i in 0..<(NUMBER_OF_THREADS - 1)) {
    last = new MyMessage(last, queueSize)
    Thread thread = new Thread(last, "Worker-" + i)
    thread.setDaemon(true)
    thread.start()
}

for (j in 0..<messagesCount) {
    last.queue.put(1);
}

int sum = 0
while (sum < NUMBER_OF_THREADS * messagesCount) {
    sum += first.queue.take();
}

println(sum);

class MyMessage implements Runnable {
    

    final BlockingQueue queue
    final def next

    MyMessage(MyMessage next, int queueSize) {
        this.queue = new ArrayBlockingQueue(queueSize)
        this.next = next
    }

    void run() {
        if (next==null) { return }
        while (true) { next.queue.put(this.queue.take() + 1) }
    }

}
// The Computer Language Benchmarks Game
// http://shootout.alioth.debian.org
//
// contributed by Pilho Kim
// From Ruby version by Jesse Millikan
// From version ported by Michael Neumann from the C gcc version,
// which was written by Christoph Bauer.

class Planet {
  def static SOLAR_MASS = 4 * Math.PI*Math.PI
  def static DAYS_PER_YEAR = 365.24
  def x, y, z, vx, vy, vz, mass

  public Planet(x, y, z, vx, vy, vz, mass) {
    this.x = x
    this.y = y
    this.z = z
    this.vx = vx * DAYS_PER_YEAR
    this.vy = vy * DAYS_PER_YEAR
    this.vz = vz * DAYS_PER_YEAR
    this.mass = mass * SOLAR_MASS
  }

  def move_from_i(bodies, nbodies, dt, i) {
    def b2, dx, dy, dz, distance, mag
    def b_mass_mag, b2_mass_mag
    while (i < nbodies) {
      /// println "nbodies = $nbodies, i = $i"
      b2 = bodies[i]
      dx = x - b2.x
      dy = y - b2.y
      dz = z - b2.z

      distance = Math.sqrt(dx * dx + dy * dy + dz * dz)
      mag = dt / (distance * distance * distance)
      b_mass_mag = mass * mag
      b2_mass_mag = b2.mass * mag

      vx -= dx * b2_mass_mag
      vy -= dy * b2_mass_mag
      vz -= dz * b2_mass_mag
      b2.vx += dx * b_mass_mag
      b2.vy += dy * b_mass_mag
      b2.vz += dz * b_mass_mag
      i++
    }

    x += dt * vx
    y += dt * vy
    z += dt * vz
  }
}

def energy(bodies) {
  def e = 0.0
  def nbodies = bodies.size

  for (i in 0 ..< nbodies) {
    b = bodies[i]
    e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz)
    for (j in (i + 1) ..< nbodies) {
      b2 = bodies[j]
      dx = b.x - b2.x
      dy = b.y - b2.y
      dz = b.z - b2.z
      distance = Math.sqrt(dx * dx + dy * dy + dz * dz)
      e -= (b.mass * b2.mass) / distance
    }
  }
  return e
}

def offset_momentum(bodies) {
  def px = 0.0
  def py = 0.0
  def pz = 0.0

  for (b in bodies) {
    m = b.mass
    px += b.vx * m
    py += b.vy * m
    pz += b.vz * m
  }

  b = bodies[0]
  b.vx = - px / Planet.SOLAR_MASS
  b.vy = - py / Planet.SOLAR_MASS
  b.vz = - pz / Planet.SOLAR_MASS
}

def BODIES = [
  // sun
  new Planet(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),

  // jupiter
  new Planet(
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03,
    7.69901118419740425e-03,
    -6.90460016972063023e-05,
    9.54791938424326609e-04),

  // saturn
  new Planet(
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03,
    4.99852801234917238e-03,
    2.30417297573763929e-05,
    2.85885980666130812e-04),

  // uranus
  new Planet(
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03,
    2.37847173959480950e-03,
    -2.96589568540237556e-05,
    4.36624404335156298e-05),

  // neptune
  new Planet(
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03,
    +1.62824170038242295e-03,
    -9.51592254519715870e-05,
    5.15138902046611451e-05)
]

n = Integer.parseInt(args[0])
offset_momentum(BODIES)

def fmt = new java.text.DecimalFormat("##0.000000000")
println fmt.format(energy(BODIES))

def nbodies = BODIES.size()
def dt = 0.01
n.times {
  def i = 0
  while (i < nbodies) {
    b = BODIES[i]
    b.move_from_i(BODIES, nbodies, dt, i + 1)
    i++
  }
}

println fmt.format(energy(BODIES))
/* The Computer Language Benchmarks Game 
   http://shootout.alioth.debian.org/

   contributed by Pilho Kim
*/

def countSieve(m, primes) {
      def i, k
      def count = 0

      i = 2
      while (i <= m) { 
          primes[i] = true
          i++
      }

      i = 2
      while (i <= m) { 
         if (primes[i]) {
            k = i + i
            while (k <= m) {
                 primes[k] = false
                 k += i
            }
            count++
         }
         i++
      }
      return count
}

def padNumber(number, fieldLen) {
      def bareNumber = "" + number
      def numSpaces = fieldLen - bareNumber.length()
      def sb = new StringBuffer(" "*numSpaces)
      sb.append(bareNumber)
      return sb.toString()
}

def n = 2
if (args.length > 0) 
    n = args[0].toInteger()
if (n < 2) 
    n = 2

def m = (1<<n)*10000
def flags = new boolean[m+1]

[n, n-1, n-2].each {
    def k = (1<<it)*10000
    def s1 = padNumber(k, 8)
    def s2 = padNumber(countSieve(k,flags), 9)
    println("Primes up to $s1$s2")
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Jochen Hinrichsen

*/

def nsieve(m) {
   def bits = new java.util.BitSet(m)
   bits.set(2, m, true)
   for (i in 2..m) {
      if (bits.get(i)) {
         (i+i..m).step(i) { j ->
            bits.clear(j)
         }
      }
   }
   bits.cardinality()	
}

def run(n) {
   int m = 2**n*10000
   print("Primes up to ${m.toString().padLeft(8)}")
   println(nsieve(m).toString().padLeft(9))
}

def n = args.length == 0 ? 2 : args[0].toInteger()
n = (int) Math.max(n, 2)

run(n)
run(n-1)
run(n-2)

/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

class Toggle {

	public state = false

	public activate() {
		state = !state
		this
	}

	public value() {
		state
	}

}

class NthToggle extends Toggle {

	public counter = 0
	public counterMax = 0

	public activate() {
		assert counter != null
		assert counterMax != null
		assert state != null
		counter ++
		if (counter >= counterMax) {
			state = !state
			counter = 0
		}
		this
	}
}

def n = args.length == 0 ? 1 : args[0].toInteger()

// Create a toggle object and activate 5 times
toggle = new Toggle(state:true)
(1..5).each() {
	println(toggle.activate().value() ? "true" : "false")
}

// Create N toggle objects and activate each 8 times
def toggles = new Toggle[n]
(0..<n).each() {
	toggles[it] = new Toggle(state:true)
}

println ""

(1..8).each() {
	toggles.each() {
		it.activate()
	}
}

def nthToggle = new NthToggle(state:true, counterMax:3)
assert nthToggle.counterMax = 3

(1..8).each() {
	println(nthToggle.activate().value() ? "true" : "false")
	// println(nthToggle.activate())
}

(1..n).each() {
	new NthToggle(state:true, counterMax:3)
}

// EOF

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

def n = Integer.parseInt(args[0])

def twothirds = 2.0/3.0
def a1 = a2 = a3 = a4 = a5 = a6 = a7 = a8 = a9 = 0.0D
def alt = -1.0D
def k = 1.0D

while (k <= n) {
   def k2 = k * k
   def k3 = k2 * k 
   def sk = Math.sin(k)
   def ck = Math.cos(k)
   alt = -alt 

   a1 += twothirds**(k-1.0)
   a2 += 1.0/Math.sqrt(k)
   a3 += 1.0/(k*(k+1.0))
   a4 += 1.0/(k3*sk*sk)
   a5 += 1.0/(k3*ck*ck)
   a6 += 1.0/k
   a7 += 1.0/k2
   a8 += alt/k
   a9 += alt/(2.0*k - 1.0)

   k += 1.0 
}

printf("%.9f\t(2/3)^k\n", a1);
printf("%.9f\tk^-0.5\n", a2);
printf("%.9f\t1/k(k+1)\n", a3);
printf("%.9f\tFlint Hills\n", a4);
printf("%.9f\tCookson Hills\n", a5);
printf("%.9f\tHarmonic\n", a6);
printf("%.9f\tRiemann Zeta\n", a7);
printf("%.9f\tAlternating Harmonic\n", a8);
printf("%.9f\tGregory\n", a9);


/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen

	Trailing 'G' stands for BigInteger (unrestricted size) in groovy
*/

class T {

	public q, r, s, t, k = 0G

	def compose(t2) {
		new T(q: q * t2.q,
			r: q * t2.r + r * t2.t,
			s: s * t2.q + t * t2.s,
			t: s * t2.r + t * t2.t)
	}

	def extract(j) {
		// groovy does not support integer division using /
		(q*j + r).divide(s*j + t)
	}

	def next() {
		k++
		q = k
		r = 4G*k+2G
		s = 0G
		t = 2G*k+1G

		this
	}
}

class Digits {

    def x = new T(q:0G, r:0G, s:0G, t:0G)
    def z = new T(q:1G, r:0G, s:0G, t:1G)

	private consume(T t) {
		z.compose(t)
	}

	private digit() {
		z.extract(3G)
	}

	private isSafe(digit) {
		digit == z.extract(4G)
	}

	def next() {
		def y = digit()
		if (isSafe(y)) {
			z = produce(y)
			return y
		} else {
			z = consume(x.next())
			return next()
		}
	}

	private produce(y) {
        new T(q:10G, r:-10G*y, s:0G, t:1G).compose(z)
	}
}

def L = 10
def n = (args.length == 0) ? 10 : args[0].toInteger()
def digits = new Digits()
def j = 0
while (n > 0){
	if (n >= L) {
		for (i in 0..<L) print digits.next()
		j += L
	} else {
		for (i in 0..<n) print digits.next()
		print(" " * (L-n-1))
		j += n;
	}
	print("\t:");
	println j
	n -= L;
}

// EOF

/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

def IM = 139968
def IA = 3877
def IC = 29573
def last = 42D

def gen_random(Double max) {
	last = (last * IA + IC) % IM
	max * last / IM
}

def n = (args.length == 0 ? 1 : args[0].toInteger()) - 1
while (n--) {
	gen_random(100D)
}

// TODO groovy does not support varargs
// def s = new java.io.PrintStream(System.out)
// s.printf("%.9f", gen_random(100D))

def nf = java.text.NumberFormat.getInstance()
nf.setMaximumFractionDigits(9)
nf.setMinimumFractionDigits(9)
nf.setGroupingUsed(false)
println nf.format(gen_random(100D))

// EOF

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Anthony Borla
   modified by Isaac Gouy
*/

def ack(x, y) {
  if (x == 0) return y+1
  if (y == 0) return ack(x-1,1)
  return ack(x-1, ack(x,y-1))
}

def fib(int n){
  if (n < 2) return 1
  return fib(n-2) + fib(n-1)
}

def fib(double n){
  if (n < 2.0D) return 1.0D
  return fib(n -2.0D) + fib(n-1.0D)
}


def tak(int x, int y, int z) {
  if (y < x) return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y))
  return z
}

def tak(double x, double y, double z) {
  if (y < x) return tak(tak(x-1.0D,y,z), tak(y-1.0D,z,x), tak(z-1.0D,x,y))
  return z
}


def n = Integer.parseInt(args[0])

printf("Ack(3,%d): %d\n", n, ack(3,n))
printf("Fib(%.1f): %.1f\n", 27.0D + n, fib(27.0D+n))
n -= 1
printf("Tak(%d,%d,%d): %d\n", n * 3, n * 2, n, tak(n*3,n*2,n))
printf("Fib(3): %d\n", fib(3)) 
printf("Tak(3.0,2.0,1.0): %.1f\n", tak(3.0D,2.0D,1.0D))

// The Computer Language Benchmark Games
// http://shootout.alioth.debian.org/
//
// contributed by James Durbin
// based very closely on Ruby version by jose fco. gonzalez


def seq = new StringBuffer();   
seq = System.in.readLines().join("\n")+"\n";

initialLength = seq.length();
seq = (seq =~">.*\n|\n").replaceAll("");
codeLength = seq.length();

[
   "agggtaaa|tttaccct" ,
   "[cgt]gggtaaa|tttaccc[acg]", 
   "a[act]ggtaaa|tttacc[agt]t",
   "ag[act]gtaaa|tttac[agt]ct", 
   "agg[act]taaa|ttta[agt]cct", 
   "aggg[acg]aaa|ttt[cgt]ccct",
   "agggt[cgt]aa|tt[acg]accct", 
   "agggta[cgt]a|t[acg]taccct", 
   "agggtaa[cgt]|[acg]ttaccct"
].each{
   println "$it "+(seq =~it).getCount();
};


[
   'B':'(c|g|t)', 'D':'(a|g|t)', 'H':'(a|c|t)', 'K':'(g|t)',
   'M':'(a|c)', 'N':'(a|c|g|t)', 'R':'(a|g)', 'S':'(c|t)',
   'V':'(a|c|g)', 'W':'(a|t)', 'Y':'(c|t)'
].each { f,r -> 
   seq = (seq =~ f).replaceAll(r);
}

println "";
println initialLength;
println codeLength;
println seq.length();


#!/bin/env groovy
/*
	$Id: regexmatch.groovy,v 1.2 2005-09-23 15:11:35 igouy-guest Exp $

	The Great Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

def	pattern = "(^|^\\D*[^\\(\\d])"				+ // must be preceeded by non-digit
            "((\\(\\d\\d\\d\\))|(\\d\\d\\d))"	+ // match 2: Area Code inner match 3: area with perens,
            									  // inner match 4: without perens
            "[ ]"								+ // area code followed by one space
            "(\\d\\d\\d)"						+ //match 5: prefix of 3 digits
            "[ -]"								+ // prefix followed by space or dash
            "(\\d\\d\\d\\d)"					+ // match 6: last 4 digits
            "(\\D.*|\$)"						  // followed by non numeric chars

def N = (args.length == 0 ? 10 : args[0].toInteger())
def lines = System.in.readLines()

for (i in 1..N) { 
	def count = 0
	lines.each() {
		def matcher = it =~ pattern
		if (matcher.matches() && (i == 1)) {
			if (matcher.group(3) == null) {
				println "${++count}: (${matcher.group(4)}) ${matcher.group(5)}-${matcher.group(6)}"
			} else {
				println "${++count}: ${matcher.group(3)} ${matcher.group(5)}-${matcher.group(6)}"
			}
		}
	}
}

// EOF

/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by James Durbin 
 * slightly modified from Java version from
 * Anthony Donnefort and Razii
 */

import java.io.*;


class ReversibleByteArray extends java.io.ByteArrayOutputStream {

  static final byte[] cmp = new byte[128];
  static{
    for (int i = 0; i < cmp.length; i++) cmp[i] = (byte) i;

    cmp[(byte)'t'] = cmp[(byte)'T'] = (byte)'A';
    cmp[(byte)'a'] = cmp[(byte)'A'] = (byte)'T';
    cmp[(byte)'g'] = cmp[(byte)'G'] = (byte)'C';
    cmp[(byte)'c'] = cmp[(byte)'C'] = (byte)'G';
    cmp[(byte)'v'] = cmp[(byte)'V'] = (byte)'B';
    cmp[(byte)'h'] = cmp[(byte)'H'] = (byte)'D';
    cmp[(byte)'r'] = cmp[(byte)'R'] = (byte)'Y';
    cmp[(byte)'m'] = cmp[(byte)'M'] = (byte)'K';
    cmp[(byte)'y'] = cmp[(byte)'Y'] = (byte)'R';
    cmp[(byte)'k'] = cmp[(byte)'K'] = (byte)'M';
    cmp[(byte)'b'] = cmp[(byte)'B'] = (byte)'V';
    cmp[(byte)'d'] = cmp[(byte)'D'] = (byte)'H';
    cmp[(byte)'u'] = cmp[(byte)'U'] = (byte)'A';
  }

   void reverse() throws Exception {
      if (count > 0) {
         int begin = 0, end = count - 1;
         while (buf[begin++] != '\n');
         while (begin <= end) {
            if (buf[begin] == '\n') begin++;
            if (buf[end] == '\n') end--;
            if (begin <= end) {
               byte tmp = buf[begin];
               buf[begin++] = cmp[buf[end]];
               buf[end--] = cmp[tmp];
            }
         }
         System.out.write(buf, 0, count);
      }
   }
}


byte[] line = new byte[82];
int read;
ReversibleByteArray buf = new ReversibleByteArray();
while ((read = System.in.read(line)) != -1) {
   int i = 0, last = 0;
   while (i < read) {
      if (line[i] == '>') {
         buf.write(line, last, i - last);
         buf.reverse();
         buf.reset();
         last = i;
      }
      i++;
   }
   buf.write(line, last, read - last);
}
buf.reverse();

/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

def approximate(n) {
    // create unit vector
	def u = [1.0D] * n

    // 20 steps of the power method
	def v = [0.0D] * n

    for (i in 1..10) {
        MultiplyAtAv(n,u,v)
        MultiplyAtAv(n,v,u)
    }

    // B=AtA         A multiplied by A transposed
    // v.Bv /(v.v)   eigenvalue of v
    double vBv = vv = 0.0D
    for (i in 0..<n) {
        vBv += u[i]*v[i]
        vv  += v[i]*v[i]
    }

    return Math.sqrt(vBv/vv)
}


/* return element i,j of infinite matrix A */
def A(i, j) {
    return (1.0D) / ((i+j)*(i+j+(1.0D))/(2.0D) +i+(1.0D))
}

/* multiply vector v by matrix A */
def MultiplyAv(n, v, Av){
    for (i in 0..<n) {
        Av[i] = 0.0D
        for (j in 0..<n) Av[i] += A(i,j)*v[j]
    }
}

/* multiply vector v by matrix A transposed */
def MultiplyAtv(n, v, Atv){
    for (i in 0..<n) {
        Atv[i] = 0.0D
        for (j in 0..<n) Atv[i] += A(j,i)*v[j]
    }
}

/* multiply vector v by matrix A and then by matrix A transposed */
def MultiplyAtAv(n, v, AtAv){
    double[] u = new double[n]
    MultiplyAv(n, v, u)
    MultiplyAtv(n, u, AtAv)
}

def n = (args.length == 0 ? 100 : args[0].toInteger())
def nf = java.text.NumberFormat.getInstance()
nf.setMaximumFractionDigits(9)
nf.setMinimumFractionDigits(9)
nf.setGroupingUsed(false)
println(nf.format(approximate(n)))

// EOF

/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen

        Assume that for both the dictionary and standard input there is only one word per line. The dictionary is based on /usr/dict/words, but we only use words that consist entirely of lowercase letters. Each program can assume that no line will exceed 128 characters (including newline).

*/

def dict = [:]

new File("spellcheck-dict.txt").eachLine() {
        dict[it] = true
}

System.in.eachLine() {
        if (!dict[it]) println it
}

// EOF
/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

def sum = 0
System.in.eachLine() {
	sum += it.toInteger()
}
println sum

// EOF

/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

def tak(x, y, z) {
	if (y >= x) return z
	return tak(tak(x-1, y, z), tak(y-1, z, x), tak(z-1, x, y))
}

def n = (args.length == 0 ? 7 : args[0].toFloat())
println tak(n*3, n*2, n*1)

// EOF


/*
	The Computer Language Shootout
	http://shootout.alioth.debian.org/

	contributed by Jochen Hinrichsen
*/

class Server implements Runnable {

	public port, reply

	public void run() {
		def replyBuffer = java.nio.ByteBuffer.wrap(reply.getBytes())
		def serverBuffer = java.nio.ByteBuffer.allocateDirect(reply.size())

		def ssc = java.nio.channels.ServerSocketChannel.open()
		ssc.socket().bind(new InetSocketAddress(InetAddress.getLocalHost(), port))
		def socketChannel = ssc.accept()

		while (true) {
   	     	serverBuffer.clear();
        	if (socketChannel.read(serverBuffer) == -1)  break

			socketChannel.write(replyBuffer)
			replyBuffer.rewind()
        }
		socketChannel.close()
	}
}

class Client implements Runnable {

	public N, M, port, request

	public void run() {
		def requestBuffer = java.nio.ByteBuffer.wrap(request.getBytes())
		def replyBuffer = java.nio.ByteBuffer.allocateDirect(64)

		def channel = java.nio.channels.SocketChannel.open()
		channel.connect(new InetSocketAddress(InetAddress.getLocalHost(), port))

        def replies = bytes = 0
        (1..N*M).each() {
			requestBuffer.rewind()
            channel.write(requestBuffer)
			replyBuffer.clear()
            bytes += channel.read(replyBuffer)
            replies++
        }
        channel.close()
        println "replies: ${replies}\tbytes: ${bytes}"
	}
}


def N = (args.length == 0 ? 10 : args[0])
def server = new Server(port:11000, reply: 'x'*64)
new Thread(server).start()

def client = new Client(N:N, M:6400, port:11000, request: 'x'*64)
new Thread(client).start()

// EOF

/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Klaus Friedel
 * converted to Groovy by Danno Ferrin
 */

import java.util.concurrent.locks.LockSupport;

public static class MessageThread extends Thread {
  MessageThread nextThread;
  volatile Integer message;

  public MessageThread(MessageThread nextThread, int name) {
    super(""+name);
    this.nextThread = nextThread;
  }

  public void run() {
    while(true) nextThread.enqueue(dequeue());
  }

  public void enqueue(Integer hopsRemaining) {
    if(hopsRemaining == 0){
      System.out.println(getName());
      System.exit(0);
    }
    // as only one message populates the ring, it's impossible
    // that queue is not empty
    message = hopsRemaining - 1;
    LockSupport.unpark(this); // work waiting...
  }

  private Integer dequeue(){
    while(message == null){
      LockSupport.park();
    }
    Integer msg = message;
    message = null;
    return msg;
  }
}

int THREAD_COUNT = 503;
int hopCount = Integer.parseInt(args[0]);

MessageThread first = null;
MessageThread last = null;
for (int i = THREAD_COUNT; i >= 1 ; i--) {
  first = new MessageThread(first, i);
  if(i == THREAD_COUNT) last = first;
}
// close the ring:
last.nextThread = first;

// start all Threads
MessageThread t = first;
t.start();
t = t.nextThread;
while(t != first) {
  t.start();
  t = t.nextThread;
}
// inject message
first.enqueue(hopCount);
first.join(); // wait for System.exit
﻿#!/bin/env groovy
/*
	$Id: wordfreq.groovy,v 1.3 2005-09-25 20:16:20 igouy-guest Exp $

	The Great Computer Language Shootout
	http://shootout.alioth.debian.org/
 
	contributed by Jochen Hinrichsen
*/

// def dict = [:]
def dict = new TreeMap()

// read input, build dictionary
System.in.eachLine() { line ->
	// split on words
	line.split("\\W").each() { word ->
		def s = word.toLowerCase()
		def entry = dict[s]
		dict[s] = (entry == null) ? 1 : entry+1
	}
}

// default sort() is smallest first
// sort for multiple properties: [ it.value, it.key ]
assert dict != null
assert dict.values() != null
assert (dict.values().sort({ l, r -> r <=> l})) != null
dict.values().sort({ l, r -> r <=> l}).each() { value ->
/*
	assert value != null
    def entry = dict.find() { e ->
        def v = e.getValue()
		assert v != null
        e.getValue() == value
    }
	assert entry != null
*/
    // println "${value.toString().padLeft(8)} ${entry.key}"
    println "${value.toString().padLeft(8)}"
}

// EOF

