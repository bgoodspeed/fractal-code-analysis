#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: ackermann.pike,v 1.1 2004-05-19 18:09:09 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

int Ack(int M, int N) {
  if (M == 0) return( N + 1 );
  if (N == 0) return( Ack(M - 1, 1) );
  return( Ack(M - 1, Ack(M, (N - 1))) );
}

int main(int argc, array(string) argv) {
  int N = (int)argv[-1] || 1;
  write("Ack(3,%d): %d\n", N, Ack(3, N));
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: ackermann.pike-2.pike,v 1.1 2004-11-10 06:09:46 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Marcus Comstedt

// this version uses memoization

mapping _ack = ([]);
int ack( int m, int n )
{
    if(m==0) return n+1;
    [int i, int a] = _ack[m]||({-1,1});
    for(i++; i<=n; i++)
	a = ack(m-1, a);
    _ack[m] = ({n,a});
    return a;
}

void main( int argc, array(string) argv )
{
    int n = (int)argv[-1];
    if( n < 1 )
	n = 1;
    write( "Ack(3,%d): %d\n", n, ack( 3, n ) );
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: ackermann.pike-3.pike,v 1.1 2004-11-10 06:09:46 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Marcus Comstedt

// this version avoids the recursive function altogether!

void main( int argc, array(string) argv )
{
    int n = (int)argv[-1];
    if( n < 1 )
	n = 1;
    write( "Ack(3,%d): %d\n", n, (8<<n)-3 );
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: ary.pike,v 1.2 2004-05-22 07:25:00 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

void main(int argc, array(string) argv)
{
    int n = (int)argv[-1];
    if (n < 1) n = 1;

    array(int) x = allocate(n);
    array(int) y = allocate(n);

    for (int i; i<n; i++) {
	x[i] = i + 1;
    }
    for (int k; k<1000; k++) {
	for (int i=n-1; i>=0; i--) {
	    y[i] += x[i];
	}
    }
    write("%d %d\n", y[0], y[-1]);
}
/* The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 * contributed by  Robert Brandner
 * based on the Java version by Jarkko Miettinen
 */

int main(int argc, array(string) argv) {

	int minDepth = 4;

	int n = argc > 1 ? (int)argv[1] : 10;

	int maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
	int stretchDepth = maxDepth + 1;
	
	write("stretch tree of depth " + stretchDepth + "\t check: " + itemCheck(bottomUpTree(0,stretchDepth)) + "\n");
	
	TreeNode longLivedTree = bottomUpTree(0,maxDepth);

	for (int depth=minDepth; depth<=maxDepth; depth+=2){
		int iterations = 1 << (maxDepth - depth + minDepth);
		int check = 0;
		for (int i=1; i<=iterations; i++){
			check += itemCheck(bottomUpTree(i,depth));
			check += itemCheck(bottomUpTree(-i,depth));
		}
		write((iterations*2) + "\t trees of depth " + depth + "\t check: " + check+"\n");
	}
	write("long lived tree of depth " + maxDepth + "\t check: "+ itemCheck(longLivedTree)+"\n");
}

TreeNode bottomUpTree(int item, int depth) {
	if (depth>0) {
		return TreeNode(item, bottomUpTree(2*item-1, depth-1), bottomUpTree(2*item, depth-1));
	}
	else {
		return TreeNode(item);
	}
}

int itemCheck(TreeNode t) {
	if (t->left==0) {return t->item;}
	else {return t->item + itemCheck(t->left) - itemCheck(t->right);}
}

class TreeNode {
	TreeNode left, right;
	int item;
	
	void create(int it, TreeNode|void l, TreeNode|void r) {
		item = it;
		left = l;
		right = r;
	}
}
/* The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 * Contributed by David Hedbor
 */

enum Color { blue, red, yellow, faded };
enum bool { false, true };
class MeetingPlace(int n)
{
  private Color first, second;
  private bool firstCall = true;
  private bool mustWait = false; 
  private Thread.Condition monitor = Thread.Condition();
  private Thread.Mutex mlock = Thread.Mutex();

  Color OtherCreaturesColor(Color me)
  {
    Thread.MutexKey key = mlock->lock();
    Color other;

    while (mustWait) {
      monitor->wait(key);
    }
    //    write("Me = %d, n = %d\n", me, n);

    if (firstCall)
    {
      if (n-- > 0)
      {
        first = me;
        firstCall = false;

        while (!firstCall) {
          monitor->wait(key);
        }
        mustWait = false;
        other = second;
      } else {
        other = faded;
      }
    } else {
      second = me;
      other = first;
      firstCall = true;
      mustWait = true;
    }
    monitor->broadcast();
    return other;
  }
};

class Creature(MeetingPlace m, Color me)
{
  int creaturesMet;

  void Be() {
    while(me != faded) { MeetOtherCreature(); }
  }

  void MeetOtherCreature()
  {
    Color other = m->OtherCreaturesColor(me);
    if (other == faded) {
      me = other;
    } else {
      creaturesMet++;
      me = Complement(other);
    }
  }
  
  Color Complement(Color other)
  {
    if (me == other) return me;
    switch(me)
    {
    case blue:
      return other == red ? yellow : red;
    case red:
      return other == blue ? yellow : blue;
    case yellow:
      return other == blue ? red : blue;
    default: return me;
    }
  }
};

array(Color) colors = ({ blue, red, yellow, blue });
array(Creature) creatures = allocate(4);
array(Thread.Thread) threads = allocate(4);

int main(int argc, array(string) argv)
{
  if(argc < 2) return 0;
  int n = (int)argv[1];

  MeetingPlace m = MeetingPlace(n);

  for (int i=0; i < sizeof(colors); i++) {
    creatures[i] = Creature(m, colors[i]);
    threads[i] = Thread.Thread(creatures[i]->Be, 0);
  }

  threads->wait();

  int meetings = 0;
  for(int i=0; i < sizeof(creatures); i++) {
    meetings += creatures[i]->creaturesMet;
  }

  write("%d\n", meetings);
  return 0;
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: echo.pike,v 1.1 2004-05-19 18:09:37 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// based on code from: Per Hedbor

#define DATA "Hello there sailor\n"

void echo_server(Stdio.Port p, int n) {
    Stdio.File f = p->accept();
    int tbytes;
    string q;
    while( (q = f->read( 8192,1  )) && strlen( q ) ) {
	tbytes += strlen(q);
	f->write( q );
    }
    write( "server processed %d bytes\n", tbytes );
}

void echo_client(int p, int n) {
    int i;
    Stdio.File f = Stdio.File();

    f->connect( "localhost", p );
    int s = strlen(DATA);
    for (i=0; i<n; i++) {
	f->write( DATA );
	if(  f->read( s ) != DATA ) {
	    werror( "Transfer error at repetition "+i+"\n");
	    _exit( 1 );
	}
    }
    f->close();
    _exit( 0 );
}

/* Fork is not really available in a threaded pike. Thus this hack. It
 * assumes the pike binary can be found in your path, and that you have
 * a /usr/bin/env
 */
void start_client( int p, int n )
{
    Process.create_process( ({ "/usr/bin/env", "pike", __FILE__,
			       (string)p, (string)n }) );
}

void main(int argc, array argv)
{
    if( argc < 3 )
    {
	int n = max((int)argv[-1],1);
	Stdio.Port p = Stdio.Port( 0 );
	int pno = (int)((p->query_address( )/" ")[1]);
	start_client( pno, n );
	echo_server( p, n );
    } else {
	echo_client( (int)argv[1], (int)argv[2] );
    }
    sleep(1);
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: except.pike,v 1.1 2004-05-19 18:09:43 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Per Hedbor

// this version requires Pike 7.1

class HiException( mixed value ) { constant IsHi = 1; }
class LoException( mixed value ) { constant IsLo = 1; }

void some_function( int num )
{
    if( mixed e = catch(  hi_function( num ) ) )
	error( "We shouldn't get here (%s)", describe_error( e ) );
}
  
int HI, LO;

void hi_function(int num)
{
    if( mixed e = catch( lo_function( num ) ) )
	if( e->IsHi )
	    HI++;
	else
	    throw( e );
}
  
void lo_function(int num)
{
    if( mixed e = catch(  blowup(num) ) )
	if( e->IsLo )
	    LO++;
	else
	    throw( e );
}
  
  
void blowup(int num)
{
    if( num & 1 )
	throw( LoException(num) );
    else
	throw( HiException(num) );
}
  
void main(int argc, array argv)
{
    int num = (int)argv[-1];
    if( num < 1 )
	num = 1;
    while(num)
	some_function( num-- );
    write( "Exceptions: HI=%d / LO=%d\n" , HI, LO );
}
/**
 * fannkuch.pike by Robert Brandner
 * heavily based on the Java JDK-client version by Paul Lofte
 */
int main(int argc, array(string) argv) {
	int n = (int)argv[1];
	write("Pfannkuchen(" + n + ") = " + fannkuch(n)+"\n");
}

int fannkuch(int n) {
    int check = 0;
    array(int) perm = allocate(n);
    array(int) perm1 = allocate(n);
    array(int) count = allocate(n);
    array(int) maxPerm = allocate(n);
    int maxFlipsCount = 0;
    int m = n - 1;

    for (int i = 0; i < n; i++) perm1[i] = i;
    int r = n;

    while (1) {
        // write-out the first 30 permutations
        if (check < 30){
          for(int i=0; i<n; i++) write((string)(perm1[i]+1));
          write("\n");
          check++;
        }

        while (r != 1) { count[r - 1] = r; r--; }
        if (!(perm1[0] == 0 || perm1[m] == m)) {
            for (int i = 0; i < n; i++) perm[i] = perm1[i];

            int flipsCount = 0;
            int k;

            while (!((k = perm[0]) == 0)) {
                int k2 = (k + 1) >> 1;
                for (int i = 0; i < k2; i++) {
                    int temp = perm[i]; perm[i] = perm[k - i]; perm[k - i] = temp;
                }
                flipsCount++;
            }

            if (flipsCount > maxFlipsCount) {
                maxFlipsCount = flipsCount;
                for (int i = 0; i < n; i++) maxPerm[i] = perm1[i];
            }
        }

        while (1) {
            if (r == n) return maxFlipsCount;
            int perm0 = perm1[0];
            int i = 0;
            while (i < r) {
                int j = i + 1;
                perm1[i] = perm1[j];
                i = j;
            }
            perm1[r] = perm0;

            count[r] = count[r] - 1;
            if (count[r] > 0) break;
            r++;
        }
    }
}

/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Lance Dillon
   significant speedup [used 'String.Buffer' for I/O] by Anthony Borla
*/

class Frequency {
  string code;
  float percent;

  void create(string c, float p) {
    code=c;
    percent=p;
  }
}

int main(int argc, array argv) {
  string ALU="GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
      "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
      "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
      "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
      "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
      "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
      "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

  array(Frequency) IUB=({ Frequency("a",0.27),
	       Frequency("c",0.12),
	       Frequency("g",0.12),
	       Frequency("t",0.27),
	       Frequency("B",0.02),
	       Frequency("D",0.02),
	       Frequency("H",0.02),
	       Frequency("K",0.02),
	       Frequency("M",0.02),
	       Frequency("N",0.02),
	       Frequency("R",0.02),
	       Frequency("S",0.02),
	       Frequency("V",0.02),
	       Frequency("W",0.02),
	       Frequency("Y",0.02)
  });

  array(Frequency) HomoSapiens=({ Frequency("a",0.3029549426680),
		       Frequency("c",0.1979883004921),
		       Frequency("g",0.1975473066391),
		       Frequency("t",0.3015094502008)
  });

  makeCumulative(HomoSapiens);
  makeCumulative(IUB);

  int n=(int)argv[1];
  makeRepeatFasta("ONE","Homo sapiens alu",ALU,n*2);
  makeRandomFasta("TWO","IUB ambiguity codes",IUB,n*3);
  makeRandomFasta("THREE","Homo sapiens frequency",HomoSapiens,n*5);

}

void makeCumulative(array(Frequency) a) {
  float cp=0.0;
  foreach (a; int ind; Frequency f) {
    cp+=f->percent;
    f->percent=cp;
  }
}

string selectRandom(array(Frequency) a) {
  float r=myrandom(1.0);
  for (int i=0; i<sizeof(a); i++)
    if (r<a[i]->percent)
      return a[i]->code;
  return a[-1]->code;
}

int LineLength=60;

void makeRandomFasta(string id, string desc, array(Frequency) a, int n) {
  int m=0;
  String.Buffer lineout = String.Buffer(); 

  write(">"+id+" "+desc+"\n");

  while (n>0) {
    if (n<LineLength)
      m=n;
    else
      m=LineLength;

    for (int i=0; i<m; i++) lineout->add(selectRandom(a));
    write("%s\n", lineout->get()); n-=LineLength;
  }
}

void makeRepeatFasta(string id, string desc, string alu, int n) {
  int m=0;
  int k=0;
  int kn=sizeof(alu);
  String.Buffer lineout = String.Buffer(); 

  write(">"+id+" "+desc+"\n");

  while (n>0) {
    if (n<LineLength)
      m=n;
    else
      m=LineLength;
    for (int i=0; i<m; i++) {
      if (k==kn)
	k=0;
      lineout->add(sprintf("%c", alu[k]));
      k++;
    }

    write("%s\n", lineout->get()); n-=LineLength;
  }
}
constant IM=139968;
constant IA=3877;
constant IC=29573;
int seed=42;

float myrandom(float max) {
  seed=(seed*IA+IC)%IM;
  return (max*seed/IM);
}

#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: fibo.pike,v 1.3 2005-04-25 19:01:38 igouy-guest Exp $
// http://www.bagley.org/~doug/shootout/

int
fib(int n) {
    if (n < 2) return(1);
    return( fib(n-2) + fib(n-1) );
}

void
main(int argc, array(string) argv) {
    int n = max((int)argv[-1], 1);
    write("%d\n", fib(n));
}

/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Adam Montague
*/

void main(int argc, array(string) argv)
{
	int n = (int)argv[1];
	float sum;
	for (int i = 1; i <= n; i++)
		sum += 1.0 / i;
	write("%.9f\n", sum);
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: hash.pike,v 1.1 2004-05-19 18:09:55 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Martin Nilsson

void main(int argc, array(string) argv)
{
    int i, c = 0;
    int n = (int)argv[-1];
    if (n < 1) n = 1;
      
    mapping(string:int) X = ([]);
    
    for (i=1; i<=n; i++) {
	X[sprintf("%x", i)] = i;
    }
    for (i=n; i>0; i--) {
	if (X[(string)i]) c++;
    }
    write("%d\n", c);
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: hash2.pike,v 1.1 2004-05-19 18:10:02 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

void main(int argc, array(string) argv) {
    int n = (int)argv[-1];
    if (n < 1) n = 1;
      
    mapping(string:int) hash1 = ([]);
    mapping(string:int) hash2 = ([]);
    for (int i=0; i<10000; i++)
	hash1["foo_" + i] = i;
    for (int i=0; i<n; i++) {
	foreach (indices(hash1), string k) {
	    hash2[k] += hash1[k];
	}
    }
    write("%d %d %d %d\n", hash1["foo_1"], hash1["foo_9999"],
	  hash2["foo_1"], hash2["foo_9999"]);
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: heapsort.pike,v 1.1 2004-05-19 18:10:10 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Fredrik Noring

#define IM 139968
#define IA   3877
#define IC  29573

int last = 42;

float
gen_random(float max) { return(max * (last = (last * IA + IC) % IM) / IM); }

void heapsort(int n, array(float) ra) {
    int l, j, ir, i;
    float rra;

    l = (n >> 1) + 1;
    ir = n;
    for (;;) {
        if (l > 1) {
            rra = ra[--l];
        } else {
            rra = ra[ir];
            ra[ir] = ra[1];
            if (--ir == 1) {
                ra[1] = rra;
                return;
            }
        }
        i = l;
        j = l << 1;
        while (j <= ir) {
            if (j < ir && ra[j] < ra[j+1]) { ++j; }
            if (rra < ra[j]) {
                ra[i] = ra[j];
                j += (i = j);
            } else {
                j = ir + 1;
            }
        }
        ra[i] = rra;
    }
}

void main(int argc, array(string) argv) {
    int N = (int)argv[-1] || 1;
    array(float) ary;
    int i;
    
    // create an array of N random floats
    ary = allocate(N+1);
    for (i=1; i<=N; i++) {
        ary[i] = gen_random(1.0);
    }

    heapsort(N, ary);

    write("%.10f\n", ary[N]);
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: hello.pike,v 1.1 2004-05-19 18:10:16 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

int main() { write("hello world\n"); }

/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
  contributed by - Lance Dillon
*/

class KNucleotide {
  string sequence;
  int count=1;

  void create(string s) {
    sequence=s;
  }

  void writeFrequencies(int k) {
    mapping(string:KNucleotide) frequencies=generateFrequencies(k);
    array(KNucleotide) list=values(frequencies);
    list=reverse(Array.sort_array(list,lambda(KNucleotide a, KNucleotide b) {
			if (a->count>b->count)
			  return 1;
			if (a->count<b->count)
			  return -1;
			if (a->sequence>b->sequence)
			  return 1;
			if (a->sequence<b->sequence)
			  return -1;
			return 0; } ));
    int sum=sizeof(sequence)-k+1;
    foreach (list; int ind; object kn) {
      predef::write("%s %03.3f\n",kn->sequence,((float)kn->count/(float)sum*100.0));
    }
    predef::write("\n");
  }

  void writeCount(string nucleotideFragment) {
    mapping(string:KNucleotide) frequencies=generateFrequencies(sizeof(nucleotideFragment));
    int count=0;
    KNucleotide item=frequencies[nucleotideFragment];
    if (item)
      count=item->count;
    predef::write("%d\t%s\n",count,nucleotideFragment);
  }


  mapping(string:KNucleotide) generateFrequencies(int length) {
    mapping(string:KNucleotide) frequencies=([]);

    void kFrequency(int offset, int k) {
      int n=sizeof(sequence)-k+1;
      for (int i=offset; i<n; i+=k) {
	string fragment=sequence[i..i+k-1];
	object item=frequencies[fragment];
	if (item) {
	  item->count++;
	} else {
	  frequencies[fragment]=KNucleotide(fragment);
	}
      }
    };

    for (int offset=0; offset<length; offset++)
      kFrequency(offset,length);
    return frequencies;
  }
}

int main(int argc, array argv) {
  Stdio.FILE r=Stdio.stdin;
  string line;
  String.Buffer buffer=String.Buffer();

  while (line=r->gets()) {
    if (line[..5]==">THREE")
      break;
  }

  while (line=r->gets()) {
    if (line[0]=='>')
      break;
    if (line[0]!=';')
      buffer+=upper_case(line);
  }
  
  object kn=KNucleotide(buffer->get());
  kn->writeFrequencies(1);
  kn->writeFrequencies(2);
  
  kn->writeCount("GGT");
  kn->writeCount("GGTA");
  kn->writeCount("GGTATT");
  kn->writeCount("GGTATTTTAATT");
  kn->writeCount("GGTATTTTAATTTATAGT");
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: lists.pike,v 1.2 2004-11-23 08:08:43 bfulgham Exp $
// http://shootout.alioth.debian.org/
// from: Per Hedbor
// Optimized by Marcus Agehall

#define SIZE 10000

#define TB(X) werror( "%s: %.2f\n", X, gauge {
#define TE()  })

int test_lists()
{
    mixed Li1, Li2, Li3;
    // create a list of integers from 1 to SIZE.
    Li1 = indices(allocate(SIZE+1))[1..];
    // copy the list to Li2.
    Li2 = copy_value( Li1 );
    // remove each element from left side of Li2 and append to
    // the right side of Li3 (preserving order)
  
    Li3 = ({});

    while( sizeof( Li2 ) )
    {
	Li3 += Li2[..0];
	Li2 = Li2[1..];
    }
    // Li2 is now empty.
    // Remove each element from right side of Li3 and append to right
    // side of Li2
    while( sizeof( Li3 ) )
    {
	Li2 += Li3[sizeof( Li3 )-1..];
	Li3 = Li3[..sizeof( Li3 )-2];
    }
    // Li2 is now reversed, and Li3 empty.
    // Reverse Li1 in place.
    Li1 = reverse( Li1 );
    if( Li1[0] != SIZE )
	return 0;
    // compare Li1 and Li2 for equality, and return the length of the list.
    if( equal( Li1, Li2 ) )
	return sizeof( Li1 );
    return 0;
}

void main(int argc, array argv)
{
    int result, num = (int)argv[-1];
    if( num <=  0 )
	num = 1;
    while( num-- )
	result = test_lists();
    write("%d\n", result );
}
// ---------------------------------------------------------------------
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// Contributed by Anthony Borla
// ---------------------------------------------------------------------

int main(int argc, array(string) argv)
{
  constant ITERATIONS = 50, LIMIT_SQR = 4.0;

  int N = (int)argv[1]; int bit_num = 0, byte_acc = 0;

  write("P4\n%d %d\n", N, N);

  for (int y = 0; y < N; y++)
  {
    for (int x = 0; x < N; x++)
    {
      float ZR = 0.0, ZI = 0.0, TR = 0.0, TI = 0.0;
      float CR = (2.0 * x / N) - 1.5, CI = (2.0 * y / N) - 1.0;
      int ESCAPE = 0;

      for (int i = 0; i < ITERATIONS; i++) 
      {
        TR = ZR * ZR - ZI * ZI + CR; TI = 2.0 * ZR * ZI + CI;
        ZR = TR; ZI = TI;

        if (ZR * ZR + ZI * ZI > LIMIT_SQR) { ESCAPE = 1; break; }
      }

      byte_acc = (byte_acc << 1) | (ESCAPE ? 0 : 1); bit_num++;

      if (bit_num == 8)
      {
        write("%c", byte_acc); byte_acc = bit_num = 0;
      }
      else if (x == N - 1)
      {
        byte_acc <<= (8 - bit_num); write("%c", byte_acc);
        byte_acc = bit_num = 0;
      }
    }
  }

  return 0;
}

#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: matrix.pike,v 1.1 2004-05-19 18:10:34 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Per Hedbor

int size = 30;

array(array(int))
mkmatrix(int rows, int cols) {
    array(array(int)) m = allocate(rows);
    int count = 1;
    for (int i=0; i<rows; i++) {
	array(int) row = allocate(cols);
	for (int j=0; j<cols; j++) {
	    row[j] = count++;
	}
	m[i] = row;
    }
    return(m);
}

void
main(int argc, array(string) argv) {
    int n = (int)argv[-1];
    if (n < 1)
      n = 1;
    
    Math.Matrix m1 = Math.Matrix(mkmatrix(size, size));
    Math.Matrix m2 = Math.Matrix(mkmatrix(size, size));
    Math.Matrix mm;
    for( int i = n; i>0; i-- )
      mm = m1 * m2;
    array q = (array(array(int)))(array)mm;
    write( "%d %d %d %d\n", q[0][0], q[2][3], q[3][2], q[4][4] );
}
// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// contributed by Adam Montague

void main(int argc, array(string) argv)
{
	int sum;
	Thread.Fifo fifo = Thread.Fifo();
	Thread.Fifo in = fifo;
	Thread.Fifo out = Thread.Fifo();
	for (int i = 0; i < 500; i++) {
		Thread.Thread(work, i, in, out);
		in = out;
		out = Thread.Fifo();
	}
	for (int i = 0; i < (int)argv[1]; i++) {
		fifo->write(0);
		sum += in->read();
	}
	write("%d\n", sum);
}

void work(int thread, Thread.Fifo in, Thread.Fifo out)
{
	for (;;) {
		out->write(in->read() + 1);
	}
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: methcall.pike,v 1.1 2004-05-19 18:10:41 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

class Toggle {
    int bool;

    object create (int start_state) {
	bool = start_state;
    }

    int value () {
	return(bool);
    }

    object activate () {
	bool = !bool;
	return(this_object());
    }
}

class NthToggle {
    inherit Toggle;

    int count_max, count;

    object create (int start_state, int max_counter) {
	::create(start_state);
	count_max = max_counter;
	count = 0;
    }

    object activate () {
	if (++count >= count_max) {
	    bool = !bool;
	    count = 0;
	}
	return(this_object());
    }
}

void main(int argc, array(string) argv) {
    int n = (int)argv[-1];
    if (n < 1) n = 1;
      
    int val = 1;
    object toggle = Toggle(val);
    for (int i=0; i<n; i++) {
	val = toggle->activate()->value();
    }
    write((toggle->value()) ? "true\n" : "false\n");

    val = 1;
    object ntoggle = NthToggle(val, 3);
    for (int i=0; i<n; i++) {
	val = ntoggle->activate()->value();
    }
    write((ntoggle->value()) ? "true\n" : "false\n");
}


#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: moments.pike,v 1.1 2004-05-19 18:10:48 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Fredrik Noring

class Moments
{
    int N;
    float median;
    float mean;
    float average_deviation;
    float standard_deviation;
    float variance;
    float skew;
    float kurtosis;
    
    void create(array(float) v)
    {
        float sum = `+(@v);
        N = sizeof(v);
        mean = sum / N;

        foreach(v, float i)
        {
            float deviation = i - mean;
            average_deviation += abs(deviation);
            variance += pow(deviation, 2);
            skew += pow(deviation, 3);
            kurtosis += pow(deviation, 4);
        }

        average_deviation /= N;
        variance /= (N - 1);
        standard_deviation = sqrt(variance);

        if (variance)
        {
            skew /= (N * variance * standard_deviation);
            kurtosis = kurtosis/(N * variance * variance) - 3.0;
        }

        sort(v);
        int mid = N/2;
        median = N % 2 ? v[mid] : (v[mid] + v[mid-1])/2;
    }
};

int main()
{
    array input = Stdio.stdin.read()/"\n";
    Moments m=Moments( (array(float)) input[..sizeof(input)-2] );

    write("n:                  %d\n", m->N);
    write("median:             %.6f\n", m->median);
    write("mean:               %.6f\n", m->mean);
    write("average_deviation:  %.6f\n", m->average_deviation);
    write("standard_deviation: %.6f\n", m->standard_deviation);
    write("variance:           %.6f\n", m->variance);
    write("skew:               %.6f\n", m->skew);
    write("kurtosis:           %.6f\n", m->kurtosis);
}
// ---------------------------------------------------------------------
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// Based on D language implementation by Dave Fladebo
//
// Contributed by Anthony Borla
// ---------------------------------------------------------------------

final class NBodySystem
{
  constant PI = 3.141592653589793;
  constant SOLAR_MASS = 4 * PI * PI;
  constant DAYS_PER_YEAR = 365.24;

  static void create()
  {
    float px, py, pz;

    foreach(bodies, Body i)
    {
      px += i->vx * i->mass; py += i->vy * i->mass; pz += i->vz * i->mass;
    }

    bodies[0]->offsetMomentum(px, py, pz);
  }

  public void advance(float dt)
  {
    float dx, dy, dz, distance, mag;

    int idx, length = sizeof(bodies);

    foreach(bodies, Body i)
    {
      foreach(bodies[idx + 1 .. length], Body j)
      {
        dx = i->x - j->x;
        dy = i->y - j->y;
        dz = i->z - j->z;

        distance = sqrt(dx * dx + dy * dy + dz * dz);
        mag = dt / (distance * distance * distance);

        i->vx -= dx * j->mass * mag;
        i->vy -= dy * j->mass * mag;
        i->vz -= dz * j->mass * mag;

        j->vx += dx * i->mass * mag;
        j->vy += dy * i->mass * mag;
        j->vz += dz * i->mass * mag;
      }

      idx += 1;
    }

    foreach(bodies, Body i)
    {
      i->x += dt * i->vx; i->y += dt * i->vy; i->z += dt * i->vz;
    }
  }

  public float energy()
  {
    float dx, dy, dz, e, distance;

    int idx, length = sizeof(bodies);

    foreach(bodies, Body i)
    {
      e += 0.5 * i->mass * (i->vx * i->vx + i->vy * i->vy + i->vz * i->vz);

      foreach(bodies[idx + 1 .. length], Body j)
      {
        dx = i->x - j->x; dy = i->y - j->y; dz = i->z - j->z;
        distance = sqrt(dx * dx + dy * dy + dz * dz);
        e -= (i->mass * j->mass) / distance;
      }

      idx += 1;
    }

    return e;
  }

  private final class Body
  {
    static void create(float x, float y, float z, float vx, float vy, float vz, float mass)
    {
      this->x = x; this->y = y; this->z = z;
      this->vx = vx; this->vy = vy; this->vz = vz;
      this->mass = mass;
    }

    void offsetMomentum(float px, float py, float pz)
    {
      vx = -px / SOLAR_MASS;
      vy = -py / SOLAR_MASS;
      vz = -pz / SOLAR_MASS;
    }

    float x, y, z, vx, vy, vz, mass;
  }

  // sun jupiter saturn uranus neptune

  private array(Body) bodies =
    ({
      Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SOLAR_MASS),

      Body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
           1.66007664274403694e-03 * DAYS_PER_YEAR, 7.69901118419740425e-03 * DAYS_PER_YEAR,
           -6.90460016972063023e-05 * DAYS_PER_YEAR, 9.54791938424326609e-04 * SOLAR_MASS),

      Body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
           -2.76742510726862411e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR,
           2.30417297573763929e-05 * DAYS_PER_YEAR, 2.85885980666130812e-04 * SOLAR_MASS),
        
      Body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
           2.96460137564761618e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR,
           -2.96589568540237556e-05 * DAYS_PER_YEAR, 4.36624404335156298e-05 * SOLAR_MASS),

      Body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
           2.68067772490389322e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR,
           -9.51592254519715870e-05 * DAYS_PER_YEAR, 5.15138902046611451e-05 * SOLAR_MASS)
    });
}

// --------------------------------

int main(int argc, array(string) argv)
{
  int N = (int)argv[1];

  NBodySystem nbs = NBodySystem();

  write("%.9f\n", nbs->energy());
  for(int i = 0; i < N; i++) nbs->advance(0.01);
  write("%.9f\n", nbs->energy());

  return 0;
}

#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: nestedloop.pike,v 1.1 2004-05-19 18:10:57 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

void main(int argc, array(string) argv) {
  int n = (int)argv[-1];
  if (n < 1) n = 1;
  int x=0;

  for (int a; a<n; a++)
      for (int b; b<n; b++)
	  for (int c; c<n; c++)
	      for (int d; d<n; d++)
		  for (int e; e<n; e++)
		      for (int f; f<n; f++)
			  x++;

  write("%d\n", x);
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Adam Montague
*/

int nsieve(int m)
{
	array(int) flags = allocate(m + 1, 1);
	int count;

	for (int i = 2; i <= m; i++) {
		if (flags[i]) {
			for (int j = i + i; j <= m; j += i) {
				flags[j] = 0;
			}
			count++;
		}
	}
	return (count);
}

int main(int argc, array(string) argv)
{
	int n = (int)argv[1];
	if (n < 2)
		n = 2;
	int m;

	m = (1 << n) * 10000;
	write("Primes up to %8d %8d\n", m, nsieve(m));

	m = (1 << n - 1) * 10000;
	write("Primes up to %8d %8d\n", m, nsieve(m));

	m = (1 << n - 2) * 10000;
	write("Primes up to %8d %8d\n", m, nsieve(m));
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Anthony Borla. This is a slightly modified version
   of, 'nsieve.pike' by Adam Montague, which uses a standard integer 
   array with 'bit twiddling' to perform data encoding / decoding
*/

int nsieve(int m)
{
	int size = (m + 31) >> 5; int count;

	array(int) flags = allocate(size, 0xffffffff);

	for (int i = 2; i < m; i++) {
		if (flags[i >> 5] & 1 << (i & 31)) {
			for (int j = i + i; j < m; j += i) {
				flags[j >> 5] &= ~(1 << (j & 31));
			}
			count++;
		}
	}
	return (count);
}

int main(int argc, array(string) argv)
{
	int n = (int)argv[1];
	if (n < 2)
		n = 2;
	int m;

	m = (1 << n) * 10000;
	write("Primes up to %8d %8d\n", m, nsieve(m));

	m = (1 << n - 1) * 10000;
	write("Primes up to %8d %8d\n", m, nsieve(m));

	m = (1 << n - 2) * 10000;
	write("Primes up to %8d %8d\n", m, nsieve(m));
}

#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: objinst.pike,v 1.1 2004-05-19 18:11:03 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

class Toggle {
    int bool;

    object create (int start_state) {
	bool = start_state;
    }

    int value () {
	return(bool);
    }

    object activate () {
	bool = !bool;
	return(this_object());
    }
}

class NthToggle {
    inherit Toggle;

    int count_max, count;

    object create (int start_state, int max_counter) {
	::create(start_state);
	count_max = max_counter;
	count = 0;
    }

    object activate () {
	if (++count >= count_max) {
	    bool = !bool;
	    count = 0;
	}
	return(this_object());
    }
}

void main(int argc, array(string) argv) {
    int n = (int)argv[-1];
    if (n < 1) n = 1;
      
    object toggle = Toggle(1);
    for (int i=0; i<5; i++) {
	toggle->activate();
	write((toggle->value()) ? "true\n" : "false\n");
    }
    for (int i=0; i<n; i++) {
	object toggle = Toggle(1);
    }

    write("\n");

    object ntoggle = NthToggle(1, 3);
    for (int i=0; i<8; i++) {
	ntoggle->activate();
	write((ntoggle->value()) ? "true\n" : "false\n");
    }
    for (int i=0; i<n; i++) {
	object ntoggle = NthToggle(1, 3);
    }
}


// ---------------------------------------------------------------------
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// Based on D language implementation by Dave Fladebo
//
// Contributed by Anthony Borla
// ---------------------------------------------------------------------

int main(int argc, array(string) argv)
{
  int N = (int)argv[1]; float alt = 1.0;

  array(float) sum = ({0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0});

  for(float d = 1.0; d <= N; d++, alt = -alt)
  {
    float d2 = d * d, d3 = d2 * d, ds = sin(d), dc = cos(d);

    sum[0] += pow(2 / 3.0,d - 1);
    sum[1] += 1 / sqrt(d);
    sum[2] += 1 / (d * (d + 1));
    sum[3] += 1 / (d3 * ds * ds);
    sum[4] += 1 / (d3 * dc * dc);
    sum[5] += 1 / d;
    sum[6] += 1 / (d2);
    sum[7] += alt / d;
    sum[8] += alt / (2 * d - 1);
  }

  write("%.9f\t(2/3)^k\n", sum[0]);
  write("%.9f\tk^-0.5\n", sum[1]);
  write("%.9f\t1/k(k+1)\n", sum[2]);
  write("%.9f\tFlint Hills\n", sum[3]);
  write("%.9f\tCookson Hills\n", sum[4]);
  write("%.9f\tHarmonic\n", sum[5]);
  write("%.9f\tRiemann Zeta\n", sum[6]);
  write("%.9f\tAlternating Harmonic\n", sum[7]);
  write("%.9f\tGregory\n", sum[8]);

  return 0;
}

// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Marcus Comstedt

int k = 0;
array(int) z = ({ 1, 0, 0, 1 });

array(int) compose(array(int) a, array(int) b)
{
  return ({ a[0]*b[0], a[0]*b[1]+a[1]*b[3],
	    a[2]*b[0]+a[3]*b[2], a[2]*b[1]+a[3]*b[3] });
}

int extract(array(int) a, int j)
{
  return (a[0]*j+a[1]) / (a[2]*j+a[3]);
}

array(int) pi_digits(int c)
{
  array(int) r = allocate(c);
  for(int i=0; i<c; i++) {
    int y;
    while((y = extract(z, 3)) != extract(z, 4)) {
      ++k;
      z = compose(z, ({k, 4*k+2, 0, 2*k+1}));
    }
    z = compose(({10, -10*y, 0, 1}), z);
    r[i] = y;
  }
  return r;
}

int main(int argc, array(string) argv)
{
  int i, n = (int)argv[1];
  for(i=10; i <= n; i+=10)
    write("%@d\t:%d\n", pi_digits(10), i);
  if((i-=10) < n)
    write("%-10{%d%}\t:%d\n", pi_digits(n-i)/1, n);
  return 0;
}
#!/usr/bin/pike
// -*- mode: pike -*- 
// $Id: prodcons.pike,v 1.2 2005-05-13 16:24:18 igouy-guest Exp $
// http://www.bagley.org/~doug/shootout/

inherit Thread.Condition: access;
inherit Thread.Mutex: mutex;
int data, consumed, produced, count;

void producer(int n) {
    for (int i=1; i<=n; i++) {
	object mtx = mutex::lock();
	while (count != 0) access::wait(mtx);
	data = i;
	count += 1;
	destruct(mtx);
	access::signal();
	produced += 1;
    }
}

void consumer(int n) {
    while (1) {
	object mtx = mutex::lock();
	while (count == 0) access::wait(mtx);
	int i = data;
	count -= 1;
	access::signal();
	destruct(mtx);
	consumed += 1;
	if (i == n) break;
    }
}

void main(int argc, array(string) argv) {
    int n = (int)argv[-1];
    if (n < 1) n = 1;
    data = consumed = produced = count = 0;
    thread_create(producer, n);
    consumer(n);
    write("%d %d\n", produced, consumed);
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: random.pike,v 1.1 2004-05-19 18:11:16 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

int IM = 139968;
int IA = 3877;
int IC = 29573;

int last = 42;

float
gen_random(float max) { return(max * (last = (last * IA + IC) % IM) / IM); }

int
main(int argc, array(string) argv) {
    int N = ((int)argv[-1] || 1) - 1;
    while (N--) {
	gen_random(100.0);
    }
    write("%.9f\n", gen_random(100.0));
    return(0);
}

// ---------------------------------------------------------------------
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// Code based on / inspired by existing, relevant Shootout submissions
//
// Contributed by Anthony Borla
// ---------------------------------------------------------------------

int ack(int x, int y)
{
  if (x == 0) return y + 1;
  if (y == 0) return ack(x - 1, 1);
  return ack(x - 1, ack(x, y - 1));
}

// --------------

int fib(int n)
{
  if (n < 2) return 1;
  return fib(n - 2) + fib(n - 1);
}

float fibflt(float n)
{
  if (n < 2.0) return 1.0;
  return fibflt(n - 2.0) + fibflt(n - 1.0);
}

// --------------

int tak(int x, int y, int z)
{
  if (y < x) return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
  return z;
}

float takflt(float x, float y, float z)
{
  if (y < x) return takflt(takflt(x - 1.0, y, z), takflt(y - 1.0, z, x), takflt(z - 1.0, x, y));
  return z;
}

// --------------------------------

int main(int argc, array(string) argv)
{
  int N = (int)argv[1];

  write("Ack(3,%d): %d\n", N, ack(3, N));
  write("Fib(%.1f): %.1f\n", 27.0 + N, fibflt(27.0 + N));

  N -= 1;
  write("Tak(%d,%d,%d): %d\n", N * 3, N * 2, N, tak(N * 3, N * 2, N));

  write("Fib(3): %d\n", fib(3));
  write("Tak(3.0,2.0,1.0): %.1f\n", takflt(3.0, 2.0, 1.0));

  return 0;
}

// ---------------------------------------------------------------------
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// Requires that the PCRE [Perl Compatible Regular Expression] library
// be installed [Pike needs to be rebuilt after this is installed].
//
// Also, makes use of code from:
//
//   http://buoy.riverweb.com:8080/viewrep/cvs/pike_modules/Public_Web_Wiki/module.pmod.in/module.pmod
//
// to approximate the expected [but not currently implemented] functionality
// of 'Regexp.split'. Many thanks to Bill Welliver for suggesting this
// approach.
//
// Contributed by Anthony Borla
// ---------------------------------------------------------------------

//
// Extracted from 'pmod' module
//
class BaseRule
{
  constant type = "BaseRule";
  static object regexp;
  static function split_fun;
  int max_iterations = 10;

  string _sprintf(mixed ... args)
  {
    return sprintf("%s(%s)", type, regexp->pattern);
  }

  void create(string match)
  {
    regexp = _Regexp_PCRE(match, Regexp.PCRE.OPTION.MULTILINE);
    split_fun = regexp->split;
  }

  array replace(string subject,string|function with, mixed|void ... args)
  {
    int i=0;
    array res = ({});
		
    for (;;)
    {
      array substrings = ({});
      array(int)|int v=regexp->exec(subject,i);

      if (intp(v) && !regexp->handle_exec_error([int]v)) break;

      if (v[0]>i) res+=({subject[i..v[0]-1]});

      if(sizeof(v)>2)
      {
        int c = 2;
        do
        {
          substrings += ({ subject[v[c]..(v[c+1]-1)] });
          c+=2;
        }
        while(c<= (sizeof(v)-2));
      }

      if (stringp(with)) res+=({with});
      else { array o = with(subject[v[0]..v[1]-1], substrings, @args); res+=o; }
 
      i=v[1];
    }

    res+=({subject[i..]});
    return res;
  }
}

// --------------------------------

constant VARIANTS = ({
  "agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]", "a[act]ggtaaa|tttacc[agt]t",
  "ag[act]gtaaa|tttac[agt]ct", "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
  "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct", "agggtaa[cgt]|[acg]ttaccct"});

constant IUBS = ([
  "B":"(c|g|t)", "D":"(a|g|t)", "H":"(a|c|t)", "K":"(g|t)",
  "M":"(a|c)", "N":"(a|c|g|t)", "R":"(a|g)", "S":"(c|g)",
  "V":"(a|c|g)", "W":"(a|t)", "Y":"(c|t)"]);

// --------------------------------

int main()
{
  // Read input data into string and record its length
  string seq = Stdio.stdin->read(); int initial_length = sizeof(seq);

  // Remove all newline and segment divider line occurrences
  seq = Regexp.replace("(>.*\n)|(\n)", seq, ""); int code_length = sizeof(seq);

  // Perform regexp counts
  foreach(VARIANTS, string var)
  {
    // 'Regexp.split' version would probably look like this:
    //
    // int number_of_matches; string pattern = "(?i)" + var;
    // 
    // if (array(string) matches = Regexp.split(pattern, seq))
    //   number_of_matches = sizeof(matches);
    //

    // 'pmod' module version
    int number_of_matches = sizeof(BaseRule("(?i)" + var)->replace(seq, "")) / 2;

    write("%s %d\n", var, number_of_matches);
  }

  // Perform replacements
  foreach(indices(IUBS), string key)
  {
    seq = Regexp.replace(key, seq, IUBS[key]);
  }

  // Print statistics
  write("\n%d\n%d\n%d\n", initial_length, code_length, sizeof(seq));

  return 0;
}

// The Computer Language Benchmarks Game
// http://shootout.alioth.debian.org/
//
// Requires a PCRE [Perl Compatible Regular Expression] enabled Pike.
//
// contributed by Anthony Borla
// Modified by Bertrand LUPART and Mirar.

array(string) VARIANTS =
  ({
    "agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]", "a[act]ggtaaa|tttacc[agt]t",
    "ag[act]gtaaa|tttac[agt]ct", "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
    "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct", "agggtaa[cgt]|[acg]ttaccct"
  });

array(string) IUB =
  ({
    "B","D","H","K","M","N","R","S","V","W","Y", 
  });
  
array(string) IUBnew =
  ({
    "(c|g|t)","(a|g|t)","(a|c|t)","(g|t)","(a|c)","(a|c|g|t)","(a|g)","(c|g)","(a|c|g)","(a|t)","(c|t)", 
  });



int main()
{
  // Read input data into string and record its length
  string seq = Stdio.stdin->read(); int initial_length = sizeof(seq);

  // Remove all newline and segment divider line occurrences
  seq = Regexp.PCRE.Studied("(>.*\n)|(\n)")->replace(seq, "");

  int code_length = sizeof(seq);

  // Perform regexp counts
  foreach(VARIANTS, string var)
  {
		int number_of_matches = 0;
		Regexp.PCRE.Studied(var, Regexp.PCRE.OPTION.CASELESS)->matchall(seq, lambda(){ number_of_matches++; });

    write("%s %d\n", var, number_of_matches);
  }

  // Perform replacements
  seq = replace(seq, IUB, IUBnew);

  // Print statistics
  write("\n%d\n%d\n%d\n", initial_length, code_length, sizeof(seq));

  return 0;
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: regexmatch.pike,v 1.1 2004-05-19 18:11:27 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Fredrik Noring

constant area = "([0-9][0-9][0-9]|\\([0-9][0-9][0-9]\\))";
constant exch = "([0-9][0-9][0-9])";
constant last = "([0-9][0-9][0-9][0-9])";

void main(int argc, array(string) argv)
{
    Regexp r = Regexp("^[^0-9\\(]*"+area+" "+exch+"[ -]"+last+"[^0-9]*$");
    array(string) phones = Stdio.stdin->read()/"\n";
    int n = (int)argv[-1];
    int count = 0;
      
    while(n--)
	foreach(phones, string phone)
	    if(array(string) parts = r->split(phone))
		if(n == 0)
		    if(parts[0][0] == '(')
			write("%d: %s %s-%s\n", ++count, @parts);
		    else
			write("%d: (%s) %s-%s\n", ++count, @parts);
}
// ---------------------------------------------------------------------
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// Contributed by Anthony Borla
// ---------------------------------------------------------------------

void dumpSegment(String.Buffer segment)
{
  constant
    LINELENGTH = 60.0,
    FROM = "wsatugcyrkmbdhvnATUGCYRKMBDHVN" / 1,
    TO = "WSTAACGRYMKVHDBNTAACGRYMKVHDBN" / 1;

  write("%s\n", ((reverse(replace(segment->get(), FROM, TO)) / LINELENGTH) * "\n"));
}

// --------------------------------

int main()
{
  String.Buffer segment = String.Buffer(); string sequence;

  while ((sequence = Stdio.stdin.gets()) != 0)
  {
    if (sequence[0] == '>')
    {
      if (segment->_sizeof() != 0) dumpSegment(segment);
      write("%s\n", sequence);
    }
    else
    {
      segment->add(sequence);
    }
  }

  dumpSegment(segment);

  return 0;
}

#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: reversefile.pike,v 1.1 2004-05-19 18:12:18 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Fredrik Noring

void main() { write((reverse(Stdio.stdin.read()/"\n")*"\n")[1..]+"\n"); }
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: sieve.pike,v 1.1 2004-05-19 18:12:27 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from: Per Hedbor

void main(int argc, array(string) argv)
{
    array(int) flags;
    int i, k, count, num;
  
    num = (int)argv[-1];
    if (num < 1)
	num = 1;
      
    while (num--) {
	count = 0;
	flags = ({ 1 })*8193;
	for (i=2; i <= 8192; i++) {
	    if (flags[i]) {
		for (k=i+i; k <= 8192; k+=i)
		    flags[k] = 0;
		count++;
	    }
	}
    }
    write("Count: %d\n", count);
}
// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// contributed by Adam Montague

float A(int i, int j)
{
	return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
}

float Av(int n, array(float) v, array(float) Av)
{
	for (int i = 0; i < n; i++) {
		Av[i] = 0.0;
		for (int j = 0; j < n; j++) {
			Av[i] += A(i, j) * v[j];
		}
	}
}

float Atv(int n, array(float) v, array(float) Atv)
{
	for (int i = 0; i < n; i++) {
		Atv[i] = 0.0;
		for (int j = 0; j < n; j++) {
			Atv[i] += A(j, i) * v[j];
		}
	}
}

float AtAv(int n, array(float) v, array(float) AtAv)
{
	array(float) u = allocate(n);
	Av(n, v, u);
	Atv(n, u, AtAv);
}

void main(int argc, array(string) argv)
{
	int n = (int)argv[1];
	array(float) u = allocate(n, 1.0);
	array(float) v = allocate(n);

	for (int i = 0; i < 10; i++) {
		AtAv(n, u, v);
		AtAv(n, v, u);
	}

	float vBv, vv;
	for (int i = 0; i < n; i++) {
		vBv += u[i] * v[i];
		vv += v[i] * v[i];
	}

	write("%.9f\n", sqrt(vBv / vv));
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: spellcheck.pike,v 1.1 2004-05-19 18:13:26 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

void main() {
    array(string) a = Stdio.read_file("Usr.Dict.Words")/"\n";
    mapping dictionary = mkmapping(a, allocate(sizeof(a), 1));
    while (string word = Stdio.stdin.gets()) {
	if (!dictionary[word]) write("%s\n", word);
    }
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: strcat.pike,v 1.1 2004-05-19 18:13:35 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from Per Hedbor

void main(int argc, array(string) argv) {
    int n = (int)argv[-1];
    if (n < 1) n = 1;
      
    String.Buffer str = String.Buffer();
    function f = str->add;
    for (int i=0; i<n; i++) {
	f("hello\n");
    }
    write("%d\n", strlen(str->get()));
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: strcat.pike-2.pike,v 1.1 2004-11-10 06:44:59 bfulgham Exp $
// http://shootout.alioth.debian.org/

void main(int argc, array(string) argv)
{
    int n = (int)argv[-1];
    if (n < 1) n = 1;
      
    string str = "";
    for (int i=0; i<n; i++) {
	str += "hello\n";
    }
    write("%d\n", strlen(str));
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: sumcol.pike-2.pike,v 1.1 2004-11-10 07:07:16 bfulgham Exp $
// http://shootout.alioth.debian.org/
// from: Henrik Grubbström

void main()
{
    int sum = 0;
    string data = "";

    while((data += Stdio.stdin.read(4096)) != "")
    {
        array(string) values = data/"\n";
        sum = `+(sum, @((array(int))values[..sizeof(values)-2]));
        data = values[-1];
    }

    write("%0d\n", sum);
}
#!/usr/bin/pike

/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Adam Montague 
*/

float takfp(float x, float y, float z)
{
	return (y >=x) ? z : takfp(takfp(x - 1.0, y, z), takfp(y - 1.0, z, x), takfp(z - 1.0, x, y));
}

void main(int argc, array(string) argv)
{
	float n = (float)argv[1];
	write("%.1f\n", takfp(n * 3.0, n * 2.0, n * 1.0));
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: wc.pike,v 1.1 2004-05-19 18:13:51 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from Per Hedbor, optimized by David Hedbor

enum State { Outside, Inside };

void main() {
    int nl = 0, nw = 0, nc = 0; // line, word and character counters
    int sl; // Size of input

    State state = Outside; // Inside or outside word

    string buf;
    string rest="";
    array l;
    do {
	buf = replace(Stdio.stdin.read( 4196 ), "\t", " ");
	if(strlen(buf)) {
	    nc += sizeof(buf);
	    l = (rest+ buf) / "\n";
	    nl += sizeof(l)-1;
	    foreach(l[..sizeof(l)-2], rest) {
		nw += sizeof(rest / " " - ({""}));
	    }
	    if(sizeof(l)>1) {
		rest = l[-1];
	    } //else rest="";
	} else {
	    nw += sizeof(rest / " " - ({""}));
	    break;
	}
    } while( 1 );
    write("%d %d %d\n", nl, nw, nc);
}
#!/usr/bin/pike
// -*- mode: pike -*-
// $Id: wordfreq.pike,v 1.2 2004-07-03 05:36:11 bfulgham Exp $
// http://shootout.alioth.debian.org/
// from: Fredrik Noring

void main()
{
    mapping(string:int) dictionary = ([]);
    string buffer = "";

    array(string) f = filter(map(enumerate(128),
				   lambda(int i)
				   {
				       return !('A' <= i && i <= 'Z' ||
						'a' <= i && i <= 'z' ||
						i == ' ') &&
					      sprintf("%c", i);
				   }), `!=, 0);
    array(string) t = allocate(sizeof(f), " ");

    for(;;)
    {
	  string data =
	      buffer + replace(lower_case(Stdio.stdin.read(4096)), f, t);
	  
	  if(!sizeof(data))
	      break;
	  
	  array(string) words = data/" ";

	  if(1 < sizeof(words) && sizeof(words[-1]))
	      buffer = words[-1],
	       words = words[..sizeof(words)-2];
	  else
	      buffer = "";

	  foreach(words, string word)
	      dictionary[word]++;
    }
    
    m_delete(dictionary, "");

    mapping(int:array(string)) revdictionary = ([]);
    array(string) words = indices(dictionary);
    array(int) freqs = values(dictionary);

    for(int i = 0; i < sizeof(dictionary); i++)
	  revdictionary[freqs[i]] += ({ words[i] });

    freqs = sort(indices(revdictionary));
    for(int i = sizeof(freqs)-1; 0 <= i; i--)
    {
	  int freq = freqs[i];
	  words = sort(revdictionary[freq]);
	  
	  for(int j = sizeof(words)-1; 0 <= j; j--)
	      write("%7d %s\n", freq, words[j]);
    }
}
