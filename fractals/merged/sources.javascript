// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Sjoerd Visscher

function ack(m, n) {
  return (m == 0
    ? n + 1
    : (n == 0
      ? ack(m - 1, 1) 
      : ack(m - 1, ack(m, n - 1))));
}

var n = arguments[0];
print("ack(3, " + n + "): " + ack(3, n));
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

var i, k;

var n = arguments[0];
var x = Array(n);
var y = Array(n);

for (i = 0; i < n; i++) {
  x[i] = i + 1;
  y[i] = 0; // Need to set all entries in i to zero or the result will be NaN 
}
for (k = 0 ; k < 1000; k++) {
  for (i = n-1; i >= 0; i--) {
    y[i] += x[i];
  }
}
print(y[0], y[n-1]);

/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy */

function TreeNode(left,right,item){
   this.left = left;
   this.right = right;
   this.item = item;
}

TreeNode.prototype.itemCheck = function(){
   if (this.left==null) return this.item;
   else return this.item + this.left.itemCheck() - this.right.itemCheck();
}

function bottomUpTree(item,depth){
   if (depth>0){
      return new TreeNode(
          bottomUpTree(2*item-1, depth-1)
         ,bottomUpTree(2*item, depth-1)
         ,item
      );
   }
   else {
      return new TreeNode(null,null,item);
   }
}


var minDepth = 4;
var n = arguments[0];
var maxDepth = Math.max(minDepth + 2, n);
var stretchDepth = maxDepth + 1;

var check = bottomUpTree(0,stretchDepth).itemCheck();
print("stretch tree of depth " + stretchDepth + "\t check: " + check);

var longLivedTree = bottomUpTree(0,maxDepth);
for (var depth=minDepth; depth<=maxDepth; depth+=2){
   var iterations = 1 << (maxDepth - depth + minDepth);

   check = 0;
   for (var i=1; i<=iterations; i++){
      check += bottomUpTree(i,depth).itemCheck();
      check += bottomUpTree(-i,depth).itemCheck();
   }
   print(iterations*2 + "\t trees of depth " + depth + "\t check: " + check);
}

print("long lived tree of depth " + maxDepth + "\t check: " 
   + longLivedTree.itemCheck());
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
   modified by Matthew Wilson */

function fannkuch(n) {
   var check = 0;
   var perm = Array(n);
   var perm1 = Array(n);
   var count = Array(n);
   var maxPerm = Array(n);
   var maxFlipsCount = 0;
   var m = n - 1;

   for (var i = 0; i < n; i++) perm1[i] = i;
   var r = n;

   while (true) {
      // write-out the first 30 permutations
      if (check < 30){
         var s = "";
         for(var i=0; i<n; i++) s += (perm1[i]+1).toString();
         print(s);
         check++;
      }

      while (r != 1) { count[r - 1] = r; r--; }
      if (!(perm1[0] == 0 || perm1[m] == m)) {
         for (var i = 0; i < n; i++) perm[i] = perm1[i];

         var flipsCount = 0;
         var k;

         while (!((k = perm[0]) == 0)) {
            var k2 = (k + 1) >> 1;
            for (var i = 0; i < k2; i++) {
               var temp = perm[i]; perm[i] = perm[k - i]; perm[k - i] = temp;
            }
            flipsCount++;
         }

         if (flipsCount > maxFlipsCount) {
            maxFlipsCount = flipsCount;
            for (var i = 0; i < n; i++) maxPerm[i] = perm1[i];
         }
      }

      while (true) {
         if (r == n) return maxFlipsCount;
         var perm0 = perm1[0];
         var i = 0;
         while (i < r) {
            var j = i + 1;
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

var n = 1*arguments[0]*1;
print("Pfannkuchen(" + n + ") = " + fannkuch(n));
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy, transliterated from Mike Pall's Lua program 
*/

function fannkuch(n) {
   var p = Array(n), q = Array(n), s = Array(n);
   var sign = 1, maxflips = 0, sum = 0, m = n-1;
   for(var i=0; i<n; i++){ p[i] = i; q[i] = i; s[i] = i; }
   do {
      // Copy and flip.
      var q0 = p[0];                                     // Cache 0th element.
      if (q0 != 0){
         for(var i=1; i<n; i++) q[i] = p[i];             // Work on a copy.
         var flips = 1;
         do { 
            var qq = q[q0]; 
            if (qq == 0){                                // ... until 0th element is 0.
               sum += sign*flips;
	       if (flips > maxflips) maxflips = flips;   // New maximum?
               break; 
            } 
 	    q[q0] = q0; 
	    if (q0 >= 3){
	       var i = 1, j = q0 - 1, t;
               do { t = q[i]; q[i] = q[j]; q[j] = t; i++; j--; } while (i < j); 
            }
	    q0 = qq; flips++;
         } while (true); 
      }
      // Permute.
      if (sign == 1){
         var t = p[1]; p[1] = p[0]; p[0] = t; sign = -1; // Rotate 0<-1.
      } else { 
         var t = p[1]; p[1] = p[2]; p[2] = t; sign = 1;  // Rotate 0<-1 and 0<-1<-2.
         for(var i=2; i<n; i++){ 
	    var sx = s[i];
	    if (sx != 0){ s[i] = sx-1; break; }
	    if (i == m) return Array(sum,maxflips);      // Out of permutations.
	    s[i] = i;
	    // Rotate 0<-...<-i+1.
	    t = p[0]; for(var j=0; j<=i; j++){ p[j] = p[j+1]; } p[i+1] = t;
         }
      }
   } while (true);
}

var n = 1*arguments[0]*1;
var pf = fannkuch(n);
print(pf[0] + "\n" + "Pfannkuchen(" + n + ") = " + pf[1]);
// The Great Computer Language Shootout
//  http://shootout.alioth.debian.org
//
//  Contributed by Ian Osgood

var last = 42, A = 3877, C = 29573, M = 139968;

function rand(max) {
  last = (last * A + C) % M;
  return max * last / M;
}

var ALU =
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
  "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
  "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
  "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
  "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
  "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
  "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

var IUB = {
  a:0.27, c:0.12, g:0.12, t:0.27,
  B:0.02, D:0.02, H:0.02, K:0.02,
  M:0.02, N:0.02, R:0.02, S:0.02,
  V:0.02, W:0.02, Y:0.02
}

var HomoSap = {
  a: 0.3029549426680,
  c: 0.1979883004921,
  g: 0.1975473066391,
  t: 0.3015094502008
}

function makeCumulative(table) {
  var last = null;
  for (var c in table) {
    if (last) table[c] += table[last];
    last = c;
  }
}

function fastaRepeat(n, seq) {
  var seqi = 0, lenOut = 60;
  while (n>0) {
    if (n<lenOut) lenOut = n;
    if (seqi + lenOut < seq.length) {
      print( seq.substring(seqi, seqi+lenOut) );
      seqi += lenOut;
    } else {
      var s = seq.substring(seqi);
      seqi = lenOut - s.length;
      print( s + seq.substring(0, seqi) );
    }
    n -= lenOut;
  }
}

function fastaRandom(n, table) {
  var line = new Array(60);
  makeCumulative(table);
  while (n>0) {
    if (n<line.length) line = new Array(n);
    for (var i=0; i<line.length; i++) {
      var r = rand(1);
      for (var c in table) {
        if (r < table[c]) {
          line[i] = c;
          break;
        }
      }
    }
    print( line.join('') );
    n -= line.length;
  }
}

var n = arguments[0]

print(">ONE Homo sapiens alu")
fastaRepeat(2*n, ALU)

print(">TWO IUB ambiguity codes")
fastaRandom(3*n, IUB)

print(">THREE Homo sapiens frequency")
fastaRandom(5*n, HomoSap)
// The Computer Language Benchmarks Game
//  http://shootout.alioth.debian.org
//
//  Contributed by Ian Osgood
//  Largely rewritten by Matthew Wilson

function fastaRepeat(n, seq) {
  var seqi = 0, len = seq.length, i, j, k, l, block, 
    str = Array(len*60+1).join(seq), lines = Array(i=j=len*len);
  while (--j>-1) { lines[j] = str.substr(60*j, 60) }
  block = lines.join("\n");
  for (j=0, k=Math.floor((l=Math.floor(n/60))/i); j<k; ++j) { print(block) }
  for (j = 0, k = l % i; j < k; ++j) { print(lines[j]) }
  if (n % 60 > 0) { print(lines[k].substr(0, n % 60)) }
}

var rand=(function() {
  var Last = 42;
  return function() { return (Last=(Last * 3877 + 29573) % 139968) / 139968 }
})();

function printLineMaker(table) {
  var h = 0, k = [], v = [], c, l=0;
  for (c in table) { l = v[h] = table[k[h++] = c]+=l; }
  return function(x) {
    var line = "";
    next: for (var i=0; i<x; ++i) {
      var r = rand(), j=0;
      for (;;++j) {
        if (r < v[j]) {
          line += k[j];
          continue next;
        }
      }
    }
    print(line);
  }
}

function fastaRandom(n, table) {
  var printLine=printLineMaker(table);
  while ((n -= 60) > -1) { printLine(60) }
  if (n<0 && n>-60) { printLine(60 + n) }
}

(function main(n) {
  var ALU = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
            "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
            "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
            "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
            "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
            "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
            "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

  var IUB = { a:0.27, c:0.12, g:0.12, t:0.27, B:0.02, D:0.02, H:0.02, K:0.02,
              M:0.02, N:0.02, R:0.02, S:0.02, V:0.02, W:0.02, Y:0.02 }

  var HomoSap = {
    a:0.3029549426680, c:0.1979883004921, g:0.1975473066391, t:0.3015094502008
  }

  print(">ONE Homo sapiens alu")
  fastaRepeat(2*n, ALU)

  print(">TWO IUB ambiguity codes")
  fastaRandom(3*n, IUB)

  print(">THREE Homo sapiens frequency")
  fastaRandom(5*n, HomoSap)
}).call(this, 1*arguments[0]*1)
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

function fib(n) {
    if (n < 2) return 1;
    return fib(n-2) + fib(n-1);
}

var n = arguments[0];
print(fib(n));

// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Isaac Gouy 

var n = arguments[0], partialSum = 0.0;
for (var d = 1; d <= n; d++) partialSum += 1.0/d;
print(partialSum.toFixed(9));

// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

var i, c = 0;
var n = arguments[0];

var X = new Object();
for (i=1; i<=n; i++) {
   X[i.toString(16)] = i;
}
for (i=n; i>0; i--) {
  if (X[i.toString()]) c++;
}
print(c);

// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

var n = arguments[0];
var hash1 = Object();
var hash2 = Object();
var arr = Array(10000);
var idx;

for (i=0; i<10000; i++) {
  idx = "foo_"+i;
  hash1[idx] = i;
  // Do this here and run loop below one less since += on an undefined
  // entry == NaN.
  hash2[idx] = hash1[idx];
}

for (i = 1; i < n; i++) {
  for(a in hash1) {
    hash2[a] += hash1[a];
  }
}

print(hash1["foo_1"], hash1["foo_9999"],
      hash2["foo_1"], hash2["foo_9999"]);
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

var IM = 139968;
var IA = 3877;
var IC = 29573;

var last = 42;

function gen_random(max) { return(max * (last = (last * IA + IC) % IM) / IM); }

function heapsort(n, ra) {
    var l, j, ir, i;
    var rra;

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


var n = arguments[0];
var ary, i;
    
// create an array of N random floats
ary = Array(n+1);
for (i=1; i<=n; i++) {
  ary[i] = gen_random(1.0);
}
heapsort(n, ary);
print(ary[n].toFixed(10));
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Isaac Gouy

print("hello world");

/*     
The Computer Language Shootout   
http://shootout.alioth.debian.org/  
Contributed by Jesse Millikan    
*/

// Return hash t with frequency in "n"
function frequency(seq, length){
 var m, i, t = {}, n = seq.length - length + 1

 for(i = 0; i < n; i++){
  m = seq.substr(i, length)
  t[m] = (t[m] || 0) + 1
 }

 t.n = n
 return t
}

function sort(seq, length){
 var f = frequency(seq, length), keys = [], k, i
 
 // Put all keys in key array in reverse
 for(k in f)
  if(k != 'n') keys.unshift(k)

 keys.sort(function(a, b){ return f[b] - f[a] })

 for(i in keys)
  print(keys[i].toUpperCase(), (f[keys[i]] * 100 / f.n).toFixed(3))

 print()
}

function find(seq, s){
 var f = frequency(seq, s.length)
 print((f[s] || 0) + "\t" + s.toUpperCase())
}

var seq="", l, others = ["ggt", "ggta", "ggtatt", "ggtattttaatt", "ggtattttaatttatagt"]

while(!readline().match(/^>THREE/)); // no body

while((l = readline()) && !l.match(/^>/))
 seq += l

sort(seq, 1)
sort(seq, 2)

for(i in others)
 find(seq, others[i])

/*
The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/
Contributed by Matthew Wilson
*/

var s="";while(!/^>TH/.test(readline()));for(var i;i=readline();s+=i);
var z,o={"ggt":3,"ggta":4,"ggtatt":6,"ggtattttaatt":12,"ggtattttaatttatagt":18};
function F(l,n,t){for(var m,i=0;i<n;++t[m=s.substring(i,++i+l-1)]||(t[m]=1));}
for(var l=1;l<3;++l) {
  var j,n=s.length-l+1,f={},keys=Array(Math.pow(4,l)),k,i=-1; F(l,n,f);
  for(k in f) keys[++i] = k; keys.sort(function(a, b){ return f[b] - f[a] });
  for(j=0;j<=i;print(keys[j].toUpperCase(),(f[keys[j++]]*100/n).toFixed(3)));
  print();
}
for(var i in o)F(z=o[i],s.length-z+1,z={}),print((z[i]||0)+"\t"+i.toUpperCase())
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by  Isaac Gouy

var SIZE = 10000;

function test_lists()
{
  var Li1, Li2, Li3;
  var tmp;
  // create a list of integers from 1 to SIZE.
  Li1 = new Array();
  for(tmp = 1; tmp <= SIZE; tmp++) Li1.push(tmp);
  // copy the list to Li2.
  Li2 = Li1.concat();

  // remove each element from left side of Li2 and append to
  // the right side of Li3 (preserving order)
  Li3 = new Array();

  while( (tmp = Li2.shift()) ) {
    Li3.push(tmp);
  } 

  // Li2 is now empty.
  // Remove each element from right side of Li3 and append to right
  // side of Li2
  while( (tmp = Li3.pop()) ) {
    Li2.push(tmp);
  } 

  // Li2 is now reversed, and Li3 empty.
  // Reverse Li1 in place.
  Li1.reverse();
  if( Li1[0] != SIZE ) return 0;
  // compare Li1 and Li2 for equality, and return the length of the list.
  for(tmp = 0; tmp < SIZE; tmp++)
    if( Li1[tmp] != Li2[tmp] ) return 0;
  return Li1.length;
}

var n = arguments[0];
var resultl

while( n-- )
  result = test_lists();
  
print(result );

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Thomas GODART (based on Greg Buchholz's C program) */

var w = 0; var h = 0; var bit_num = 0;
var byte_acc = 0;
var i = 0; var iter = 50;
var x = 0; var y = 0; var limit2 = 4;
var Zr = 0; var Zi = 0; var Cr = 0; var Ci = 0; var Tr = 0; var Ti = 0;

var h = 3000;
var w = h;

document.write ("P4\n" + w + " " + h + "\n");

for (y = 0 ; y < h ; y++)
{
   for (x = 0 ; x < w ; x++)
   {
      Zr = 0; Zi = 0; Tr = 0; Ti = 0.0;

      Cr = (2.0 * x / w - 1.5); Ci = (2.0 * y / h - 1.0);

      for (i = 0 ; i < iter && (Tr + Ti <= limit2) ; i++)
      {
         Zi = 2.0 * Zr * Zi + Ci;
         Zr = Tr - Ti + Cr;
         Tr = Zr * Zr;
         Ti = Zi * Zi;
      }

      byte_acc = byte_acc << 1;
      if (Tr + Ti <= limit2) byte_acc = byte_acc | 1;

      bit_num++;

      if (bit_num == 8)
      {
         document.write (String.fromCharCode(byte_acc));
         byte_acc = 0;
         bit_num = 0;
      }
      else if (x == w - 1)
      {
         byte_acc = byte_acc << (8 - w % 8);
         document.write (String.fromCharCode(byte_acc));
         byte_acc = 0;
         bit_num = 0;
      }
   }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Thomas GODART (based on Greg Buchholz's C program)
   modified by TA
*/
var i, x, y,
    bit_num = 0,
    byte_acc = 0,
    iter = 50,
    limit = 4,
    Zr, Zi, Cr, Ci, Tr, Ti,
    d = +arguments[0];

print("P4\n" + d + " " + d + "\n");

for (y = 0; y < d; y += 1) {
  for (x = 0; x < d; x += 1) {
    Zr = 0,
    Zi = 0,
    Tr =0,
    Ti =0,
    Cr = 2 * x / d - 1.5,
    Ci = 2 * y / d - 1;

    for (i = 0; i < iter && Tr + Ti <= limit; i += 1) {
      Zi = 2 * Zr * Zi + Ci,
      Zr = Tr - Ti + Cr,
      Tr = Zr * Zr,
      Ti = Zi * Zi;
    }

    byte_acc <<= 1;

    if (Tr + Ti <= limit) {
      byte_acc |=  1;
    }

    bit_num += 1;

    if (bit_num === 8) {
      print(String.fromCharCode(byte_acc));
      byte_acc = 0,
      bit_num = 0;
    } else if (x === d - 1) {
      byte_acc <<= 8 - d % 8;
      print(String.fromCharCode(byte_acc));
      byte_acc = 0,
      bit_num = 0;
    }
  }
}
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

var SIZE=30;

function mkmatrix(rows, cols) {
  var i, j, count = 1;
  var m = new Array(rows);
  for (i = 0; i < rows; i++) {
    m[i] = new Array(cols);
    for (j = 0; j < cols; j++) {
      m[i][j] = count++;
    }
  }
  return m;
}

function mmult(rows, cols,  m1, m2, m3) {
  var i, j, k, val;
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      val = 0;
      for (k = 0; k < cols; k++) {
	val += m1[i][k] * m2[k][j];
      }
      m3[i][j] = val;
    }
  }
  return m3;
}

var n = arguments[0];
var i;
var m1 = mkmatrix(SIZE, SIZE);
var m2 = mkmatrix(SIZE, SIZE);
var mm = mkmatrix(SIZE, SIZE);

for (i = 0; i < n; i++) {
  mmult(SIZE, SIZE, m1, m2, mm);
}
print(mm[0][0], mm[2][3], mm[3][2], mm[4][4]);
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Sjoerd Visscher


function Toggle(start_state) {
   this.state = start_state;
}

Toggle.prototype.value = function() {
   return this.state;
}

Toggle.prototype.activate = function() {
   this.state = !this.state;
   return this;
}


function NthToggle (start_state, max_counter) {
   Toggle.call(this, start_state);
   this.count_max = max_counter;
   this.count = 0;
}

NthToggle.prototype = new Toggle;

NthToggle.prototype.activate = function() {
   if (++this.count >= this.count_max) {
     this.state = !this.state;
     this.count = 0;
   }
   return this;
}

var n = arguments[0];
var i;
var val = true;
var toggle = new Toggle(val);
for (i=0; i<n; i++) {
  val = toggle.activate().value();
}
print(toggle.value() ? "true" : "false");

val = true;
var ntoggle = new NthToggle(val, 3);
for (i=0; i<n; i++) {
  val = ntoggle.activate().value();
}
print(ntoggle.value() ? "true" : "false");
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy */

var PI = 3.141592653589793;
var SOLAR_MASS = 4 * PI * PI;
var DAYS_PER_YEAR = 365.24;

function Body(x,y,z,vx,vy,vz,mass){
   this.x = x;
   this.y = y;
   this.z = z;
   this.vx = vx;
   this.vy = vy;
   this.vz = vz;
   this.mass = mass;
}

Body.prototype.offsetMomentum = function(px,py,pz) {
   this.vx = -px / SOLAR_MASS;
   this.vy = -py / SOLAR_MASS;
   this.vz = -pz / SOLAR_MASS;
   return this;
}

function Jupiter(){
   return new Body(
      4.84143144246472090e+00,
      -1.16032004402742839e+00,
      -1.03622044471123109e-01,
      1.66007664274403694e-03 * DAYS_PER_YEAR,
      7.69901118419740425e-03 * DAYS_PER_YEAR,
      -6.90460016972063023e-05 * DAYS_PER_YEAR,
      9.54791938424326609e-04 * SOLAR_MASS
   );
}

function Saturn(){
   return new Body(
      8.34336671824457987e+00,
      4.12479856412430479e+00,
      -4.03523417114321381e-01,
      -2.76742510726862411e-03 * DAYS_PER_YEAR,
      4.99852801234917238e-03 * DAYS_PER_YEAR,
      2.30417297573763929e-05 * DAYS_PER_YEAR,
      2.85885980666130812e-04 * SOLAR_MASS
   );
}

function Uranus(){
   return new Body(
      1.28943695621391310e+01,
      -1.51111514016986312e+01,
      -2.23307578892655734e-01,
      2.96460137564761618e-03 * DAYS_PER_YEAR,
      2.37847173959480950e-03 * DAYS_PER_YEAR,
      -2.96589568540237556e-05 * DAYS_PER_YEAR,
      4.36624404335156298e-05 * SOLAR_MASS
   );
}

function Neptune(){
   return new Body(
      1.53796971148509165e+01,
      -2.59193146099879641e+01,
      1.79258772950371181e-01,
      2.68067772490389322e-03 * DAYS_PER_YEAR,
      1.62824170038242295e-03 * DAYS_PER_YEAR,
      -9.51592254519715870e-05 * DAYS_PER_YEAR,
      5.15138902046611451e-05 * SOLAR_MASS
   );
}

function Sun(){
   return new Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SOLAR_MASS);
}


function NBodySystem(bodies){
   this.bodies = bodies;
   var px = 0.0;
   var py = 0.0;
   var pz = 0.0;
   var size = this.bodies.length;
   for (var i=0; i<size; i++){
      var b = this.bodies[i];
      var m = b.mass;
      px += b.vx * m;
      py += b.vy * m;
      pz += b.vz * m;
   }
   this.bodies[0].offsetMomentum(px,py,pz);
}

NBodySystem.prototype.advance = function(dt){
   var dx, dy, dz, distance, mag;
   var size = this.bodies.length;

   for (var i=0; i<size; i++) {
      var bodyi = this.bodies[i];
      for (var j=i+1; j<size; j++) {
         var bodyj = this.bodies[j];
         dx = bodyi.x - bodyj.x;
         dy = bodyi.y - bodyj.y;
         dz = bodyi.z - bodyj.z;

         distance = Math.sqrt(dx*dx + dy*dy + dz*dz);
         mag = dt / (distance * distance * distance);

         bodyi.vx -= dx * bodyj.mass * mag;
         bodyi.vy -= dy * bodyj.mass * mag;
         bodyi.vz -= dz * bodyj.mass * mag;

         bodyj.vx += dx * bodyi.mass * mag;
         bodyj.vy += dy * bodyi.mass * mag;
         bodyj.vz += dz * bodyi.mass * mag;
      }
   }

   for (var i=0; i<size; i++) {
      var body = this.bodies[i];
      body.x += dt * body.vx;
      body.y += dt * body.vy;
      body.z += dt * body.vz;
   }
}

NBodySystem.prototype.energy = function(){
   var dx, dy, dz, distance;
   var e = 0.0;
   var size = this.bodies.length;

   for (var i=0; i<size; i++) {
      var bodyi = this.bodies[i];

      e += 0.5 * bodyi.mass *
         ( bodyi.vx * bodyi.vx
         + bodyi.vy * bodyi.vy
         + bodyi.vz * bodyi.vz );

      for (var j=i+1; j<size; j++) {
         var bodyj = this.bodies[j];
         dx = bodyi.x - bodyj.x;
         dy = bodyi.y - bodyj.y;
         dz = bodyi.z - bodyj.z;

         distance = Math.sqrt(dx*dx + dy*dy + dz*dz);
         e -= (bodyi.mass * bodyj.mass) / distance;
      }
   }
   return e;
}


var n = arguments[0];
var bodies = new NBodySystem( Array( 
   Sun(),Jupiter(),Saturn(),Uranus(),Neptune() 
));

print(bodies.energy().toFixed(9));
for (var i=0; i<n; i++){ bodies.advance(0.01); }
print(bodies.energy().toFixed(9));
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

var n = arguments[0];
var x=0;
var a=n;

// Using while() is faster than for()
while(a--) {
   var b=n; while(b--) {
      var c=n; while(c--) {
         var d=n; while(d--) {
            var e=n; while(e--) {
               var f=n; while(f--) {
                  x++;
               }
            }
         }
      }
   }
}
print(x);
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// modified by Isaac Gouy


function pad(number,width){
   var s = number.toString();
   var prefixWidth = width - s.length;
   if (prefixWidth>0){
      for (var i=1; i<=prefixWidth; i++) s = " " + s;
   }
   return s;
}

function nsieve(m, isPrime){
   var i, k, count;

   for (i=2; i<=m; i++) { isPrime[i] = true; }
   count = 0;

   for (i=2; i<=m; i++){
      if (isPrime[i]) {
         for (k=i+i; k<=m; k+=i) isPrime[k] = false;
         count++;
      }
   }
   return count;
}


var n = arguments[0];
if (n<2) n = 2;

var m = (1<<n)*10000;
var flags = Array(m+1);

print("Primes up to " + pad(m,8) + " " +  pad(nsieve(m,flags),8));

m = (1<<n-1)*10000;
print("Primes up to " + pad(m,8) + " " +  pad(nsieve(m,flags),8));

m = (1<<n-2)*10000;
print("Primes up to " + pad(m,8) + " " +  pad(nsieve(m,flags),8));

// The Great Computer Language Shootout
//  http://shootout.alioth.debian.org
//
//  Contributed by Ian Osgood

function pad(n,width) {
  var s = n.toString();
  while (s.length < width) s = ' ' + s;
  return s;
}

function primes(isPrime, n) {
  var i, count = 0, m = 10000<<n, size = m+31>>5;

  for (i=0; i<size; i++) isPrime[i] = 0xffffffff;
  
  for (i=2; i<m; i++)
    if (isPrime[i>>5] & 1<<(i&31)) {
      for (var j=i+i; j<m; j+=i)
        isPrime[j>>5] &= ~(1<<(j&31));
      count++;
    }

  print("Primes up to" + pad(m,9) + pad(count,9));
}

var n = arguments[0]
var isPrime = new Array((10000<<n)+31>>5)

primes(isPrime, n)
primes(isPrime, n-1)
primes(isPrime, n-2)
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Sjoerd Visscher


function Toggle(start_state) {
   this.state = start_state;
}

Toggle.prototype.value = function() {
   return this.state;
}

Toggle.prototype.activate = function() {
   this.state = !this.state;
   return this;
}


function NthToggle (start_state, max_counter) {
   Toggle.call(this, start_state);
   this.count_max = max_counter;
   this.count = 0;
}

NthToggle.prototype = new Toggle;

NthToggle.prototype.activate = function() {
   if (++this.count >= this.count_max) {
     this.state = !this.state;
     this.count = 0;
   }
   return this;
}


var n = arguments[0];
var i;

var toggle = new Toggle(true);
for (i = 0; i < 5; i++) {
  toggle.activate();
  print(toggle.value() ? "true" : "false");
}
for (i = 0; i < n; i++) {
  toggle = new Toggle(true);
}

print("");

var ntoggle = new NthToggle(1, 3);
for (i = 0; i < 8; i++) {
  ntoggle.activate();
  print((ntoggle.value()) ? "true" : "false");
}
for (i = 0; i < n; i++) {
   ntoggle = new NthToggle(1, 3);
}
// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// contributed by Isaac Gouy

var n = arguments[0]; 
var a1 = a2 = a3 = a4 = a5 = a6 = a7 = a8 = a9 = 0.0;   
var twothirds = 2.0/3.0;
var alt = -1.0;
var k2 = k3 = sk = ck = 0.0;

for (var k = 1; k <= n; k++){
   k2 = k*k;
   k3 = k2*k;
   sk = Math.sin(k);
   ck = Math.cos(k);
   alt = -alt;

   a1 += Math.pow(twothirds,k-1);
   a2 += Math.pow(k,-0.5);
   a3 += 1.0/(k*(k+1.0));
   a4 += 1.0/(k3 * sk*sk);
   a5 += 1.0/(k3 * ck*ck);
   a6 += 1.0/k;
   a7 += 1.0/k2;
   a8 += alt/k;
   a9 += alt/(2*k -1);
}
print(a1.toFixed(9) + "\t(2/3)^k");
print(a2.toFixed(9) + "\tk^-0.5");
print(a3.toFixed(9) + "\t1/k(k+1)");
print(a4.toFixed(9) + "\tFlint Hills");
print(a5.toFixed(9) + "\tCookson Hills");
print(a6.toFixed(9) + "\tHarmonic");
print(a7.toFixed(9) + "\tRiemann Zeta");
print(a8.toFixed(9) + "\tAlternating Harmonic");
print(a9.toFixed(9) + "\tGregory");

// The Computer Language Benchmarks Game
//  http://shootout.alioth.debian.org
//
//  Contributed by Matthew Wilson 
//  biginteger derived from Tom Wu's jsbn.js


var compareTo, multiply, divide, addTo, add, intValue, shiftLeft, nbv;

function main($n) {
  var $i=1, $s="", $d, neg10=nbv(-10), three=nbv(3), ten=nbv(10), g = 1, $g,
  digits=Array(10), $z0=nbv(1), $z1=nbv(0), $z2=nbv(1), negdigits=Array(10),
  k = 0, $k, l = 2, $l, a;
  
  for(var i=0; i<10; ++i) { negdigits[i] = multiply(digits[i] = nbv(i),neg10) }
  
  do {
    while ( compareTo($z0,$z2) > 0
         || ($d = intValue(divide(add(multiply($z0,three),$z1),$z2))) != 
             intValue(divide(add(shiftLeft($z0,2),$z1),$z2))
    ) {
      $z1 = multiply($z1,$g = nbv(g+=2));
      $z2 = multiply($z2,$g);
      addTo($z1, multiply($z0,$l = nbv(l+=4)), $z1);
      $z0 = multiply($z0,$k = nbv(++k));
    }
    $z0 = multiply($z0,ten);
    $z1 = multiply($z1,ten);
    addTo($z1, multiply($z2,negdigits[$d]), $z1);
    $s += $d;
    
    if ($i % 10 == 0) { print($s+"\t:"+$i); $s="" }
  } while (++$i <= $n)
  
  if (($i = $n % 10) != 0) { $s += Array(11-$i).join(' ') }
  if ($s.length > 0) { print($s+"\t:"+$n) }
}

var functions;
load('/home/dunham/shootout/bench/Include/javascript/biginteger.js');

compareTo=functions[0];
multiply=functions[1];
divide=functions[2];
addTo=functions[3];
add=functions[4];
nbv=functions[5];
shiftLeft=functions[6];
intValue=functions[7];

main.call(this, 1*arguments[0]*1)
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Ian Osgood 

var last = 42;
var A = 3877;
var C = 29573;
var M = 139968;

function rand(max) {
  last = (last * A + C) % M;
  return max * last / M;
}

var n = arguments[0];
for (var i=1; i<n; i++) rand(100);
print(rand(100).toFixed(9));
// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// contributed by Isaac Gouy

function ack(m,n){
   if (m==0) { return n+1; }
   if (n==0) { return ack(m-1,1); }
   return ack(m-1, ack(m,n-1) );
}

function fib(n) {
    if (n < 2){ return 1; }
    return fib(n-2) + fib(n-1);
}

function tak(x,y,z) {
  if (y >= x) return z;
  return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y));
}

var n = parseInt(arguments[0]);
print("Ack(3," + n + "): " + ack(3,n));
print("Fib(" + (27.0+n).toFixed(1) + "): " + fib(27.0+n).toFixed(1));
n--; print("Tak(" + 3*n + "," + 2*n + "," + n + "): " + tak(3*n,2*n,n));
print("Fib(3): " + fib(3));
print("Tak(3.0,2.0,1.0): " + tak(3.0,2.0,1.0).toFixed(1));
// The Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Jesse Millikan
// Base on the Ruby version by jose fco. gonzalez

var l, input = "", ilen, clen, 
 seqs = [
  /agggtaaa|tttaccct/ig,
  /[cgt]gggtaaa|tttaccc[acg]/ig,
  /a[act]ggtaaa|tttacc[agt]t/ig,
  /ag[act]gtaaa|tttac[agt]ct/ig,
  /agg[act]taaa|ttta[agt]cct/ig,
  /aggg[acg]aaa|ttt[cgt]ccct/ig,
  /agggt[cgt]aa|tt[acg]accct/ig,
  /agggta[cgt]a|t[acg]taccct/ig,
  /agggtaa[cgt]|[acg]ttaccct/ig],
 subs = { 
  B: '(c|g|t)', D: '(a|g|t)', H: '(a|c|t)', K: '(g|t)', 
  M: '(a|c)', N: '(a|c|g|t)', R: '(a|g)', S: '(c|t)', 
  V: '(a|c|g)', W: '(a|t)', Y: '(c|t)' }
 
// readline strips the newline...
while(l = readline()) input += l + "\n"
ilen = input.length

// There is no in-place substitution
input = input.replace(/>.*\n|\n/g,"")
clen = input.length

for(i in seqs)
 print(seqs[i].source, (input.match(seqs[i]) || []).length)
 // match returns null if no matches, so replace with empty

for(k in subs)
 input = input.replace(k, subs[k], "g")
 // search string, replacement string, flags

print()
print(ilen)
print(clen)
print(input.length)

// The Computer Language Benchmarks Game
// http://shootout.alioth.debian.org/
//
// contributed by Jesse Millikan
// Base on the Ruby version by jose fco. gonzalez
// fixed by Matthew Wilson

var i = "", ilen, clen, j, q = [ /agggtaaa|tttaccct/ig,
  /[cgt]gggtaaa|tttaccc[acg]/ig, /a[act]ggtaaa|tttacc[agt]t/ig,
  /ag[act]gtaaa|tttac[agt]ct/ig, /agg[act]taaa|ttta[agt]cct/ig,
  /aggg[acg]aaa|ttt[cgt]ccct/ig, /agggt[cgt]aa|tt[acg]accct/ig,
  /agggta[cgt]a|t[acg]taccct/ig, /agggtaa[cgt]|[acg]ttaccct/ig],
  b = [ /B/g, '(c|g|t)', /D/g, '(a|g|t)', /H/g, '(a|c|t)', /K/g, '(g|t)',
  /M/g, '(a|c)', /N/g, '(a|c|g|t)', /R/g, '(a|g)', /S/g, '(c|g)',
  /V/g, '(a|c|g)', /W/g, '(a|t)', /Y/g, '(c|t)']

while(j = readline()) i+=j+"\n"; ilen = i.length

i = i.replace(/^>.*\n|\n/mg, ''); clen = i.length

for(j = 0; j<q.length; ++j) print(q[j].source, (i.match(q[j]) || []).length)

for(j = -1; j<b.length - 1;) i = i.replace(b[++j], b[++j])

print(["", ilen, clen, i.length].join("\n"))
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Jos Hirth
*/

var line, out, reverseFormat, complement;

complement = {
   y: 'R',
   v: 'B',
   w: 'W',
   t: 'A',
   u: 'A',
   r: 'Y',
   s: 'S',
   n: 'N',
   m: 'K',
   k: 'M',
   h: 'D',
   g: 'C',
   d: 'H',
   b: 'V',
   c: 'G',
   a: 'T',
   Y: 'R',
   V: 'B',
   W: 'W',
   T: 'A',
   U: 'A',
   R: 'Y',
   S: 'S',
   N: 'N',
   M: 'K',
   K: 'M',
   H: 'D',
   G: 'C',
   D: 'H',
   B: 'V',
   C: 'G',
   A: 'T'
};

reverseFormat = function (a, complement) {
   var i, l, line, c = 1, out;
   out = '';
   for (l = a.length; l--;) {
      line = a[l];
      for (i = line.length; i--; c++) {
         out += complement[line[i]];
         if (c === 60) {
            print(out);
            out = '';
            c = 0;
         }
      }
   }
   if (out.length) {
      print(out);
   }
};

out = [];
while ((line = readline())) {
   if (line[0] !== '>') {
      out.push(line);
   } else {
      reverseFormat(out, complement);
      out = [];
      print(line);
   }
}

reverseFormat(out, complement);
/*
The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/
Contributed by Matthew Wilson
*/

(function(complement,print,readline) {
  var l, seq="";
  print(l = readline());
  for (;;) { try {
    if ((l = readline()).length == 60) {
      seq += l;
    } else if (/^>/.test(l)) {
      complement(seq);
      seq = "";
      print(l);
    } else {
      seq += l;
    }
  } catch(e){
    if (typeof(seq)!='undefined' && seq.length > 0) {
      complement(seq);
    }
    break;
  }}
})((function(complement,print) {
  return function(seq) {
    var l = seq.length;
    for (;;) {
      var line="";
      if (l >= 60) {
        for (var i=l-1, j=l-61; i>j; --i) {
          line += complement[seq.charCodeAt(i)]
        }
        l-=60;
        print(line);
      } else if (l > 0) {
        for (var i=l-1; i>-1; --i) {
          line += complement[seq.charCodeAt(i)]
        }
        print(line);
        break;
      } else {
        break;
      }
    }
  }
})((function() {
  var complement=[],
    keys ='WSATUGCYRKMBDHVNwsatugcyrkmbdhvn',
    comps='WSTAACGRYMKVHDBNWSTAACGRYMKVHDBN';
  for(var i=0; i<32; ++i)
    complement[keys.charCodeAt(i)]
      = comps[i];
  
  return complement;
})(), print), print, readline)
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by David Hedbor
// modified by Isaac Gouy

var flags, flagsorig = Array(8193);
var i, k, count;
var n = arguments[0];

for (i = 2; i <= 8192; i++) {  flagsorig[i] = 1; }
while (n--) {
  count = 0;
  flags = flagsorig.concat();
  for (i = 2; i <= 8192; i++) {
    if (flags[i]) {
      for (k=i+i; k <= 8192; k+=i)
	flags[k] = 0;
      count++;
    }
  }
}

print("Count:", count);
// The Great Computer Language Shootout
//  http://shootout.alioth.debian.org
//
//  Contributed by Ian Osgood

function padded(n,width) {
  var s = n.toString();
  while (s.length < width) s = ' ' + s;
  return s;
}

function primes(e) {
  var i, count = 0, n = 10000 << e;
  var isPrime = new Array(n);

  for (i=0; i<n; i++) isPrime[i] = true;
  
  for (i=2; i<n; i++)
    if (isPrime[i]) {
      for (var j=i+i; j<n; j+=i) isPrime[j] = false;
      count++;
    }

  print("Primes up to" + padded(n,9) + padded(count,9));
}

var n = arguments[0]

primes(n)
primes(n-1)
primes(n-2)
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Ian Osgood

function A(i,j) {
  return 1/((i+j)*(i+j+1)/2+i+1);
}

function Au(u,v) {
  for (var i=0; i<u.length; ++i) {
    var t = 0;
    for (var j=0; j<u.length; ++j)
      t += A(i,j) * u[j];
    v[i] = t;
  }
}

function Atu(u,v) {
  for (var i=0; i<u.length; ++i) {
    var t = 0;
    for (var j=0; j<u.length; ++j)
      t += A(j,i) * u[j];
    v[i] = t;
  }
}

function AtAu(u,v,w) {
  Au(u,w);
  Atu(w,v);
}

function spectralnorm(n) {
  var i, u=[], v=[], w=[], vv=0, vBv=0;
  for (i=0; i<n; ++i) {
    u[i] = 1; v[i] = w[i] = 0; 
  }
  for (i=0; i<10; ++i) {
    AtAu(u,v,w);
    AtAu(v,u,w);
  }
  for (i=0; i<n; ++i) {
    vBv += u[i]*v[i];
    vv  += v[i]*v[i];
  }
  return Math.sqrt(vBv/vv);
}

print(spectralnorm(arguments[0]).toFixed(9));
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Isaac Gouy

var n = arguments[0];
var str = new String("");
while(n--){ str += "hello\n"; }
print(str.length);
/*     
The Computer Language Shootout   
http://shootout.alioth.debian.org/  
Contributed by Jesse Millikan    
*/

t = 0
while(l = readline()) t+=parseInt(l)
print(t)
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// contributed by Ian Osgood

function tak(x,y,z) {
  if (y >= x) return z;
  return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y));
}
var n = arguments[0];
print( tak(n*3, n*2, n).toFixed(1) );
