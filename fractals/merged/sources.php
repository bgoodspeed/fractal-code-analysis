#!/usr/bin/php -f
<?php
/*
 $Id: ackermann.php,v 1.2 2004-10-11 04:47:17 bfulgham Exp $
 http://www.bagley.org/~doug/shootout/
 from Alexander Klimov
*/
function Ack($m, $n){
  if($m == 0) return $n+1;
  if($n == 0) return Ack($m-1, 1);
  return Ack($m - 1, Ack($m, ($n - 1)));
}
$n = ($argc == 2) ? $argv[1] : 1;
$r = Ack(3,$n);
print "Ack(3,$n): $r\n";
?>
<?
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q ary.php 9000
*/ 


$n = ($argc == 2) ? $argv[1] : 1;

for ($i=0; $i<$n; $i++) $x[$i] = $i + 1;
    
$y = array_pad(array(),$n,0);
for ($k=0; $k<1000; $k++){
   $j = $n;
   while ($j--) $y[$j] += $x[$j];
}

printf("%d %d\n", $y[0], $y[$n-1]);

?>
<?php 
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Peter Baltruschat
   modified by Levi Cameron
*/

function bottomUpTree($item, $depth)
{
   if (!$depth) return array(null,null,$item);
   $item2 = $item + $item;
   $depth--;
   return array(
      bottomUpTree($item2-1,$depth),
      bottomUpTree($item2,$depth),
      $item);
}

function itemCheck($treeNode) { 
   return $treeNode[2]
      + ($treeNode[0][0] === null ? itemCheck($treeNode[0]) : $treeNode[0][2])
      - ($treeNode[1][0] === null ? itemCheck($treeNode[1]) : $treeNode[1][2]);
}

$minDepth = 4;

$n = ($argc == 2) ? $argv[1] : 1;
$maxDepth = max($minDepth + 2, $n);
$stretchDepth = $maxDepth + 1;

$stretchTree = bottomUpTree(0, $stretchDepth);
printf("stretch tree of depth %d\t check: %d\n",
$stretchDepth, itemCheck($stretchTree));
unset($stretchTree);

$longLivedTree = bottomUpTree(0, $maxDepth);

$iterations = 1 << ($maxDepth);
do
{
   $check = 0;
   for($i = 1; $i <= $iterations; ++$i)
   {
      $t = bottomUpTree($i, $minDepth);
      $check += itemCheck($t);
      unset($t);
      $t = bottomUpTree(-$i, $minDepth);
      $check += itemCheck($t);
      unset($t);
   }
   
   printf("%d\t trees of depth %d\t check: %d\n", $iterations<<1, $minDepth, $check);
   
   $minDepth += 2;
   $iterations >>= 2;
}
while($minDepth <= $maxDepth);

printf("long lived tree of depth %d\t check: %d\n",
$maxDepth, itemCheck($longLivedTree));
?>
<? /* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/
   
   contributed by Isaac Gouy (PHP novice)
   
   php -q dispatch.php 10
*/


class BottleState {
   function Next($b){}
   function Tag(){}
   
   function InitialState(){ global $Empty; return $Empty; }   
}

class EmptyState extends BottleState {
   function Next($b){ global $Full; $b->State($Full); }
   function Tag(){ return 1; }
}

class FullState extends BottleState  {
   function Next($b){ global $Sealed; $b->State($Sealed); }
   function Tag(){ return 2; }
}

class SealedState extends BottleState  {
   function Next($b){ global $Empty; $b->State($Empty); }
   function Tag(){ return 3; }
}

$Empty = new EmptyState;
$Full = new FullState;
$Sealed = new SealedState;


class PressurizedBottleState extends BottleState {
   function InitialState(){  global $UnpressurizedEmpty; return $UnpressurizedEmpty; }  
}

class UnpressurizedEmptyState extends PressurizedBottleState {
   function Next($b){ global $UnpressurizedFull; $b->State($UnpressurizedFull); }
   function Tag(){ return 4; }
}

class UnpressurizedFullState extends PressurizedBottleState {
   function Next($b){ global $PressurizedUnsealed; $b->State($PressurizedUnsealed); }
   function Tag(){ return 5; }
}

class PressurizedUnsealedState extends PressurizedBottleState {
   function Next($b){ global $PressurizedSealed; $b->State($PressurizedSealed); }
   function Tag(){ return 6; }
}

class PressurizedSealedState extends PressurizedBottleState {
   function Next($b){ global $UnpressurizedEmpty; $b->State($UnpressurizedEmpty); }
   function Tag(){ return 7; }
}

$UnpressurizedEmpty = new UnpressurizedEmptyState;
$UnpressurizedFull = new UnpressurizedFullState;
$PressurizedUnsealed = new PressurizedUnsealedState;
$PressurizedSealed = new PressurizedSealedState;


class Bottle {
   var $bottleState, $id;
      
   function Bottle($id){
      $this->id = $id;
      $this->bottleState = $this->InitialState();   
   } 
   
   function State($s){
      $this->bottleState = $s;
   }     

   function Cycle(){
      $this->Fill(); $this->Seal(); $this->Empty_();
   }   
   
   function InitialState(){
      return BottleState::InitialState();
   }      

   function Fill(){
      $this->bottleState ->Next($this);
   } 
         
   function Seal(){
      $this->bottleState ->Next($this);
   }
      
   function Empty_(){
      $this->bottleState ->Next($this);
   }    
   
   function Check($c){
      return $this->bottleState->Tag() + $this->id + $c;
   }          
}


class PressurizedBottle extends Bottle {

   function PressurizedBottle($id){
      Bottle::Bottle($id);    
   }
      
   function InitialState(){
      return PressurizedBottleState::InitialState();
   }  

   function Cycle(){
      $this->Fill(); $this->Pressurize(); $this->Seal(); $this->Empty_();
   } 
   
   function Pressurize(){
      $this->bottleState ->Next($this);
   }    
}


function BottleCheck($a1, $a2, $a3, $a4, $a5, $i){
   $a1->Cycle(); $a2->Cycle(); $a3->Cycle(); $a4->Cycle(); $a5->Cycle();
   $c = $i % 2;
   return $a1->Check($c) + $a2->Check($c) + $a3->Check($c) 
      + $a4->Check($c) + $a5->Check($c);
}


$n = $argv[1];

$b1 = new Bottle(1); $b2 = new Bottle(2);
$b3 = new Bottle(3); $b4 = new Bottle(4);
$b5 = new Bottle(5); $b6 = new Bottle(6);
$b7 = new Bottle(7); $b8 = new Bottle(8);
$b9 = new Bottle(9); $b0 = new Bottle(0);

$p1 = new PressurizedBottle(1); $p2 = new PressurizedBottle(2);
$p3 = new PressurizedBottle(3); $p4 = new PressurizedBottle(4);
$p5 = new PressurizedBottle(5); $p6 = new PressurizedBottle(6);
$p7 = new PressurizedBottle(7); $p8 = new PressurizedBottle(8);
$p9 = new PressurizedBottle(9); $p0 = new PressurizedBottle(0);

$check = 0;
for ($i=1; $i<=$n; $i++){
   $check += BottleCheck($b1,$b2,$b3,$b4,$b5,$i);
   $check += BottleCheck($b6,$b7,$b8,$b9,$b0,$i);

   $check += BottleCheck($p1,$p2,$p3,$p4,$p5,$i);
   $check -= BottleCheck($p6,$p7,$p8,$p9,$p0,$i);
}
print "$check";
?>
<? 
/* 
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy
   multicore by anon
 */

function Fannkuch($n,$_i,$print){
   $check = 0;
   $perm = array();
   $perm1 = array();
   $count = array();
   $maxFlipsCount = 0;
   $m = $n - 1;

   $perm1 = range(0, $n-1);
   $r = $n;

   if (!$print) {
      $perm1[$_i] = $n - 1;
      $perm1[$n-1] = $_i;
   }

   while (TRUE) {
      // write-out the first 30 permutations
      if ($print){

         foreach($perm1 as $v) echo $v+1;
         echo "\n";
         if (++$check >= 30) {
            return Fannkuch($n, $_i, FALSE);
         }
      }

      while ($r !== 1){
         $count[$r-1] = $r--;
      }
      if ($perm1[0] !== 0 && $perm1[$m] !== $m){
         $perm = $perm1;
         $flipsCount = 0;

         $k = $perm[0];
         do {
            $i = 1;
            $j = $k - 1;
            while ($i < $j) {
               $tmp = $perm[$i];
               $perm[$i++] = $perm[$j];
               $perm[$j--] = $tmp;
            }
            ++$flipsCount;
            $tmp = $perm[$k];
            $perm[$k] = $k;
         } while ($k = $tmp);

         if ($flipsCount > $maxFlipsCount) {
            $maxFlipsCount = $flipsCount;
         }
      }

      do {
         if ($r >= $m) return $maxFlipsCount;
         $perm0 = $perm1[0];
         $i = 0;
         while ($i < $r) {
            $perm1[$i++] = $perm1[$i];
         }
         $perm1[$r] = $perm0;

         if (--$count[$r] > 0) break;
         ++$r;
      } while(TRUE);
   }
}
function pipe() {
   // socketpair(2), alternative to pipe(2)
   return stream_socket_pair(STREAM_PF_UNIX, STREAM_SOCK_STREAM, 0);
}

$n = (int) $argv[1];

// fork() one process for each $n
$pipes = array();
for ($i = 1; $i < $n - 1; ++$i) {
   $pipe = pipe();
   $pipes[] = $pipe[0];
   $pipe = $pipe[1];
   $pid = pcntl_fork();
   if ($pid === -1) {
      die('could not fork');
   } else if ($pid) {
      continue;
   }
   $result = Fannkuch($n,$i,FALSE);
   fwrite($pipe, pack('N', $result));
   exit(0);
}

$result = Fannkuch($n,0,TRUE);

// gather & print result
foreach($pipes as $pipe) {
   $data = fread($pipe, 4);
   $data = unpack('N', $data);
   $data = $data[1];

   if ($data > $result) {
      $result = $data;
   }

   pcntl_wait($s);
}

printf("Pfannkuchen(%d) = %d\n", $n, $result);

<? /* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   
   contributed by Isaac Gouy, transliterated from Mike Pall's Lua program 
*/

function Fannkuch($n){
   $p = $q = $s = array(); 
   $sign = 1; $maxflips = $sum = 0; $m = $n-1;
   for ($i=0; $i<$n; $i++){ $p[$i] = $i; $q[$i] = $i; $s[$i] = $i; }      
   do {
      // Copy and flip.
      $q0 = $p[0];                                          // Cache 0th element.
      if ($q0 != 0){
         for($i=1; $i<$n; $i++) $q[$i] = $p[$i];            // Work on a copy.
         $flips = 1;
         do { 
            $qq = $q[$q0]; 
            if ($qq == 0){                                  // ... until 0th element is 0.
               $sum += $sign*$flips;
	       if ($flips > $maxflips) $maxflips = $flips;  // New maximum?
               break; 
            } 
 	    $q[$q0] = $q0; 
	    if ($q0 >= 3){
	       $i = 1; $j = $q0 - 1;
               do { $t = $q[$i]; $q[$i] = $q[$j]; $q[$j] = $t; $i++; $j--; } while ($i < $j); 
            }
	    $q0 = $qq; $flips++;
         } while (true); 
      }
      // Permute.
      if ($sign == 1){
         $t = $p[1]; $p[1] = $p[0]; $p[0] = $t; $sign = -1; // Rotate 0<-1.
      } else { 
         $t = $p[1]; $p[1] = $p[2]; $p[2] = $t; $sign = 1;  // Rotate 0<-1 and 0<-1<-2.
         for($i=2; $i<$n; $i++){ 
	    $sx = $s[$i];
	    if ($sx != 0){ $s[$i] = $sx-1; break; }
	    if ($i == $m) return array($sum,$maxflips);     // Out of permutations.
	    $s[$i] = $i;
	    // Rotate 0<-...<-i+1.
	    $t = $p[0]; for($j=0; $j<=$i; $j++){ $p[$j] = $p[$j+1]; } $p[$i+1] = $t;
         }
      }
   } while (true);
}

$n = $argv[1];
list($checksum,$pf) = Fannkuch($n);
printf("%d\nPfannkuchen(%d) = %d", $checksum, $n, $pf);
?>

<?php
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Wing-Chung Leung
   modified by Isaac Gouy
*/

# error_reporting(E_STRICT);

define ('IM', 139968);
define ('IA', 3877);
define ('IC', 29573);

function gen_random($max) {
   static $last = 42;
   return $max * ($last = ($last * IA + IC) % IM) / IM;
}

/* Weighted selection from alphabet */

function makeCumulative(&$genelist) {
   $count = count($genelist);
   for ($i=1; $i < $count; $i++) {
      $genelist[$i][1] += $genelist[$i-1][1];
   }
}


function selectRandom(&$a) {
   $r = gen_random(1);
   $hi = sizeof($a);

   for ($i = 0; $i < $hi; $i++) {
      if ($r < $a[$i][1]) return $a[$i][0];
   }
   return $a[$hi-1][0];
}

/* Generate and write FASTA format */

define ('LINE_LENGTH', 60);


function makeRandomFasta($id, $desc, &$genelist, $n) {
   print(">$id $desc\n");

   for ($todo = $n; $todo > 0; $todo -= LINE_LENGTH) {
      $pick = '';
      $m = $todo < LINE_LENGTH ? $todo : LINE_LENGTH;
      for ($i=0; $i < $m; $i++) $pick .= selectRandom(&$genelist);
      $pick .= "\n";
      print( $pick );
   }
}


function makeRepeatFasta($id, $desc, $s, $n) {
   echo ">$id $desc\n";
   $i = 0; $sLength = strlen($s); $lineLength = LINE_LENGTH; 
   while ($n > 0) {
      if ($n < $lineLength) $lineLength = $n;
      if ($i + $lineLength < $sLength){ 
         print(substr($s,$i,$lineLength)); print("\n");
         $i += $lineLength;
      } else {
         print(substr($s,$i));
         $i = $lineLength - ($sLength - $i);
         print(substr($s,0,$i)); print("\n");
      }
      $n -= $lineLength;
   }
}


/* Main -- define alphabets, make 3 fragments */

$iub=array(
   array('a', 0.27),
   array('c', 0.12),
   array('g', 0.12),
   array('t', 0.27),
   
   array('B', 0.02),
   array('D', 0.02),
   array('H', 0.02),
   array('K', 0.02),
   array('M', 0.02),
   array('N', 0.02),
   array('R', 0.02),
   array('S', 0.02),
   array('V', 0.02),
   array('W', 0.02),
   array('Y', 0.02)
);

$homosapiens = array(
   array('a', 0.3029549426680),
   array('c', 0.1979883004921),
   array('g', 0.1975473066391),
   array('t', 0.3015094502008)
);

$alu =
   'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' .
   'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' .
   'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' .
   'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' .
   'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' .
   'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' .
   'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

$n = 1000;

if ($_SERVER['argc'] > 1) $n = $_SERVER['argv'][1];

   makeCumulative(&$iub);
   makeCumulative(&$homosapiens);

   makeRepeatFasta('ONE', 'Homo sapiens alu', $alu, $n*2);
   makeRandomFasta('TWO', 'IUB ambiguity codes', $iub, $n*3);
   makeRandomFasta('THREE', 'Homo sapiens frequency', $homosapiens, $n*5);
?>
<?php
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Wing-Chung Leung
   modified by Isaac Gouy
   modified by anon
 */

ob_implicit_flush(1);
ob_start(NULL, 4096);

$last = 42.0;
function gen_random(&$last, &$randoms, $max = 1.0, $ia = 3877.0, $ic = 29573.0, $im = 139968.0) {
   foreach($randoms as &$r) {
      $r = $max * ($last = ($last * $ia + $ic) % $im) / $im;
   }
}

/* Weighted selection from alphabet */

function makeCumulative(&$genelist) {
   $cumul = 0.0;
   foreach($genelist as $k=>&$v) {
      $cumul = $v += $cumul;
   }
}


/* Generate and write FASTA format */

function makeRandomFasta(&$genelist, $n) {
   $width = 60;
   $lines = (int) ($n / $width);
   $pick = str_repeat('?', $width)."\n";
   $randoms = array_fill(0, $width, 0.0);
   global $last;

   // full lines
   for ($i = 0; $i < $lines; ++$i) {
      gen_random($last, $randoms);
      $j = 0;
      foreach ($randoms as $r) {
         foreach($genelist as $k=>$v) {
            if ($r < $v) {
               break;
            }
         }
         $pick[$j++] = $k;
      }
      echo $pick;
   }

   // last, partial line
   $w = $n % $width;
   if ($w !== 0) {
      $randoms = array_fill(0, $w, 0.0);
      gen_random($last, $randoms);
      $j = 0;
      foreach ($randoms as $r) {
         foreach($genelist as $k=>$v) {
            if ($r < $v) {
               break;
            }
         }
         $pick[$j++] = $k;
      }
      $pick[$w] = "\n";
      echo substr($pick, 0, $w+1);
   }

}


function makeRepeatFasta($s, $n) {
   $i = 0; $sLength = strlen($s); $lineLength = 60;
   while ($n > 0) {
      if ($n < $lineLength) $lineLength = $n;
      if ($i + $lineLength < $sLength){
         print(substr($s,$i,$lineLength)); print("\n");
         $i += $lineLength;
      } else {
         print(substr($s,$i));
         $i = $lineLength - ($sLength - $i);
         print(substr($s,0,$i)); print("\n");
      }
      $n -= $lineLength;
   }
}


/* Main -- define alphabets, make 3 fragments */

$iub=array(
   'a' => 0.27,
   'c' => 0.12,
   'g' => 0.12,
   't' => 0.27,

   'B' => 0.02,
   'D' => 0.02,
   'H' => 0.02,
   'K' => 0.02,
   'M' => 0.02,
   'N' => 0.02,
   'R' => 0.02,
   'S' => 0.02,
   'V' => 0.02,
   'W' => 0.02,
   'Y' => 0.02
);

$homosapiens = array(
   'a' => 0.3029549426680,
   'c' => 0.1979883004921,
   'g' => 0.1975473066391,
   't' => 0.3015094502008
);

$alu =
   'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' .
   'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' .
   'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' .
   'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' .
   'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' .
   'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' .
   'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

$n = 1000;

if ($_SERVER['argc'] > 1) $n = $_SERVER['argv'][1];

makeCumulative($iub);
makeCumulative($homosapiens);

echo ">ONE Homo sapiens alu\n";
makeRepeatFasta($alu, $n*2);

echo ">TWO IUB ambiguity codes\n";
makeRandomFasta($iub, $n*3);

echo ">THREE Homo sapiens frequency\n";
makeRandomFasta($homosapiens, $n*5);

#!/usr/bin/php -f
<?php
/*
 $Id: fibo.php,v 1.4 2005-04-25 19:01:38 igouy-guest Exp $
 http://shootout.alioth.debian.org/
*/
function fibo($n){
    return(($n < 2) ? 1 : fibo($n - 2) + fibo($n - 1));
}
$n = ($argc == 2) ? $argv[1] : 1;
$r = fibo($n);
print "$r\n";
?>
<? /* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/
   
   contributed by Isaac Gouy 
   
   php -q harmonic.php 1000
*/


$n = ($argc == 2) ? $argv[1] : 600000;

$partialSum = 0.0;
for ($i=1; $i<=$n; $i++) $partialSum += 1.0 / $i;

printf("%.9f\n", $partialSum);
?>
#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q hash.php 100000
*/ 


$n = ($argc == 2) ? $argv[1] : 1;

$i = 0; while ($i++ < $n) $x[dechex($i)] = $i; 

$count = 0;
while ($n--) 
    if (isset($x[$n])) $count++; 

print "$count\n";

?>
#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   contributed by Isaac Gouy 

   php -q hash2.php 200
*/ 


$n = ($argc == 2) ? $argv[1] : 1;

for ($i = 0; $i < 10000; $i++) 
   $hash1['foo_'.$i] = $i;

for ($i = $n; $i > 0; $i--) 
   foreach($hash1 as $key => $value) 
      if (isset($hash2[$key])){ 
         $hash2[$key] += $value; 
      } else { 
         $hash2[$key] = $value; 
      }
         
print "$hash1[foo_1] $hash1[foo_9999] $hash2[foo_1] $hash2[foo_9999]\n";

?>
#!/usr/bin/php -f
<?php
/*
 $Id: heapsort.php,v 1.2 2004-10-11 04:47:32 bfulgham Exp $
 http://shootout.alioth.debian.org/
*/
define("IM", 139968);
define("IA", 3877);
define("IC", 29573);

$LAST = 42;
function gen_random ($n) {
    global $LAST;
    return( ($n * ($LAST = ($LAST * IA + IC) % IM)) / IM );
}

function heapsort ($n, &$ra) {
    $l = ($n >> 1) + 1;
    $ir = $n;

    while (1) {
	if ($l > 1) {
	    $rra = $ra[--$l];
	} else {
	    $rra = $ra[$ir];
	    $ra[$ir] = $ra[1];
	    if (--$ir == 1) {
		$ra[1] = $rra;
		return;
	    }
	}
	$i = $l;
	$j = $l << 1;
	while ($j <= $ir) {
	    if (($j < $ir) && ($ra[$j] < $ra[$j+1])) {
		$j++;
	    }
	    if ($rra < $ra[$j]) {
		$ra[$i] = $ra[$j];
		$j += ($i = $j);
	    } else {
		$j = $ir + 1;
	    }
	}
	$ra[$i] = $rra;
    }
}


$N = ($argc == 2) ? $argv[1] : 1;

for ($i=1; $i<=$N; $i++) {
    $ary[$i] = gen_random(1);
}

/*
for ($i=0; $i<$N; $i++) {
    printf("%4d %.15f\n", $i, $ary[$i]);
}
*/

heapsort($N, $ary);

printf("%.10f\n", $ary[$N]);
?>
#!/usr/bin/php
<?php
print "hello world\n";
?>
<?php
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Damien Bonvillain
   modified by anon
   fixed by Isaac Gouy
 */

ob_implicit_flush(1);
ob_start(NULL, 4096);

$sequence = read_sequence('THREE');

fclose(STDIN);

// sequence read, let's write some stats
write_freq($sequence, 1);
write_freq($sequence, 2);
write_count($sequence, 'GGT');
write_count($sequence, 'GGTA');
write_count($sequence, 'GGTATT');
write_count($sequence, 'GGTATTTTAATT');
write_count($sequence, 'GGTATTTTAATTTATAGT');

/* functions definitions follow */

function read_sequence($id) {
   $id = '>' . $id;
   $ln_id = strlen($id);
   $fd = STDIN;
   // reach sequence three
   $line = '';
   while($line !== '' || !feof($fd)) {
      $line = stream_get_line($fd, 100, "\n");
      if($line[0] == '>' && strncmp($line, $id, $ln_id) === 0) {
         break;
      }
   }
   if(feof($fd)) {
      // sequence not found
      exit(-1);
   }
   // next, read the content of the sequence
   $sequence = '';
   while($line !== '' || !feof($fd)) {
      $line = stream_get_line($fd, 100, "\n");
      if (!isset($line[0])) continue;
      $c = $line[0];
      if ($c === ';') continue;
      if ($c === '>') break;
      // append the uppercase sequence fragment,
      // must get rid of the CR/LF or whatever if present
      $sequence .= $line;
   }
   return strtoupper($sequence);
}

function write_freq($sequence, $key_length) {
   $map = generate_frequencies($sequence, $key_length);
   uasort($map, 'freq_name_comparator');
   foreach($map as $key => $val) {
      printf ("%s %.3f\n", $key, $val);
   }
   echo "\n";
}

function write_count($sequence, $key) {
   $map = generate_frequencies($sequence, strlen($key), false);
   if (isset($map[$key])) $value = $map[$key];
   else $value = 0;
   printf ("%d\t%s\n", $value, $key);
}

/**
 * Returns a map (key, count or freq(default))
 */
function generate_frequencies($sequence, $key_length, $compute_freq = true) {
   $result = array();
   $total = strlen($sequence) - $key_length;
   $i = $total;
   if ($key_length === 1) { 
      do {
         $key = $sequence[$i--];
         if (isset($result[$key])) ++$result[$key];
         else $result[$key] = 1;
      } while ($i);
   } else {
      do {
         $key = substr($sequence, $i--, $key_length);
         if(isset($result[$key])) ++$result[$key];
         else $result[$key] = 1;
      } while ($i);
   }
   if($compute_freq) {
      foreach($result as $k => $v) {
         $result[$k] = $v * 100 / $total;
      }
   }
   return $result;
}


function freq_name_comparator($a, $b) {
   if ($a == $b) return 0;
   return  ($a < $b) ? 1 : -1;
}


<?
/* 
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Damien Bonvillain
   fixed by Isaac Gouy
*/

$sequence = read_sequence('THREE');

fclose(STDIN);

$jobs = array(
   array('write_freq', 1),
   array('write_freq', 2),
   array('write_count', 'GGT'),
   array('write_count', 'GGTA'),
   array('write_count', 'GGTATT'),
   array('write_count', 'GGTATTTTAATT'),
   array('write_count', 'GGTATTTTAATTTATAGT'),
);

$tok = ftok(__FILE__, chr(time() & 255));
$queue = msg_get_queue($tok);

$parent = TRUE;
$count = count($jobs);
for ($i = 1; $i < $count; ++$i) {
   $pid = pcntl_fork();
   if ($pid === -1) {
      die('could not fork');
   } else if ($pid) {
      continue;
   }
   $parent = FALSE;
   break;
}
if ($parent) {
   $i = 0;
}

$func = $jobs[$i][0];
$arg =  $jobs[$i][1];

ob_start();

$func($sequence, $arg);

$result = array($i, ob_get_clean());

if (!msg_send($queue, 2, $result, TRUE, FALSE, $errno)) {
   var_dump("$errno");
   var_dump(msg_stat_queue($queue));
}

if (!$parent) {
   exit(0);
}

$results = array();
foreach($jobs as $job) {
    msg_receive($queue, 2, $msgtype, 4096, $result, TRUE);
   $results[$result[0]] = $result[1];
   pcntl_wait($s);
}

ksort($results);
foreach ($results as $result) {
   echo $result;
}

msg_remove_queue($queue);


/* functions definitions follow */

function read_sequence($id) {
   $id = '>' . $id;
   $ln_id = strlen($id);
   $fd = STDIN;
   // reach sequence three
   $line = '';
   while($line !== '' || !feof($fd)) {
      $line = stream_get_line($fd, 100, "\n");
      if($line[0] == '>' && strncmp($line, $id, $ln_id) === 0) {
         break;
      }
   }
   if(feof($fd)) {
      echo "sequence not found\n";
      exit(-1);
   }
   // next, read the content of the sequence
   $sequence = '';
   while($line !== '' || !feof($fd)) {
      $line = stream_get_line($fd, 100, "\n");
      if (!isset($line[0])) continue;
      $c = $line[0];
      if ($c === ';') continue;
      if ($c === '>') break;
      // append the uppercase sequence fragment,
      // must get rid of the CR/LF or whatever if present
      $sequence .= $line;
   }
   return strtoupper($sequence);
}

function write_freq($sequence, $key_length) {
   $map = generate_frequencies($sequence, $key_length);
   uasort($map, 'freq_name_comparator');
   foreach($map as $key => $val) {
      printf ("%s %.3f\n", $key, $val);
   }
   echo "\n";
}

function write_count($sequence, $key) {
   $map = generate_frequencies($sequence, strlen($key), false);
   if (isset($map[$key])) $value = $map[$key];
   else $value = 0;
   printf ("%d\t%s\n", $value, $key);
}

/**
 * Returns a map (key, count or freq(default))
 */
function generate_frequencies($sequence, $key_length, $compute_freq = true) {
   $result = array();
   $total = strlen($sequence) - $key_length;
   $i = $total;
   if ($key_length === 1) { 
      do {
         $key = $sequence[$i--];
         if (isset($result[$key])) ++$result[$key];
         else $result[$key] = 1;
      } while ($i);
   } else {
      do {
         $key = substr($sequence, $i--, $key_length);
         if(isset($result[$key])) ++$result[$key];
         else $result[$key] = 1;
      } while ($i);
   }
   if($compute_freq) {
      foreach($result as $k => $v) {
         $result[$k] = $v * 100 / $total;
      }
   }
   return $result;
}

function freq_name_comparator($a, $b) {
   if ($a == $b) return 0;
   return  ($a < $b) ? 1 : -1;
}

#!/usr/bin/php -f
<?
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q lists.php 18
*/ 


$n = ($argc == 2) ? $argv[1] : 1;
$size = 10000;
$L1Size = 0;

while ($n--){
   $L1 = range(1,$size);
   $L2 = $L1;
   $L3 = array();

   while ($L2) array_push($L3, array_shift($L2));
   while ($L3) array_push($L2, array_pop($L3));   
   $L1 = array_reverse($L1);
   
   if ($L1[0] != $size){
      print "First item of L1 != SIZE\n"; break; }
   
   for ($i=0; $i<$size; $i++)   
      if ($L1[$i] != $L2[$i]){ print "L1 != L2\n"; break 2; }
    
   $L1Size = sizeof($L1);
   unset($L1); unset($L2); unset($L3);
}
print "$L1Size\n";

?>
<?php
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Thomas GODART (based on Greg Buchholz's C program) 
   modified by anon
*/

ob_implicit_flush(1);
ob_start(NULL, 4096);


$h = ($argc == 2) ? ((int)$argv[1]) : 600;
$w = $h;

printf ("P4\n%d %d\n", $w, $h);

$bit_num = 128;
$byte_acc = 0;

$yfac = 2.0 / $h;
$xfac = 2.0 / $w;

for ($y = 0 ; $y < $h ; ++$y)
{
   $result = array('c*');

   $Ci = $y * $yfac - 1.0;

   for ($x = 0 ; $x < $w ; ++$x)
   {
      $Zr = 0; $Zi = 0; $Tr = 0; $Ti = 0.0;

      $Cr = $x * $xfac - 1.5;

      do {
         for ($i = 0 ; $i < 50 ; ++$i)
         {
            $Zi = 2.0 * $Zr * $Zi + $Ci;
            $Zr = $Tr - $Ti + $Cr;
            $Tr = $Zr * $Zr;
            if (($Tr+($Ti = $Zi * $Zi)) > 4.0) break 2;
         }
         $byte_acc += $bit_num;
      } while (FALSE);

      if ($bit_num === 1) {
         $result[] = $byte_acc;
         $bit_num = 128;
         $byte_acc = 0;
      } else {
         $bit_num >>= 1;
      }
   }
   if ($bit_num !== 128) {
      $result[] = $byte_acc;
      $bit_num = 128;
      $byte_acc = 0;
   }
   echo call_user_func_array('pack', $result);
}

<?
/* 
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Thomas GODART (based on Greg Buchholz's C program)
   multicore by anon
 */

function getProcs() {
   $procs = 1;
   if (file_exists('/proc/cpuinfo')) {
      $procs = preg_match_all('/^processor\s/m', file_get_contents('/proc/cpuinfo'), $discard);
   }
   $procs <<= 1;
   return $procs;
}

$h = (int) (($argc == 2) ? $argv[1] : 600);
$w = $h;

if ($w % 8) {
   fprintf(STDERR, "width %d not multiple of 8\n", $w);
   exit(1);
}

printf ("P4\n%d %d\n", $w, $h);

$shsize = $w * ($w >> 3);
$shmop = shmop_open(ftok(__FILE__, chr(time() & 255)), 'c', 0644, $shsize);

if (!$shmop) {
   echo "faild to shmop_open()\n";
   exit(1);
}

$bit_num = 128;
$byte_acc = 0;

$yfac = 2.0 / $h;
$xfac = 2.0 / $w;

$shifted_w = $w >> 3;
$step = 1;

$procs = getProcs();
$child = $procs - 1;
while ($child > 0) {
   $pid = pcntl_fork();
   if ($pid === -1) {
      die('could not fork');
   } else if ($pid) {
      --$child;
      continue;
   }
   break;
}

$step = $procs;
$y = $child;

for ( ; $y < $h ; $y+=$step)
{
   $result = array('c*');

   $Ci = $y * $yfac - 1.0;

   for ($x = 0 ; $x < $w ; ++$x)
   {
      $Zr = 0; $Zi = 0; $Tr = 0; $Ti = 0.0;

      $Cr = $x * $xfac - 1.5;

      do {
         for ($i = 0 ; $i < 50 ; ++$i)
         {
            $Zi = 2.0 * $Zr * $Zi + $Ci;
            $Zr = $Tr - $Ti + $Cr;
            $Tr = $Zr * $Zr;
            if (($Tr+($Ti = $Zi * $Zi)) > 4.0) break 2;
         }
         $byte_acc += $bit_num;
      } while (FALSE);

      if ($bit_num === 1) {
         $result[] = $byte_acc;
         $bit_num = 128;
         $byte_acc = 0;
      } else {
         $bit_num >>= 1;
      }
   }
   if ($bit_num !== 128) {
      $result[] = $byte_acc;
      $bit_num = 128;
      $byte_acc = 0;
   }
   shmop_write($shmop, call_user_func_array('pack', $result), $y * $shifted_w);
}

if ($child > 0) {
   exit(0);
}

$child = $procs - 1;
$status = 0;
while ($child-- > 0) {
   pcntl_wait($status);
}

$step = $shsize >> 3;
for($i = 0; $i < $shsize; $i+=$step) {
   echo shmop_read($shmop, $i, $step);
}
shmop_delete($shmop);

#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
       
   php -q matrix.php 600
*/ 


set_time_limit(0);

$SIZE = 30;

function mkMatrix ($rows, $cols) {
    $count = 1;
    for ($i=0; $i<$rows; $i++) {
	for ($j=0; $j<$cols; $j++) {
	    $m[$i][$j] = $count++;
	}
    }
    return $m;
}


function mkZeroMatrix ($rows, $cols) {
    for ($i=0; $i<$rows; $i++) {
	for ($j=0; $j<$cols; $j++) {
	    $m[$i][$j] = 0;
	}
    }
    return $m;
}


function mmult ($rows, $cols, &$m1, &$m2, &$mm) {
    for ($i=0; $i<$rows; $i++) {
	for ($j=0; $j<$cols; $j++) {
	    $x = 0;
	    for ($k=0; $k<$cols; $k++) {
		$x += $m1[$i][$k] * $m2[$k][$j];
	    }
	    $mm[$i][$j] = $x;
	}
    }
    return $mm;
}


$n = ($argc == 2) ? $argv[1] : 1;

$m1 = mkMatrix($SIZE, $SIZE);
$m2 = mkMatrix($SIZE, $SIZE);
$mm = mkZeroMatrix($SIZE, $SIZE);

while ($n--) 
    mmult($SIZE, $SIZE, $m1, $m2, $mm);


print "{$mm[0][0]} {$mm[2][3]} {$mm[3][2]} {$mm[4][4]}\n"; 

?>
#!/usr/bin/php -f
<?
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q objinst.php 1500000
*/ 


class Toggle {
   var $_state;
   function Toggle($startState){ $this->_state = $startState; }
   function value(){ return $this->_state; }
   function activate(){ $this->_state = ! $this->_state; }
}


class NthToggle extends Toggle {
   var $_countMax;
   var $_count;

   function NthToggle($startState,$max){
      Toggle::Toggle($startState);
      $this->_countMax = $max; 
      $this->_count = 0;      
   }

   function activate(){ 
      $this->_count++;
      if ($this->_count >= $this->_countMax){
         $this->_state = ! $this->_state;
         $this->_count = 0;       
      }
   }
}


$n = ($argc == 2) ? $argv[1] : 1;

$toggle = new Toggle(TRUE);
for ($i=0; $i<$n; $i++){
   $toggle->activate();
   $toggle->value();   
}

$toggle->value() ? print "true\n" : print "false\n";


$ntoggle = new NthToggle(TRUE,3);
for ($i=0; $i<$n; $i++){
   $ntoggle->activate();
   $ntoggle->value();   
}

$ntoggle->value() ? print "true\n" : print "false\n";

?>
<?
/* The Great Computer Language Shootout
      http://shootout.alioth.debian.org/
      contributed by Isaac Gouy 

      php -q moments.php < in.txt > out.txt
*/ 


$numbers = file("php://stdin");

$n = sizeof($numbers);
$sum = 0.0;
for ($i=0; $i<$n; $i++){
   $numbers[$i] = doubleval($numbers[$i]);
   $sum += $numbers[$i];
}

$dev = $adev = $variance = $skew = $kurtosis = 0.0; 
$mean = $sum / $n;

for ($i=0; $i<$n; $i++){
   $dev = $numbers[$i] - $mean;   
   $adev += abs($dev);   
   $variance += ($dev2 = $dev * $dev); 
   $skew += $dev2 * $dev;
   $kurtosis += $dev2 * $dev2;
}

$adev /= $n;   
$variance /= $n-1;     
$sdev = sqrt($variance);

if ($variance != 0.0){
   $skew /= $n * $variance * $sdev;
   $kurtosis = $kurtosis/($n * $variance * $variance) - 3.0;
}

sort($numbers);
$mid = $n / 2;
$median = ($n % 2 != 0) ?
   $numbers[$mid] : ($numbers[$mid] + $numbers[$mid-1]) / 2.0;

printf("n:                  %d\n", $n);
printf("median:             %0.6f\n", $median);
printf("mean:               %0.6f\n", $mean);
printf("average_deviation:  %0.6f\n", $adev);
printf("standard_deviation: %0.6f\n", $sdev);
printf("variance:           %0.6f\n", $variance);
printf("skew:               %0.6f\n", $skew);
printf("kurtosis:           %0.6f\n", $kurtosis);

?>
<?
/* The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

contributed by anon
modified by Sergey Khripunov
*/

function energy(&$b) {
   $e = 0.0;
   for ($i=0,$m=sizeof($b);$i<$m;$i++) {
       $b1=$b[$i]; 
       $e += 0.5*$b1[6]*($b1[3]*$b1[3]+$b1[4]*$b1[4]+$b1[5]*$b1[5]);
       for ($j=$i+1; $j<$m; $j++) {
	  $b2=$b[$j];
	  $dx=$b1[0]-$b2[0]; $dy=$b1[1]-$b2[1]; $dz=$b1[2]-$b2[2];
	  $e -= ($b1[6]*$b2[6])/sqrt($dx*$dx + $dy*$dy + $dz*$dz);
       }
   }
   return $e;
}

$pi=3.141592653589793;
$solar_mass=4*$pi*$pi;
$days_per_year=365.24;

$bodies = array(array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, $solar_mass), //Sun
		array(4.84143144246472090E+00, // Jupiter
		      -1.16032004402742839E+00,
		      -1.03622044471123109E-01,
		      1.66007664274403694E-03 * $days_per_year,
		      7.69901118419740425E-03 * $days_per_year,
		      -6.90460016972063023E-05 * $days_per_year,
		      9.54791938424326609E-04 * $solar_mass),
		array(8.34336671824457987E+00, // Saturn
		      4.12479856412430479E+00,
		      -4.03523417114321381E-01,
		      -2.76742510726862411E-03 * $days_per_year,
		      4.99852801234917238E-03 * $days_per_year,
		      2.30417297573763929E-05 * $days_per_year,
		      2.85885980666130812E-04 * $solar_mass),
		array(1.28943695621391310E+01, // Uranus
		      -1.51111514016986312E+01,
		      -2.23307578892655734E-01,
		      2.96460137564761618E-03 * $days_per_year,
		      2.37847173959480950E-03 * $days_per_year,
		      -2.96589568540237556E-05 * $days_per_year,
		      4.36624404335156298E-05 * $solar_mass),
		array(1.53796971148509165E+01, // Neptune
		      -2.59193146099879641E+01,
		      1.79258772950371181E-01,
		      2.68067772490389322E-03 * $days_per_year,
		      1.62824170038242295E-03 * $days_per_year,
		      -9.51592254519715870E-05 * $days_per_year,
		      5.15138902046611451E-05 * $solar_mass));

// offset_momentum
$px=$py=$pz=0.0;
foreach ($bodies as &$e) {
    $px+=$e[3]*$e[6]; 
    $py+=$e[4]*$e[6]; 
    $pz+=$e[5]*$e[6];
} 
$bodies[0][3]=-$px/$solar_mass;
$bodies[0][4]=-$py/$solar_mass;
$bodies[0][5]=-$pz/$solar_mass;

$pairs = array();
for ($i=0,$m=count($bodies); $i<$m; $i++) 
   for ($j=$i+1; $j<$m; $j++) 
      $pairs[] = array(&$bodies[$i], &$bodies[$j]);

$n = $argv[1];

printf("%0.9f\n", energy($bodies));

$i=0; 
do {

    foreach ($pairs as &$p) {
	$a=&$p[0]; $b=&$p[1];
	$dx=$a[0]-$b[0]; $dy=$a[1]-$b[1]; $dz=$a[2]-$b[2];

	$dist = sqrt($dx*$dx + $dy*$dy + $dz*$dz);
	$mag = 0.01/($dist*$dist*$dist);
	$mag_a = $a[6]*$mag; $mag_b = $b[6]*$mag;
	
	$a[3]-=$dx*$mag_b; $a[4]-=$dy*$mag_b; $a[5]-=$dz*$mag_b;
	$b[3]+=$dx*$mag_a; $b[4]+=$dy*$mag_a; $b[5]+=$dz*$mag_a;
    } 

    foreach ($bodies as &$b) {
        $b[0]+=0.01*$b[3]; $b[1]+=0.01*$b[4]; $b[2]+=0.01*$b[5];
    } 

} while(++$i<$n);

printf("%0.9f\n", energy($bodies));

?>
#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q nestedloop.php 18
*/ 


$n = ($argc == 2) ? $argv[1] : 1;

$x = 0;
$a = $n; while ($a--){
   $b = $n; while ($b--){
      $c = $n; while ($c--){
         $d = $n; while ($d--){
            $e = $n; while ($e--){
               $f = $n; while ($f--) 
                  $x++; }}}}}
 
print "$x\n";

?>
<?php

/* The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * contributed by Charles G.
 * modified by Isaac Gouy
 */

function nsieve($m)
{
    $flags = ' ';
    $flags[$m] = ' ';
    
    $count = 0;
    for ($i = 2; $i < $m; ++$i)
        if ($flags[$i] == ' ') {
            ++$count;
            for ($j = $i << 1; $j < $m; $j += $i)
                $flags[$j] = 'x';
        }

    printf("Primes up to %8d %8d\n", $m, $count);

}

$m = $argv[1];
for ($i = 0; $i < 3; $i++)
    nsieve(10000 << ($m-$i));
?>
<?php /* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Peter Baltruschat
*/

function primes($size)
{
   $flags = array_fill(0, ($size>>5) + 1, ~0);
   $count = 0;
   for($i = 2; $i < $size; ++$i)
   {
      $offset = $i>>5;
      $mask = 1<<($i - ($offset<<5));
      if($flags[$offset] & $mask)
      {
         ++$count;
         for($j = $i<<1; $j <= $size; $j += $i)
         {
            $offset = $j>>5;
            $mask = 1<<($j - ($offset<<5));
            if($flags[$offset] & $mask)
            {
               $flags[$offset] ^= $mask;
            }
         }
      }
   }
   unset($flags);
   printf("Primes up to %8d %8d\n", $size, $count);
}

$n = ($argv[1] != '') ? $argv[1] : $_GET['n'];

$size = 10000 * (1<<$n);
primes($size);
primes($size>>1);
primes($size>>2);
?>
#!/usr/bin/php -f
<?
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q objinst.php 1500000
*/ 


class Toggle {
   var $_state;
   function Toggle($startState){ $this->_state = $startState; }
   function value(){ return $this->_state; }
   function activate(){ $this->_state = ! $this->_state; }
}


class NthToggle extends Toggle {
   var $_countMax;
   var $_count;

   function NthToggle($startState,$max){
      Toggle::Toggle($startState);
      $this->_countMax = $max; 
      $this->_count = 0;      
   }

   function activate(){ 
      $this->_count++;
      if ($this->_count >= $this->_countMax){
         $this->_state = ! $this->_state;
         $this->_count = 0;       
      }
   }
}


$n = ($argc == 2) ? $argv[1] : 1;

$toggle = new Toggle(TRUE);
for ($i=0; $i<5; $i++){
   $toggle->activate();
   $toggle->value() ? print "true\n" : print "false\n";   
}

for ($i=0; $i<$n; $i++) new Toggle(TRUE);
print "\n";

$ntoggle = new NthToggle(TRUE,3);
for ($i=0; $i<8; $i++){
   $ntoggle->activate();
   $ntoggle->value() ? print "true\n" : print "false\n";   
}

for ($i=0; $i<$n; $i++) new NthToggle(TRUE,3);

?>
<? /* The Computer Language Shootout 
   http://shootout.alioth.debian.org/  
   contributed by Isaac Gouy */

$n = $argv[1];

$a1 = 0.0; $a2 = 0.0; $a3 = 0.0; $a4 = 0.0; $a5 = 0.0; 
$a6 = 0.0; $a7 = 0.0; $a8 = 0.0; $a9 = 0.0;

$twothirds = 2.0/3.0;
$alt = -1.0;

for ($k=1; $k<=$n; $k++){
   $k2 = $k*$k;
   $k3 = $k2*$k;
   $sk = sin($k);
   $ck = cos($k);
   $alt = -$alt;

   $a1 += pow($twothirds,$k-1);
   $a2 += pow($k,-0.5);
   $a3 += 1.0/($k*($k+1.0));
   $a4 += 1.0/($k3 * $sk*$sk);
   $a5 += 1.0/($k3 * $ck*$ck);
   $a6 += 1.0/$k;
   $a7 += 1.0/$k2;
   $a8 += $alt/$k;
   $a9 += $alt/(2*$k -1);
}

printf("%.9f\t(2/3)^k\n", $a1);
printf("%.9f\tk^-0.5\n", $a2);
printf("%.9f\t1/k(k+1)\n", $a3);
printf("%.9f\tFlint Hills\n", $a4);
printf("%.9f\tCookson Hills\n", $a5);
printf("%.9f\tHarmonic\n", $a6);
printf("%.9f\tRiemann Zeta\n", $a7);
printf("%.9f\tAlternating Harmonic\n", $a8);
printf("%.9f\tGregory\n", $a9);

?>
<? /* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/
   
   contributed by Isaac Gouy 
   
   php -q pidigits.php 27
*/


class Transformation {
   var $q, $r, $s, $t, $k;

   function Transformation($q, $r, $s, $t){
      $this->q = $q;
      $this->r = $r;      
      $this->s = $s;
      $this->t = $t;               
   }
   
   function Unity(){
      return new Transformation("1", "0", "0", "1");              
   }   
   
   function Zero(){
      return new Transformation("0", "0", "0", "0");              
   }      
   
      
   function Compose($a){
      $qq = bcmul($this->q, $a->q);
      $qrrt = bcadd(bcmul($this->q, $a->r), bcmul($this->r, $a->t));
      $sqts = bcadd(bcmul($this->s, $a->q), bcmul($this->t, $a->s));
      $srtt = bcadd(bcmul($this->s, $a->r), bcmul($this->t, $a->t));   
      return new Transformation($qq, $qrrt, $sqts, $srtt);
   }
   
   function Extract($j){
      $bigj = strval($j);
      $qjr = bcadd(bcmul($this->q, $bigj), $this->r);
      $sjt = bcadd(bcmul($this->s, $bigj), $this->t);
      $d = bcdiv($qjr, $sjt);
      return floor($d);
   }
      
   function Next(){ 
      $this->k = $this->k + 1;
      $this->q = strval($this->k);
      $this->r = strval(4*$this->k + 2);
      $this->s = "0";
      $this->t = strval(2*$this->k + 1);
      return $this;      
   }                
}



class PiDigitStream {
   var $z, $x, $inverse;

   function PiDigitStream(){
      $this->z = Transformation::Unity();
      $this->x = Transformation::Zero();      
      $this->inverse = Transformation::Zero();   
   }
   
   function Produce($j){
      $i = $this->inverse;
      $i->q = "10";
      $i->r = strval(-10*$j);
      $i->s = "0";
      $i->t = "1";
      return $i->Compose($this->z);
   }   

   function Consume($a){
      return $this->z ->Compose($a);  
   }
   
   function Digit(){
      return $this->z ->Extract(3);  
   }  
   
   function IsSafe($j){
      return $j == ($this->z ->Extract(4));  
   }    

   function Next(){
      $y = $this->Digit();
      if ($this->IsSafe($y)){
         $this->z = $this->Produce($y);
         return $y;
      } else {
         $this->z = $this->Consume($this->x ->Next());
         return $this->Next();      
      }
   } 
}


$n = $argv[1];
$i = 0;
$length = 10;
$pidigit = new PiDigitStream;

while ($n > 0){
   if ($n < $length){
      for ($j=0; $j<$n; $j++) printf("%d",$pidigit->Next());
      for ($j=$n; $j<$length; $j++)  print " ";
      $i += $n;
   } else {
      for ($j=0; $j<$length; $j++) printf("%d",$pidigit->Next());
      $i += $length;   
   }
   print "\t:$i\n";
   $n -= $length;
}
?>
<?php /* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Peter Baltruschat
*/
function Transformation_Compose($tr, $a)
{
   return array(
      gmp_mul($tr[0], $a[0]),
      gmp_add(gmp_mul($tr[0], $a[1]), gmp_mul($tr[1], $a[3])),
      gmp_add(gmp_mul($tr[2], $a[0]), gmp_mul($tr[3], $a[2])),
      gmp_add(gmp_mul($tr[2], $a[1]), gmp_mul($tr[3], $a[3]))
   );
}
function Transformation_Compose2($y, $a)
{
   return array(
      gmp_mul(10, $a[0]),
      gmp_add(gmp_mul(10, $a[1]), gmp_mul(gmp_mul(-10, $y), $a[3])),
      $a[2],
      $a[3]
   );
}
function Transformation_Extract($tr, $j)
{
   return gmp_div_q(
      gmp_add(gmp_mul($tr[0], $j), $tr[1]),
      gmp_add(gmp_mul($tr[2], $j), $tr[3])
   );
}
function Transformation_Next(&$tr)
{
   $tr[3] = (++$tr[0]<<1) + 1;
   $tr[1] = $tr[3]<<1;
   $tr[2] = 0;
   return $tr;
}
function Pidigit_Next(&$pd, $times)
{
   $digits = '';
   $z = $pd[0];
   do
   {
      $y = Transformation_Extract($z, 3);
      do
      {
         $z = Transformation_Compose($z, Transformation_Next($pd[1]));
         $y = Transformation_Extract($z, 3);
      }
      while(0 != gmp_cmp(Transformation_Extract($z, 4), $y));
      $z = Transformation_Compose2($y, $z);
      $digits .= gmp_strval($y);
   }
   while(--$times);
   $pd[0] = $z;
   return $digits;
}

$n = (int) $argv[1];
$i = 0;
$pidigit = array(array(1, 0, 0, 1), array(0, 0, 0, 0));

while($n)
{
   if($n < 10)
   {
      printf("%s%s\t:%d\n", Pidigit_Next($pidigit, $n), str_repeat(' ', 10 - $n), $i + $n);
      break;
   }
   else
   {
      printf("%s\t:%d\n", Pidigit_Next($pidigit, 10), $i += 10);
   }
   $n -= 10;
}
?>
<?php

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

function isPrime($k){
   if ($k < 2){ return False; }
   if ($k < 4){ return True; }
   if ($k%2 == 0){ return False; }
   if (($k+1)%6 != 0 && ($k-1)%6 != 0){ return False; }

   $limit = ceil(sqrt($k));
   for ($i = 5; $i <= $limit; $i += 2){
      if ($k % $i == 0) { return False; }
   }
   return True; 
}


function Prime($n){
   $count = 0;
   $primeNumber = 0;

   for ($k = 1; $count < $n; $k++){
      if (isPrime($k)) {
         $count++;
         $primeNumber = $k;
      }
   }
   return $primeNumber;
}


$n = $argv[1];

printf("1st prime is %d\n", Prime(1));
printf("2nd prime is %d\n", Prime(2));

for ($i = 10*$n; $i <= 50*$n; $i += 10*$n)
   printf("%dth prime is %d\n", $i, Prime($i));

?>
<?php

/* The Computer Language Benchmarks Game 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

function Prime($n){
   $count = 0;
   $primeNumber = 0;

   if ($n < 2){
      if ($n == 1){ $count = 1; $primeNumber = 2; }
   } 
   else { 
      $count = 2; $primeNumber = 3; 
   }

   for ($k = 5; $count < $n; $k += 2){

      if (($k+1)%6 == 0 || ($k-1)%6 == 0){

         $isTrivial = True;
         $limit = ceil(sqrt($k));

         for ($i = 5; $i <= $limit; $i += 2)
            if ($k % $i == 0) { $isTrivial = False; break; }

         if ($isTrivial) {
            $count++;
            $primeNumber = $k;
         }
      }
   }

   return $primeNumber;
}


$n = $argv[1];

printf("1st prime is %d\n", Prime(1));
printf("2nd prime is %d\n", Prime(2));

for ($i = 10*$n; $i <= 50*$n; $i += 10*$n)
   printf("%dth prime is %d\n", $i, Prime($i));

?>
#!/usr/bin/php -f
<?php
/*
 $Id: random.php,v 1.3 2004-10-11 04:47:51 bfulgham Exp $
 http://shootout.alioth.debian.org/
*/
define("IM", 139968);
define("IA", 3877);
define("IC", 29573);

$LAST = 42;
function gen_random ($max) {
    global $LAST;
    return( ($max * ($LAST = ($LAST * IA + IC) % IM)) / IM );
}

$N = (($argc == 2) ? $argv[1] : 1) - 1;
while ($N--) {
    gen_random(100.0);
}

printf("%.9f\n", gen_random(100.0));
?>
<? /* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/
   
   contributed by Isaac Gouy (PHP novice)
   
   php -q raytracer.php 32
*/


define("INFINITY", log(0) * -1.0);
define("EPSILON", 1.49012e-08);

define("LEVELS", 6);
define("SS", 4);

class Vector {
   var $x, $y, $z; 
   
   function Vector($x,$y,$z){
      $this->x = $x; $this->y = $y; $this->z = $z;            
   }
   
   function plus(&$b){
      return new Vector(
         $this->x + $b->x, $this->y + $b->y, $this->z + $b->z );      
   }   
   
   function minus(&$b){
      return new Vector(
         $this->x - $b->x, $this->y - $b->y, $this->z - $b->z );        
   }   
   
   function dot(&$b){
      return ($this->x * $b->x) + 
         ($this->y * $b->y) + ($this->z * $b->z);      
   }   
   
   function scaledBy($s){
      return new Vector($this->x * $s, $this->y * $s, $this->z * $s);           
   }        
   
   function normalized(){
      return $this->scaledBy(1.0/sqrt( $this->dot($this) ));          
   }      
                        
}


class Ray {
   var $origin, $direction; 
   
   function Ray(&$origin,&$direction){
      $this->origin = $origin;
      $this->direction = $direction;                 
   }                           
}


class IntersectionPoint {
   var $distance, $normal; 
   
   function IntersectionPoint($distance,&$normal){
      $this->distance = $distance;
      $this->normal = $normal;         
   }                            
}


class Scene {

   function sphereScene($level,&$center,$radius){
      $sphere = new Sphere($center,$radius);
      if ($level==1){
         return $sphere;
      }
      else {
         $scene = new Group( new Sphere($center,3.0*$radius) );
         $scene->add($sphere);
         $rn = 3.0*$radius / sqrt(12.0);
         
         for ($dz=-1; $dz<=1; $dz+=2){
            for ($dx=-1; $dx<=1; $dx+=2){   
            
               $c2 = new Vector(
                    $center->x - $dx*$rn
                  , $center->y + $rn
                  , $center->z - $dz*$rn
                  );    
                  
               $scene->add( Scene::sphereScene($level-1, $c2, $radius/2.0) );                          
            }            
         }   
         return $scene;
      }     
   } 
         
   function traceRay(&$ray,&$light){   
      $p = $this->intersect($ray,
         new IntersectionPoint(INFINITY, new Vector(0.0, 0.0, 0.0)) );                            
      if (is_infinite($p->distance)){ return 0.0; }
                             
      $greyscale = -1.0 * ($p->normal->dot($light));
      if ($greyscale <= 0.0){ return 0.0; }   
      
      $rayOrigin = $ray->origin;
      $scaledDirection = $ray->direction->scaledBy($p->distance);
      $scaledNormal = $p->normal->scaledBy(EPSILON);
      $o = $rayOrigin->plus($scaledDirection);
      $o = $o->plus($scaledNormal);          
              
      $v = new Vector(0.0, 0.0, 0.0);      
      $shadowRay = new Ray($o, $v->minus($light));      
      $shadowp = $this->intersect(
         $shadowRay, new IntersectionPoint(INFINITY, $p->normal) );      
          
      if (is_infinite($shadowp->distance)){ return $greyscale; }  
      else { return 0.0; }         
   }                                                           
}


// a leaf node in the scene tree
class Sphere extends Scene {
   var $center, $radius; 
   
   function Sphere(&$center,$radius){   
      $this->center = $center;
      $this->radius = $radius;               
   }     
   
   function distance(&$ray){
      $v = $this->center -> minus($ray->origin);                      
      $b = $v->dot($ray->direction);                      
      $disc = $b*$b - $v->dot($v) + $this->radius*$this->radius;       
      if ($disc < 0.0){ return INFINITY; } // No intersection
        
      $d = sqrt($disc);   
      $t1 = $b + $d;  
      if ($t1 < 0.0){ return INFINITY; }             
              
      $t2 = $b - $d;     
      if ($t2 > 0.0){ return $t2; } else { return $t1; }                     
   }   
      
   function intersect(&$ray,&$p){      
      $d = $this->distance($ray);              
      if ($d < $p->distance){                                     
         $rayOrigin = $ray->origin;
         $rayDirection = $ray->direction;
         $scaledDirection = $rayDirection->scaledBy($d);  
                
         $v = $rayOrigin->plus($scaledDirection->minus( $this->center ));                           
         $p = new IntersectionPoint($d, $v->normalized());         
      }             
      return $p; 
   }                           
}


// non-leaf node in the scene tree
class Group extends Scene {
   var $bound, $scenes;        
       
   function Group(&$bound){
      $this->bound = $bound;  
      $this->scenes = array();      
   }           
   
   function intersect(&$ray,&$p){     
      if (($this->bound -> distance($ray)) < $p->distance){                         
         foreach ($this->scenes as $each){
            $p = $each->intersect($ray,$p);                  
         }      
      }           
      return $p; 
   }    
      
   function add(&$s){  
      array_unshift($this->scenes, $s);   
   }                      
}

////////////////////////////////////////////////////////////////////

$n = ($argc == 2) ? $argv[1] : 1;

$scene = Scene::sphereScene(LEVELS, new Vector(0.0, -1.0, 0.0), 1.0);

printf("P5\n%d %d\n255\n", $n,$n);

for ($y=$n-1; $y>=0; --$y){
   for ($x=0; $x<$n; ++$x){
   
      $greyscale = 0.0;
      for ($dx=0; $dx<SS; ++$dx){
         for ($dy=0; $dy<SS; ++$dy){   
         
            $v = new Vector(
                 $x + ($dx/(double)SS) - ($n/2.0)
               , $y + ($dy/(double)SS) - ($n/2.0)
               , $n );                                                       
                             
            $ray = new Ray(new Vector(0.0, 0.0, -4.0), $v->normalized());  
                                           
            $u = new Vector(-1.0, -3.0, 2.0);                          
            $greyscale += $scene->traceRay($ray, $u->normalized());                                    
         }
      }                     
      echo chr( 0.5 + 255.0*$greyscale/(double)(SS*SS) );                                          
   }
}           
?>
<? /* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/
  
   contributed by Isaac Gouy (PHP novice)
   
   php -q raytracer.php 32
*/


define("INFINITY", log(0) * -1.0);
define("EPSILON", 1.49012e-08);

define("LEVELS", 6);
define("SS", 4);


function &newVector($x,$y,$z){
   return array($x,$y,$z);          
}

function &add(&$a,&$b){
   return array($a[0]+$b[0], $a[1]+$b[1], $a[2]+$b[2]);      
}   

function &subtract(&$a,&$b){
   return array($a[0]-$b[0], $a[1]-$b[1], $a[2]-$b[2]);        
}   

function dot(&$a,&$b){
   return $a[0]*$b[0] + $a[1]*$b[1] + $a[2]*$b[2];      
}   

function &scaleBy(&$a,$s){
   return array($a[0]*$s, $a[1]*$s, $a[2]*$s);         
}        

function &normalize(&$a){
   return scaleBy($a,1.0/sqrt(dot($a,$a)));          
}


function &newRay($origin,$direction){
   return array("origin"=>$origin,"direction"=>$direction); 
}


function &newIntersectionPoint($distance,$normal){
   return array("distance"=>$distance,"normal"=>$normal); 
}



class Scene {

   function &sphereScene($level,&$center,$radius){
      $sphere = new Sphere($center,$radius);
      if ($level==1){
         return $sphere;
      }
      else {
         $scene = new Group( new Sphere($center,3.0*$radius) );
         $scene->add($sphere);
         $rn = 3.0*$radius / sqrt(12.0);
         
         for ($dz=-1; $dz<=1; $dz+=2){
            for ($dx=-1; $dx<=1; $dx+=2){   
            
               $c2 = newVector(
                    $center[0] - $dx*$rn
                  , $center[1] + $rn
                  , $center[2] - $dz*$rn
                  );    
                  
               $scene->add( Scene::sphereScene($level-1, $c2, $radius/2.0) );                          
            }            
         }   
         return $scene;
      }     
   } 
         
   function traceRay(&$ray,&$light){   
      $p = $this->intersect($ray,
         newIntersectionPoint(INFINITY, newVector(0.0, 0.0, 0.0)) );                                  
      if (is_infinite($p["distance"])){ return 0.0; }
                                                  
      $greyscale = -1.0 * dot($p["normal"],$light);
      if ($greyscale <= 0.0){ return 0.0; }   
      
      $rayOrigin = $ray["origin"];
      $scaledDirection = scaleBy($ray["direction"], $p["distance"]);
      $scaledNormal = scaleBy($p["normal"],EPSILON);
      $o = add($rayOrigin,$scaledDirection);
      $o = add($o,$scaledNormal);          
              
      $v = newVector(0.0, 0.0, 0.0);      
      $shadowRay = newRay($o, subtract($v,$light));      
      $shadowp = $this->intersect(
         $shadowRay, newIntersectionPoint(INFINITY, $p["normal"]) );      
          
      if (is_infinite($shadowp["distance"])){ return $greyscale; }  
      else { return 0.0; }         
   }                                                           
}


// a leaf node in the scene tree
class Sphere extends Scene {
   var $center, $radius; 
   
   function Sphere($center,$radius){   
      $this->center = $center;
      $this->radius = $radius;               
   }     
   
   function distance(&$ray){
      $v = subtract($this->center,$ray["origin"]);                      
      $b = dot($v,$ray["direction"]);                      
      $disc = $b*$b - dot($v,$v) + $this->radius*$this->radius;       
      if ($disc < 0.0){ return INFINITY; } // No intersection
        
      $d = sqrt($disc);   
      $t1 = $b + $d;  
      if ($t1 < 0.0){ return INFINITY; }             
              
      $t2 = $b - $d;     
      if ($t2 > 0.0){ return $t2; } else { return $t1; }                     
   }   
      
   function intersect(&$ray,&$p){      
      $d = $this->distance($ray);                    
      if ($d < $p["distance"]){                                
         $rayOrigin = $ray["origin"];
         $rayDirection = $ray["direction"];
         $scaledDirection = scaleBy($rayDirection,$d);  
                
         $v = add($rayOrigin,subtract($scaledDirection,$this->center ));                           
         $p = newIntersectionPoint($d, normalize($v));         
      }             
      return $p; 
   }                           
}


// non-leaf node in the scene tree
class Group extends Scene {
   var $bound, $scenes;        
       
   function Group($bound){
      $this->bound = $bound;  
      $this->scenes = array();      
   }           
   
   function intersect(&$ray,&$p){     
      if (($this->bound -> distance($ray)) < $p["distance"]){                         
         foreach ($this->scenes as $each){
            $p = $each->intersect($ray,$p);                  
         }      
      }           
      return $p; 
   }    
      
   function add($s){  
      array_unshift($this->scenes, $s);   
   }                      
}


////////////////////////////////////////////////////////////////////

$n = ($argc == 2) ? $argv[1] : 1;

$scene = Scene::sphereScene(LEVELS, newVector(0.0, -1.0, 0.0), 1.0);

printf("P5\n%d %d\n255\n", $n,$n);

for ($y=$n-1; $y>=0; --$y){
   for ($x=0; $x<$n; ++$x){
   
      $greyscale = 0.0;
      for ($dx=0; $dx<SS; ++$dx){
         for ($dy=0; $dy<SS; ++$dy){   
         
            $v = newVector(
                 $x + ($dx/(double)SS) - ($n/2.0)
               , $y + ($dy/(double)SS) - ($n/2.0)
               , $n );                                                       
                              
            $ray = newRay(newVector(0.0, 0.0, -4.0), normalize($v));  
                                                                  
            $u = newVector(-1.0, -3.0, 2.0);                          
            $greyscale += $scene->traceRay($ray, normalize($u));                                    
         }
      }          
      echo chr( 0.5 + 255.0*$greyscale/(double)(SS*SS) );                                          
   }
}           
?>
<? /* The Computer Language Shootout 
   http://shootout.alioth.debian.org/  
   contributed by Isaac Gouy 
*/

function ack($m, $n){
   if($m == 0) return $n+1;
   if($n == 0) return ack($m-1, 1);
   return ack($m - 1, ack($m, ($n - 1)));
}

function fib($n){ 
   if($n < 2) { return 1; }
   else { return fib($n - 2) + fib($n - 1); }
}

function tak($x,$y,$z){
   if($y < $x){ return tak( tak($x-1,$y,$z), tak($y-1,$z,$x), tak($z-1,$x,$y)); }
   else { return $z; }
}

$n = $argv[1]; 
echo "Ack(3,$n): ", ack(3,$n),"\n";
printf("Fib(%.1f): %.1f\n", 27.0+$n, fib(27.0+$n));
$n--; printf("Tak(%d,%d,%d): %d\n", 3*$n,2*$n,$n, tak(3*$n,2*$n,$n));

printf("Fib(3): %d\n", fib(3));
printf("Tak(3.0,2.0,1.0): %.1f\n", tak(3.0,2.0,1.0));
?>
<?php
#
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by Danny Sauer
# modified by Josh Goldfoot

# regexp matches

#ini_set("memory_limit","40M");

$variants = array(
    'agggtaaa|tttaccct',
    '[cgt]gggtaaa|tttaccc[acg]',
    'a[act]ggtaaa|tttacc[agt]t',
    'ag[act]gtaaa|tttac[agt]ct',
    'agg[act]taaa|ttta[agt]cct',
    'aggg[acg]aaa|ttt[cgt]ccct',
    'agggt[cgt]aa|tt[acg]accct',
    'agggta[cgt]a|t[acg]taccct',
    'agggtaa[cgt]|[acg]ttaccct',
);

# IUB replacement parallel arrays
$IUB = array(); $IUBnew = array();
$IUB[]='/B/';     $IUBnew[]='(c|g|t)';
$IUB[]='/D/';     $IUBnew[]='(a|g|t)';
$IUB[]='/H/';     $IUBnew[]='(a|c|t)';
$IUB[]='/K/';     $IUBnew[]='(g|t)';
$IUB[]='/M/';     $IUBnew[]='(a|c)';
$IUB[]='/N/';     $IUBnew[]='(a|c|g|t)';
$IUB[]='/R/';     $IUBnew[]='(a|g)';
$IUB[]='/S/';     $IUBnew[]='(c|g)';
$IUB[]='/V/';     $IUBnew[]='(a|c|g)';
$IUB[]='/W/';     $IUBnew[]='(a|t)';
$IUB[]='/Y/';     $IUBnew[]='(c|t)';

# sequence descriptions start with > and comments start with ;
#my $stuffToRemove = '^[>;].*$|[\r\n]';
$stuffToRemove = '^>.*$|\n'; # no comments, *nix-format test file...

# read in file
$contents = file_get_contents('php://stdin');
$initialLength = strlen($contents);

# remove things
$contents = preg_replace("/$stuffToRemove/m", '', $contents);
$codeLength = strlen($contents);

# do regexp counts
foreach ($variants as $regex){
    print $regex . ' ' . preg_match_all("/$regex/i", $contents, $discard). "\n";
}

# do replacements
$contents = preg_replace($IUB, $IUBnew, $contents);

print "\n" .
      $initialLength . "\n" .
      $codeLength . "\n" .
      strlen($contents) . "\n" ;
?>
<?php
#
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by Danny Sauer
# modified by Josh Goldfoot
# modified by Sergey Khripunov

# regexp matches

#ini_set("memory_limit","40M");

$variants = array(
    'agggtaaa|tttaccct',
    '[cgt]gggtaaa|tttaccc[acg]',
    'a[act]ggtaaa|tttacc[agt]t',
    'ag[act]gtaaa|tttac[agt]ct',
    'agg[act]taaa|ttta[agt]cct',
    'aggg[acg]aaa|ttt[cgt]ccct',
    'agggt[cgt]aa|tt[acg]accct',
    'agggta[cgt]a|t[acg]taccct',
    'agggtaa[cgt]|[acg]ttaccct',
);

# IUB replacement parallel arrays
$IUB = array(); $IUBnew = array();
$IUB[]='/B/S';     $IUBnew[]='(c|g|t)';
$IUB[]='/D/S';     $IUBnew[]='(a|g|t)';
$IUB[]='/H/S';     $IUBnew[]='(a|c|t)';
$IUB[]='/K/S';     $IUBnew[]='(g|t)';
$IUB[]='/M/S';     $IUBnew[]='(a|c)';
$IUB[]='/N/S';     $IUBnew[]='(a|c|g|t)';
$IUB[]='/R/S';     $IUBnew[]='(a|g)';
$IUB[]='/S/S';     $IUBnew[]='(c|g)';
$IUB[]='/V/S';     $IUBnew[]='(a|c|g)';
$IUB[]='/W/S';     $IUBnew[]='(a|t)';
$IUB[]='/Y/S';     $IUBnew[]='(c|t)';

# sequence descriptions start with > and comments start with ;
#my $stuffToRemove = '^[>;].*$|[\r\n]';
$stuffToRemove = '^>.*$|\n'; # no comments, *nix-format test file...

# read in file
$contents = file_get_contents('php://stdin');
$initialLength = strlen($contents);

# remove things
$contents = preg_replace("/$stuffToRemove/mS", '', $contents);
$codeLength = strlen($contents);

# do regexp counts
foreach ($variants as &$regex){
    print $regex . ' ' . preg_match_all('/'.$regex.'/iS', $contents, $discard). "\n";
}

# do replacements
$contents = preg_replace($IUB, $IUBnew, $contents);

print "\n" .
      $initialLength . "\n" .
      $codeLength . "\n" .
      strlen($contents) . "\n" ;
?>
#!/usr/bin/php -f 
<?
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q regexmatch.php 12000 < input.txt
*/ 


$regex = "/" 
         ."(?:^|[^\d\(])" 
         ."(?:\((\d\d\d)\)|(\d\d\d))"    
         ."[ ]" 
         ."(\d\d\d)"
         ."[ -]"
         ."(\d\d\d\d)"
         ."\D" 
         ."/";


$n = ($argc == 2) ? $argv[1] : 1;

$phoneNumbers = file("php://stdin");
$last = sizeof($phoneNumbers)-1;
$count = 0;

while ($n--)
   for ($i=0; $i<$last; $i++){
      preg_match($regex, $phoneNumbers[$i], $match);
      if (($n == 0) && $match){ 
         $m = $match[1] ? $match[1] : $match[2];
         printf("%d: (%s) %s-%s\n", ++$count, $m, $match[3], $match[4]);
         }
   }

?>
<?php
#
# TheComputer Language Shootout
# http://shootout.alioth.debian.org/
#
# reverse complement in PHP
# contributed by Danny Sauer
#

# We'll need some definitions
define( 'LINE_LENGTH', 60 );
define( 'SRC', 'CGATMKRYVBHD');
define( 'DST', 'GCTAKMYRBVDH');
$str = '';
$seq = '';

# read in the file, a line at a time
while( !feof(STDIN) ) {
    $str = trim(fgets(STDIN));
    if( $str[0] == '>' ){
        # if we're on a comment line, print the previous seq and move on
        print_seq();
        echo $str, "\n";
    }else{
        # otherwise, just append to the sequence
        $seq .= $str;
    }
}
print_seq();

exit;

# print the sequence out, if it exists
function print_seq(){
    global $seq; # no time-consuming argument passing for us! :)
    if($seq != ''){
        echo wordwrap( strrev( strtr(strtoupper($seq), SRC, DST) ),
                       LINE_LENGTH, "\n", true ), "\n";
    }
    $seq = '';
}
?>
<?php
#
# TheComputer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# reverse complement in PHP
# contributed by Danny Sauer
# modified by anon

ob_implicit_flush(1);
ob_start(NULL, 4096);

$str = '';
$seq = '';

# read in the file, a line at a time
$stdin = STDIN;
while( $str !== '' || !feof($stdin) ) {
    $str = stream_get_line($stdin, 100, "\n");
    if( isset($str[0]) && $str[0] === '>' ){
        # if we're on a comment line, print the previous seq and move on
        print_seq($seq);
        echo $str, "\n";
    }else{
        # otherwise, just append to the sequence
        $seq .= $str;
    }
}
print_seq($seq);

exit;

# print the sequence out, if it exists
function print_seq(&$seq){
    if ( $seq !== '' ) {
        echo chunk_split( strrev( strtr($seq, 'ACBDGHKMNSRUTWVYacbdghkmnsrutwvy', 'TGVHCDMKNSYAAWBRTGVHCDMKNSYAAWBR') ),
		60, "\n");
    }
    $seq = '';
}
?>
#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q reversefile.php < in.txt > out.txt
*/ 


$fp = fopen("php://stdin", "r");
while ($b = fread($fp, 4096)) $blocks[]=$b;
fclose($fp);

$lines = explode("\n", implode('',$blocks));
$last = count($lines)-1;

// Skip the null char string terminator
if (ord($lines[$last])==0) $last--;  

for ($i=$last; $i>=0; $i--) echo $lines[$i], "\n";

?>
#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q sieve.php 9000
*/ 


$n = ($argc == 2) ? $argv[1] : 1;

$stop = 8192;
$count = 0;

while ($n-- > 0) {
   $count = 0;
   $i = $stop; while ($i--) $isPrime[$i] = 1;

   $i = 2; while ($i++ < $stop){
      if ($isPrime[$i]){
         for ($k=$i+$i; $k<=$stop; $k+=$i) $isPrime[$k] = 0;
         $count++;
      }
   }
} 
print "Count: $count\n";

?>
<? 
/* The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

contributed by Isaac Gouy
modified by anon
*/


function A(&$i, &$j){
   return 1.0 / ( ( ( ($i+$j) * ($i+$j+1) ) >> 1 ) + $i + 1 );
}

function Av(&$n,&$v){
   global $_tpl;
   $Av = $_tpl;
   for ($i = 0; $i < $n; ++$i) {
      $sum = 0.0;
      foreach($v as $j=>$v_j) {
         $sum += A($i,$j) * $v_j;
      }
      $Av[$i] = $sum;
   }
   return $Av;
}

function Atv(&$n,&$v){
   global $_tpl;
   $Atv = $_tpl;
   for($i = 0; $i < $n; ++$i) {
      $sum = 0.0;
      foreach($v as $j=>$v_j) {
         $sum += A($j,$i) * $v_j;
      }
      $Atv[$i] = $sum;
   }
   return $Atv;
}

function AtAv(&$n,&$v){
   $tmp = Av($n,$v);
   return Atv($n, $tmp);
}

$n = intval(($argc == 2) ? $argv[1] : 1);
$u = array_fill(0, $n, 1.0);
$_tpl = array_fill(0, $n, 0.0);

for ($i=0; $i<10; $i++){
   $v = AtAv($n,$u);
   $u = AtAv($n,$v);
}

$vBv = 0.0;
$vv = 0.0;
$i = 0;
foreach($v as $val) {
   $vBv += $u[$i]*$val;
   $vv += $val*$val;
   ++$i;
}
printf("%0.9f\n", sqrt($vBv/$vv));

<? 
/* 
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy
   multicore by anon
 */


function A(&$i, &$j){
   return 1.0 / ( ( ( ($i+$j) * ($i+$j+1) ) >> 1 ) + $i + 1 );
}

function Av(&$n,&$v,&$start,&$end){
   global $_tpl;
   $Av = $_tpl;
   for ($i = $start; $i < $end; ++$i) {
      $sum = 0.0;
      foreach($v as $j=>$v_j) {
         $sum += A($i,$j) * $v_j;
      }
      $Av[$i] = $sum;
   }
   return $Av;
}

function Atv(&$n,&$v,&$start,&$end){
   global $_tpl;
   $Atv = $_tpl;
   for($i = $start; $i < $end; ++$i) {
      $sum = 0.0;
      foreach($v as $j=>$v_j) {
         $sum += A($j,$i) * $v_j;
      }
      $Atv[$i] = $sum;
   }
   return $Atv;
}

function AtAv(&$n,&$v,&$start,&$end,&$sync){

   $tmp = Av($n, $v, $start, $end);
   if ($sync) sync($tmp);

   $tmp = Atv($n, $tmp, $start, $end);
   if ($sync) sync($tmp);

   return $tmp;
}

function sync(&$tmp) {

   global $parent,$chunk_data_size,$total_data_size,$pipe,$pipes;

   if (!$parent) {
      array_unshift($tmp, 'd*');
      $data = call_user_func_array('pack', $tmp);
      safe_write($pipe, $data);
      $tmp = array_merge(array(), unpack('d*', safe_read($pipe, $total_data_size)));
   } else {
      $tmps = array(array('d*'));
      foreach($pipes as $pipe) {
         $tmps[] = unpack('d*', safe_read($pipe, $chunk_data_size));
      }
      $tmps[] = &$tmp;
      $tmp = call_user_func_array('array_merge', $tmps);

      $data = call_user_func_array('pack', $tmp);
      foreach($pipes as $pipe) {
         safe_write($pipe, $data);
      }
      array_shift($tmp);
   }
}

function safe_write($fd, $data) {
   $len = strlen($data);
   do {
      $w = fwrite($fd, $data);
      $len -= $w;
   } while($len && ($data = substr($data, $w)) !== FALSE);
}
function safe_read($fd, $len) {
   $data = '';
   while ($len > 0) {
      $d = fread($fd, $len);
      $len -= strlen($d);
      $data .= $d;
   }
   return $data;
}
function pipe() {
   return stream_socket_pair(STREAM_PF_UNIX, STREAM_SOCK_STREAM, 0);
}


$n = (int) (($argc == 2) ? $argv[1] : 1);

$procs = 1;
if (file_exists('/proc/cpuinfo')) {
   $procs = preg_match_all('/^processor\s/m', file_get_contents('/proc/cpuinfo'), $discard);
}

if ($n < $procs) {
   $procs = 1;
}

$chunk_size = (int) ($n / $procs);
$double_size = strlen(pack('d', 0.0));
$chunk_data_size = $double_size * $chunk_size;
$total_data_size = $double_size * $n;

$pipes = array();
$parent = FALSE;
for($i = 0; $i < $procs; ++$i) {
   $range_begin = $i * $chunk_size;
   if ($i < ($procs - 1)) {
      $pipe = pipe();
      $pipes[] = $pipe[0];
      $pipe = $pipe[1];
      $range_end = $range_begin + $chunk_size;
      $pid = pcntl_fork();
      if ($pid === -1) {
         die('could not fork');
      } else if ($pid) {
         continue;
      }
      break;
   } else {
      $range_end = $n;
      $parent = TRUE;
   }
}

$u = array_fill(0, $n, 1.0);
$_tpl = array_fill($range_begin, $range_end - $range_begin, 0.0);
$sync = $procs > 0;

for ($i=0; $i<10; $i++){
   $v = AtAv($n,$u,$range_begin,$range_end,$sync);
   $u = AtAv($n,$v,$range_begin,$range_end,$sync);
}

if (!$parent) {
   exit(0);
}

$childs = $procs - 1;
while ($childs--) {
   pcntl_wait($s);
}

$vBv = 0.0;
$vv = 0.0;
$i = 0;
foreach($v as $val) {
   $vBv += $u[$i]*$val;
   $vv += $val*$val;
   ++$i;
}
printf("%0.9f\n", sqrt($vBv/$vv));

#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q spellcheck.php < input.txt
*/ 


$fp = fopen("Usr.Dict.Words", "r");
while ($line = fgets($fp, 128)) { $dict[chop($line)] = 1; }
fclose($fp);


$fp = fopen("php://stdin", "r");
while ($line = fgets($fp, 128)) {
    $line = chop($line);

    if (!isset($dict[$line])) print "$line\n";
}
fclose($fp);

?>
#!/usr/bin/php -f
<?php
/*
 $Id: strcat.php,v 1.2 2004-10-11 04:48:01 bfulgham Exp $
 http://shootout.alioth.debian.org/
*/
$n = ($argc == 2) ? $argv[1] : 1;
$str = "";
while ($n-- > 0) {
    $str .= "hello\n";
}
$len = strlen($str);
print "$len\n";
?>
#!/usr/bin/php -f
<?php
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q sumcol.php < in.txt
*/ 


$sum = 0;

$fp = fopen("php://stdin", "r");
while ($line = fgets($fp, 128)) $sum += intval($line); 
fclose($fp);

print "$sum\n"; 

?>
<? /* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/
   
   contributed by Isaac Gouy 
   
   php -q takfp.php 10
*/

function Takfp($x, $y, $z){
   if ($y>=$x){ 
      return $z; 
   }
   else {
      return Takfp( Takfp($x-1.0,$y,$z), Takfp($y-1.0,$z,$x),  Takfp($z-1.0,$x,$y));
   }   
}

$n = ($argc == 2) ? $argv[1] : 1;
printf("%.1f\n", Takfp($n*3.0,$n*2.0,$n*1.0));

?>
<? /* The Great Computer Language Shootout
      contributed by Isaac Gouy 

      php -q wc.php < in.txt 
*/ 


$nl = $nw = $nc = 0;
$hasSplitWord = FALSE;

$fp = fopen("php://stdin", "r");
while ($block = fread($fp, 4096)){

   $nc += strlen($block);

   $pos = 0;
   while ($pos = strpos($block,"\n", $pos+1)) $nl++;


   $words = preg_split('/\s+/', $block);

   if ($size = sizeof($words)){
      $nw += $size;                 // count all the splits as words 
                                    // and then correct for empty words
      if (strlen($words[0])){          
         if ($hasSplitWord) $nw--;  // we counted the split-word twice
      } else {
         $nw--;                     // there was no first word
      }
      $hasSplitWord = strlen($words[$size-1]);
      if (!$hasSplitWord) $nw--;    // there was no last word
   }

   unset($words);
}
fclose($fp);

print "$nl $nw $nc\n";

?>
#!/usr/bin/php -f
<?
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy 

   php -q wordfreq.php < input.txt > output.txt
*/ 


$hasSplitWord = FALSE;

$fp = fopen("php://stdin", "r");

while ($block = fread($fp, 4096)){

   $words = preg_split('/[^a-zA-Z]+/', $block);

   $first = 0;
   $last = sizeof($words) - 1;

   if ($last >= $first){
      
      // some words will be split across $block's      

      if (strlen($words[$first])){          
         if ($hasSplitWord){ $words[$first] = $splitWord.$words[$first]; }
      } else {
         if ($hasSplitWord){ $words[$first] = $splitWord; } 
         else { $first++; }      
      }
      if ($hasSplitWord = strlen($words[$last])){
         $splitWord = $words[$last]; 
      }


      for ($i=$first; $i<$last; $i++){
         $w = strtolower($words[$i]);
         if (isset($counts[$w])){ $counts[$w]++; } else { $counts[$w] = 1; }
      }
   }
   unset($words);
} 
fclose($fp);



function CmpCountAndName($a, $b){
   if ($a[1] == $b[1]){ return strcmp($b[0], $a[0]); }
   else { return $a[1] < $b[1]; }
}

while (list($k,$v) = each($counts)) $wordcounts[] = array($k,$v);
usort($wordcounts,'CmpCountAndName');
while (list($k,$v) = each($wordcounts)) printf("%7d %s\n", $v[1], $v[0]);

?>
