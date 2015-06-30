The Computer Language Shootout http://shootout.alioth.debian.org/

REBOL [Title: "ackermann" Date: 21-Jun-2003 Author: "Isaac Gouy (Rebol novice)"]

; get the command line arg
argN: does [
   n: to-integer system/script/args
   either n > 0 [n] [1]
   ]



n: argN

ack: func [m n][
   if m = 0 [return n + 1]
   if n = 0 [return ack (m - 1) 1]
   return ack (m - 1) (ack m (n - 1)) 
   ]

print rejoin ["Ack(3," n "): " ack 3 n] 
REBOL [
	Title: "binary-trees"
	Author: "Robert Brandner"
]

node: make object! [
	left: none
	right: none
	item: none
]

check-item: func [n] [return either not n/left 
   [n/item][n/item + (check-item n/left) - (check-item n/right)]]

bottom-up: func [i depth /local n l r][
	either depth > 0 [l: bottom-up (2 * i - 1) (depth - 1)
					  r: bottom-up (2 * i) (depth - 1)
					  n: make node [left: l right: r item: i]]
					 [n: make node [item: i]]
	return n
]

n: either n: system/script/args [to integer! n] [4]
min-depth: 4
max-depth: max (min-depth + 2) n
stretch-depth: n + 1
stretched-tree: bottom-up 0 stretch-depth
print rejoin ["stretch tree of depth " stretch-depth "^- check: " check-item stretched-tree]
longlived-tree: bottom-up 0 n
for depth min-depth max-depth 2 [
	iterations: to-integer 2 ** (max-depth - depth + min-depth)
	check: 0
	repeat i iterations [
 		t: bottom-up i depth
		check: check + (check-item t)
 		t: bottom-up - i depth
		check: check + (check-item t)
	]
	print rejoin [2 * iterations "^- trees of depth " depth "^- check: " check]
]
print rejoin ["long lived tree of depth " n "^- check: " check-item longlived-tree]
REBOL [
	Title: "fannkuch"
	Author: "Robert Brandner"
]

n: either n: system/script/args [to integer! n] [7]

times-rotated: make block! n
insert/dup times-rotated 0 n
perm: make block! n
repeat i n [append perm i]

next-permutation: does [
	for r 2 n 1 [
		; rotate the first r items to the left
		temp: pick perm 1
		for i 1 (r - 1) 1 [poke perm i (pick perm (i + 1))]
		poke perm r temp
		poke times-rotated r ((pick times-rotated r) + 1)
		reminder: (pick times-rotated r) // r
		if reminder <> 0 [return perm]
	]
	return none
]

count-flips: does [
	pk: copy perm
	cnt: 0
	while [pk/1 <> 1][
		reverse/part pk pk/1
		cnt: cnt + 1
	]
	return cnt
]

mx: 0
show: 0
while [perm] [
	if (show < 30) [print rejoin perm show: show + 1]
	mx: max mx count-flips
	perm: next-permutation
]
print rejoin ["Pfannkuchen(" n ") = " mx]
rebol[
	Title:  "fasta"
	Author: "Tom Conlin"
	Date:    2005-11-29
	purpose: {	The Great Computer Language Shootout
	            http://shootout.alioth.debian.org/gp4/benchmark.php?test=fasta
	         }
	summary: [rebol fasta tom conlin 2005-11-29]
    version: 0.1
]

ALU: {GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG
GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA
CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT
ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA
GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG
AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC
AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA}

IUB: [0.27 0.12 0.12 0.27] loop 11[insert tail IUB 0.02]
HomoSap: [0.3029549426680 0.1979883004921 0.1975473066391 0.3015094502008]

repeat-fasta: func [n [integer!] seq[series!] /local rest][
	seq: replace/all seq "^/" ""
	loop to integer!(n / 60)[
		either 60 < length? seq
			[print copy/part seq 60
			 seq: skip seq 60
			][rest: 60 - length? seq
			 print head insert tail copy seq copy/part head seq rest
			 seq: skip head seq rest
			]
	]
	if not zero? rest: n // 60 [
		either rest <= length? seq
			[print copy/part seq rest]
			[print head insert tail copy seq copy/part head seq rest - length? seq]
	]
]

rand-fasta: func [n[integer!] probs[block!] /local prev line][
	symbol: ["a" "c" "g" "t" "B" "D" "H" "K" "M" "N" "R" "S" "V" "W" "Y"]
	line: make string! 60
	probs: next probs
	forall probs[change probs add first back probs first probs]
	probs: head probs
	loop to integer! (n / 60)[
		loop 60 [insert tail line pick symbol bin-srch probs gen-rand]
		print line clear line
	]
	loop n // 60[insert tail line pick symbol bin-srch probs gen-rand]
	if not empty? line [print line]
]

gen-rand: does[
	prev: prev * 3877 + 29573 // 139968
	.00000714449016918152720622 * prev
]
bin-srch: func[b k /local l m h][
	l: 1 h: length? b m: to integer!(h - l / 2)+ l
	while[l <= h][either k = b/:m [return m]
		[either k > b/:m[l: m + 1][h: m - 1]]
		m: to integer!(h - l / 2)+ l
	] 1 + m
]

n: either n: system/script/args[to integer! n][1000]
print ">ONE Homo sapiens alu"
repeat-fasta 2 * n ALU
prev: 42
print ">TWO IUB ambiguity codes"
rand-fasta  3 * n IUB
print ">THREE Homo sapiens frequency"
rand-fasta  5 * n HomoSap

rebol [
  Title: harmonic
  Name: {The Great Computer Language Shootout}
  Home: {http://shootout.alioth.debian.org/}
  Author: {Anthony Borla}
  Date: 2005-12-31
  Version: 1.0
]

; -------------

harmonic: func [n /local sum] [
  sum: 0.0
  repeat i n [sum: sum + (1 / i)]
  sum
]

; -------------

N: either N: system/script/args [to-integer N][1]

print round/to harmonic N 0.000000001
The Computer Language Shootout http://shootout.alioth.debian.org/

REBOL [ Title: "startup" Date: 21-Jun-2003 Author: "Isaac Gouy (Rebol novice)" ]

print "hello world"

rebol[
	Title:  "k-nucleotide"
	Author: "Tom Conlin"
	Date:    2005-11-14
	purpose: {	The Great Computer Language Shootout
	            http://shootout.alioth.debian.org/
	         }
	summary: [rebol k-nucleotide tom conlin 2005-11-27]
    version: 1.17
]
;;; format utility 
decimal-pad: func[d[number!] p[integer!] /local r s t][
	d: to string! d
	either s: find/tail d "."
		[   either p >= length? s
			[insert/dup tail s "0" p - length? s]
			[	t: skip s p
				r: either #"5" > first t[0][10]
				while[not zero? r: to integer! r / 10][
					t: back t if #"." == first t[t: back t]
					r: -47 + first t
					change t r // 10
					if all[9 < r any[head? t #"-" == first t]][
						insert d "0" s: next s
					]
				]
				clear skip s p
			]
		]
		[insert tail d "." insert/dup tail d "0" p]
	d
]

;;; read line-by-line a redirected FASTA format file from stdin
set-modes system/ports/input [lines: false binary: false]

;;; extract DNA sequence THREE
here: find/tail find/tail copy system/ports/input "^/>THREE " "^/"
fasta: replace/all  here "^/" ""
len: 1 + length? fasta

k-nucl: func ["function to update a hash of k-nucleotide keys and count values"
    k [integer!] n[series!] hash [block!] /local l m ][
        m: copy/part n k
        either k = length? m
        [either l: find hash/:k m
                [l: next l change l 1 + first l]
                [insert tail hash/:k m insert tail hash/:k 1]
        ][return]
]

;;; count all the 3- 4- 6- 12- and 18-nucleotide sequences,
kay: [1 2 3 4 6 12 18]
mers: make block! last kay ;;; could sort if not ordered
loop last kay [insert/only tail mers make hash![]]

forall fasta[ kay: head kay
        forall kay[k-nucl first kay fasta mers]
]

;;; for all the 1-nucleotide and 2-nucleotide sequences,
;;; sorted by descending frequency and then ascending k-nucleotide key
repeat i 2 [sort/skip mers/:i 2 sort/skip head reverse mers/:i 2
        foreach [n c] head reverse mers/:i[
            print[uppercase n decimal-pad 100 * c / (len - i) 3]
        ]print ""
]

;;; write the count and code for specific sequences
foreach seq ["GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT"][
        l: length? seq ;;; newer REBOL versions would not need this line
        print rejoin[either c: select mers/:l seq [c]["0"] tab seq]
]
REBOL [
	Title: "mandelbrot" 
	Author: "Robert Brandner"
	Version: 1.1
]
n: either n: system/script/args [to-integer n] [200]

header: rejoin ["P4^(0A)" n " " n "^(0A)"]
write-io system/ports/output header length? header  ; to write newline as 0A in windows

nn: n - 1
iter: 50
limit2: 4.0
byte_acc: #"^@"
bit_num: 0

for y 0 nn 1 [
	for x 0 nn 1 [
		zr: zi: 0.0
		zr2: zi2: 0.0
		cr: ((2.0 * x) / n) - 1.5
        ci: ((2.0 * y) / n) - 1.0
        dot: true
        loop iter [        
        	tr: zr2 - zi2 + cr
            ti: (2 * (zr * zi)) + ci
    		zr: tr
    		zi: ti 
        	zr2: zr * zr
        	zi2: zi * zi
    		if ((zr2 + zi2) > limit2) [dot: false break]
    	]
		byte_acc: byte_acc * 2
		if dot [byte_acc: byte_acc or 1]
		bit_num: bit_num + 1
        if (bit_num == 8) or (x == nn) [
        	byte_acc: byte_acc * (2 ** (8 - bit_num))
			write-io system/ports/output to-string byte_acc 1  ; needed to write ^(null)
			byte_acc: #"^@"
			bit_num: 0
    	]    	
	]
]
REBOL [
	Title: "nbody" 
	Author: "Robert Brandner"
	Version: 1.0
]

solar-mass: 4 * pi * pi
days-per-year: 365.24

body: make object! [
	x: y: z: vx: vy: vz: mass: 0
	offset-momentum: func [px py pz][
		vx: - px / solar-mass
		vy: - py / solar-mass
		vz: - pz / solar-mass
   ]
]

sun: make body [
	mass: solar-mass
]

jupiter: make body [
	x: 4.84143144246472090e+00
	y: -1.16032004402742839e+00
	z: -1.03622044471123109e-01
	vx: 1.66007664274403694e-03 * days-per-year
	vy: 7.69901118419740425e-03 * days-per-year
	vz: -6.90460016972063023e-05 * days-per-year
	mass: 9.54791938424326609e-04 * solar-mass
]

saturn: make body [
	x: 8.34336671824457987e+00
	y: 4.12479856412430479e+00
	z: -4.03523417114321381e-01
	vx: -2.76742510726862411e-03 * days-per-year
	vy: 4.99852801234917238e-03 * days-per-year
	vz: 2.30417297573763929e-05 * days-per-year
	mass: 2.85885980666130812e-04 * solar-mass
]

uranus: make body [
	x: 1.28943695621391310e+01
	y: -1.51111514016986312e+01
	z: -2.23307578892655734e-01
	vx: 2.96460137564761618e-03 * days-per-year
	vy: 2.37847173959480950e-03 * days-per-year
	vz: -2.96589568540237556e-05 * days-per-year
	mass: 4.36624404335156298e-05 * solar-mass
]

neptune: make body [
	x: 1.53796971148509165e+01
	y: -2.59193146099879641e+01
	z: 1.79258772950371181e-01
	vx: 2.68067772490389322e-03 * days-per-year
	vy: 1.62824170038242295e-03 * days-per-year
	vz: -9.51592254519715870e-05 * days-per-year
	mass: 5.15138902046611451e-05 * solar-mass
]

bodies: [sun jupiter saturn uranus neptune]

advance: func [dt][
	forall bodies [
		b1: get first bodies
		rest: next bodies
		forall rest [
			b2: get first rest
			dx: b1/x - b2/x
			dy: b1/y - b2/y
			dz: b1/z - b2/z
			distance: square-root ((dx ** 2) + (dy ** 2) + (dz ** 2))
			mag: dt / (distance ** 3)
			b2massmag: b2/mass * mag
			b1massmag: b1/mass * mag
			b1/vx: b1/vx - (dx * b2massmag)
			b1/vy: b1/vy - (dy * b2massmag)
			b1/vz: b1/vz - (dz * b2massmag)
			b2/vx: b2/vx + (dx * b1massmag)
			b2/vy: b2/vy + (dy * b1massmag)
			b2/vz: b2/vz + (dz * b1massmag)
		]
	]
	forall bodies [
		b1: get first bodies
		b1/x: b1/x + (dt * b1/vx)	
		b1/y: b1/y + (dt * b1/vy)
		b1/z: b1/z + (dt * b1/vz)
	]		
]

energy: func [/local dx dy dz distance e b1 b2][
	e: 0.0
	forall bodies [
		b1: get first bodies
		rest: next bodies
		e: e + (0.5 * b1/mass * ((b1/vx ** 2) + (b1/vy ** 2) + (b1/vz ** 2)))
		forall rest [
			b2: get first rest
			dx: b1/x - b2/x
			dy: b1/y - b2/y
			dz: b1/z - b2/z
			distance: square-root ((dx ** 2) + (dy ** 2) + (dz ** 2))
			e: e - ((b1/mass * b2/mass) / distance)
		]
	]
	e
]

n: either n: system/script/args [to-integer n] [1000]

px: py: pz: 0
forall bodies [
	b1: get first bodies
	px: px + (b1/vx * b1/mass)
	py: py + (b1/vy * b1/mass)
	pz: pz + (b1/vz * b1/mass)
]	
sun/offset-momentum px py pz

print round/to energy 1e-09
loop n [advance 0.01]
print round/to energy 1e-09
REBOL [
	Title: "sieve of eratosthenes"
	File: %sieve.r
	Version: 0.2
	Author: "Robert Brandner"
	Date: 26-January-2006
]

n: either n: system/script/args [to integer! n] [2]

sieve: func [m] [
	prim: make block! m
	insert/dup prim true m
	head prim
	cnt: 0
	for i 2 m 1 [
		if (pick prim i) [
			cnt: cnt + 1
			for k (i + i) m i [
				poke prim k false
			]
		]
	]
	return cnt
]

; couldn't find something like printf in rebol ...
pretty: func [n] [
	str: make string! 8
	insert/dup str " " (8 - (length? to-string n))
	append str n
]

m: to-integer (2 ** n) * 10000
print ["Primes up to" pretty m pretty sieve m]
m: to-integer (2 ** (n - 1)) * 10000
print ["Primes up to" pretty m pretty sieve m]
m: to-integer (2 ** (n - 2)) * 10000
print ["Primes up to" pretty m pretty sieve m]

REBOL [
  Title: partialsums
  Name: {The Great Computer Language Shootout}
  Home: {http://shootout.alioth.debian.org/}
  Author: {Anthony Borla}
  Date: 2006-03-03
  Version: 1.0
  Notes: {Based on D language version by Dave Fladebo}
]

; -------------

a1: a2: a3: a4: a5: a6: a7: a8: a9: 0.0

; -------------

computeSums: func [n /local d d2 d3 ds dc alt] [
  alt: 1.0

  repeat d n [
    d2: d * d
    d3: d2 * d
    ds: sine d
    dc: cosine d

    a1: a1 + ((2.0 / 3.0) ** (d - 1.0))
    a2: a2 + (1.0 / (square-root d))
    a3: a3 + (1.0 / (d * (d + 1.0)))

    a4: a4 + (1.0 / (d3 * ds * ds))
    a5: a5 + (1.0 / (d3 * dc * dc))
    a6: a6 + (1.0 / d)

    a7: a7 + (1.0 / d2)
    a8: a8 + (alt / d)
    a9: a9 + (alt / ((2.0 * d) - 1.0))

    alt: -(alt)
  ]
]

; -------------

showSums: func [] [
  print rejoin [round/to a1 0.000000001 "^-(2/3)^^k"]
  print rejoin [round/to a2 0.000000001 "^-k^^-0.5"]
  print rejoin [round/to a3 0.000000001 "^-1/k(k+1)"]
  print rejoin [round/to a4 0.000000001 "^-Flint Hills"]
  print rejoin [round/to a5 0.000000001 "^-Cookson Hills"]
  print rejoin [round/to a6 0.000000001 "^-Harmonic"]
  print rejoin [round/to a7 0.000000001 "^-Riemann Zeta"]
  print rejoin [round/to a8 0.000000001 "^-Alternating Harmonic"]
  print rejoin [round/to a9 0.000000001 "^-Gregory"]
]

; -------------

N: either N: system/script/args [to-integer N][1]

computeSums N
showSums

quit/return 0

rebol[
	summary: [rebol random tom conlin 2005-11-26]
	version 0.1
]
;do %decimal-pad.r
decimal-pad: func [d[number!] p[integer!] /local r s t][
	d: to string! d
	either s: find/tail d "."
		[   either p >= length? s
			[insert/dup tail s "0" p - length? s]
			[	t: skip s p
				r: either #"5" > first t[0][10]
				while[not zero? r: to integer! r / 10][
					t: back t if #"." == first t[t: back t]
					r: -47 + first t
					change t r // 10
					if all[9 < r any[head? t #"-" == first t]][
						insert d "0" s: next s
					]
				]
				clear skip s p
			]
		]
		[insert tail d "." insert/dup tail d "0" p]
	d
]

prandom: context[
	IM: 139968
	IA: 3877
	IC: 29573
	last: 42.0
	gen-prandom: func [max[decimal!]][
		last:  last * IA + IC // IM
		max * last / IM
	]
	set 'advance-prandom func[N [integer!]][loop N[gen-prandom 100.0]]
	set 'reset-prandom does[last: 42.0]
]
N: either N: system/script/args[N][1]
print decimal-pad advance-prandom to integer! N 9

REBOL [
  Title: recursive
  Name: {The Great Computer Language Shootout}
  Home: {http://shootout.alioth.debian.org/}
  Author: {Anthony Borla}
  Date: 2006-02-27
  Version: 1.1
  Notes: {Code based on / inspired by existing, relevant Shootout submissions}
]

; -------------

ack: func [x y] [
  if x == 0 [return y + 1]
  if y == 0 [return (ack x - 1 1)]
  return (ack x - 1 (ack x y - 1))
]

; -------------

fib: func [n] [
  if n < 2 [return 1]
  return (fib n - 2) + (fib n - 1)
]

; -------------

tak: func [x y z] [
  if y < x [return (tak (tak x - 1.0 y z) (tak y - 1.0 z x) (tak z - 1.0 x y))]
  return z
]

; -------------

N: either N: system/script/args [to-integer N][1]

print rejoin ["Ack(3," N "): " (ack 3 N)]
print rejoin ["Fib(" (27.0 + N) "): " round/to (fib 27.0 + N) 0.1]

N: N - 1
print rejoin ["Tak(" (N * 3) "," (N * 2) "," N "): " round/to (tak N * 3 N * 2 N) 1]

print rejoin ["Fib(3): " (fib 3)]
print rejoin ["Tak(3.0,2.0,1.0): " round/to (tak 3.0 2.0 1.0) 0.1]

quit/return 0

REBOL [
    Title: "reverse-complement"
    Author: "Robert Brandner"
    Version: "0.3"
]
    
rev-compl3: func [/local res src dst cnt][
    reverse seq
    src: copy "CGATMKRYVBHD"
    dst: copy "GCTAKMYRBVDH"
    res: copy seq
    while [not tail? src][
        run: seq
        while [run: find run first src][
            poke res (index? run) (first dst)
            run: next run
        ]            
        src: next src
        dst: next dst
    ]
    forskip res 60 [print copy/part res 60]
]

seq: copy ""
i: input
while [i] [
    either (i/1 = #">") [
        if (length? seq) > 0 [
            rev-compl3
            seq: copy ""
        ]
        print i
    ][
        append seq i
    ]
    i: input
]
if (length? seq) > 0 [rev-compl3]

REBOL [
  Title: spectralnorm
  Name: {The Great Computer Language Shootout}
  Home: {http://shootout.alioth.debian.org/}
  Author: {Anthony Borla}
  Date: 2006-05-04
  Version: 1.0
]

; -----------------------------

approximate: func [n /local u v vBv vV] [
  u: make block! n
  insert/dup u 1.0 n

  v: make block! n
  insert/dup v 0.0 n

  vBv: vV: 0.0

  loop 10 [
    mulAtAv n u v
    mulAtAv n v u
  ]

  repeat i n [
    vBv: vBv + ((pick u i) * (pick v i))
    vV: vV + ((pick v i) * (pick v i))
  ]

  return square-root (vBv / vV)
]

; -------------

A: func [i j] [
  i: i - 1
  j: j - 1
  return 1.0 / ((((i + j) * ( i + j + 1.0)) / 2.0) + i + 1.0)
]

; -------------

mulAv: func [n v av] [
  repeat i n [
    poke av i 0.0
    repeat j n [
      poke av i ((pick av i) + ((A i j) * (pick v j)))
    ]
  ]
]

; -------------

mulAtV: func [n v atv] [
  repeat i n [
    poke atv i 0.0
    repeat j n [
      poke atv i ((pick atv i) + ((A j i) * (pick v j)))
    ]
  ]
]

; -------------

mulAtAv: func [n v atav /local u] [
  u: make block! n
  insert/dup u 0.0 n
  mulAv n v u
  mulAtV n u atav
]

; -----------------------------

N: either N: system/script/args [to-integer N][1]

print round/to (approximate N) 0.000000001

quit/return 0

rebol[ 
    summary: [rebol sum-file tom conlin 2005-11-26]
    version: 0.2
]
set-modes system/ports/input[lines: false binary: false]
b: load copy system/ports/input s: 0
foreach i b[s: s + i]
print s

rebol [
  Title: takfp
  Name: {The Computer Language Shootout}
  Home: {http://shootout.alioth.debian.org/}
  Author: {Anthony Borla}
  Date: 2005-12-31
  Version: 1.0
]

; -------------

tak: func [x y z] [either y >= x [z][tak tak x - 1.0 y z tak y - 1.0 z x tak z - 1.0 x y]]

; -------------

N: either N: system/script/args [to-integer N][1]
print tak N * 3.0 N * 2.0 N * 1.0
