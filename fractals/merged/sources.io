/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Ian Osgood */

ack := block(i, j,
    if (i == 0, j+1,
    if (j == 0, ack(i-1, 1),
                ack(i-1, ack(i, j-1)) ) )
)

n := args at(1) asNumber
writeln("Ack(3,", n, "): ", ack(3,n))
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Ian Osgood */

TreeNode := Object clone do(
    bottomUpTree := method(n,d,
        self item := n
        if (d>0,   n=2*n; d=d-1 
            self left  := TreeNode clone bottomUpTree(n-1, d)
            self right := TreeNode clone bottomUpTree(n,   d)
        )
        self
    )
    itemCheck := method(
        if (self hasSlot("left"), item + left itemCheck - right itemCheck, item)
    )
)

minDepth := 4
maxDepth := System args at(1) asNumber max(minDepth+2)

check := TreeNode clone bottomUpTree(0, maxDepth+1) itemCheck
writeln("stretch tree of depth ", maxDepth+1, "\t check: ", check)

longLivedTree := TreeNode clone bottomUpTree(0, maxDepth)

for (depth, minDepth, maxDepth, 2,
    iterations := 1 clone shiftLeft(maxDepth - depth + minDepth)
    check = 0
    for (i, 1, iterations,
        check = check + TreeNode clone bottomUpTree( i, depth) itemCheck
        check = check + TreeNode clone bottomUpTree(-i, depth) itemCheck
    )
    writeln(iterations*2, "\t trees of depth ", depth, "\t check: ", check)
)

check = longLivedTree itemCheck
writeln("long lived tree of depth ", maxDepth, "\t check: ", check)
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Ian Osgood */

n := args at(1) asNumber
meetings := 0
first := second := Nil

Creature := Object clone do(
    setColor  := method(n, self color := n; self)
    setRed    := method( setColor(1) )
    setYellow := method( setColor(2) )
    setBlue   := method( setColor(3) )

    complement := method(other,
        if (color == other, color, 6 - color - other)
    )
    meet := method(
        yield; while(Lobby second, yield)
        if (Lobby first) then (
            Lobby second = self
            other := Lobby first color
            Lobby first = Nil
        ) else (
            if (n==0, return Nil, Lobby n=n-1)
            Lobby first = self
            yield; while(Lobby second isNil, yield)
            other := Lobby second color
            Lobby second = Nil
        )
        other
    )
    run := method(
        while (other := meet,
            color = complement(other)
            Lobby meetings = Lobby meetings + 1
        )
    )
)

Creature clone setBlue   @@run
Creature clone setRed    @@run
Creature clone setYellow @@run
Creature clone setBlue   @@run

while(Scheduler activeActorCount > 1, yield)

meetings println

/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Gavin Harrison */

n := args at(1) asNumber
meetings := 0
first := second := Nil

Creature := Object clone do(
    setColor  := method(n, self color := n; self)
    setRed    := method( setColor(1) )
    setYellow := method( setColor(2) )
    setBlue   := method( setColor(3) )

    complement := method(other,
        if (color == other, color, 6 - color - other)
    )
    meet := method(
        yield; while(Lobby second, yield)
        if (Lobby first) then (
            Lobby second = self
            other := Lobby first color
            Lobby first = Nil
        ) else (
            if (n==0, return Nil, Lobby n=n-1)
            Lobby first = self
            yield; while(Lobby second isNil, yield)
            other := Lobby second color
            Lobby second = Nil
        )
        other
    )
    run := method(
        while (other := meet,
            color = complement(other)
            Lobby meetings = Lobby meetings + 1
        )
    )
)

Creature clone setBlue   @@run
Creature clone setRed    @@run
Creature clone setYellow @@run
Creature clone setBlue   @@run

while(Coroutine yieldingCoros size > 1, yield)

meetings println
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Ian Osgood */

fannkuch := method(n,
    a := List clone
    for (i,1,n, a append(i))
    r := n
    counts := a clone
    count := maxFlips := 0

    loop (
        // display the first 30 permutations
        if (count < 30, writeln(a join("")); count = count + 1)

        // eliminate bad choices
        if (a first != 1 and a last != n,
            // pour the batter
            p := a clone
            flips := 0
            // start flipping
            while ((j := p first) > 1,
                // reverse 0..j-1
                i := -1
                while ((i=i+1) < (j=j-1), p swapIndices(i,j))
                flips = flips + 1
            )
            if (flips > maxFlips, maxFlips = flips)
        )

        // generate another permutation
        while (r>1, counts atPut(r-1, r); r=r-1)
        loop (
            // -roll(r)
            a atInsert(r, a removeAt(0))
            
            if (counts atPut(r, counts at(r) - 1) > 0, break)

            if ((r=r+1) == n, return maxFlips)
        )
    )
)

n := System args at(1) asNumber
f := fannkuch(n)
writeln("Pfannkuchen(", n, ") = ", f)
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Ian Osgood */

ALU := String with(
       "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
       "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
       "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
       "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
       "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
       "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
       "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")

# probabilities for symbols in Fasta codes

IUB := list(0.27, 0.12, 0.12, 0.27)
11 repeat( IUB append(0.02) )

HomoSap := list(0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008)

Fasta := Object clone do(
    last := 42
    gen_random := method(
        (last = ((last * 3877 + 29573) % 139968)) / 139968
    )
    repeat := method(n,seq, i := li := 0
        line := Sequence clone setSize(60)
        n repeat(
            line atPut(li, seq at(i))
            li = li + 1
            if (li == line size, line println; li = 0)
            i = ((i+1) % seq size)
        )
        if (li != 0, line setSize(li) println)
    )
    codes := "acgtBDHKMNRSVWY"
    random := method(n,probs, sum := li := 0
        line := Sequence clone setSize(60)
        probs mapInPlace(prob, sum = sum + prob)
        n repeat(
            r := gen_random; i := 0
            while (r > probs at(i), i = i + 1)
            line atPut(li, codes at(i))
            li = li + 1
            if (li == line size, line println; li = 0)
        )
        if (li != 0, line setSize(li) println)
    )
)

n := System args at(1) asNumber

">ONE Homo sapiens alu" println
Fasta repeat(2*n, ALU)

">TWO IUB ambiguity codes" println
Fasta random(3*n, IUB)

">THREE Homo sapiens frequency" println
Fasta random(5*n, HomoSap)
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Ian Osgood */

sum := 0
for (d,1,args at(0) asNumber, sum = sum + 1/d)
sum asString(0,9) println

/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org   
 
   contributed by Isaac Gouy */

"hello world\n" print
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Ian Osgood, Steve Dekorte */

MandelbrotSet := Object clone do(
    max_iterations := 50
    limit          := 2 squared
    dim            := 200
    org_r := -1.5
    org_i := -1
    ext_r :=  2
    ext_i :=  2
        
    calc := method(
        cr := Vector clone setSize(dim squared)
        ci := Vector clone setSize(dim squared)

        size := 0
        for(y, 0, dim-1,
            Ci := ((y * ext_i / dim) + org_i)
            for(x, 0, dim-1,
                Cr := ((x * ext_r / dim) + org_r)
                cr atPut(size, Cr)
                ci atPut(size, Ci)
                size = size + 1
            )
        )

        zr := cr clone
        zi := ci clone
        zr2 := Vector clone setSize(size)
        zi2 := Vector clone setSize(size)
        temp := Vector clone setSize(size)
        
        max_iterations repeat(
            temp copy(zr) *= zi
        
            zr2 copy(zr) square
            zi2 copy(zi) square
        
            zr copy(zr2) -= zi2 += cr
            zi copy(temp) *= 2  += ci
        )

        self result := zi2 + zr2
    )

    printSet := method(
        writeln("P4\n", dim, " ", dim)
        out := File standardOutput
        i := 0
        dim repeat(
            (dim / 8) repeat(
                pixel := 0
                8 repeat(
                    pixel = pixel shiftLeft(1)
                    if (limit > result at(i), pixel = pixel | 1)
                    i = i + 1
                )
                out write(pixel asCharacter)
            )
            /* if (dim%8!=0, ) */
        )
    )
)

MandelbrotSet dim := System args at(1) asNumber
MandelbrotSet do( calc; printSet )
/* The Computer Language Shootout
   http://shootout.alioth.debian.org
   Contributed by Gavin Harrison */


create_coro := method(n,
	if(n > 1,
		coro := @create_coro(n-1)
		yield
		coro+1
	,
		yield
		1
	)
)

coro := @create_coro(500)
count := 0
(args at(1) asNumber) repeatTimes(
	yield
	count = coro + count
)

count println
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Ian Osgood */

# similar protocol as Vector (floats), but with Numbers (doubles)
Point := List clone do(
    setSlot("*", method(n, r := Point clone; self foreach(v, r append(v * n)); r) )
    setSlot("/", method(n, r := Point clone; self foreach(v, r append(v / n)); r) )
    square := method( r := Point clone; self foreach(v, r append(v squared)); r)
    # faster in-place operations
    copy := method(p, p foreach(i,v, self atPut(i,v)); self)
    setSlot("*=", method(n, mapInPlace(v, v * n)) )
    setSlot("-=", method(p, mapInPlace(i,v, v - p at(i))) )
    setSlot("+=", method(p, mapInPlace(i,v, v + p at(i))) )
    distanceTo := method(p,
        d := 0
        self foreach(i,v, d = d +(v -(p at(i)) squared) )
        d sqrt
    )
)
vector := method( Point clone appendSeq(thisMessage argsEvaluatedIn(sender)) )

Body := Object clone do(
    solarMass := 4 * Number constants pi squared
    daysPerYear := 365.24
)

bodies := list(
    # Sun
    Body clone do(
    	p := vector(0,0,0)
    	v := vector(0,0,0)
        mass := solarMass
    ),
	# Jupiter
    Body clone do(
        p := vector( 4.84143144246472090e00,
                    -1.16032004402742839e00,
                    -1.03622044471123109e-01)
        v := vector( 1.66007664274403694e-03,
                     7.69901118419740425e-03,
                    -6.90460016972063023e-05) * daysPerYear
        mass :=      9.54791938424326609e-04 * solarMass
    ),
    # Saturn
    Body clone do(
        p := vector( 8.34336671824457987e00,
                     4.12479856412430479e00,
                    -4.03523417114321381e-01)
        v := vector(-2.76742510726862411e-03,  
                     4.99852801234917238e-03,
                     2.30417297573763929e-05) * daysPerYear
        mass :=      2.85885980666130812e-04 * solarMass
    ),
    # Uranus
    Body clone do(
        p := vector( 1.28943695621391310e01,
                    -1.51111514016986312e01,
                    -2.23307578892655734e-01)
        v := vector( 2.96460137564761618e-03,
                     2.37847173959480950e-03,
                    -2.96589568540237556e-05) * daysPerYear
        mass :=      4.36624404335156298e-05 * solarMass
    ),
    # Neptune
    Body clone do(
        p := vector( 1.53796971148509165e01,
                    -2.59193146099879641e01,
                     1.79258772950371181e-01)
        v := vector( 2.68067772490389322e-03,  
                     1.62824170038242295e-03,
                    -9.51592254519715870e-05) * daysPerYear
        mass :=      5.15138902046611451e-05 * solarMass
    )
)

# offset momentum

p := vector(0,0,0)
bodies foreach (body, p -= body v * body mass)
bodies at(0) v = p / Body solarMass

bodies energy := method(
    e := 0
    self foreach (i, body,
        e = e + body mass * body v square sum / 2
        last(size-i-1) foreach (body2,
            e = e - body mass * body2 mass / body p distanceTo(body2 p)
        )
    )
    e
)

bodies advance := method(dt,
    dp := vector(0,0,0); dp2 := vector(0,0,0)
    self foreach(i, body,
        last(size-i-1) foreach (body2,
            dp2 copy(dp copy(body p) -= body2 p)
            mag := dt / body p distanceTo(body2 p) pow(3)
            body  v -= dp  *= (body2 mass * mag)
            body2 v += dp2 *= (body  mass * mag)
        )
    )
    self foreach(body, body p += (dp copy(body v) *= dt) )
)

bodies energy asString(0,9) println

System args at(1) asNumber repeatTimes( bodies advance(0.01) )

bodies energy asString(0,9) println
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Ian Osgood */

Sequence pad := method(w, s:=self; while (s size < w, s=" ".. s) )

sieve := Object clone do(
    flags := List clone
    show := method(n,
        flags empty preallocateToSize(n)
        n repeat(flags append(Object))  # true
        primes := 0
        for (i, 2, n-1,
            if (flags at(i),
                primes = primes + 1
                if (i+i < n, for (j, i+i, n-1, i, flags atPut(j, Nil) ) )
            )
        )
        writeln("Primes up to", n asString pad(9), primes asString pad(9))
    )
)

n := System args at(1) asNumber
sieve show(10000 shiftLeft(n))
sieve show(10000 shiftLeft(n-1))
sieve show(10000 shiftLeft(n-2))

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Robert Brandner */


mkbuf := method(n,
	b := List clone
	b preallocateToSize(n)
	n repeat(b append(true))
	return b
)

nsieve := method(n,
	primes := mkbuf(n)
	cnt := 0
	for(i, 2, n, 
		if(primes at(i),
			k := i + i
			while (k < n,
				primes atPut(k, false)
				k = k + i
			)
			cnt = cnt + 1
		)
	)
	writeln("Primes up to", n asString alignRight(9, " "), cnt asString alignRight(9, " "))
)

n := System args at(1) asNumber
nsieve( (1<<n)*10000 )
nsieve( (1<<(n-1))*10000 )
nsieve( (1<<(n-2))*10000 )
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Ian Osgood */

Sequence pad := method(w, s:=self; while (s size < w, s=" ".. s) )
Number clearBit := method(b, &(1 clone shiftLeft(b) bitwiseComplement) )

sieve := Object clone do(
    flags := Sequence clone do(
        isSet := method(i, at(i/8) at(i%8) != 0)
        clear := method(i, atPut(i/8, at(i/8) clearBit(i%8) ) )
    )
    show := method(n,
        flags setSize((n/8) ceil)
        for (i, 0, flags size - 1, flags atPut(i, 255))
        primes := 0
        for (i, 2, n-1,
            if (flags isSet(i),
                primes = primes + 1
                if (i+i < n, for (j, i+i, n-1, i, flags clear(j) ) )
            )
        )
        writeln("Primes up to", n asString pad(9), primes asString pad(9))  
    )
)

n := System args at(1) asNumber
sieve show(10000 shiftLeft(n))
sieve show(10000 shiftLeft(n-1))
sieve show(10000 shiftLeft(n-2))
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org

   based on D language implementation by Dave Fladebo 
   contributed by Anthony Borla */


n := System args at(1) asNumber

s0 := 0 ; s1 := 0 ; s2 := 0 ; s3 := 0 ; s4 := 0 ; s5 := 0 ; s6 := 0
s7 := 0 ; s8 := 0 ; d2 := 0 ; d3 := 0 ; ds := 0 ; dc := 0 ; alt := 1

for (d, 1, n,

  d2 = d * d ; d3 = d2 * d ; ds = (d sin) ; dc = (d cos)

  s0 = s0 + ((2.0 / 3.0) pow(d - 1))
  s1 = s1 + 1 / (d sqrt)
  s2 = s2 + 1 / (d * (d + 1))
  s3 = s3 + 1 / (d3 * ds * ds)
  s4 = s4 + 1 / (d3 * dc * dc)
  s5 = s5 + 1 / d
  s6 = s6 + 1 / d2
  s7 = s7 + alt / d
  s8 = s8 + alt / (2 * d - 1)

  alt = -alt
)

(s0 asString(0,9) .. "\t(2/3)^k") println
(s1 asString(0,9) .. "\tk^-0.5") println
(s2 asString(0,9) .. "\t1/k(k+1)") println
(s3 asString(0,9) .. "\tFlint Hills") println
(s4 asString(0,9) .. "\tCookson Hills") println
(s5 asString(0,9) .. "\tHarmonic") println
(s6 asString(0,9) .. "\tRiemann Zeta") println
(s7 asString(0,9) .. "\tAlternating Harmonic") println
(s8 asString(0,9) .. "\tGregory") println

/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Ian Osgood */

Rand := Object clone do(
    IA := 3877
    IC := 29573
    IM := 139968
    last := 42
    gen_random := method(max,
        max * (last = ((last * IA + IC) % IM)) / IM
    )
)

(args at(0) asNumber - 1) repeatTimes( Rand gen_random(100) )

Rand gen_random(100) asString(0,9) println

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/  
   contributed by Anthony Borla */

ack := method(x, y,
  if(x == 0, return y + 1)
  if(y == 0, return ack(x - 1, 1))
  return ack(x - 1, ack(x, y - 1))
)


fib := method(n,
  if(n < 2, return 1)
  return fib(n - 1) + fib(n - 2)
)


tak := method(x, y, z,
  if(y < x, return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y)))
  return z
)


n := System args at(1) asNumber

// Use interpolation for complex string expressions
"Ack(3,#{n}): #{ack(3, n)}" interpolate println
"Fib(#{(27.0 + n) asString(0,1)}): #{fib(27.0 + n) asString(0,1)}" interpolate println

n := n - 1
"Tak(#{n * 3},#{n * 2},#{n}): #{tak(n * 3, n * 2, n * 1)}" interpolate println

// Use concatenation for simpler ones
("Fib(3): " .. fib(3)) println
("Tak(3.0,2.0,1.0): " .. (tak(3.0, 2.0, 1.0) asString(0,1))) println

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Ian Osgood */

seq := Sequence clone do(
    complementAt := method(i,
        "TVGHefCDijMlKNopqYSAABWxRz" at(at(i)-65)  # "A" at(0)
    )
    revComp := method( if (size == 0, return self)
        j := uppercase size
        for (i, 0, size / 2 - 0.25,
            j = j - 1
            t := complementAt(j)
            atPut(j, complementAt(i)) atPut(i, t)
        )
        self
    )
    output := method( if (size == 0, return self)
        width := 60
        start := 0; end := width
        while (end < size,
            slice(start,end) println
            start = end; end = end + width
        )
        slice(start) println
        self
    )
)

input := File standardInput
while (line := input readLine,
    if (line beginsWithSeq(">"),
        seq revComp output empty; line println,
        seq appendSeq(line) )
)
seq revComp output
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Gavin Harrison */

SNorm:=Object clone do(
   App:=method(n,
      n1:=n-1
      u:=List clone preallocateToSize(n)
      v:=List clone preallocateToSize(n)
      n repeat(u append(1);v append(0))
      10 repeat(MAtAv(n,u,v);MAtAv(n,v,u))
      vBv:=vv:=vAt:=0
      for(i,0,n1,vAt=v at(i);vBv=vBv+u at(i)*vAt;vv=vv+(vAt*vAt))
      (vBv/vv)sqrt)
   A:=method(i,j,ij:=i+j;1/(ij*(ij+1)/2+i+1))
   MAv:=method(n,v,Av,
      n1:=n-1
      for(i_i,0,n1,
         for(i_j,0,n1,Av atPut(i_i,Av at(i_i)+A(i_i,i_j)*v at(i_j)))))
   MAtv:=method(n,v,Atv,
      n1:=n-1
      for(i_i,0,n1,
         Atv atPut(i_i,0)
         for(i_j,0,n1,Atv atPut(i_i,Atv at(i_i)+A(i_j, i_i)*v at(i_j)))))
   MAtAv:=method(n,v,AtAv,
      u:=List clone preallocateToSize(n)
      n repeat(u append(0))
      MAv(n,v,u)
      MAtv(n,u,AtAv)))
n:=System args at(1) asNumber
SNorm App(n) asString(0,9) println
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Ian Osgood */

sum := 0
file := File standardInput
while (line := file readLine, sum = sum + line asNumber)
sum println

/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Ian Osgood */

tak := block(x,y,z,
    if (y >= x, z, tak( tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y) ) )
)

n := args at(0) asNumber

tak(n*3, n*2, n) asString(0,1) println

