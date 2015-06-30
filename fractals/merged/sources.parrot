#!./parrot -C
# Contributed by Shane Calimlim, Leopold Toetsch, and
# others from the Perl-internals mailing list.
#
.sub main :main
    .param pmc argv
    .local int argc
    argc = elements argv
    .local int x, y, r
    x = 3
    y = 9
    if argc == 1 goto go
    $S0 = argv[1]
    if argc == 2 goto xdefault
    x = $S0
    $S0 = argv[2]
    y = $S0
    goto go
xdefault:
    y = $S0
go:
    $P0 = getinterp
    $P0.'recursion_limit'(1000000)
    r = ack(x, y)
    .local pmc args
    args = new .ResizableIntegerArray
    push args, x
    push args, y
    push args, r
    $S0 = sprintf "Ack(%d, %d) = %d\n", args
    print $S0
.end

.sub ack
    .param int x
    .param int y
    if x goto a1
	$I0 = y + 1
	.return ($I0)
a1:
    if y goto a2
	$I0 = x - 1
	$I1 = 1
	.return ack($I0, $I1)
a2: 
    $I2 = y - 1
    $I3 = ack(x, $I2)
    $I4 = x - 1
    .return ack($I4, $I3)
.end

#!/usr/bin/parrot -C
#
# binarytrees.pir N         (N = 16 for shootout)
# by Joshua Isom, modified by Leopold Toetsch
.sub itemcheck
	.param pmc node
	$I0 = exists node[0]
	unless $I0 goto final
	.local pmc tmp
	tmp = node[0]
	unless_null tmp, else
	$I0 = node[2]
	.return($I0)
else:
	# tmp = node[0]
	$I0 = itemcheck(tmp)
	tmp = node[1]
	$I1 = itemcheck(tmp)
	$I0 -= $I1
	$I1 = node[2]
	$I0 += $I1
final:
	.return($I0)
.end

.sub bottomuptree
	.param int item
	.param int dep
	.local pmc left, right, tree
        .local int item2
	unless dep > 0 goto else
	item2 = item * 2
	$I0 = item2 - 1
	dec dep
	left = bottomuptree($I0, dep)
	right = bottomuptree(item2, dep)
	goto endif
else:
	null left
	null right
endif:
	tree = new .FixedPMCArray
	tree = 3
	tree[0] = left
	tree[1] = right
	tree[2] = item
	.return(tree)
.end

.sub main :main
	.param pmc argv
	.local int N, dep, mindepth, maxdepth, stretchdepth
	.local pmc stretchtree, longlivedtree, tmptree
	$S0 = argv[1]
	N = $S0
	mindepth = 4
	unless N < 6 goto else
	maxdepth = mindepth + 2
	goto endif
else:
	maxdepth = N
endif:
	stretchdepth = maxdepth + 1
	$I0 = 0
	stretchtree = bottomuptree($I0, stretchdepth)
	$I0 = itemcheck(stretchtree)

	print "stretch tree of depth "
	print stretchdepth
	print "\t check: "
	print $I0 
	print "\n"

	null stretchtree
	$I0 = 0
	longlivedtree = bottomuptree($I0, maxdepth)

	dep = mindepth
beginfor_1:

	.local int i, iterations, check

	$N0 = maxdepth - dep
	$N0 += mindepth
	$N1 = 2
	$N2 = pow $N1, $N0
	iterations = $N2
	
	check = 0

	i = 1
	beginfor_2:
       noop

			tmptree = bottomuptree(i, dep)
			$I0 = itemcheck(tmptree)
			check += $I0
			$I0 = 0 - i
			tmptree = bottomuptree($I0, dep)
			$I0 = itemcheck(tmptree)
			check += $I0
		
	inc i
	if i <= iterations goto beginfor_2
	$I0 = iterations * 2
	print $I0 
	print "\t trees of depth "
	print dep
	print "\t check: " 
	print check
	print "\n"


	dep += 2
	if dep <= maxdepth goto beginfor_1

	$I0 = itemcheck(longlivedtree)
	print "long lived tree of depth "
	print maxdepth
	print "\t check: "
	print $I0 
	print "\n"

.end
#!/usr/bin/parrot -j
# Contributed by Joshua Isom
.sub fannkuch
	.param int n
	.local pmc perm, perm1, count
	perm  = new .FixedIntegerArray
	perm1 = new .FixedIntegerArray
	count = new .FixedIntegerArray
	.local int flips, flipsMax, r, i, k, didpr
	.local int n1
	n1 = n
	dec n1
	
	if n > 1 goto countok
	.return(0)
countok:
	perm  = n
	perm1 = n
	count = n
	i = 0
for_1:
	perm1[i] = i
	inc i
	if i < n goto for_1
	r = n
	didpr = 0
	flipsMax = 0
beginwhile_1:
	unless didpr < 30 goto endif_1
			i = 0
		for_2:
			$I0 = perm1[i]
			inc $I0
			print $I0
		inc i
		if i < n goto for_2
		print "\n"
		inc didpr
	endif_1: 
	for_3:
		unless r != 1 goto endfor_3
			$I0 = r - 1
			count[$I0] = r
		dec r
		goto for_3
	endfor_3:
	$I0 = perm1[0]
	$I1 = iseq $I0, 0
	$I0 = perm1[n1]
	$I2 = iseq $I0, n1
	$I0 = or $I1, $I2
	if $I0 goto endif_2
			flips = 0
			perm = clone perm1
			k = perm1[0]
		dowhile_1:
			.local int j
			i = 1
			j = k - 1
			for_5:
			unless i < j goto endfor_5
				$I0 = perm[i]
				$I1 = perm[j]
				perm[i] = $I1
				perm[j] = $I0
			inc i
			dec j
			goto for_5
			endfor_5:
			inc flips
			j = perm[k]
			perm[k] = k
			k = j
		if k goto dowhile_1
		unless flipsMax < flips goto endif_3
			flipsMax = flips
		endif_3:
endif_2:
while_1:
	unless r == n goto endif_4
		.return(flipsMax)
	endif_4:
	.local int perm0
	perm0 = perm1[0]
	i = 0
	beginwhile_2:
		unless i < r goto endwhile_2
		k = i + 1
		$I0 = perm1[k]
		perm1[i] = $I0
		i = k
	goto beginwhile_2
	endwhile_2:
	perm1[r] = perm0
	$I0 = count[r]
	dec $I0
	count[r] = $I0
	if $I0 > 0 goto beginwhile_1
	inc r
	goto while_1
.end

.sub main :main
	.param pmc argv
	.local int argc
	$S0 = argv[1]
	$I0 = $S0
	$I1 = fannkuch($I0)
	print "Pfannkuchen("
	print $S0
	print ") = "
	print $I1
	print "\n"
	exit 0
.end
#!./parrot -C
#
# fasta.pir N         (N = 2500000 for shootout)
# by Joshua Isom

# 48.2 sec on AMD@2000/512K cache

.sub makeCumulative
	.param pmc genelist
	.param int count
	.local float cp
	.local int i
	cp = 0.0
	i = 0 
beginfor:
	unless i < count goto endfor
	$N0 = genelist[i;1]
	cp += $N0
	genelist[i;1] = cp
	inc i 
	goto beginfor
endfor:
.end

.sub selectRandom
	.param pmc genelist
	.param int count
	.local float r
	r = gen_random(1.0)
	.local int i, lo, hi

	$N0 = genelist[0;1]
	unless r < $N0 goto endif
	$S0 = genelist[0;0]
	.return($S0)
endif:
	lo = 0
	hi = count - 1
beginwhile:
	$I0 = lo + 1
	unless hi > $I0 goto endwhile
	i = hi + lo
	i /= 2
	$N0 = genelist[i;1]
	unless r < $N0 goto else_1
	hi = i
	goto endif_1
else_1:
	lo = i
endif_1:
	goto beginwhile
endwhile:
	$S0 = genelist[hi;0]
	.return($S0)
.end

.const int LINE_LENGTH = 60

.sub makeRandomFasta
	.param string id
	.param string desc
	.param pmc genelist
	.param int count
	.param int n
	.local int todo, i, m
	todo = n

	print ">"
	print id
	print " "
	print desc
	print "\n"
	
	.local string pick
beginfor:
	unless todo > 0 goto endfor

	unless todo < LINE_LENGTH goto else
		m = todo
	goto endif
	else:
		m = LINE_LENGTH
	endif:
	
	i = 0
beginfor_1:
	unless i < m goto endfor_1
	$S0 = selectRandom(genelist, count)
	pick .= $S0
	inc i
	goto beginfor_1
endfor_1:
	print pick
	print "\n"
	pick = ''

	todo -= LINE_LENGTH
	goto beginfor
endfor:
.end

.sub makeRepeatFasta
	.param string id
	.param string desc
	.param string s
	.param int n
	.local int todo, k, kn, m
	todo = n
	k = 0
	kn = length s

	print ">"
	print id
	print " "
	print desc
	print "\n"
	
beginfor:
	unless todo > 0 goto endfor

	unless todo < LINE_LENGTH goto else
		m = todo
	goto endif
	else:
		m = LINE_LENGTH
	endif:

beginwhile:
	$I0 = kn - k
	unless m >= $I0 goto endwhile
	$S0 = substr s, k
	print $S0
	$I0 = kn - k
	m -= $I0
	k = 0
	goto beginwhile
endwhile:

	$S0 = substr s, k, m
	print $S0
	print "\n"
	k += m

	todo -= LINE_LENGTH
	goto beginfor
endfor:
.end

.macro InitStruct (iub, i, char, num)
	$P0 = new .FixedPMCArray
	$P0 = 2
	.iub[.i] = $P0
	.iub[.i;0] = .char
	.iub[.i;1] = .num
.endm

.sub main :main
	.param pmc argv
	.local pmc stdout
	.local int n
	# stdout is linebuffered per default - make it block buffered
	stdout = getstdout
	stdout.'setbuf'(40960)
	$I0 = argv
	unless $I0 > 2 goto argsok
	n = 1000
	goto argsdone
argsok:
	$S0 = argv[1]
	n = $S0
argsdone:
	load_bytecode "random_lib.pir"

	.local pmc iub
	iub = new .FixedPMCArray
	iub = 15
	.InitStruct(iub, 0, "a", 0.27)
	.InitStruct(iub, 1, "c", 0.12)
	.InitStruct(iub, 2, "g", 0.12)
	.InitStruct(iub, 3, "t", 0.27)

	.InitStruct(iub, 4, "B", 0.02)
	.InitStruct(iub, 5, "D", 0.02)
	.InitStruct(iub, 6, "H", 0.02)
	.InitStruct(iub, 7, "K", 0.02)
	.InitStruct(iub, 8, "M", 0.02)
	.InitStruct(iub, 9, "N", 0.02)
	.InitStruct(iub, 10, "R", 0.02)
	.InitStruct(iub, 11, "S", 0.02)
	.InitStruct(iub, 12, "V", 0.02)
	.InitStruct(iub, 13, "W", 0.02)
	.InitStruct(iub, 14, "Y", 0.02)

	.local pmc homosapiens
	homosapiens = new .FixedPMCArray
	homosapiens = 4
	.InitStruct(homosapiens, 0, "a", 0.3029549426680)
	.InitStruct(homosapiens, 1, "c", 0.1979883004921)
	.InitStruct(homosapiens, 2, "g", 0.1975473066391)
	.InitStruct(homosapiens, 3, "t", 0.3015094502008)

	.local string alu
	alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

	makeCumulative(iub, 15)
	makeCumulative(homosapiens, 4)
	
	$I0 = n * 2
	makeRepeatFasta("ONE", "Homo sapiens alu", alu, $I0)
	$I0 = n * 3
	makeRandomFasta ("TWO", "IUB ambiguity codes", iub, 15, $I0)
	$I0 = n * 5
	makeRandomFasta ("THREE", "Homo sapiens frequency", homosapiens, 4, $I0)
.end

=head1 NAME

examples/shootout/harmonic.pir - Partial sum of Harmonic series

=head1 SYNOPSIS

    % ./parrot examples/shootout/harmonic.pir -j 10000000

=head1 DESCRIPTION

Translated from C code by Greg Buchholz into PIR
by Peter Baylies <pbaylies@gmail.com>.

The C code is at:
   The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

=cut

.sub 'main' :main
	.param pmc argv
	.local int argc
	.local int n
	.local num i, sum
	i = 1
	sum = 0
	argc = argv
	n = 10000000
	if argc <= 1 goto NREDO
	$S0 = argv[1]
	n = $S0
NREDO:	$N1 = 1 / i
	sum += $N1
	inc i
	dec n
	if n goto NREDO

      	$P0 = new .FixedFloatArray
	$P0 = 1
	$P0[0] = sum
	$S0 = sprintf "%.9f\n", $P0
        print $S0
	end
.end
# Contributed by Brent Fulgham, as cribbed from the Parrot web site.
.sub _main
	print "hello world\n"
	end
.end
#!./parrot -C
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
# 
# Contributed by Joshua Isom

.sub main :main
	.local pmc stdin
	.local string line
	stdin = getstdin
	# Skip to block THREE
beginwhile_1:
	line = readline stdin
	$S0 = chopn line, -6
	if $S0 != ">THREE" goto beginwhile_1
	line = ''
	.local string seq
beginwhile_2:
	chopn line, 1
	seq .= line
	line = readline stdin
	$I0 = length line
	unless $I0 goto endwhile_2
	$S0 = chopn line, -1
	if $S0 != ">" goto beginwhile_2
endwhile_2:
	upcase seq
	sort_seq(seq, 1)
	sort_seq(seq, 2)
	find_seq(seq, "GGT")
	find_seq(seq, "GGTA")
	find_seq(seq, "GGTATT")
	find_seq(seq, "GGTATTTTAATT")
	find_seq(seq, "GGTATTTTAATTTATAGT")
.end

.sub sort_seq
	.param string seq
	.param int len
	.local int i, seqend
	.local pmc table
	table = new .Hash
	i = 0
	seqend = length seq
beginfor:
	unless i < seqend goto endfor
	$S0 = substr seq, i, len
	$I1 = length $S0
	if $I1 < len goto endfor
	$I0 = table[$S0]
	inc $I0
	table[$S0] = $I0
	inc i
	goto beginfor
endfor:
	sort_n_print(table, i)
	print "\n"
.end

.include "iterator.pasm"
.sub sort_n_print
	.param pmc table
	.param int seqlen
	.local int i

	.local pmc array
	array = new .FixedPMCArray
	$I0 = elements table
	array = $I0
	
	.local pmc iter
   	iter = new .Iterator, table
	set iter, .ITERATE_FROM_START
	i = 0
iter_loop_1:
	unless iter goto iter_end_1
	$S0 = shift iter
	$I0 = table[$S0]
	$P0 = new .FixedPMCArray
	$P0 = 2
	array[i] = $P0
	array[i;0] = $S0
	array[i;1] = $I0
	inc i
	goto iter_loop_1
iter_end_1:

	$P0 = global "sort"
	array."sort"($P0)
	
	$I0 = array
	i = 0
beginfor:
	unless i < $I0 goto endfor
	$S0 = array[i;0]
	$N0 = array[i;1]
	print $S0
	print " "
	$P0 = new .FixedFloatArray
	$P0 = 1
	$N1 = seqlen
	$N0 /= $N1
	$N0 *= 100
	$P0[0] = $N0
	$S0 = sprintf "%.3f\n", $P0
	print $S0
	inc i
	goto beginfor
endfor:
.end

.sub sort
	.param pmc a
	.param pmc b
	$I0 = a[1]
	$I1 = b[1]
	$I2 = cmp $I1, $I0
	.return($I2)
.end

.sub find_seq
	.param string seq
	.param string s
	.local int i
	i = 0
	$I0 = 0
beginwhile:
	$I2 = $I0 + 1
	$I0 = index seq, s, $I2
	if $I0 == -1 goto endwhile
	inc i
	goto beginwhile
endwhile:
	print i
	print "\t"
	print s
	print "\n"
.end

=head1 NAME

examples/shootout/mandelbrot.pir - Print the Mandelbrot set

=head1 SYNOPSIS

    % ./parrot examples/shootout/mandelbrot.pir -j 600 > out.pbm

=head1 DESCRIPTION

This outputs a pbm file of the Mandelbrot set. Defaults to 200x200.

Translated from C code by Greg Buchholz into PIR
by Peter Baylies <pbaylies@gmail.com>.

The C code is at:
   The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

=cut

.sub 'main' :main
	.param pmc argv
#    int w, h, x, y, bit_num = 0;
#    char byte_acc = 0;
#    int i, iter = 50;
#    double limit = 2.0;
#    double Zr, Zi, Cr, Ci, Tr, Ti;
	.local int    w, h, x, y, bit_num, byte_acc, i, iter
	.local num    limit, Zr, Zi, Cr, Ci, Tr, Ti
	.sym int argc
	bit_num = 0
	byte_acc = 0
	iter = 50
	limit = 2.0
#	slight optimization here -- nothing a decent C compiler wouldn't already do :)
	limit = limit * limit
	argc = argv
	w = 200
	if argc <= 1 goto noarg
#	w = atoi(argv[1]);
	$S0 = argv[1]
	w = $S0
#	h = w
noarg:	h = w
#	printf("P4\n%d %d\n",w,h);
	print "P4\n"
	print w
	print " "
	print h
	print "\n"
	y = 0
YREDO:
        x = 0
XREDO:
#       Zr = 0.0; Zi = 0.0;
	Zr = 0.0
	Zi = 0.0
#       Cr = (2*(double)x/w - 1.5);
	Cr = x
	Cr /= w
	Cr *= 2	
	Cr -= 1.5
#	Ci=(2*(double)y/h - 1);
	Ci = y
	Ci /= h
	Ci *= 2
	Ci -= 1

	i = 0
IREDO:
#	Tr = Zr*Zr - Zi*Zi + Cr;
        $N1 = Zr * Zr
        $N2 = Zi * Zi
        Tr = $N1 - $N2
        Tr += Cr
#       Ti = 2*Zr*Zi + Ci;
        Ti = 2
	Ti *= Zr
        Ti *= Zi
        Ti += Ci
#	Zr = Tr; Zi = Ti;
	Zr = Tr
	Zi = Ti
#	if (Zr*Zr+Zi*Zi > limit*limit) break;
	$N1 = Zr * Zr
	$N2 = Zi * Zi
        $N1 += $N2
        if $N1 > limit goto IBRK
	inc i
        if i < iter goto IREDO
IBRK:	
	byte_acc <<= 1
        if $N1 <= limit goto SLA
	byte_acc |= 0
	goto SLE
SLA:	byte_acc |= 1
SLE:	inc bit_num
	if bit_num != 8 goto NTST1
PRINT:	chr $S1, byte_acc
	print $S1
	byte_acc = 0
	bit_num = 0
	goto NTSTE
NTST1:	$I1 = w
	dec $I1
	goto NTSTE
	if x != $I1 goto NTSTE
	$I1 = w
	$I1 %= 8
	$I1 = 8 - $I1
	byte_acc <<= $I1
	goto PRINT
NTSTE:
	inc x
        if x < w goto XREDO

        inc y
        if y < h goto YREDO
	end
.end
#!./parrot -C
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# 
# Contributed by Joshua Isom

# save on repetition of code
.macro InitBodies (bodies, i, num1, num2, num3, num4, num5, num6, num7)
	$N0 = .num4 * 365.24
	$N1 = .num5 * 365.24
	$N2 = .num6 * 365.24
	$N3 = .num7 * 39.478417604357428
	$P0 = new .FixedFloatArray
	$P0 = 7
	.bodies[.i] = $P0
	.bodies[.i;0] = .num1
	.bodies[.i;1] = .num2
	.bodies[.i;2] = .num3
	.bodies[.i;3] = $N0
	.bodies[.i;4] = $N1
	.bodies[.i;5] = $N2
	.bodies[.i;6] = $N3
.endm

.sub main :main
	.param pmc argv
	.local int argc, n, nbodies
	argc = argv
	n = 1000000
	unless argc > 1 goto argsok
	$S0 = argv[1]
	n = $S0
argsok:
	.local pmc bodies
	bodies = new .FixedPMCArray
	bodies = 5
	# Sun
	.InitBodies(bodies, 0, 0, 0, 0, 0, 0, 0, 1)
	# Jupiter
	.InitBodies(bodies, 1,
	 4.84143144246472090e+00,
	-1.16032004402742839e+00,
	-1.03622044471123109e-01,
	 1.66007664274403694e-03,
	 7.69901118419740425e-03,
	-6.90460016972063023e-05,
	 9.54791938424326609e-04)
	# Saturn
	.InitBodies(bodies, 2,
	 8.34336671824457987e+00,
	 4.12479856412430479e+00,
	-4.03523417114321381e-01,
	-2.76742510726862411e-03,
	 4.99852801234917238e-03,
	 2.30417297573763929e-05,
	 2.85885980666130812e-04)
	# Uranus
	.InitBodies(bodies, 3,
	 1.28943695621391310e+01,
	-1.51111514016986312e+01,
	-2.23307578892655734e-01,
	 2.96460137564761618e-03,
	 2.37847173959480950e-03,
	-2.96589568540237556e-05,
	 4.36624404335156298e-05)
	# Neptune
	.InitBodies(bodies, 4,
	 1.53796971148509165e+01,
	-2.59193146099879641e+01,
	 1.79258772950371181e-01,
	 2.68067772490389322e-03,
	 1.62824170038242295e-03,
	-9.51592254519715870e-05,
	 5.15138902046611451e-05)

	nbodies = bodies
	offset_momentum(nbodies, bodies)
	$N0 = energy(nbodies, bodies)
	.local pmc spf
	spf = new .FixedFloatArray
	spf = 1
	spf[0] = $N0
	$S0 = sprintf "%.9f\n", spf
	print $S0
	.local int i
	i = 0
beginfor:
	unless i < n goto endfor
		advance(nbodies, bodies, 0.01)
	inc i
	goto beginfor
endfor:
	
	$N0 = energy(nbodies, bodies)
	spf[0] = $N0
	$S0 = sprintf "%.9f\n", spf
	print $S0
	exit 0
.end

.sub offset_momentum
	.param int nbodies
	.param pmc bodies
	.local float px, py, pz
	px = 0.0
	py = 0.0
	pz = 0.0
	.local int i
	i = 0
beginfor:
	unless i < nbodies goto endfor
	$N0 = bodies[i;3]
	$N1 = bodies[i;6]
	$N0 *= $N1
	px += $N0
	$N0 = bodies[i;4]
	$N1 = bodies[i;6]
	$N0 *= $N1
	py += $N0
	$N0 = bodies[i;5]
	$N1 = bodies[i;6]
	$N0 *= $N1
	pz += $N0
	inc i
	goto beginfor
endfor:
	# bodies[0].vx = - px / solar_mass;
	px /= -39.478417604357428
	bodies[0;3] = px
	py /= -39.478417604357428
	bodies[0;4] = py
	pz /= -39.478417604357428
	bodies[0;5] = pz
.end

.sub energy
	.param int nbodies
	.param pmc bodies
	.local float e, tmp
	.local int i, j
	e = 0.0
	i = 0
beginfor_0:
	unless i < nbodies goto endfor_0

	# e += 0.5 * b->mass * (b->vx * b->vx + b->vy * b->vy + b->vz * b->vz);
	$N0 = bodies[i;6] # mass
	$N0 *= 0.5

	$N1 = bodies[i;3] # vx
	$N3 = pow $N1, 2.0
	
	$N1 = bodies[i;4] # vy
	$N2 = pow $N1, 2.0
	$N3 += $N2
	
	$N1 = bodies[i;5] # vz
	$N2 = pow $N1, 2.0
	$N3 += $N2

	$N0 *= $N3
	
	e += $N0
	
	j = i + 1
beginfor_1:
	unless j < nbodies goto endfor_1
	.local float dx, dy, dz, distance

	# dx = b->x - b2->x;
	$N0 = bodies[i;0]
	$N1 = bodies[j;0]
	dx = $N0 - $N1

	# dy = b->y - b2->y;
	$N0 = bodies[i;1]
	$N1 = bodies[j;1]
	dy = $N0 - $N1

	# dz = b->z - b2->z;
	$N0 = bodies[i;2]
	$N1 = bodies[j;2]
	dz = $N0 - $N1

	# distance = sqrt(dx * dx + dy * dy + dz * dz);
	$N0 = pow dx, 2.0
	$N1 = pow dy, 2.0
	$N2 = pow dz, 2.0
	$N0 += $N1
	$N0 += $N2
	distance = sqrt $N0
	
	# e -= (b->mass * b2->mass) / distance;
	$N0 = bodies[i;6]
	$N1 = bodies[j;6]
	$N0 *= $N1
	$N0 /= distance
	e -= $N0
	
	inc j
	goto beginfor_1
endfor_1:

	inc i
	goto beginfor_0
endfor_0:
	.return(e)
.end

.sub advance
	.param int nbodies
	.param pmc bodies
	.param float dt
	.local int i, j
	.local float dx, dy, dz, distance, mag
	i = 0
beginfor_0:
	unless i < nbodies goto endfor_0
	
	j = i + 1
	beginfor_1:
		unless j < nbodies goto endfor_1

		# dx = b->x - b2->x;
		$N0 = bodies[i;0]
		$N1 = bodies[j;0]
		dx = $N0 - $N1
		# dy = b->y - b2->y;
		$N0 = bodies[i;1]
		$N1 = bodies[j;1]
		dy = $N0 - $N1
		# dz = b->z - b2->z;
		$N0 = bodies[i;2]
		$N1 = bodies[j;2]
		dz = $N0 - $N1

		# distance = sqrt(dx * dx + dy * dy + dz * dz);
		$N0 = pow dx, 2.0
		$N1 = pow dy, 2.0
		$N0 += $N1
		$N1 = pow dz, 2.0
		$N0 += $N1
		distance = sqrt $N0

		# mag = dt / (distance * distance * distance);
		$N0 = pow distance, 3.0
		mag = dt / $N0

		# b->vx -= dx * b2->mass * mag;
		$N0 = bodies[j;6]
		$N0 *= dx
		$N0 *= mag
		$N1 = bodies[i;3]
		$N1 -= $N0
		bodies[i;3] = $N1

		# b->vy -= dy * b2->mass * mag;
		$N0 = bodies[j;6]
		$N0 *= dy
		$N0 *= mag
		$N1 = bodies[i;4]
		$N1 -= $N0
		bodies[i;4] = $N1

		# b->vz -= dz * b2->mass * mag;
		$N0 = bodies[j;6]
		$N0 *= dz
		$N0 *= mag
		$N1 = bodies[i;5]
		$N1 -= $N0
		bodies[i;5] = $N1

		# b2->vx += dx * b->mass * mag;
		$N0 = bodies[i;6]
		$N0 *= dx
		$N0 *= mag
		$N1 = bodies[j;3]
		$N1 += $N0
		bodies[j;3] = $N1

		# b2->vy += dy * b->mass * mag;
		$N0 = bodies[i;6]
		$N0 *= dy
		$N0 *= mag
		$N1 = bodies[j;4]
		$N1 += $N0
		bodies[j;4] = $N1

		# b2->vz += dz * b->mass * mag;
		$N0 = bodies[i;6]
		$N0 *= dz
		$N0 *= mag
		$N1 = bodies[j;5]
		$N1 += $N0
		bodies[j;5] = $N1

		inc j
		goto beginfor_1
	endfor_1:

	inc i
	goto beginfor_0
endfor_0:

	i = 0
beginfor_2:
	unless i < nbodies goto endfor_2
	# b->x += dt * b->vx;
	$N0 = bodies[i;3]
	$N1 = dt * $N0
	$N0 = bodies[i;0]
	$N0 += $N1
	bodies[i;0] = $N0

	# b->y += dt * b->vy;
	$N0 = bodies[i;4]
	$N1 = dt * $N0
	$N0 = bodies[i;1]
	$N0 += $N1
	bodies[i;1] = $N0

	# b->z += dt * b->vz;
	$N0 = bodies[i;5]
	$N1 = dt * $N0
	$N0 = bodies[i;2]
	$N0 += $N1
	bodies[i;2] = $N0

	inc i
	goto beginfor_2
endfor_2:
	
.end
#!./parrot -j
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# 
# Contributed by Joshua Isom
# speed up  from 1m25 to 6s by Leopold Toetsch

.const int x = 0
.const int y = 1
.const int z = 2
.const int vx = 3
.const int vy = 4
.const int vz = 5
.const int m = 6

# save on repetition of code
.macro InitBodies (bodies, i, num1, num2, num3, num4, num5, num6, num7)
	$N0 = .num4 * 365.24
	$N1 = .num5 * 365.24
	$N2 = .num6 * 365.24
	$N3 = .num7 * 39.478417604357428
	$P0 = new .FixedFloatArray
	$P0 = 7
	.bodies[.i] = $P0
	.bodies[.i; x] = .num1
	.bodies[.i; y] = .num2
	.bodies[.i; z] = .num3
	.bodies[.i; vx] = $N0
	.bodies[.i; vy] = $N1
	.bodies[.i; vz] = $N2
	.bodies[.i; m] = $N3
.endm

.sub main :main
	.param pmc argv
	.local int argc, n, nbodies
	argc = argv
	n = 1000000
	unless argc > 1 goto argsok
	$S0 = argv[1]
	n = $S0
argsok:
	.local pmc bodies
	bodies = new .FixedPMCArray
	bodies = 5
	# Sun
	.InitBodies(bodies, 0, 0, 0, 0, 0, 0, 0, 1)
	# Jupiter
	.InitBodies(bodies, 1,
	 4.84143144246472090e+00,
	-1.16032004402742839e+00,
	-1.03622044471123109e-01,
	 1.66007664274403694e-03,
	 7.69901118419740425e-03,
	-6.90460016972063023e-05,
	 9.54791938424326609e-04)
	# Saturn
	.InitBodies(bodies, 2,
	 8.34336671824457987e+00,
	 4.12479856412430479e+00,
	-4.03523417114321381e-01,
	-2.76742510726862411e-03,
	 4.99852801234917238e-03,
	 2.30417297573763929e-05,
	 2.85885980666130812e-04)
	# Uranus
	.InitBodies(bodies, 3,
	 1.28943695621391310e+01,
	-1.51111514016986312e+01,
	-2.23307578892655734e-01,
	 2.96460137564761618e-03,
	 2.37847173959480950e-03,
	-2.96589568540237556e-05,
	 4.36624404335156298e-05)
	# Neptune
	.InitBodies(bodies, 4,
	 1.53796971148509165e+01,
	-2.59193146099879641e+01,
	 1.79258772950371181e-01,
	 2.68067772490389322e-03,
	 1.62824170038242295e-03,
	-9.51592254519715870e-05,
	 5.15138902046611451e-05)

	nbodies = bodies
	offset_momentum(nbodies, bodies)
	$N0 = energy(nbodies, bodies)
	.local pmc spf
	spf = new .FixedFloatArray
	spf = 1
	spf[0] = $N0
	$S0 = sprintf "%.9f\n", spf
	print $S0
	.local int i
	i = 0
beginfor:
	unless i < n goto endfor
		advance(nbodies, bodies, 0.01)
	inc i
	goto beginfor
endfor:
	
	$N0 = energy(nbodies, bodies)
	spf[0] = $N0
	$S0 = sprintf "%.9f\n", spf
	print $S0
	exit 0
.end

.sub offset_momentum
	.param int nbodies
	.param pmc bodies
	.local float px, py, pz
	px = 0.0
	py = 0.0
	pz = 0.0
	.local int i
	i = 0
beginfor:
	unless i < nbodies goto endfor
	$N0 = bodies[i; vx]
	$N1 = bodies[i; m]
	$N0 *= $N1
	px += $N0
	$N0 = bodies[i; vy]
	$N1 = bodies[i; m]
	$N0 *= $N1
	py += $N0
	$N0 = bodies[i; vz]
	$N1 = bodies[i; m]
	$N0 *= $N1
	pz += $N0
	inc i
	goto beginfor
endfor:
	# bodies[0].vx = - px / solar_mass;
	px /= -39.478417604357428
	bodies[0; vx] = px
	py /= -39.478417604357428
	bodies[0; vy] = py
	pz /= -39.478417604357428
	bodies[0; vz] = pz
.end

.sub energy
	.param int nbodies
	.param pmc bodies
	.local float e, tmp
	.local int i, j
	e = 0.0
	i = 0
beginfor_0:
	unless i < nbodies goto endfor_0

	# e += 0.5 * b->mass * (b->vx * b->vx + b->vy * b->vy + b->vz * b->vz);
	$N0 = bodies[i; m] # mass
	$N0 *= 0.5

	$N1 = bodies[i; vx] # vx
	$N3 = pow $N1, 2.0
	
	$N1 = bodies[i; vy] # vy
	$N2 = pow $N1, 2.0
	$N3 += $N2
	
	$N1 = bodies[i; vz] # vz
	$N2 = pow $N1, 2.0
	$N3 += $N2

	$N0 *= $N3
	
	e += $N0
	
	j = i + 1
beginfor_1:
	unless j < nbodies goto endfor_1
	.local float dx, dy, dz, distance

	# dx = b->x - b2->x;
	$N0 = bodies[i; x]
	$N1 = bodies[j; x]
	dx = $N0 - $N1

	# dy = b->y - b2->y;
	$N0 = bodies[i; y]
	$N1 = bodies[j; y]
	dy = $N0 - $N1

	# dz = b->z - b2->z;
	$N0 = bodies[i; z]
	$N1 = bodies[j; z]
	dz = $N0 - $N1

	# distance = sqrt(dx * dx + dy * dy + dz * dz);
	$N0 = dx * dx
	$N1 = dy * dy
	$N2 = dz * dz
	$N0 += $N1
	$N0 += $N2
	distance = sqrt $N0
	
	# e -= (b->mass * b2->mass) / distance;
	$N0 = bodies[i; m]
	$N1 = bodies[j; m]
	$N0 *= $N1
	$N0 /= distance
	e -= $N0
	
	inc j
	goto beginfor_1
endfor_1:

	inc i
	goto beginfor_0
endfor_0:
	.return(e)
.end

.sub advance
	.param int nbodies
	.param pmc bodies
	.param float dt
	.local int i, j
	.local float dx, dy, dz, distance, mag
	.local float bx, by, bz, bm, bvx, bvy, bvz
	.local float b2x, b2y, b2z, b2m
	.local pmc bi, bj
	i = 0
beginfor_0:
	unless i < nbodies goto endfor_0
	bi = bodies[i]
	bx = bi[x]
	by = bi[y]
	bz = bi[z]
	bm = bi[m]
	bvx = bi[vx]
	bvy = bi[vy]
	bvz = bi[vz]
	
	j = i + 1
	beginfor_1:
		unless j < nbodies goto endfor_1
		bj = bodies[j]
		b2x = bj[x]
		b2y = bj[y]
		b2z = bj[z]
		b2m = bj[m]

		# dx = b->x - b2->x;
		dx = bx - b2x
		# dy = b->y - b2->y;
		dy = by - b2y
		# dz = b->z - b2->z;
		dz = bz - b2z

		# distance = sqrt(dx * dx + dy * dy + dz * dz);
		$N0 = dx * dx
		$N1 = dy * dy
		$N0 += $N1
		$N1 = dz * dz
		$N0 += $N1
		distance = sqrt $N0

		# mag = dt / (distance * distance * distance);
		$N0 = distance * distance
		$N0 *= distance
		mag = dt / $N0

		# b->vx -= dx * b2->mass * mag;
		$N0 = dx * b2m
		$N0 *= mag
		bvx -= $N0

		# b->vy -= dy * b2->mass * mag;
		$N0 = dy * b2m
		$N0 *= mag
		bvy -= $N0

		# b->vz -= dz * b2->mass * mag;
		$N0 = dz * b2m
		$N0 *= mag
		bvz -= $N0

		# b2->vx += dx * b->mass * mag;
		$N0 = dx * bm
		$N0 *= mag
		$N1 = bj[vx]
		$N1 += $N0
		bj[vx] = $N1

		# b2->vy += dy * b->mass * mag;
		$N0 = dy * bm
		$N0 *= mag
		$N1 = bj[vy]
		$N1 += $N0
		bj[vy] = $N1

		# b2->vz += dz * b->mass * mag;
		$N0 = dz * bm
		$N0 *= mag
		$N1 = bj[vz]
		$N1 += $N0
		bj[vz] = $N1

		inc j
		goto beginfor_1
	endfor_1:
	bi[vx] = bvx
	bi[vy] = bvy
	bi[vz] = bvz

	inc i
	goto beginfor_0
endfor_0:

	i = 0
beginfor_2:
	unless i < nbodies goto endfor_2
	bi = bodies[i]
	# b->x += dt * b->vx;
	$N0 = bi[vx]
	$N1 = dt * $N0
	$N0 = bi[x]
	$N0 += $N1
	bi[x] = $N0

	# b->y += dt * b->vy;
	$N0 = bi[vy]
	$N1 = dt * $N0
	$N0 = bi[y]
	$N0 += $N1
	bi[y] = $N0

	# b->z += dt * b->vz;
	$N0 = bi[vz]
	$N1 = dt * $N0
	$N0 = bi[z]
	$N0 += $N1
	bi[z] = $N0

	inc i
	goto beginfor_2
endfor_2:
	
.end
#!./parrot -j
#
# nsieve N  (N = 9 for shootout)
# by Leopold Toetsch
# reset bits

.sub primes_in_range
    .param int M
    .local pmc flags
    .local int i, count
    flags = new .FixedBooleanArray
    flags = M
    i = 2
lp0:
    flags[i] = 1
    inc i
    if i < M goto lp0
    i = 2
    count = 0
lp1:
     $I0 = flags[i]
     unless $I0 goto not_p
     .local int j
     j = i + i
     if j >= M goto done
lp2:
     flags[j] = 0
     j += i
     if j < M goto lp2
done:
     inc count
not_p:
     inc i
     if i < M goto lp1
    .return (count)
.end
.sub main :main
    .param pmc argv
    $S0 = argv[1]
    .local int i, j, N, M, count
    N = $S0
    null i
    null j
loop:
    $I0 = N - j
    inc j
    $I1 = 1 << $I0
    M = $I1 * 10000
    count = primes_in_range(M)
    $P0 = new .FixedIntegerArray
    $P0 = 2
    $P0[0] = M
    $P0[1] = count
    $S0 = sprintf "Primes up to %8u %8u\n", $P0
    print $S0
    inc i
    if i < 3 goto loop
.end
    
#!/usr/bin/parrot -j
#
# nsievebits N  (N = 9 for shootout)
# by Leopold Toetsch
# reset bits

.sub primes_in_range
    .param int M
    .local pmc flags
    .local int i, count
    flags = new FixedBooleanArray
    flags = M
    i = 2
lp0:
    flags[i] = 1
    inc i
    if i < M goto lp0
    i = 2
    count = 0
lp1:
     $I0 = flags[i]
     unless $I0 goto not_p
     .local int j
     j = i + i
     if j >= M goto done
lp2:
     flags[j] = 0
     j += i
     if j < M goto lp2
done:
     inc count
not_p:
     inc i
     if i < M goto lp1
    .return (count)
.end
.sub main :main
    .param pmc argv
    $S0 = argv[1]
    .local int i, j, N, M, count
    N = $S0
    null i
    null j
loop:
    $I0 = N - j
    inc j
    $I1 = 1 << $I0
    M = $I1 * 10000
    count = primes_in_range(M)
    $P0 = new .FixedIntegerArray
    $P0 = 2
    $P0[0] = M
    $P0[1] = count
    $S0 = sprintf "Primes up to %8u %8u\n", $P0
    print $S0
    inc i
    if i < 3 goto loop
.end
    
#!/usr/bin/parrot -j
#
# nsievebits N  (N = 9 for shootout)
# by Leopold Toetsch

# set bits - this might be cheating see nsievebits for resetting bits

.sub primes_in_range
    .param int M
    .local pmc flags
    .local int i, prim, count
    flags = new FixedBooleanArray
    flags = M
    i = 2
    count = 0
lp1:
     $I0 = flags[i]
     if $I0 goto not_p
     .local int j
     j = i + i
     if j >= M goto done
lp2:
     flags[j] = 1
     j += i
     if j < M goto lp2
done:
     inc count
not_p:
     inc i
     if i < M goto lp1
    .return (count)
.end
.sub main :main
    .param pmc argv
    $S0 = argv[1]
    .local int i, j, N, M, count
    N = $S0
    null i
    null j
loop:
    $I0 = N - j
    inc j
    $I1 = 1 << $I0
    M = $I1 * 10000
    count = primes_in_range(M)
    $P0 = new .FixedIntegerArray
    $P0 = 2
    $P0[0] = M
    $P0[1] = count
    $S0 = sprintf "Primes up to %8u %8u\n", $P0
    print $S0
    inc i
    if i < 3 goto loop
.end
    
#!./parrot -j
#
# partialsums N  (N = 2500000 for shootout)
#
# By Joshua Isom

.sub main :main
	.param pmc argv
	.local int k, n
	.local num sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8, sum9, a
	.local pmc parray
	.local string result
	parray = new .FixedFloatArray
	parray = 9
	$I0 = argv
	n = 2500000
	unless $I0 == 2 goto argok
	$S0 = argv[1]
	n = $S0
argok:

	sum1 = 0.0
	sum2 = 0.0
	sum3 = 0.0
	sum4 = 0.0
	sum5 = 0.0
	sum6 = 0.0
	sum7 = 0.0
	sum8 = 0.0
	sum9 = 0.0
	a = -1.0
	$N0 = 2.0 / 3.0
	$I2 = 2
	k = 1
beginfor:
	# This is what overoptimized looks like....
	$N1 = sqrt k
	$N1 = 1.0 / $N1
	sum2 += $N1
	$N1 = k + 1.0
	$N1 *= k
	$N1 = 1.0 / $N1
	sum3 += $N1
	$N1 = k * k
	$N2 = 1.0 / $N1
	sum7 += $N2
	$N1 *= k
	$N2 = sin k
	$N2 *= $N2
	$N2 *= $N1
	$N2 = 1.0 / $N2
	sum4 += $N2
	$N2 = cos k
	$N2 *= $N2
	$N2 *= $N1
	$N2 = 1.0 / $N2
	sum5 += $N2
	$N1 = 1.0 / k
	sum6 += $N1
	neg a
	$N1 = a / k
	sum8 += $N1
	$N1 = 2.0 * k
	dec $N1
	$N1 = a / $N1
	sum9 += $N1
lastfor:
	$I1 = k - 1
	$N1 = pow $N0, $I1
	sum1 += $N1
	inc k
	if k <= n goto beginfor
	dec $I2
	if $I2 goto lastfor

	parray[0] = sum1
	parray[1] = sum2
	parray[2] = sum3
	parray[3] = sum4
	parray[4] = sum5
	parray[5] = sum6 
	parray[6] = sum7
	parray[7] = sum8
	parray[8] = sum9

	result = sprintf <<"END", parray
%.9f\t(2/3)^k
%.9f\tk^-0.5
%.9f\t1/k(k+1)
%.9f\tFlint Hills
%.9f\tCookson Hills
%.9f\tHarmonic
%.9f\tRiemann Zeta
%.9f\tAlternating Harmonic
%.9f\tGregory
END
	print result

.end

#!./parrot -C
#
# pidigits N  (N = 1000 for shootout)
#
# this requires libgmp (GMP) on the machine
#
# by Leopold Toetsch
# not really optimized yet

#def gen_x():
#    return imap(lambda k: (k, 4*k + 2, 0, 2*k + 1), count(1))

.pragma n_operators 1

.sub count
    .param pmc start
loop:
    .yield(start)
    inc start
    goto loop
.end
		
.sub gen_x
    .param pmc start
    .local pmc k
loop:
    k = count(start)
    $P0 = k + 0
    $P1 = k * 4
    inc $P1
    inc $P1
    $P2 = new .Integer
    $P3 = k * 2
    inc $P3
    .yield ($P0, $P1, $P2, $P3)
    goto loop
.end

#def compose((aq, ar, as_, at), (bq, br, bs, bt)):
#    return (aq * bq,
#            aq * br + ar * bt,
#            as_ * bq + at * bs,
#            as_ * br + at * bt)

.sub "compose"
    .param pmc aq
    .param pmc ar
    .param pmc as
    .param pmc at
    .param pmc bq
    .param pmc br
    .param pmc bs
    .param pmc bt
    $P0 = aq * bq
    $P1 = aq * br
    $P12 = ar * bt
    $P1 = $P1 + $P12
    $P2 = as * bq
    $P22 = at * bs
    $P2 = $P2 + $P22
    $P3 = as * br
    $P32 = at * bt
    $P3 =  $P3 + $P32
    .return ($P0, $P1, $P2, $P3)
.end

#def extract((q, r, s, t), j):
#    return (q*j + r) // (s*j + t)

.sub extract
    .param pmc q
    .param pmc r
    .param pmc s
    .param pmc t
    .param pmc j
    $P0 = q * j
    $P0 = $P0 + r
    $P1 = s * j
    $P1 = $P1 + t
    $P0 = $P0 // $P1
    .return ($P0)
.end

#def pi_digits():
#
#    z = (1, 0, 0, 1)
#    x = gen_x()

.sub pi_digits
    .local pmc x0,x1,x2,x3, y, z0,z1,z2,z3, one, three, four
    z0 = new .Integer
    z0 = 1
    z1 = new .Integer
    z2 = new .Integer
    z3 = new .Integer
    z3 = 1
    one = new .Integer
    one = 1
    three = new .Integer
    three = 3
    four = new .Integer
    four = 4
#    while 1:
#        y = extract(z, 3)
loop1:
	y = extract(z0, z1, z2, z3, three)
	
#        while y != extract(z, 4):
loop2:
	    $P0 = extract(z0, z1, z2, z3, four)
	    if y == $P0 goto end_loop2
	
#            z = compose(z, x.next())
	    ($P1, $P2, $P3, $P4) = gen_x(one)
	    (z0, z1, z2, z3) = "compose"(z0, z1, z2, z3, $P1, $P2, $P3, $P4)
	
#            y = extract(z, 3)
	    y = extract(z0, z1, z2, z3, three)
	goto loop2
end_loop2:
#        z = compose((10, -10*y, 0, 1), z)
#        yield y
	$P5 = new .Integer
	$P5 = 10
	$P6 = new .Integer
	$P6 = -10
	$P6 = $P6 * y
	$P7 = new .Integer
	$P8 = new .Integer
	$P8 = 1
	(z0, z1, z2, z3) = "compose"($P5, $P6, $P7, $P8, z0, z1, z2, z3)
	.yield (y)
    goto loop1
.end

#def main():
#    n = int(sys.argv[1])
#    digits = pi_digits()
#    width = 10
#    for i in xrange(width, n+1, width):
#        print "%s\t:%d" % ("".join(imap(str, islice(digits, width))), i)
#    if n % width > 0:
#        print "%s\t:%d" % ("".join(imap(str, islice(digits, n % width))).ljust(width), n)
#
#main()

.sub main :main
    .param pmc argv
    $S0 = argv[1]
    .local int i, N, width
    width = 10
    N = $S0
    null i
loop:
    $P0 = pi_digits()
    print $P0
    inc i
    $I0 = i % width
    if $I0 goto n
    print "\t:"
    print i
    print "\n" 
n:
    if i < N goto loop
    $I0 = i % width
    unless $I0 goto done
    $I0 = width - $I0
rest:
        print " "
        dec $I0
        if $I0 goto rest
    print "\t:"
    print N
    print "\n" 
done:
.end
    
.sub main :main
	.param pmc argv
	$S0 = argv[1]
	$I0 = $S0
	load_bytecode "random_lib.pir"
	$P0 = global "gen_random"
while_1:
	$P0(100.0)
	dec $I0
	if $I0 > 1 goto while_1
	$N0 = $P0(100.0)
	$P0 = new .FixedFloatArray
	$P0 = 1
	$P0[0] = $N0
	$S0 = sprintf "%.9f\n", $P0
	print $S0
	.return(0)
.end

.const float IM = 139968.0
.const float IA = 3877.0
.const float IC = 29573.0

.sub gen_random
	.param float max
	.local pmc last
	.include "include/errors.pasm"
	errorson .PARROT_ERRORS_GLOBALS_FLAG
	last = global "last"
	unless last goto lastok
	last = new .Float
	last = 42.0
	global "last" = last
lastok:
	$N0 = last
	$N0 *= IA
	$N0 += IC
	$N0 %= IM
	$N1 = max
	$N1 *= $N0
	$N1 /= IM
	last = $N0
	.return($N1)
.end

#!./parrot -j
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# 
# Contributed by Joshua Isom

# I0 = last
# I1 is the counter for the loop, a do while loop instead of a while loop
# N2 is the argument for gen_random
# N3 is the return from gen_random
main:
	get_params "(0)", P0
	elements I0, P0
	eq I0, 2, hasargs
	set I1, 900000
	branch argsdone
hasargs:
	set S0, P0[1]
	set I1, S0
argsdone:
	set I0, 42
	dec I1
while_1:
	set N2, 100.0
	bsr gen_random
	dec I1 
	if I1, while_1 
	bsr gen_random
	new P0, .FixedFloatArray
	set P0, 1 
	set P0[0], N3
	sprintf S0, "%.9f\n", P0 
	print S0 
	end

.constant IM 139968
.constant IA 3877
.constant IC 29573

gen_random:
	mul I0, .IA
	add I0, .IC
	mod I0, .IM
	set N1, I0
	mul N3, N2, N1
	div N3, .IM
	ret

#!./parrot -C
#
# random.pir N         (N = 900000 for shootout)
# by Joshua Isom

.sub main :main
	.param pmc argv
	$S0 = argv[1]
	$I0 = $S0
while_1:
	gen_random(100.0)
	dec $I0
	if $I0 > 1 goto while_1
	$N0 = gen_random(100.0)
	$P0 = new .FixedFloatArray
	$P0 = 1
	$P0[0] = $N0
	$S0 = sprintf "%.9f\n", $P0
	print $S0
	.return(0)
.end

.const float IM = 139968.0
.const float IA = 3877.0
.const float IC = 29573.0

.sub gen_random
	.param float max
	.local float last
	last = 42.0
loop:
	$N0 = last
	$N0 *= IA
	$N0 += IC
	$N0 %= IM
	$N1 = max
	$N1 *= $N0
	$N1 /= IM
	last = $N0
	.yield($N1)
	get_params "(0)", max
	goto loop
.end

#!./parrot -Oc -Cj
#
# Ack by Leopold Toetsch
# Fib and Tak by Joshua Isom

# use less registers (leo) 
# time ./parrot -Oc -Cj recursive-2.pir 11
# real 2.32 s   (AMD X2@2000)

.sub main :main
    .param pmc argv
    .local int argc, n
    argc = argv
    n = 1
    unless argc == 2 goto argsok
    $S0 = argv[1]
    n = $S0
argsok:
    $P0 = getinterp
    $P0.'recursion_limit'(100000)

    .local pmc array
    array = new .FixedFloatArray
    array = 11

    dec n

    $I0 = n + 1
    $I1 = ack(3, $I0)
    array[0] = $I0
    array[1] = $I1

    $N0 = 28.0 + n
    array[2] = $N0
    $N0 = FibNum($N0)
    array[3] = $N0

    $I0 = n * 3
    $I1 = n * 2
    array[4] = $I0
    array[5] = $I1
    array[6] = n
    $I0 = TakInt($I0, $I1, n)
    array[7] = $I0

    $I0 = FibInt(3)
    array[8] = $I0

    $N0 = TakNum(3.0, 2.0, 1.0)
    array[9] = $N0

    $S0 = sprintf <<"END", array
Ack(3,%d): %d
Fib(%.1f): %.1f
Tak(%d,%d,%d): %d
Fib(3): %d
Tak(3.0,2.0,1.0): %.1f
END
    print $S0
.end

.sub ack
    .param int x
    .param int y
    unless x goto a1
    unless y goto a2
    dec y
    y = ack(x, y)
    dec x
    .return ack(x, y)
a1:
    inc y
    .return (y)
a2:
    dec x
    .return ack(x, 1)
.end

.sub FibInt
    .param int n
    unless n < 2 goto endif
    .return(1)
endif:
    .local int tmp
    tmp = n - 2
    $I0 = FibInt(tmp)
    tmp = n - 1
    $I1 = FibInt(tmp)
    $I0 += $I1
    .return($I0)
.end

.sub FibNum
    .param num n
    unless n < 2.0 goto endif
    .return(1.0)
endif:
    .local num tmp
    tmp = n - 2.0
    $N0 = FibNum(tmp)
    dec n
    n = FibNum(n)
    $N0 += n
    .return($N0)
.end

.sub TakNum
    .param float x
    .param float y
    .param float z
    unless y >= x goto endif
    .return(z)
endif:
    .local float tmp
    tmp = x - 1
    $N0 = TakNum(tmp, y, z)
    tmp = y - 1
    $N1 = TakNum(tmp, z, x)
    dec z
    z = TakNum(z, x, y)
    .return TakNum($N0, $N1, z)
.end

.sub TakInt
    .param int x
    .param int y
    .param int z
    unless y >= x goto endif
    .return(z)
endif:
    .local int tmp
    tmp = x - 1
    $I0 = TakInt(tmp, y, z)
    tmp = y - 1
    tmp = TakInt(tmp, z, x)
    dec z
    z   = TakInt(z, x, y)
    .return TakInt($I0, tmp, z)
.end

#!./parrot
# by Joshua Isom

.sub main :main
	load_bytecode "PGE.pbc"
	.local pmc p6rule_compile, rulesub, match, variants, variants_p5, iub, iter, matches, capt
	.local string pattern, chunk, seq, key, replacement
	.local int readlen, chunklen, seqlen, finallen, i, varnum, count
	p6rule_compile = compreg "PGE::P6Regex"
	
	# Store the regexes we need...
	variants = new .FixedStringArray
	variants = 9
	variants[0] = '      agggtaaa|tttaccct      '
	variants[1] = '<[cgt]>gggtaaa|tttaccc<[acg]>'
	variants[2] = 'a<[act]>ggtaaa|tttacc<[agt]>t'
	variants[3] = 'ag<[act]>gtaaa|tttac<[agt]>ct'
	variants[4] = 'agg<[act]>taaa|ttta<[agt]>cct'
	variants[5] = 'aggg<[acg]>aaa|ttt<[cgt]>ccct'
	variants[6] = 'agggt<[cgt]>aa|tt<[acg]>accct'
	variants[7] = 'agggta<[cgt]>a|t<[acg]>taccct'
	variants[8] = 'agggtaa<[cgt]>|<[acg]>ttaccct'
	# and store the p5regex style for printing
	variants_p5 = new .Hash
	variants_p5['      agggtaaa|tttaccct      '] = 'agggtaaa|tttaccct'
	variants_p5['<[cgt]>gggtaaa|tttaccc<[acg]>'] = '[cgt]gggtaaa|tttaccc[acg]'
	variants_p5['a<[act]>ggtaaa|tttacc<[agt]>t'] = 'a[act]ggtaaa|tttacc[agt]t'
	variants_p5['ag<[act]>gtaaa|tttac<[agt]>ct'] = 'ag[act]gtaaa|tttac[agt]ct'
	variants_p5['agg<[act]>taaa|ttta<[agt]>cct'] = 'agg[act]taaa|ttta[agt]cct'
	variants_p5['aggg<[acg]>aaa|ttt<[cgt]>ccct'] = 'aggg[acg]aaa|ttt[cgt]ccct'
	variants_p5['agggt<[cgt]>aa|tt<[acg]>accct'] = 'agggt[cgt]aa|tt[acg]accct'
	variants_p5['agggta<[cgt]>a|t<[acg]>taccct'] = 'agggta[cgt]a|t[acg]taccct'
	variants_p5['agggtaa<[cgt]>|<[acg]>ttaccct'] = 'agggtaa[cgt]|[acg]ttaccct'

	iub = new .Hash
	iub['b'] = '(c|g|t)'
	iub['d'] = '(a|g|t)'
	iub['h'] = '(a|c|t)'
	iub['k'] = '(g|t)'
	iub['m'] = '(a|c)'
	iub['n'] = '(a|c|g|t)'
	iub['r'] = '(a|g)'
	iub['s'] = '(c|g)'
	iub['v'] = '(a|c|g)'
	iub['w'] = '(a|t)'
	iub['y'] = '(c|t)'
	
	############################################
	# Read in the file
beginwhile:
	chunk = read 65535
	chunklen = length chunk
	unless chunklen goto endwhile
	# They don't say you have to match case insenitive...
	downcase chunk
	seq .= chunk
	goto beginwhile
endwhile:
	readlen = length seq

	#############################################
	# Remove all junk
	pattern = '[ ( [ \> \N*: ] )  | \N*:(\n) ]*'
	rulesub = p6rule_compile(pattern)
	match = rulesub(seq)

	capt = match[0]
stripfind:
	unless capt goto endstripfind
	$P0 = pop capt
	$I0 = $P0."from"()
	$I1 = $P0."to"()
	$I1 -= $I0
	substr seq, $I0, $I1, ''
	goto stripfind
endstripfind:
	seqlen = length seq
	
	###########################################
	# Count the matches
	varnum = elements variants
	i = 0
beginfor:
	count = 0
	unless i < varnum goto endfor

	pattern = variants[i]
	# The spec says to print the p5 style regex, shame on them
	$S0 = variants_p5[pattern]
	print $S0
	print " "
	# And out of spite, use p6 rules anyway
	rulesub = p6rule_compile(pattern)
	match = rulesub(seq)

match_loop:
	unless match goto next
	inc count
	match."next"()
	goto match_loop
next:
	inc i
	print count
	print "\n"
	goto beginfor
endfor:

	#####################################################
	# Final replace to make the sequence a p5 style regex
	.include "iterator.pasm"
	iter = new .Iterator, iub
	set iter, .ITERATE_FROM_START
	matches = new .ResizablePMCArray
iter_loop:
	unless iter goto iter_end
	key = shift iter
	replacement = iub[key]
	# Ok, using a regex to match a single fixed character is probably excessive
	# But it's what's wanted...
	rulesub = p6rule_compile(key)
	match = rulesub(seq)

##########################################
switch:
	unless match goto endswitch
	$I0 = match."from"()
	$I1 = match."to"()
	$I1 -= $I0
	$P0 = new .FixedIntegerArray
	$P0 = 2
	$P0[0] = $I0
	$P0[1] = $I1
	push matches, $P0
	match."next"()
	goto switch
endswitch:

switchloop:
	unless matches goto endswitchloop
	$P0 = pop matches
	$I0 = $P0[0]
	$I1 = $P0[1]
	substr seq, $I0, $I1, replacement
	goto switchloop
endswitchloop:
#############################################
	goto iter_loop
iter_end:
	finallen = length seq

	print "\n"
	print readlen
	print "\n"
	print seqlen
	print "\n"
	print finallen
	print "\n"
.end
#!/usr/bin/parrot -j
# Reads from stdin a file in the format made by fasta.pir
# N = 2500000 for fasta

# 2.2 s on AMD@2000/512K cache

# Original by Joshua Isom, heavily hacked by Leopold Toetsch

# create tr table at compile-time
# tr{wsatugcyrkmbdhvnATUGCYRKMBDHVN}
#            {WSTAACGRYMKVHDBNTAACGRYMKVHDBN};

.sub tr_00_init :immediate
    .local pmc tr_array
    tr_array = new .FixedIntegerArray   # Todo char array
    tr_array = 256                      # Python compat ;)
    .local string from, to
    from = 'wsatugcyrkmbdhvnATUGCYRKMBDHVN'
    to   = 'WSTAACGRYMKVHDBNTAACGRYMKVHDBN'
    .local int i, ch, r, len
    len = length from
    null i
loop:
    ch = ord from, i
    r  = ord to,   i
    tr_array[ch] = r
    inc i
    if i < len goto loop
    .return(tr_array)
.end

.sub main :main
	.local pmc stdin, stdout
	.local string line, seq
	stdin = getstdin
	stdout = getstdout
	# stdout is linebuffered per default - make it block buffered
	stdout.'setbuf'(8192)

	seq = ''

beginwhile:
	line = readline stdin
	unless line goto endwhile
	$I0 = ord line
	unless $I0 == 62 goto else   # '>' 
		if seq == '' goto no_rev
		print_revcomp(seq)
		seq = ''
	no_rev:
		print line
		goto endif
	else:
		chopn line, 1
		seq .= line
	endif:
	goto beginwhile
endwhile:
	if seq == '' goto done
	print_revcomp(seq)
done:
.end

.sub print_revcomp
	.param string line
	.local int i, linelen, ch
	linelen = length line

	reverse line

	.const .Sub tr_00 = 'tr_00_init'
	trans line, tr_00

	i = 0
	$S0 = 'x'
print_loop:	
	$S0 = substr_r line, i, 60
	print $S0
	print "\n"
	i += 60
	if i >= linelen goto done
	goto print_loop
done:
	$S0 = ''
.end

#!./parrot -j
#
# spectralnorm.pir N         (N = 100 for shootout)
# by Michal Jurosz

.sub eval_A
	.param int i
	.param int j

	# return 1.0/((i+j)*(i+j+1)/2+i+1);
	$N0 = i + j
	$N1 = $N0 + 1
	$N0 *= $N1
	$N0 /= 2
	$N0 += i
	$N0 += 1
	$N0 = 1 / $N0
	.return ($N0)
.end


.sub eval_A_times_u
	.param int N
	.param pmc u
	.param pmc Au

	.local int i, j

	i = 0
beginfor_i:
	unless i < N goto endfor_i
		Au[i] = 0
		j = 0
	beginfor_j:
		unless j < N goto endfor_j
			# Au[i]+=eval_A(i,j)*u[j]
			$N0 = eval_A(i,j)
			$N1 = u[j]
			$N0 *= $N1
			$N1 = Au[i]
			$N0 += $N1
			Au[i] = $N0

		inc j
		goto beginfor_j
	endfor_j:

	inc i
	goto beginfor_i
endfor_i:
.end


.sub eval_At_times_u
	.param int N
	.param pmc u
	.param pmc Au

	.local int i, j

	i = 0
beginfor_i:
	unless i < N goto endfor_i
		Au[i] = 0
		j = 0
	beginfor_j:
		unless j < N goto endfor_j
			# Au[i]+=eval_A(j,i)*u[j]
			$N0 = eval_A(j,i)
			$N1 = u[j]
			$N0 *= $N1
			$N1 = Au[i]
			$N0 += $N1
			Au[i] = $N0

		inc j
		goto beginfor_j
	endfor_j:

	inc i
	goto beginfor_i
endfor_i:
.end	


.sub eval_AtA_times_u
	.param int N
	.param pmc u
	.param pmc AtAu

	.local pmc v
	v = new .FixedFloatArray
	v = N

	eval_A_times_u(N,u,v)
	eval_At_times_u(N,v,AtAu)
.end


.sub main :main
	.param pmc argv
	$S0 = argv[1]
	.local int N
	N = $S0

	.local pmc u, v
	u = new .FixedFloatArray
	u = N
	v = new .FixedFloatArray
	v = N

	.local int i

	i = 0
beginfor_init:
	unless i < N goto endfor_init
		u[i] = 1
	inc i
	goto beginfor_init
endfor_init:


	i = 0
beginfor_eval:
	unless i < 10 goto endfor_eval
	    eval_AtA_times_u(N,u,v)
	    eval_AtA_times_u(N,v,u)
	inc i
	goto beginfor_eval
endfor_eval:

	.local float vBv, vv
  	vBv = 0.0
  	vv = 0.0

	i = 0
beginfor_calc:
	unless i < N goto endfor_calc
		# vBv+=u[i]*v[i]; vv+=v[i]*v[i];
		$N0 = u[i]
		$N1 = v[i]
		$N0 *= $N1
		vBv += $N0
		$N0 = $N1
		$N0 *= $N0
		vv += $N0
	inc i
	goto beginfor_calc
endfor_calc:
  	
	# print "%0.9f" % (sqrt(vBv/vv))
	$N0 = vBv / vv
	$N0 = sqrt $N0
	.local pmc spf
	spf = new .FixedFloatArray
	spf = 1
	spf[0] = $N0
	$S0 = sprintf "%.9f\n", spf
	print $S0
	exit 0
.end

#!/usr/bin/parrot -j
#
# by Joshua Isom
.sub main :main
	.local pmc stdin
	.local string line
	.local int count, tmp, linelen
	count = 0
	stdin = getstdin
beginwhile:
	line = readline stdin
	linelen = length line
	unless linelen goto endwhile
	tmp	= line
	count += tmp
	goto beginwhile
endwhile:
	print count
	print "\n"
.end

#!/usr/bin/parrot -C
# by Joshua Isom
.sub main :main
	.param pmc argv
	.local int argc, n
	argc = argv
	n = 1
	unless argc == 2 goto argsok
	$S0 = argv[1]
	n = $S0
argsok:
	.local float f
	$N0 = n
	$N0 *= 3
	$N1 = n
	$N1 *= 2
	$N2 = n
	$N2 *= 1
	f = Tak($N0, $N1, $N2)
	$P0 = new .FixedFloatArray
	$P0 = 1
	$P0[0] = f
	$S0 = sprintf "%.1f\n", $P0
	print $S0
.end

.sub Tak
	.param float x
	.param float y
	.param float z
	unless y >= x goto endif
	.return(z)
endif:
	.local float tmp
	tmp = x - 1
	$N0 = Tak(tmp, y, z)
	tmp = y - 1
	$N1 = Tak(tmp, z, x)
	tmp = z - 1
	$N2 = Tak(tmp, x, y)
	.return Tak($N0, $N1, $N2)
.end

