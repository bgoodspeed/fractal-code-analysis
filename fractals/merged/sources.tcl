#!/usr/bin/tclsh
# $Id: ackermann.tcl,v 1.9 2005-11-05 21:30:15 igouy-guest Exp $
# http://shootout.alioth.debian.org/
#
# Updated based on ideas from Stefan Finzel
#
# Further optimized by Mark Butler. 
#
# vim: set filetype=tcl:

proc ack {n m} {
    if {$m} {
	if {$n} {
	    #
	    # incr is quicker than an equivalent expr, as long as
	    # you don't mind altering the variable.
	    return [ack [ack [incr n -1] $m] [incr m -1]]
	} else {
	    return [ack 1 [incr m -1]]
	}
    } else {
	return [incr n]
    }
}

interp recursionlimit {} 20000
set N [lindex $argv 0]
if {$N < 1} {set N 1}
puts "Ack(3,$N): [ack $N 3]"
#!/usr/bin/tclsh
# $Id: ackermann.tcl-2.tcl,v 1.1 2004-11-10 06:09:46 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from Michael A. Cleverly

proc unknown {args} {
    if {![regexp {^Ack \((\d+),(\d+)\)$} [lindex $args 0] => m n]} {
        error "Invalid command name \"[lindex $args 0]\""
    }

    if {$m == 0} {
        set result [expr {$n + 1}]
    } elseif {$n == 0} {
        set result ["Ack ([expr {$m - 1}],1)"]
    } else {
        set result ["Ack ([expr {$m - 1}],["Ack ($m,[expr {$n - 1}])"])"]
    }

    proc "Ack ($m,$n)" {} "return $result"
    return $result
}

proc main {NUM} {
    if {![string is integer -strict $NUM] || $NUM < 1} {
        set NUM 1
    } elseif {$NUM > 8} {
        error "$NUM is out of range.  Should be from 1 to 8."
    }

    set result [expr {["Ack (3,0)"] + ["Ack (3,$NUM)"] - ["Ack (3,0)"]}]
    puts "Ack(3,$NUM): $result"
}

main [lindex $argv 0]
#!/usr/bin/tclsh
# $Id: ackermann.tcl-3.tcl,v 1.2 2005-03-30 22:24:56 sgeard-guest Exp $
# http://shootout.alioth.debian.org/
#
# Updated based on ideas from Stefan Finzel
#
# Further optimized by Mark Butler.
#
# vim: set filetype=tcl:

proc ack {m n} {
    if {$m} {
	if {$n} {
	    #
	    # incr is quicker than an equivalent expr, as long as
	    # you don't mind altering the variable.
	    return [ack [expr {$m - 1}] [ack $m [incr n -1]]]
	} else {
	    return [ack [incr m -1] 1]
	}
    } else {
	return [incr n]
    }
}

interp recursionlimit {} 10000
set N [lindex $argv 0]
if {$N < 1} {set N 1}
puts "Ack(3,$N): [ack 3 $N]"
#!/usr/bin/tclsh
# $Id: ary.tcl,v 1.3 2004-06-30 07:28:57 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program is modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.
# With suggestions by Juan Carlos Gil Montoro

proc main {} {
    global argv
    set n [lindex $argv 0]
    set last [expr {$n - 1}]
    for {set i 0} {$i < $n} {incr i} {
	set x($i) [expr {$i + 1}]
	set y($i) 0
    }
    for {set k 0} {$k < 1000} {incr k} {
	for {set j $last} {$j >= 0} {incr j -1} {
	    incr y($j) $x($j)
	}
    }
    puts "$y(0) $y($last)"
}

main
#!/usr/bin/tclsh
##
## The Computer Lannguage Shootout
## http://shootout.alioth.debian.org/
## Contributed by Heiner Marxen
##
## "binary-trees"	for Tcl
## Call:	tclsh binarytrees.tcl 16
##
## $Id: binarytrees.tcl,v 1.2 2005-12-03 17:02:14 sgeard-guest Exp $

## A tree node is implemented as a [list] with 3 elements:
##	[0] left  subtree
##	[1] right subtree
##	[2] item
## An empty tree is an empty list {}, an thus has [llength] 0.

proc ItemCheck {tree} {
    if {![llength [lindex $tree 0]]} {
	return [lindex $tree 2]
    } else {
	return [expr {             [lindex $tree 2]
		      + [ItemCheck [lindex $tree 0]]
		      - [ItemCheck [lindex $tree 1]]}]
    }
}

proc BottomUpTree {item depth} {
    if {$depth > 0} {
	set ndepth [expr {$depth - 1}]
	return [list [BottomUpTree [expr {2 * $item - 1}] $ndepth] \
		     [BottomUpTree [expr {2 * $item    }] $ndepth] \
		     $item
	       ]
    } else {
	return [list {} {} $item]
    }
}

proc tellTree {typ depth check} {
    puts "$typ tree of depth $depth\t check: $check"
}

proc main {argv} {
    set N [lindex $argv 0]

    set minDepth 4

    if {($minDepth + 2) > $N} {
	set maxDepth [expr {$minDepth + 2}]
    } else {
	set maxDepth $N
    }

    set stretchDepth [expr {$maxDepth + 1}]

    set stretchTree [BottomUpTree 0 $stretchDepth]
    tellTree "stretch" $stretchDepth [ItemCheck $stretchTree]
    set stretchTree {}

    set longLivedTree [BottomUpTree 0 $maxDepth]

    for {set dep $minDepth} {$dep <= $maxDepth} {incr dep 2} {
	set iterations [expr {1 << ($maxDepth - $dep + $minDepth)}]
	set check 0
	for {set i 1} {$i <= $iterations} {incr i} {
	    set tempTree [BottomUpTree $i $dep]
	    set check [expr {$check + [ItemCheck $tempTree]}]
	    set tempTree {}

	    set tempTree [BottomUpTree [expr {-$i}] $dep]
	    set check [expr {$check + [ItemCheck $tempTree]}]
	    set tempTree {}
	}

	puts "[expr {$iterations * 2}]\t trees of depth $dep\t check: $check"
    }

    tellTree "long lived" $maxDepth [ItemCheck $longLivedTree]

    return 0
}

main $argv
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# Contributed by Andrew McParland

package require Thread

proc meeting_place {} {
    thread::create {
        proc init {meetings} {
            global meetings_left first
            set meetings_left $meetings
            set first 1
        }
        # Process the request to meet
        proc meet {tid colour} {
            global meetings_left first first_colour first_tid
            if {$meetings_left == 0} {
                thread::send -async $tid [list met "Faded"]
            } else {
                if {$first} {
                    set first_tid $tid
                    set first_colour $colour
                    set first 0
                } else {
                    # Tell the 2 creatures the colour of the other creature
                    thread::send -async $first_tid [list met $colour]
                    thread::send -async $tid [list met $first_colour]
                    incr meetings_left -1
                    set first 1
                }
            }
        }
        thread::wait
    }
}

proc create_creature {} {
    thread::create -joinable {
        proc start {initial_colour MeetingPlaceId} {
            global colour MeetingPlace meetings
            set colour $initial_colour
            set MeetingPlace $MeetingPlaceId
            set meetings 0
            # Start the meeting process
            thread::send -async $MeetingPlace [list meet [thread::id] $colour]
        }
        # Called when met another creature
        proc met {col} {
            global meetings MeetingPlace colour
            if {$col ne "Faded"} {
                set colour [change_colour $colour $col]
                incr meetings
                # Request another meeting
                thread::send -async $MeetingPlace [list meet [thread::id] $colour]
            } else {
                # Fade away...
                tsv::incr creatures sum $meetings
                thread::release
            }
        }
        proc change_colour {col1 col2} {
            if {$col1 eq $col2} {return $col1}
            switch $col1 {
                "Blue" {return [expr {$col2 eq "Red" ? "Yellow" : "Red"}]}
                "Red" {return [expr {$col2 eq "Blue" ? "Yellow" : "Blue"}]}
                "Yellow" {return [expr {$col2 eq "Blue" ? "Red" : "Blue"}]}
                default {return $col1}
            }
        }
        thread::wait
    }
}

# Initialise thread-shared sum of creatures met
tsv::set creatures sum 0

# Create the meeting place thread
set MeetingPlace [meeting_place]

set colours [list Blue Red Yellow Blue]

# Start the creature threads
foreach c $colours {
    lappend threads [create_creature]
}

# Initialise the meeting place and start each creature
thread::send $MeetingPlace [list init [lindex $argv 0]]
foreach t $threads c $colours {
    thread::send $t [list start $c $MeetingPlace]
}

# Wait for the creature threads to finish
foreach t $threads {
    thread::join $t
}

# Print sum of meetings
puts [tsv::set creatures sum]
#!/usr/bin/tclsh

# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# from: Kristoffer Lawson
# with help from Miguel Sofer
# modified by Daniel South

proc Server {channel clientaddr clientport} {
    fconfigure $channel -buffering line -encoding unicode
    set rLength 0
    while {![eof $channel]} {
	if {[gets $channel r] > 0} {
	    puts $channel $r
	    # Extra increase because [gets] doesn't return \n
	    incr rLength [string length $r]
	    incr rLength
	}
    }
    puts "server processed $rLength bytes"
    exit
}

proc doChild {num} {
    set fd [socket localhost 9900]
    fconfigure $fd -buffering line -encoding unicode
    set msg "Hello there sailor"

    while {[incr num -1] >= 0} {
	puts $fd $msg
	while {![gets $fd r]} {}
	if {$r ne $msg} {error "Received different message: $r."; exit}
    }
    close $fd
}

set n [lindex $argv 0]

if {[llength $argv] == 2} {
    doChild $n
} else {
    socket -server Server 9900
    exec [info nameofexecutable] [info script] $n & &
    vwait forever
}
#!/usr/bin/tclsh
# $Id: except.tcl,v 1.1 2004-05-19 18:09:43 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Kristoffer Lawson
# modified by Miguel Sofer

set HI 0
set LO 0

proc some_function {num} {
    if {[catch {hi_function $num} result]} {
        puts stderr "We shouldn't get here ($result)"
    }
}

proc hi_function {num} {
    if {[set exc [catch {lo_function $num}]] == 11} {
        # handle
	incr ::HI
    } else {
        # rethrow
	return -code $exc
    }
}

proc lo_function {num} {
    if {[set exc [catch {blowup $num}]] == 10} {
        # handle
	incr ::LO
    } else {
        # rethrow
	return -code $exc
    }
}

proc blowup {num} {
    if {$num % 2} {
        #error "Lo_exception"
	return -code 10
    } else {
        #error "Hi_exception"
	return -code 11
    }
}

proc main {} {
    global argv HI LO
    set NUM [lindex $argv 0]
    if {$NUM < 1} {
        set NUM 1
    }
    incr NUM
    while {[incr NUM -1]} {
        some_function $NUM
    }
    puts "Exceptions: HI=$HI / LO=$LO"
}

main
#!/usr/bin/tclsh
##
## The Computer Lannguage Shootout
## http://shootout.alioth.debian.org/
## Contributed by Heiner Marxen
##
## "fannkuch"	for Tcl
## Call:	tclsh fannkuch.tcl 9
##
## $Id: fannkuch.tcl-2.tcl,v 1.1 2005-12-04 23:58:38 igouy-guest Exp $

proc fannkuch {n} {
    if {$n < 1} {
	return 0
    }
    set n1 [expr {$n - 1}]		;# just caches n-1

    set nL [list]			;# caches list < $n
    for {set i 0} {$i < $n} {incr i} {
	lappend nL $i
    }
    foreach j $nL {
	set L [list]
	set k $j
	for {set i 0} {$i < $k} {incr i; incr k -1} {
	    lappend L $i $k
	}
	set IK($j) $L		;# caches inner loop as above
	;# quadratic overhead is ok for factorial usage
    }

    foreach i $nL {
	set perm1($i) $i		;# initial (trivial) permu
    }

    set r        $n
    set didpr    0
    set flipsMax 0
    while 1 {
	if {$didpr < 30} {
	    foreach i $nL {
		puts -nonewline "[expr {1 + $perm1($i)}]"
	    }
	    puts ""
	    incr didpr
	}
	for {} {$r != 1} {incr r -1} {
	    set count([expr {$r-1}]) $r
	}

	if {! ($perm1(0) == 0  ||  $perm1($n1) == $n1)} {
	    set flips 0

	    ;#array set perm [array get perm1]	;# is slower
	    foreach i $nL {
		set perm($i) $perm1($i)		;# perm = perm1
	    }
	    for {set k $perm(0)} {$k} {set k $perm(0)} {
		;#for {set i 0} {$i < $k} {incr i; incr k -1}
		foreach {i k} $IK($k) {
		    set t $perm($i)
		    set    perm($i) $perm($k)
		    set              perm($k) $t
		    ;# the foreach exchange approach is much slower, here
		}
		incr flips
	    }

	    if {$flipsMax < $flips} {
		set flipsMax $flips
	    }
	}

	while 1 {
	    if {$r == $n} {
		return $flipsMax
	    }
	    ;# rotate down perm[0..r] by one
	    set perm0 $perm1(0)
	    for {set i 0} {$i < $r} {} {
		set perm1($i) $perm1([incr i])	;# tricky: increment embedded
	    }
	    set perm1($r) $perm0
	    if {[incr count($r) -1] > 0} {
		break
	    }
	    incr r
	}
    }
}

proc main {argv} {
    set n 0
    if {[llength $argv]} {
	set n [lindex $argv 0]
    }
    puts "Pfannkuchen($n) = [fannkuch $n]"
    return 0
}

main $argv
#!/usr/bin/tclsh

# Fasta benchmark
#
# Contributed by Michael Schlenker

foreach {IM IA IC last} {139968 3877 29573 42} break

set alu GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGT
append alu CAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCC
append alu GGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCC
append alu GGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACT
append alu CCGTCTCAAAAA

set iub {a 0.27 c 0.12 g 0.12 t 0.27 B 0.02 D 0.02 H 0.02 K 0.02 M 0.02 N 0.02
	 R 0.02 S 0.02 V 0.02 W 0.02 Y 0.02}

set hsapiens {a 0.3029549426680 c 0.1979883004921 g 0.1975473066391
		 t 0.3015094502008}

proc make_gen_random {} {
    set params [list IM $::IM IA $::IA IC $::IC]
    set body [string map $params {
	expr {($max * [set ::last [expr {($::last * IA + IC) % IM}]]) / IM}}]
    proc gen_random {max} $body
}

proc make_cumulative {table} {
    set prob 0.0
    set pl [list]

    foreach {char p} $table {lappend pl [set prob [expr {$prob + $p}]] $char}
    return $pl
}

proc make_repeat_fasta {id desc src n} {
    foreach {width s e} {59 0 59} break

    puts ">$id $desc"
    set l [string length $src]
    set ls [string repeat $src [expr {([incr n -1] / $l)+1 }]]
    while {$e < $n} {
	puts [string range $ls $s $e]
	set s [incr e]
	incr e $width
    }
    puts [string range $ls $s $n]
}

proc make_random_fasta {id desc src n} {
    foreach {width line} {60 ""} break

    puts ">$id $desc"
    set prob [make_cumulative $src]
    for {set i 0} {$i < $n} {incr i} {
	set rand [gen_random 1.0]
	foreach {p c} $prob {
	    if {$p > $rand} {
		append line $c
		break
	    }
	}
	if {[string length $line] == $width} {
	    puts $line
	    set line ""
	}
    }
    if {[string length $line]} {puts $line}
}

proc main {n} {
    make_gen_random
    make_repeat_fasta ONE "Homo sapiens alu" $::alu [expr {$n*2}]
    make_random_fasta TWO "IUB ambiguity codes" $::iub [expr {$n*3}]
    make_random_fasta THREE "Homo sapiens frequency" $::hsapiens [expr {$n*5}]
}

set N [lindex $argv 0]
if {$N < 1} {set N 1}
main $N
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org
# contributed by Michael Schlenker
# small modification by Andrew McParland

foreach {IM IA IC last} {139968 3877 29573 42} break

set alu GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGT
append alu CAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCC
append alu GGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCC
append alu GGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACT
append alu CCGTCTCAAAAA

set iub {a 0.27 c 0.12 g 0.12 t 0.27 B 0.02 D 0.02 H 0.02 K 0.02 M 0.02 N 0.02
	 R 0.02 S 0.02 V 0.02 W 0.02 Y 0.02}

set hsapiens {a 0.3029549426680 c 0.1979883004921 g 0.1975473066391
		 t 0.3015094502008}

proc make_gen_random {} {
    set params [list IM $::IM IA $::IA IC $::IC]
    set body [string map $params {
	expr {($max * [set ::last [expr {($::last * IA + IC) % IM}]]) / IM}}]
    proc gen_random {max} $body
}

proc make_cumulative {table} {
    set prob 0.0
    set pl [list]

    foreach {char p} $table {lappend pl [set prob [expr {$prob + $p}]] $char}
    return $pl
}

proc make_repeat_fasta {id desc src n} {
    foreach {width s e s2 e2} {59 0 59 0 59} break

    puts ">$id $desc"
    set src2 "$src$src"
    set l [string length $src]
    while {$e < $n} {
        puts [string range $src2 $s2 $e2]
	set s [incr e]
	incr e $width
	set s2 [expr {$s % $l}]
	set e2 [expr {$s2 + $width}]
    }
    set rem [expr {$n % ($width + 1)}]
    if {$rem} {puts [string range $src2 $s2 [expr {$s2 + $rem - 1}]]}
}

proc make_random_fasta {id desc src n} {
    foreach {width line} {60 ""} break

    puts ">$id $desc"
    set prob [make_cumulative $src]
    for {set i 0} {$i < $n} {incr i} {
	set rand [gen_random 1.0]
	foreach {p c} $prob {
	    if {$p > $rand} {
		append line $c
		break
	    }
	}
	if {[string length $line] == $width} {
	    puts $line
	    set line ""
	}
    }
    if {[string length $line]} {puts $line}
}

proc main {n} {
    make_gen_random
    make_repeat_fasta ONE "Homo sapiens alu" $::alu [expr {$n*2}]
    make_random_fasta TWO "IUB ambiguity codes" $::iub [expr {$n*3}]
    make_random_fasta THREE "Homo sapiens frequency" $::hsapiens [expr {$n*5}]
}

set N [lindex $argv 0]
if {$N < 1} {set N 1}
main $N
#!/usr/bin/tclsh
# $Id: fibo.tcl,v 1.6 2005-04-25 19:01:39 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

# with help from: Kristoffer Lawson

proc fib {n} {
    if {$n < 2} {
	return 1
    } else {
	return [expr {[fib [incr n -2]] + [fib [incr n]]}]
    }
}

interp recursionlimit {} 10000
puts [fib $argv]
#/usr/bin/tclsh
# $Id: harmonic.tcl,v 1.4 2005-11-05 21:30:15 igouy-guest Exp $
# http://shootout.alioth.debian.org/
#
# Contributed by Robert Seeger 
proc main {inputs} {
    set n [lindex $inputs 0]
    for {set i 1 ; set result 0.0} {$i <= $n} {incr i} {
        set result [expr {$result + (1.0 / $i)}]
    }

    format "%.9f" $result
}

puts [main $argv]
#!/usr/bin/tclsh
# $Id: hash.tcl,v 1.1 2004-05-19 18:09:55 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program is modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

proc main {} {
    global argv
    set n [lindex $argv 0]
    for {set i 1} {$i <= $n} {incr i} {
        set x([format {%x} $i]) $i
    }
    set c 0
    for {set i $n} {$i > 0} {incr i -1} {
	if {[info exists x($i)]} {
	    incr c
	}
    }
    puts $c
}

main
#!/usr/bin/tclsh
# $Id: hash2.tcl,v 1.2 2004-11-30 07:10:03 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Branko Vesligaj and Hemang Lavana

proc main {} {
    global argv
    set n [lindex $argv 0]
    for {set i 0} {$i < 10000} {incr i} {
	set hash1(foo_$i) $i
    }
    for {set i $n} {$i > 0} {incr i -1} {
	foreach k [array names hash1] {
            if {[info exists hash2($k)]} {
                set hash2($k) [expr {$hash1($k) + $hash2($k)}]
            } else {
		set hash2($k) $hash1($k)
	    }
	}
    }
    puts [join [list $hash1(foo_1) $hash1(foo_9999) $hash2(foo_1) $hash2(foo_9999) ] " "]
}

main
#!/usr/bin/tclsh
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# sped up by Miguel Sofer's function generator
# modified by Daniel South

foreach {IM IA IC last} {139968 3877 29573 42} break

proc make_gen_random {} {
    set params [list IM $::IM IA $::IA IC $::IC]
    set body [string map $params {
	expr {($max * [set ::last [expr {($::last * IA + IC) % IM}]]) / IM}}]
    proc gen_random {max} $body
}

proc heapsort {n ra_name} {
    upvar 1 $ra_name ra

    set l [expr {($n >> 1) + 1}]
    while 1 {
        if {$l > 1} {
            set rra [lindex $ra [incr l -1]]
        } else {
	    set rra [lindex $ra $n]
	    lset ra $n [lindex $ra 1]
	    if {[incr n -1] == 1} {return [lset ra 1 $rra]}
        }
	set i $l
	set j [expr {$l << 1}]
        while {$j <= $n} {
	    if {$j < $n && [lindex $ra $j] < [lindex $ra [expr {$j + 1}]]} {
		incr j
	    }
            if {$rra < [lindex $ra $j]} {
		lset ra $i [lindex $ra $j]
		set i $j
                incr j $j
            } else {
		set j $n
		incr j
            }
        }
        lset ra $i $rra
    }
}

proc main {n} {
    make_gen_random
    for {set i 1} {$i <= $n} {incr i} {lappend data [gen_random 1.0]}
    incr n -1
    heapsort $n data
    puts [format "%.10f" [lindex $data $n]]
}

set N [lindex $argv 0]
if {$N < 1} {set N 1}
main $N
#!/usr/bin/tclsh
# $Id: hello.tcl,v 1.1 2004-05-19 18:10:16 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

puts "hello world"
#!/usr/bin/tclsh

#  The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by Daniel South 


proc kFrequency {s k framesize} {
   global freq

   set n [string length $s]
   incr k -1
   incr n -$k
   for {set i [incr framesize -1]} {$i < $n} {incr i} {
      set c [string range $s $i [incr i $k]]
      if {[catch {incr freq($c)}]} {set freq($c) 1}
   }
}


proc frequency {s k} {
   array unset ::freq
   set sum 0

   for {set i 0} {$i < $k} {} {kFrequency $s $k [incr i]}
   foreach {fragment count} [array get ::freq] {
      lappend sortheap [list $fragment $count]
      incr sum $count
   }
   foreach item [lsort -integer -index 1 -decreasing [lsort $sortheap]] {
      set percent [expr {double([lindex $item 1]) / $sum * 100}]
      puts [format "%s %0.3f" [lindex $item 0] $percent]
   }
   puts ""
}

proc count {s fragment} {
    array unset ::freq
    set count 0

    set k [string length $fragment]
    for {set i 0} {$i < $k} {} {kFrequency $s $k [incr i]}
    if {[info exists ::freq($fragment)]} {set count $::freq($fragment)}
    puts $count\t$fragment
}

proc main {} {
    while {[gets stdin line] != -1} {if {[string match ">THREE*" $line]} break}
    while {[gets stdin line] != -1} {append sequence $line}
    set sequence [string toupper $sequence]

    frequency $sequence 1
    frequency $sequence 2

    count $sequence "GGT"
    count $sequence "GGTA"
    count $sequence "GGTATT"
    count $sequence "GGTATTTTAATT"
    count $sequence "GGTATTTTAATTTATAGT"
}

main
#!/usr/bin/tclsh
# $Id: lists.tcl,v 1.1 2004-05-19 18:10:25 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from Kristoffer Lawson
# Modified by Tom Wilkason

set SIZE 10000

proc K {a b} {set a}

proc ldelete {listName index} {
    upvar $listName list
    ;# Replace a deletion with null, much faster
    set list [lreplace [K $list [set list {}]] $index $index]
}

proc lreverse {_list} {
    upvar $_list List
    for {set i [expr {[llength $List] - 1}]} {$i >= 0} {incr i -1} {
	lappend Li1r [lindex $List $i]
    }
    set List $Li1r
    unset Li1r
}

proc test_lists {args} {
    # create a list of integers (Li1) from 1 to SIZE
    for {set i 1} {$i <= $::SIZE} {incr i} {lappend Li1 $i}
    # copy the list to Li2 (not by individual items)
    set Li2 $Li1
    # remove each individual item from left side of Li2 and
    # append to right side of Li3 (preserving order)
    lreverse Li2
    foreach {item} $Li2 {
	lappend Li3 [lindex $Li2 end]
	ldelete Li2 end
    }
    # Li2 must now be empty
    # remove each individual item from right side of Li3 and
    # append to right side of Li2 (reversing list)
    foreach {item} $Li3 {
	lappend Li2 [lindex $Li3 end]
	ldelete Li3 end
    }
    # Li3 must now be empty
    # reverse Li1 in place
    lreverse Li1
    # check that first item is now SIZE
    if {[lindex $Li1 0] != $::SIZE} {
	return "fail size [lindex $Li1 0]"
    }
    # compare Li1 and Li2 for equality
    # and return length of the list
    if {$Li1 == $Li2} {
	return [llength $Li1]
    } else {
	return "fail compare"
    }
}

proc main {args} {
    global argv
    set NUM [lindex $argv 0]
    if {$NUM < 1} {
	set NUM 1
    }
    while {$NUM > 0} {
	set result [test_lists]
	incr NUM -1
    }
    puts $result
}

main
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Stephane Arnold

proc main {W} {
    set H $W

    puts stdout "P4\n$W $H"
	fconfigure stdout -translation binary
	set iter 50
	set limit2 4.0
	set wscale [expr {2./$W}]
	set hscale [expr {2./$H}]
	set offset [expr {$W%8}]
    for {set y 0} {$y < $H} {incr y} {
        set Ci [expr {$hscale* double($y) -1.0}]
        for {set xb 0} {$xb < $W} {incr xb 8} {
			set bits 0
			set xbb [expr {$xb+8 > $W ? $W : $xb+8}]
			for {set x $xb} {$x<$xbb} {incr x} {
			  set bits [expr {$bits<<1}]
			   set Zr [set Zi 0.0]
            set Zrq [set Ziq 0.0]
            set Cr [expr {$wscale * double($x)- 1.5}]
            for {set i 0} {$i<$iter} {incr i} {
               set Zri [expr {$Zr*$Zi}]
               set Zr [expr { $Zrq - $Ziq + $Cr }]
               set Zi [expr { $Zri + $Zri + $Ci }]
               set Zrq [expr {$Zr*$Zr}]
               set Ziq [expr {$Zi*$Zi}]
               if {$Zrq + $Ziq > $limit2} {
                  incr bits
                  break
               }
            }
         }
         if {$xb+7>=$W} {set bits [expr {(($bits+1)<<$offset)-1}]}
         puts -nonewline stdout [binary format c [expr {255-$bits}]]
        }
    }
}
eval main $argv
#!/usr/bin/tclsh
# $Id: mandelbrot.tcl-3.tcl,v 1.1 2005-06-16 01:07:01 greg-guest Exp $
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Hemang Lavana

proc main {argv} {
    set bit_num  [set byte_acc 0]
    set H [set W [lindex $argv 0]]

    puts stdout "P4\n$W $H"
    for {set y 0; set iter 50; set limit2 4.0; set W_1 [expr {$W-1}]; set Wshift [expr {8-$W%8}];} {$y < $H} {incr y} {
        set Ci [expr {2.0 * $y / $H - 1}]
        for {set x 0} {$x < $W} {incr x} {
            set Zr [set Zi 0.0]
            set Cr [expr {2.0 * $x / $W - 1.5}]
            for {set i 0} {$i < $iter} {incr i} {
                set Tr [expr { $Zr * $Zr - $Zi * $Zi + $Cr }]
                set Ti [expr { 2.0 * $Zr * $Zi + $Ci }]
                set Zr $Tr; set Zi $Ti
                if {[set isOverLimit [expr {($Zr * $Zr + $Zi * $Zi) > $limit2}]]} {break}
            }
            incr bit_num
            set byte_acc [expr {2 * $byte_acc + ($isOverLimit? 0 : 1)}]
            if {$bit_num == "8"} {
                puts -nonewline stdout [binary format c $byte_acc]
                set bit_num [set byte_acc 0]
            } elseif {$x == $W_1} {
                set byte_acc [expr { $byte_acc << $Wshift }]
                puts -nonewline stdout [binary format c $byte_acc]
                set bit_num [set byte_acc 0]
            }
        }
    }
}

main $argv
#!/usr/bin/tclsh
# $Id: matrix.tcl,v 1.2 2005-03-31 14:42:10 sgeard-guest Exp $
# http://www.bagley.org/~doug/shootout/

# This program based on the original from:
# "The What, Why, Who, and Where of Python" By Aaron R. Watters
# http://www.networkcomputing.com/unixworld/tutorial/005/005.html

# modified to avoid matrix size checks
# --Doug

# additional speedups by Kristoffer Lawson and Miguel Sofer

set size 30

proc mkmatrix {rows cols} {
    set count 0
    for {set i 0} {$i < $rows} {incr i} {
	set row {}
	for {set j 0} {$j < $cols} {incr j} {lappend row [incr count]}
	lappend mx $row
    }
    return $mx
}

proc mmult {m1 m2} {
    set cols [lindex $m1 0]
    foreach row1 $m1 {
	foreach {row i} {{} 0} break
	foreach - $cols {
	    set elem 0
	    foreach elem1 $row1 row2 $m2 {
		incr elem [expr {$elem1 * [lindex $row2 $i]}]
	    }
	    lappend row $elem
	    incr i
	}
	lappend result $row
    }
    return $result
}

proc main {n} {
    set m1 [mkmatrix $::size $::size]
    set m2 [mkmatrix $::size $::size]
    while {[incr n -1] > -1} {set m [mmult $m1 $m2]}

    puts "[lindex $m 0 0] [lindex $m 2 3] [lindex $m 3 2] [lindex $m 4 4]"
}

set N [lindex $argv 0]
if {$N < 1} {set N 1}
main $N
## The Computer Language Benchmarks Game
## http://shootout.alioth.debian.org/
## contributed by Mark Smithfield, 
## tiny modification by Andrew McParland

proc bump { next_thread msg } {
	if { $next_thread == 0  } { expr {$msg} } else {
		$next_thread eval [list bump [expr {$msg+1}]]
	}
}
set N [lindex $argv 0]
interp recursionlimit {} 1024
for {set i 0} {$i < 500} {incr i} {
	interp create -safe thread-$i
	interp alias thread-$i bump {} bump thread-[expr {$i+1}]
}
interp alias thread-[expr {$i-1}] bump {} bump 0
set cc 0
for {set i 0} {$i < $N} {incr i} {incr cc [bump thread-0 0]}
puts $cc
#!/usr/bin/tclsh
# $Id: methcall.tcl,v 1.1 2005-04-11 18:34:14 igouy-guest Exp $
# http://shootout.alioth.debian.org/
#
# Contributed by Hemang Lavana

package require XOTcl

::xotcl::Class Toggle
Toggle instproc init {start_state} {
    ::xotcl::my instvar state
    set state $start_state
}
Toggle instproc value {} {
    ::xotcl::my instvar state
    return $state
}
Toggle instproc activate {} {
    ::xotcl::my instvar state
    set state [expr {!$state}]
    return [::xotcl::self]
}

::xotcl::Class NthToggle -superclass Toggle
NthToggle instproc init {start_state max_counter} {
    ::xotcl::next $start_state
    ::xotcl::my instvar counter count_max
    set counter 0
    set count_max $max_counter
}
NthToggle instproc activate {} {
    ::xotcl::my instvar state counter count_max
    incr counter 1
    if {$counter >= $count_max} {
        set state [expr {!$state}]
        set counter 0
    }
    return [::xotcl::self]
}

proc print_boolean {value} {
    if {$value} {
        puts "true"
    } else {
        puts "false"
    }
}

proc main {argv} {
    set n  [lindex $argv 0]

    Toggle toggle TRUE
    for {set i 0} {$i<$n} {incr i} {
        set value [[toggle activate] value]
    }
    print_boolean $value

    NthToggle ntoggle TRUE 3
    for {set i 0} {$i<$n} {incr i} {
        set value [[ntoggle activate] value]
    }
    print_boolean $value
}
main $argv
#!/usr/bin/tclsh
# $Id: moments.tcl,v 1.4 2005-03-31 14:51:07 sgeard-guest Exp $
# http://www.bagley.org/~doug/shootout/

proc main {} {
    foreach {sum average_deviation variance skew kurtosis} {0 0 0 0 0} break

    set nums [read stdin]
    foreach num $nums { incr sum $num }
    set n [llength $nums]
    set mean [expr {double($sum) / $n}]

    foreach num $nums {
	set deviation [expr {$num - $mean}]
	set dev2 [expr {$deviation * $deviation}]
	set dev3 [expr {$dev2 * $deviation}]
	set dev4 [expr {$dev3 * $deviation}]
	set average_deviation [expr {$average_deviation + abs($deviation)}]
	set variance [expr {$variance + $dev2}]
	set skew [expr {$skew + $dev3}]
	set kurtosis [expr {$kurtosis + $dev4}]
    }

    set average_deviation [expr {$average_deviation / $n}]
    set variance [expr {$variance / ($n - 1)}]
    set standard_deviation [expr {sqrt($variance)}]

    if {$variance} {
	set skew [expr {$skew / ($n * $variance * $standard_deviation)}]
	set kurtosis [expr {$kurtosis / ($n * $variance * $variance) - 3}]
    }

    set nums [lsort -integer $nums]
    set mid [expr {int($n / 2)}]
    if [expr {$n % 2}] {
	set median [lindex $nums $mid]
    } else {
	set a [lindex $nums $mid]
	set b [lindex $nums [incr mid -1]]
	set median [expr {double($a + $b) / 2}]
    }

    puts [format "n:                  %d" $n]
    puts [format "median:             %f" $median]
    puts [format "mean:               %f" $mean]
    puts [format "average_deviation:  %f" $average_deviation]
    puts [format "standard_deviation: %f" $standard_deviation]
    puts [format "variance:           %f" $variance]
    puts [format "skew:               %f" $skew]
    puts [format "kurtosis:           %f" $kurtosis]
}

main
#!/usr/bin/tclsh
# $Id: nbody.tcl-2.tcl,v 1.1 2005-04-18 05:34:15 igouy-guest Exp $
#
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Daniel South
# Modified by Hemang Lavana

set PI 3.141592653589793
set SOLAR_MASS [expr {4 * $PI * $PI}]
set DAYS_PER_YEAR 365.24

proc init {body var_values} {
    global x y z vx vy vz mass
    foreach {var value} $var_values {lappend $var $value}
}

init Sun "x 0 y 0 z 0 vx 0 vy 0 vz 0 mass $SOLAR_MASS"

init Jupiter "x    4.84143144246472090e+00"
init Jupiter "y    -1.16032004402742839e+00"
init Jupiter "z    -1.03622044471123109e-01"
init Jupiter "vx   [expr {1.66007664274403694e-03  * $DAYS_PER_YEAR}]"
init Jupiter "vy   [expr {7.69901118419740425e-03  * $DAYS_PER_YEAR}]"
init Jupiter "vz   [expr {-6.90460016972063023e-05 * $DAYS_PER_YEAR}]"
init Jupiter "mass [expr {9.54791938424326609e-04  * $SOLAR_MASS}]"

init Saturn "x    8.34336671824457987e+00"
init Saturn "y    4.12479856412430479e+00"
init Saturn "z    -4.03523417114321381e-01"
init Saturn "vx   [expr {-2.76742510726862411e-03 * $DAYS_PER_YEAR}]"
init Saturn "vy   [expr {4.99852801234917238e-03  * $DAYS_PER_YEAR}]"
init Saturn "vz   [expr {2.30417297573763929e-05  * $DAYS_PER_YEAR}]"
init Saturn "mass [expr {2.85885980666130812e-04  * $SOLAR_MASS}]"

init Uranus "x    1.28943695621391310e+01"
init Uranus "y    -1.51111514016986312e+01"
init Uranus "z    -2.23307578892655734e-01"
init Uranus "vx   [expr {2.96460137564761618e-03  * $DAYS_PER_YEAR}]"
init Uranus "vy   [expr {2.37847173959480950e-03  * $DAYS_PER_YEAR}]"
init Uranus "vz   [expr {-2.96589568540237556e-05 * $DAYS_PER_YEAR}]"
init Uranus "mass [expr {4.36624404335156298e-05  * $SOLAR_MASS}]"

init Neptune "x    1.53796971148509165e+01"
init Neptune "y    -2.59193146099879641e+01"
init Neptune "z    1.79258772950371181e-01"
init Neptune "vx   [expr {2.68067772490389322e-03  * $DAYS_PER_YEAR}]"
init Neptune "vy   [expr {1.62824170038242295e-03  * $DAYS_PER_YEAR}]"
init Neptune "vz   [expr {-9.51592254519715870e-05 * $DAYS_PER_YEAR}]"
init Neptune "mass [expr {5.15138902046611451e-05  * $SOLAR_MASS}]"


proc advance {b dt} {
    global x y z vx vy vz mass

    for {set i 0; set n [llength $b]} {$i < $n} {incr i} {
        for {set j [expr {$i+1}]} {$j < $n} {incr j} {
            set dx [expr {[lindex $x $i] - [lindex $x $j]}]
            set dy [expr {[lindex $y $i] - [lindex $y $j]}]
            set dz [expr {[lindex $z $i] - [lindex $z $j]}]

            set d [expr {sqrt($dx * $dx + $dy * $dy + $dz * $dz)}]
            set mag [expr {$dt / ($d * $d * $d)}]
            set magmult1 [expr {[lindex $mass $j] * $mag}]
            set magmult2 [expr {[lindex $mass $i] * $mag}]

            lset vx $i [expr {[lindex $vx $i] - ($dx * $magmult1)}]
            lset vy $i [expr {[lindex $vy $i] - ($dy * $magmult1)}]
            lset vz $i [expr {[lindex $vz $i] - ($dz * $magmult1)}]

            lset vx $j [expr {[lindex $vx $j] + ($dx * $magmult2)}]
            lset vy $j [expr {[lindex $vy $j] + ($dy * $magmult2)}]
            lset vz $j [expr {[lindex $vz $j] + ($dz * $magmult2)}]
        }
    }

    for {set i 0; set n [llength $b]} {$i < $n} {incr i} {
        lset x $i [expr {[lindex $x $i] + ($dt * [lindex $vx $i])}]
        lset y $i [expr {[lindex $y $i] + ($dt * [lindex $vy $i])}]
        lset z $i [expr {[lindex $z $i] + ($dt * [lindex $vz $i])}]
    }
}


proc energy {b} {
    global x y z vx vy vz mass
    set e 0

    for {set i 0; set n [llength $b]} {$i < $n} {incr i} {
        set e [expr {$e + (0.5 * [lindex $mass $i] * (     \
                     ([lindex $vx $i] * [lindex $vx $i]) + \
                     ([lindex $vy $i] * [lindex $vy $i]) + \
                     ([lindex $vz $i] * [lindex $vz $i]) ))}]

        for {set j [expr {$i+1}]} {$j < $n} {incr j} {
            set dx [expr {[lindex $x $i] - [lindex $x $j]}]
            set dy [expr {[lindex $y $i] - [lindex $y $j]}]
            set dz [expr {[lindex $z $i] - [lindex $z $j]}]

            set d [expr {sqrt($dx * $dx + $dy * $dy + $dz * $dz)}]
            set e [expr {$e - (([lindex $mass $i] * [lindex $mass $j]) / $d)}]
      }
   }
   return $e
}


proc offsetMomentum {b} {
    global x y z vx vy vz mass SOLAR_MASS
    foreach {px py pz} {0 0 0} break

    for {set i 0; set n [llength $b]} {$i < $n} {incr i} {
        set px [expr {$px + [lindex $vx $i] * [lindex $mass $i]}]
        set py [expr {$py + [lindex $vy $i] * [lindex $mass $i]}]
        set pz [expr {$pz + [lindex $vz $i] * [lindex $mass $i]}]
    }
    set i [lsearch -exact $b Sun]
    lset vx $i [expr {-$px / $SOLAR_MASS}]
    lset vy $i [expr {-$py / $SOLAR_MASS}]
    lset vz $i [expr {-$pz / $SOLAR_MASS}]
}


proc main {n} {
    if {$n eq "" || $n < 1} {set n 1000}
    set bodyNames "Sun Jupiter Saturn Uranus Neptune"

    offsetMomentum $bodyNames
    puts [format "%0.9f" [energy $bodyNames]]

    for {set i 0} {$i < $n} {incr i} {advance $bodyNames 0.01}
    puts [format "%0.9f" [energy $bodyNames]]
}

main [lindex $argv 0]
#!/usr/bin/tclsh
# $Id: nestedloop.tcl,v 1.1 2004-05-19 18:10:57 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from Tom Wilkason

proc main {} {
    global argv
    set n [lindex $argv 0]
    set x 0
    incr n 1
    set a $n
    while {[incr a -1]} {
	set b $n
	while {[incr b -1]} {
	    set c $n
	    while {[incr c -1]} {
		set d $n
		while {[incr d -1]} {
		    set e $n
		    while {[incr e -1]} {
			set f $n
			while {[incr f -1]} {
			    incr x
			}
		    }
		}
	    }
	}
    }
    puts $x
}

main
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by Robert Seeger and Simon Geard


package require Tcl 8.4

proc main {n} {
    foreach value [list $n [incr n -1] [incr n -1]] {
        set num [expr { int(pow(2, $value) * 10000) }]
        puts [format "Primes up to %8d %8d" $num [nsieve $num]]
    }
}

proc nsieve {n} {
    set data {}
    for {set i 0} {$i <= $n} {incr i} {
        lappend data 1
    }

    set count 0
    for {set i 2} {$i <= $n} {incr i} {
        if { [lindex $data $i] } {
            for {set j [expr {$i + $i}]} {$j <= $n} {incr j $i} {
                lset data $j 0
            }
            incr count
        }
    }
    
    return $count
}

main [lindex $argv 0]
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Hemang Lavana, 
# small modification by Andrew McParland

proc nsieve {m} {
    set NBITS 32

    set init_val [expr {0xffffffff}]
    set max [expr {$m / $NBITS}]
    for {set i 0} {$i < $max} {incr i} {lappend data $init_val}

    for {set count 0; set i 2} {$i < $m} {incr i} {
	if {[lindex $data [expr {$i / $NBITS}]] & (1 << $i % $NBITS)} {
            for {set j [expr {$i + $i}]} {$j < $m} {incr j $i} {
                set j_idx [expr {$j / $NBITS}]
	        lset data $j_idx [expr {[lindex $data $j_idx] & ~(1 << $j % $NBITS)}]
            }
	    incr count
        }
    }
    return $count
}

proc main {n} {

    if {[llength $n] > 1 || $n < 2} {
        puts stderr "usage: [file tail $::argv0] N ;#N >= 2, specified value of N = $n"
        exit 2
    }
    foreach value [list $n [incr n -1] [incr n -1]] {
        set m [expr {(1 << $value) * 10000}]
        set count [nsieve $m]
        puts [format "Primes up to %8u %8u" $m $count]
    }
}
main $argv
#!/usr/bin/tcl
# The Great Computer Language Shootout
#    http://shootout.alioth.debian.org/
#    contributed by Yahalom emet

#    tcl 1500000

package require Itcl
itcl::class Toggle {
    protected variable _state;
    constructor {startState} { set _state $startState; }
    public method value {} { return $_state; }
    public method activate {} { set _state [expr {!$_state}]; }
}


itcl::class NthToggle  {
    inherit Toggle
    protected variable _countMax;
    protected variable _count;

    constructor {startState max} {Toggle::constructor $startState} {
	set _countMax  $max;
	set _count  0;
    }

    public method activate {} {
	incr _count;
	if {$_count >= $_countMax} {
	    set _state  [expr {!$_state}]
	    set _count  0;
	}
    }
}

set n  [lindex $argv 0]

Toggle toggle TRUE;
for {set i 0} {$i<5} {incr i} {
    toggle activate;
    if {[toggle value]} {
	puts "true"
    } else {
	puts "false";
    }
}
puts ""

for {set i 0} {$i<$n} {incr i} {
    Toggle toggle$i 0
}

NthToggle ntoggle TRUE 3;
for {set i 0} {$i<8} {incr i} {
    ntoggle activate;
    if {[ntoggle value]} {
	puts "true"
    } else {
	puts "false";
    }
}

for {set i 0} {$i<$n} {incr i} {
    NthToggle ntoggle$i TRUE 3
}
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# Contributed by Andrew McParland, based on the C# entry

proc compute {n} {
    foreach var [list a1 a2 a3 a4 a5 a6 a7 a8 a9] {
        set $var 0.0
    }
    set alt -1.0
    set twothirds [expr {2.0/3.0}]
    
    for {set k 1} {$k <= $n} {incr k} {
        set k2 [expr { pow($k,2) }]
        set k3 [expr { $k2*$k }]
        set sk [expr { sin($k) }]
        set ck [expr { cos($k) }]
        set alt [expr { -$alt }]

        set a1 [expr { $a1 + pow($twothirds,$k-1.0) }]
        set a2 [expr { $a2 + pow($k,-0.5) }]
        set a3 [expr { $a3 + 1.0/($k*($k+1.0)) }]
        set a4 [expr { $a4 + 1.0/($k3 * $sk*$sk) }]
        set a5 [expr { $a5 + 1.0/($k3 * $ck*$ck) }]
        set a6 [expr { $a6 + 1.0/$k }]
        set a7 [expr { $a7 + 1.0/$k2 }]
        set a8 [expr { $a8 + $alt/$k }]
        set a9 [expr { $a9 + $alt/(2.0*$k-1.0) }]
    }
    puts [format "%.9f\t(2/3)^k" $a1]
    puts [format "%.9f\tk^-0.5" $a2]
    puts [format "%.9f\t1/k(k+1)" $a3]
    puts [format "%.9f\tFlint Hills" $a4]
    puts [format "%.9f\tCookson Hills" $a5]
    puts [format "%.9f\tHarmonic" $a6]
    puts [format "%.9f\tRiemann Zeta" $a7]
    puts [format "%.9f\tAlternating Harmonic" $a8]
    puts [format "%.9f\tGregory" $a9]
}

compute [lindex $argv 0]
## The Computer Lannguage Shootout
## http://shootout.alioth.debian.org/
## contributed by Hemang Lavana
## modified on advice from Mark Janssen

proc compose {aQRST bQRST} {
    foreach {aQ aR aS aT} $aQRST break
    foreach {bQ bR bS bT} $bQRST break
    set rQ [expr {$aQ * $bQ}]
    set rR [expr {$aQ * $bR + $aR * $bT}]
    set rS [expr {$aS * $bQ + $aT * $bS}]
    set rT [expr {$aS * $bR + $aT * $bT}]
    return [list $rQ $rR $rS $rT]
}

proc produce {QRST J} {
    return [compose [list 10 [expr {-10*$J}] 0 1] $QRST]
}

proc extract {QRST J} {
    foreach {Q R S T} $QRST break
    return [expr {($Q * $J + $R) / ($S * $J + $T)}]
}

proc nextX {} {
    global pidigit
    set k [incr pidigit(k)]
    return [list $k [expr {4*$k+2}] 0 [expr {2*$k+1}]]
}

proc nextPidigit {} {
    global pidigit
    set digit [extract $pidigit(z) 3]
    while {$digit != [extract $pidigit(z) 4]} {
        set pidigit(z) [compose $pidigit(z) [nextX]]
        set digit [extract $pidigit(z) 3]
    }
    set pidigit(z) [produce $pidigit(z) $digit]
    return $digit
}

proc newPidigit {} {
    global pidigit
    set pidigit(z) [list 1 0 0 1]
    set pidigit(k) 0
    return
}

proc generatePidigits {n} {
    set pi_digits [newPidigit]
    for {set i 0} {$i < $n} {incr i} {append pi_digits [nextPidigit]}
    return $pi_digits
}

proc main {n} {
    if {$n eq "" || $n < 27} {set n 27}
    set width 10
    set pi_digits [generatePidigits $n]

    set max [expr {$n-$width}]
    set len [expr {$width-1}]
    for {set i 0} {$i < $max} {} {
        puts "[string range $pi_digits $i [incr i $len]]\t:[incr i]"
    }
    if {$i < $n} {
        puts [format "%-${width}s\t:%s" [string range $pi_digits $i $n] $n]
    }
}
main [lindex $argv 0]
#!/usr/bin/tclsh
# $Id: random.tcl,v 1.4 2005-09-29 17:36:19 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/
# from Miguel Sofer 

foreach {IM IA IC last} {139968 3877 29573 42} break

proc make_gen_random {} {
    set params [list IM $::IM IA $::IA IC $::IC]
    set body [string map $params {
	expr {($max * [set ::last [expr {($::last * IA + IC) % IM}]]) / IM}}]
    proc gen_random {max} $body
}

proc main {n} {
    make_gen_random
    while {[incr n -1] > 0} {gen_random 100.0}
    puts [format "%.9f" [gen_random 100.0]]
}

main $argv
# ----------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Michael Schlenker
# modified by Andrew McParland
# ----------------------------------------------------------------------

proc Ack {x y} {
    expr { $x == 0 ? $y+1 : ($y == 0 ? [Ack [expr {$x-1}] 1] :
       [Ack [expr {$x-1}] [Ack $x [expr {$y -1}]]])}
}
proc Fib {n} {
    expr {$n < 2 ? 1 : [Fib [expr {$n -2}]] + [Fib [expr {$n -1}]]}
}
proc FibFP {n} {
    expr {double($n) < 2.0 ? 1.0 : [Fib [expr {$n -2.0}]] + [Fib [expr {$n -1.0}]]}
}
proc Tak {x y z} {
    expr { $y < $x ? ([Tak [Tak [expr {$x-1}] $y $z] [Tak [expr {$y-1}] $z $x] \
       [Tak [expr {$z-1}] $x $y]]) : $z }
}
proc TakFP {x y z} {
    expr { double($y) < double($x) ? double([Tak [Tak [expr {$x-1.0}] $y $z] \
       [Tak [expr {$y-1}] $z $x] [Tak [expr {$z-1}] $x $y]]) : double($z) }
}

proc main {argv} {
    set n [lindex $argv 0]
    incr n -1
    set n1 [expr {$n+1}]
    set n28 [expr {28.0+$n}]
    set n3 [expr {$n*3}]
    set n2 [expr {$n*2}]
    puts [format "Ack(3,%d): %d" $n1 [Ack 3 $n1]]
    puts [format "Fib(%.1f): %.1f" $n28 [FibFP $n28]]
    puts [format "Tak(%d,%d,%d): %d" $n3 $n2 $n [Tak $n3 $n2 $n]]
    puts [format "Fib(3): %d" [Fib 3 ]]
    puts [format "Tak(3.0,2.0,1.0): %.1f" [TakFP 3.0 2.0 1.0]]
    return 0;
}

interp recursionlimit {} 100000
main $argv
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# contributed by Heiner Marxen, modified for regexp by Andrew McParland

proc regex-dna {} {
    set seq [read stdin]
    set ilen    [string length $seq]

    regsub -all -line {^>.*\n|\n} $seq {} seq
    set clen    [string length $seq]

    foreach pat [list   {agggtaaa|tttaccct}     \
            {[cgt]gggtaaa|tttaccc[acg]} \
            {a[act]ggtaaa|tttacc[agt]t} \
            {ag[act]gtaaa|tttac[agt]ct} \
            {agg[act]taaa|ttta[agt]cct} \
            {aggg[acg]aaa|ttt[cgt]ccct} \
            {agggt[cgt]aa|tt[acg]accct} \
            {agggta[cgt]a|t[acg]taccct} \
            {agggtaa[cgt]|[acg]ttaccct} \
        ] {
    set cnt [regexp -all -nocase -- $pat $seq]
    puts "$pat $cnt"
    }

    lappend map B {(c|g|t)} D {(a|g|t)} H {(a|c|t)} K {(g|t)}
    lappend map M {(a|c)}   N {(a|c|g|t)}   R {(a|g)}   S {(c|g)}
    lappend map V {(a|c|g)} W {(a|t)}   Y {(c|t)}

    foreach {in out} $map {
        regsub -all $in $seq $out seq
    }

    puts {}
    puts $ilen
    puts $clen
    puts [string length $seq]

    return 0
}

regex-dna
#!/usr/bin/tclsh
# $Id: regexmatch.tcl,v 1.2 2005-03-31 15:25:14 sgeard-guest Exp $
# http://www.bagley.org/~doug/shootout/
# from: Miguel Sofer, with modifications by Kristoffer Lawson

proc main {n} {
    set data [read stdin]
    set count 0
    set rExp {(?:^|[^\d(])(\(\d{3}\)|\d{3}) (\d{3}[ -]\d{4})(?:$|[^\d])}

    while {[incr n -1] > -1} {
	foreach {-- area num} [regexp -all -line -inline $rExp $data] {
	    set pnum "([string trim $area () ]) [string map {" " -} $num]"
	    if {!$n} { puts "[incr count]: $pnum" }
	}
    }
}

set N [lindex $argv 0]
if {$N < 1} {set N 1}
main $argv
#!/usr/bin/tclsh
# $Id: revcomp.tcl,v 1.3 2005-09-29 17:36:19 igouy-guest Exp $
# http://shootout.alioth.debian.org/
#
# reverse-complement benchmark for shootout.alioth.debian.org 
#
# contributed by Michael Schlenker <mic42@users.sourceforge.net>
#
proc main {} {
    set acc ""
    while {[gets stdin line] != -1} {
	if {[string match ">*" $line]} {
	    if {[string length $acc]} {
		put_reverse_fasta $head $acc
		set acc ""
	    }
	    set head $line
	} else {
	    append acc $line
	}
    }
    put_reverse_fasta $head $acc
}

proc put_reverse_fasta {head body} {
    set l [string length $body]
    set body [string map {A T a T C G c G G C g C T A t A U A u A M K m K R Y \
	    r Y Y R y R K M k M V B v B H D h D D H d H B V b V} $body]
    while {$l} {append out [string index $body [incr l -1]]}
    incr l -1
    set body $head
    while {[incr l] < [string length $out]} {
	append body \n[string range $out $l [incr l 59]]
    }
    puts $body
}

main
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by Michael Schlenker
# optimized by Peter Niemayer
 
proc main {} {
  fconfigure stdout -buffering full -buffersize 16384 -translation binary
  set acc ""
  while {[gets stdin line] != -1} {
     if {[string index $line 0] == ">"} {
       if {[string length $acc]} {
          put_reverse_fasta $head $acc
          set acc ""
       }
       set head $line
     } else {
       append acc $line
     }
  }
  put_reverse_fasta $head $acc
}

set map {A T a T C G c G G C g C T A t A U A u A M K m K R Y \
 r Y Y R y R K M k M V B v B H D h D D H d H B V b V}

proc put_reverse_fasta {head body} {
  global map
  set out [string reverse [string map $map $body]]
  set n [string length $out]
  puts $head
  for {set l -1} {[incr l] < $n} {} {
    puts [string range $out $l [incr l 59]]
  }
}

main
#!/usr/bin/tclsh
# $Id: reversefile.tcl,v 1.1 2004-05-19 18:12:19 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from: Miguel Sofer

proc main {} {
    set lines [split [read stdin] "\n"]
    
    fconfigure stdout -buffering full

    for {set i [expr {[llength $lines]-2}]} {$i >= 0} {incr i -1} {
        puts [lindex $lines $i]
    }
}

main
#!/usr/bin/tclsh
# $Id: sieve.tcl,v 1.1 2004-05-19 18:12:28 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from: Kristoffer Lawson

proc sieve {num} {
    while {$num > 0} {
	incr num -1
	set count 0
	for {set i 2} {$i <= 8192} {incr i 1} {
	    set flags($i) 1
	}
	for {set i 2} {$i <= 8192} {incr i 1} {
	    if {$flags($i) == 1} {
		# remove all multiples of prime: i
		for {set k [expr {$i+$i}]} {$k <= 8192} {incr k $i} {
		    set flags($k) 0
		}
		incr count 1
	    }
	}
    }
    return $count
}

set NUM [lindex $argv 0]
if {$NUM < 1} {
    set NUM 1
}

set count [sieve $NUM]
puts "Count: $count"
#!/usr/bin/tclsh
##
## The Computer Lannguage Shootout
## http://shootout.alioth.debian.org/
## Contributed by Heiner Marxen
##
## "spectral-norm"	for Tcl
## Call:	tclsh spectral-norm.tcl N
##
## $Id: spectralnorm.tcl,v 1.1 2005-12-08 03:02:41 igouy-guest Exp $

proc A {i j} {
    return [expr {1.0 / (($i+$j)*($i+$j+1)/2 + $i+1)}]
}

set mulBody {
    set r [list]
    for {set i 0} {$i < $n} {incr i} {
	set sum 0.0
	for {set j 0} {$j < $n} {incr j} {
	    set sum [expr {$sum + [A_i_j] * [lindex $v $j]}]
	}
	lappend r $sum
    }
    return $r
}

proc mulAv  {n v} [string map [list A_i_j {A $i $j}] $mulBody]
proc mulAtv {n v} [string map [list A_i_j {A $j $i}] $mulBody]

proc mulAtAv {n v} {
    return [mulAtv $n [mulAv $n $v]]
}

proc approximate {n} {
    for {set i 0} {$i < $n} {incr i} {
	lappend u 1.0
    }

    for {set i 0} {$i < 10} {incr i} {
	set v [mulAtAv $n $u]
	set u [mulAtAv $n $v]
    }

    set vBv 0.0
    set vv  0.0
    for {set i 0} {$i < $n} {incr i} {
	set vi  [lindex $v $i]
	set vBv [expr {$vBv + $vi * [lindex $u $i]}]
	set vv  [expr {$vv  + $vi * $vi           }]
    }
    return [expr {sqrt( $vBv / $vv )}]
}

proc main {argv} {
    set n 100
    if {[llength $argv]} {set n [lindex $argv 0]}

    puts [format "%.9f" [approximate $n]]

    return 0
}

main $argv
#!/usr/bin/tclsh
# $Id: spellcheck.tcl,v 1.2 2005-03-31 15:39:55 sgeard-guest Exp $
# http://www.bagley.org/~doug/shootout/
# from: Miguel Sofer
# some modifications suggested by Kristoffer Lawson

proc main {} {
    set 1 [open "Usr.Dict.Words" r]
    foreach 2 [read $1 [file size "Usr.Dict.Words"]] {set $2 1}
    close $1

    fconfigure stdout -buffering full
    while {[gets stdin 1] >= 0} {if {[catch {set $1}]} {puts $1}}
}

main
#!/usr/bin/tclsh
# $Id: strcat.tcl,v 1.1 2004-05-19 18:13:36 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from: Kristoffer Lawson

proc main {n} {
    incr n
    while {[incr n -1]} {
        append str "hello\n"
    }
    puts [string length $str]
}

main [lindex $argv 0]
#!/usr/bin/tclsh
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# contributed by Donald Arseneau

proc main {} {
    set sum 0
    catch {
        while {1} { incr sum [gets stdin] }
    }
    puts $sum
}

main
#!/usr/bin/tclsh
#
#  The Great Computer Language Shootout
#  http://shootout.alioth.debian.org/
#
#  Contributed by David Jones

proc tak {x y z} {
    if {$y >= $x} {return $z}
    return [tak [tak [expr {$x - 1.0}] $y $z] [tak [expr {$y - 1.0}] $z $x] [tak [expr {$z - 1.0}] $x $y]]
}

set N [lindex $argv 0]
if {$N < 1} {set N 1}
puts [tak [expr {3.0 * $N}] [expr {2.0 * $N}] [expr {1.0 * $N}]]
#!/usr/bin/tclsh
# $Id: tcpecho.tcl,v 1.1 2005-03-18 06:26:26 bfulgham Exp $
# http://shootout.alioth.debian.org/
#
# Contributed by Robert Seeger and Randy Melton
proc Server {channel clientaddr clientport} {
    set reply [string repeat x 64]
    fconfigure $channel -buffersize 64 -encoding binary

    while { ![eof $channel] } {
        read $channel 64
        puts -nonewline $channel $reply
    }
    set ::forever 1
}

proc doChild {num} {
    set request [string repeat x 64]

    set bytes 0
    set replies 0

    set fd [socket localhost 9900]
    fconfigure $fd -buffersize 64 -encoding binary

    set num [expr {$num * 6400}]
    while { [incr num -1] >= 0 } {
        puts -nonewline $fd $request
        incr bytes [string length [read $fd 64]]
        incr replies
    }

    close $fd
    puts "replies: $replies\tbytes: $bytes"
    exit
}

if { [llength $argv] == 2} {
    doChild [lindex $argv 1]
} else {
    socket -server Server 9900

    exec [info script] Child [lindex $argv 0] &
    vwait ::forever

    exit
}

#!/usr/bin/tclsh
# $Id: tcprequest.tcl,v 1.2 2005-03-21 02:51:18 bfulgham Exp $
# http://shootout.alioth.debian.org/
#
# Contributed by Robert Seeger and Randy Melton
proc Server {channel clientaddr clientport} {
    set reply [string repeat x 4096]
    fconfigure $channel -buffersize 4096 -encoding binary

    while { ![eof $channel] } {
        read $channel 64
        puts -nonewline $channel $reply
    }
    set ::forever 1
}

proc doChild {num} {
    set request [string repeat x 64]

    set bytes 0
    set replies 0

    set fd [socket localhost 9900]
    fconfigure $fd -buffersize 64 -encoding binary

    set num [expr {$num * 100}]
    while { [incr num -1] >= 0 } {
        puts -nonewline $fd $request
        incr bytes [string length [read $fd 4096]]
        incr replies
    }

    close $fd
    puts "replies: $replies\tbytes: $bytes"
    exit
}

if { [llength $argv] == 2} {
    doChild [lindex $argv 1]
} else {
    socket -server Server 9900

    exec [info script] Child [lindex $argv 0] &
    vwait ::forever

    exit
}

#!/usr/bin/tclsh
##
## The Computer Lannguage Shootout
## http://shootout.alioth.debian.org/
## Contributed by Heiner Marxen
##
## "tcpsocket"	for Tcl
## Call:	stage /path/to/tcpsocket.tcl N
##
## $Id: tcpsocket.tcl,v 1.1 2005-12-15 03:27:42 igouy-guest Exp $

set PORT	11000
set BUFSIZ	1024
set REQSIZ	64

set repCSlist	[list 2 64  7 4096  1 409600]	;# pairs of Count and Size

proc fdConfig {fd} {
    fconfigure $fd -buffersize $::BUFSIZ -encoding binary -translation binary
}

proc doClient {n} {
    set fd {}
    for {set i 0} {$i < 3} {incr i} {
	if {![catch {set fd [socket localhost $::PORT]}]} break
	puts stderr ">>Client: retry..."
	after 100
    }
    fdConfig $fd

    set request [string repeat x $::REQSIZ]
    set replies 0
    set bytes   0.0		;# shall be summed as FP value

    foreach {cnt siz} $::repCSlist {
	set requests [expr {$n * $cnt}]
	for {set i 0} {$i < $requests} {incr i} {
	    puts -nonewline $fd $request ; flush $fd
	    set got [string length [read $fd $siz]]
	    incr replies
	    set  bytes   [expr {$bytes + $got}]
	}
    }

    puts "replies: $replies\tbytes: [format {%.0f} $bytes]"
}

proc Server {fd clientaddr clientport} {
    fdConfig $fd

    foreach {cnt siz} $::repCSlist {
	set requests [expr {$::N * $cnt}]
	set reply    [string repeat y $siz]

	for {set i 0} {$i < $requests} {incr i} {
	    set got [string length [read $fd $::REQSIZ]]
	    if {$got < $::REQSIZ} {
		puts stderr ">>Server: short request: $got < $::REQSIZ"
		break
	    }
	    puts -nonewline $fd $reply ; flush $fd
	}
    }

    set ::ready 1
}

proc doServer {n} {
    set ::N $n				;# tell "Server" via global var
    socket -server Server $::PORT
    vwait ::ready
}

proc main {argv} {
    set n 10
    if {[llength $argv]} {set n [lindex $argv 0]}

    if {$n > 0} {
	doClient $n
    } else {
	doServer [expr {abs($n)}]
    }
    return 0
}

main $argv
#!/usr/bin/tclsh
# $Id: tcpstream.tcl,v 1.2 2005-03-21 02:51:18 bfulgham Exp $
# http://shootout.alioth.debian.org/
#
# Contributed by Robert Seeger and Randy Melton
proc Server {channel clientaddr clientport} {
    set reply [string repeat x 409600]
    fconfigure $channel -buffersize 409600 -encoding binary

    while { [string length [read $channel 64]] == 64 } {
        puts -nonewline $channel $reply
    }

    set ::forever 1
}

proc doChild {num} {
    set request [string repeat x 64]

    set bytes 0
    set replies 0

    set fd [socket localhost 9900]
    fconfigure $fd -buffersize 64 -encoding binary

    while { [incr num -1] >= 0 } {
        puts -nonewline $fd $request
        incr bytes [string length [read $fd 409600]]
        incr replies
    }

    close $fd
    puts "replies: $replies\tbytes: $bytes"
    exit
}

if { [llength $argv] == 2} {
    doChild [lindex $argv 1]
} else {
    socket -server Server 9900

    exec [info script] Child [lindex $argv 0] &
    vwait ::forever

    exit
}

# The Computer Lannguage Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Julian Noble

 set ring_size 503
 set N 10000000
 package require Thread
 set script {
    proc run {} {
    	set t -2
    	thread::mutex lock %m%
    	while {$t != -1} {
    		thread::cond wait %c% %m%
    		set t [tsv::incr TOK t -1]
    		thread::cond notify %cnext%
    	}
    	thread::mutex unlock %m%
    	puts stdout "%i%"
    	thread::send -async %main% {set ::done 1}
    	thread::cond destroy %c%
    	return
    }
    %do%
 }
 set t1 [set tnext [thread::create {thread::wait}]]
 set c1 [set c [thread::cond create]]
 set m [thread::mutex create]
 for {set i $ring_size} {$i >1} {incr i -1} {
    set cnext $c
    set c [thread::cond create]
    set tnext [thread::create [string map [list %main% [thread::id] %i% $i %m% $m %c% $c %cnext% $cnext %n% $tnext %do% run] $script]]
 }
 #close the ring
 set script [string map [list %main% [thread::id] %i% 1 %m% $m %c% $c1 %cnext% $c %n% $tnext %do% "thread::send -async [thread::id] {set ::start 1};run"] $script]
 thread::send -async $t1 $script
 vwait ::start
 after 5
 tsv::set TOK t $N
 thread::cond notify $c1
 vwait ::done 
#!/usr/bin/tclsh
# $Id: wc.tcl,v 1.2 2005-03-30 22:22:23 sgeard-guest Exp $
# http://www.bagley.org/~doug/shootout/

# this program is modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

# Modified by Miguel Sofer

proc main {} {
    foreach {nl nc nw inword} {0 0 0 0} break

    while {[set data [read stdin 4096]] != {}} {
	incr nc [string length $data]
	set T1 [split $data "\n\r\t "]
	set T2 [lsearch -all -inline -exact -not $T1 {}]
	if {$inword && ([lindex $T1 0] == {})} {incr nw}
	set inword 0
	if {[llength $T2]} {
	    incr nw [llength $T2]
	    if {[lindex $T1 end] != {}} {
		incr nw -1
		set inword 1
	    }
	}
	incr nl [llength [split $data "\n\r"]]
	incr nl -1
    }
    puts "$nl $nw $nc"
}

main
#!/usr/bin/tclsh
#
# $Id: wordfreq.tcl,v 1.4 2005-06-10 21:22:02 sgeard-guest Exp $
#
# http://shootout.alioth.debian.org/
# with help from: Tom Wilkason and Branko Vesligaj
#
# Speed increase by Roy Terry
proc main {} {

    set punc {\{\}"'\\!@#$%^&*()-_+=|[]:;,.~`?0123456789}
    foreach c [split $punc ""] {lappend map $c " "}
    while {[set data [read stdin 4096]] != {}} {
        if {[gets stdin extra] != -1} {append data $extra}
        set line [string map $map $data]
        foreach word [string tolower $line] {
            if {[catch {incr count($word)}]} {set count($word) 1}
        }
    }
    foreach {word cnt}  [array get count] {
        lappend lines [format "%7d %s" $cnt $word]
    }
    puts [join [lsort -decreasing $lines] "\n"]
}


main
