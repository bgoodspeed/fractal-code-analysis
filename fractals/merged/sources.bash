#!/bin/bash
# $Id: ackermann.bash,v 1.3 2005-05-12 15:08:19 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/
# from Steve Fink

function Ack () {
    if [ $1 -eq 0 ]; then
        Ack=$[ $2 + 1 ]
    elif [ $2 -eq 0 ]; then
        Ack $[ $1 - 1 ] 1
    else
        Ack $1 $[ $2 - 1 ]
        local SubAck=$Ack
        Ack $[ $1 - 1 ] $SubAck
    fi
}

n=${1:-1}
Ack 3 $n
echo "Ack(3,$n): $Ack"
#!/bin/bash
# http://shootout.alioth.debian.org/

# this program is modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages by Brian W. Kernighan and
# Christopher J. Van Wyk.

# this version creates a $cnt vairable for the for loop
# Bash version (samething) by David Pyke JUL-20 

n=${1:-1}

declare -ai X
declare -ai Y
declare -i j
declare -i i
declare -i last

last=$(($n-1));
cnt=

i=0;
while [ $i -le $last  ] ; do
	j=$(($i+1));
	X[$i]=$j;
	cnt="$i $cnt"
	i=$j;
done

k=0;
while [ $k -le 999  ] ; do

	for i in $cnt; do
		Y[$i]=$((${Y[$i]} + ${X[$i]}));
	done;

	k=$(( $k + 1));
done

echo "${Y[0]} ${Y[$last]}"


#!/bin/bash
# $Id: fibo.bash,v 1.3 2005-04-25 19:01:38 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

function fib {
    if [ $1 -lt 2 ] ; then
	echo 1
    else
	echo $[ `fib $[ $1 - 2 ]` + `fib $[ $1 - 1 ]` ]
    fi
}

N=${1:-1}
fib $N
#!/bin/bash
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

function d2x()
{
  printf '%x' ${1}
}

# ------------------------------- #

if [ $# -ne 1 ] ; then exit 1 ; fi
if ! echo "$1" | grep -q '^[[:digit:]]*$' ; then exit 1 ; fi

N=$1 ; C=0

for ((i=1 ; i <= ${N} ; i++)) ; do
  HASH=_`d2x ${i}` ; eval ${HASH}=${i}
done

for ((i=1 ; i <= ${N} ; i++)) ; do
  HASH=_${i} ; eval _HASH=\$$HASH
  if [ -n "${_HASH}" ] ; then let C+=1 ; fi
done

echo ${C}
#!/bin/bash
# $Id: hash2.bash,v 1.2 2005-05-13 16:24:17 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/ 
# from Steve Fink

n=${1:-1}

while [ $n -gt 0 ]; do
    # Initialize the array
    i=0
    while [ $i -lt 10000 ]; do
        eval H1_foo_$i=$i
        i=$[ $i + 1 ]
    done

    # Iterate through the hash values, attempting to keep to the spirit of
    # eg perl's keys %h
    for assign in $(  # Get around the subshell problem
        set | grep -a '^H1_' | cut -d= -f1 | cut -c4- | while read; do
            eval h2=\${H2_${REPLY}:-0}
            eval h1=\$H1_$REPLY
            echo H2_$REPLY=$[ $h2 + $h1 ]
        done
    ); do
        eval $assign
    done

    n=$[ $n - 1 ]
done

echo "$H1_foo_1 $H1_foo_9999 $H2_foo_1 $H2_foo_9999"
#!/bin/bash
# $Id: hello.bash,v 1.1 2004-05-19 18:10:16 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

echo "hello world"
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

BCGEN="/tmp/$USER$$.awk"

# Generate script to perform arbitrary precision calculations
(
cat<<'EOF'
  BEGIN{
    print "sum=0; n=0; median=0; mean=0; avgdev=0; stddev=0; variance=0; skew=0; kurtosis=0;"
  }

  {
    print "data[n++] = " $1 "; sum += " $1 ";"
  }

  END{
    print "define abs(m) {"
    print "if (m < 0) return(-m) else return(m);"
    print "}" 

    print "mid = n / 2; k = mid - 1;"
    print "if (n % 2) median = data[mid] else median = (data[k] + data[mid]) / 2;"

    print "mean = sum / n;"     

    print "for (i = 0; i < n; i++) {"
    print "dev = data[i] - mean;"
    print "avgdev = avgdev + abs(dev);"
    print "variance = variance + dev ^ 2;"
    print "skew = skew + dev ^ 3;"
    print "kurtosis = kurtosis + dev ^ 4;"
    print "}"

    print "avgdev = avgdev / n;"
    print "variance = variance / (n - 1);"
    print "stddev = sqrt(variance);"
    print "if (variance > 0) {"
    print "skew = skew / (n * variance * stddev);"
    print "kurtosis = kurtosis / (n * variance * variance) - 3.0;"
    print "}"

    print "n; median; mean; avgdev; stddev; variance; skew; kurtosis;"
  }
EOF
) > ${BCGEN}

# Compute statistics
SUMMARY=(`sort -n | awk -f ${BCGEN} | bc -l`)

# Display results
printf 'n:                  %d\n' ${SUMMARY[0]}
printf 'median:             %0.6f\n' ${SUMMARY[1]}
printf 'mean:               %0.6f\n' ${SUMMARY[2]}
printf 'average_deviation:  %0.6f\n' ${SUMMARY[3]}
printf 'standard_deviation: %0.6f\n' ${SUMMARY[4]}
printf 'variance:           %0.6f\n' ${SUMMARY[5]}
printf 'skew:               %0.6f\n' ${SUMMARY[6]}
printf 'kurtosis:           %0.6f\n' ${SUMMARY[7]}

# Cleanup
rm -f ${BCGEN}
#!/bin/bash
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

if [ $# -ne 1 ] ; then exit 1 ; fi
if ! echo "$1" | grep -q '^[[:digit:]]*$' ; then exit 1 ; fi

N=$1 ; A=$N ; X=0

while [ $A -gt 0 ] ; do 
  B=$N ; let A-=1
  while [ $B -gt 0 ] ; do 
    C=$N ; let B-=1
    while [ $C -gt 0 ] ; do 
      D=$N ; let C-=1
      while [ $D -gt 0 ] ; do 
        E=$N ; let D-=1
        while [ $E -gt 0 ] ; do 
          F=$N ; let E-=1
          while [ $F -gt 0 ] ; do 
            let X+=1 ; let F-=1
          done
        done
      done
    done
  done
done

echo $X
#!/bin/bash
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

readonly IA=3877 IC=29573 IM=139968

# ------------------------------- #

function gen_seed()
{
  local S=${1} N=${2}

  for ((i=0 ; i < ${N} ; i++)) ; do
    let S=(S*IA+IC)%IM
  done

  echo ${S}
}

function gen_random()
{
  local S=${1} M=${2} PC=$((${3}+1)) PO='%0.'${3}'f\n'

  local R=`echo -e "${S} ${M} \x2A ${PC} k ${IM} / p q" | dc`

  printf ${PO} ${R}
}

# ------------------------------- #

if [ $# -ne 1 ] ; then exit 1 ; fi
if ! echo "$1" | grep -q '^[[:digit:]]*$' ; then exit 1 ; fi

gen_random `gen_seed 42 ${1}` 100 9

#!/bin/bash

# http://www.bagley.org/~doug/shootout/
# from David Pyke
# bash doesnt do floating point :( but we dont need it
# if we do fractional maths

#declare -r A=3877
#declare -r C=29573
#declare -r M=139968

#LAST=42

#function gen_random(){
#	LAST=$(( (($LAST * $A) + $C) % $M ))
#
#	RETVAL=$(( $1 * $LAST / $M ))
#	RETREM=$(( $1 * $LAST % $M )) 
#}

#N=$[ ${1:-1} -1 ]

#while [ $N -gt 0 ]; do
#	gen_random 100
#	N=$[ $N - 1 ]
#done
#	gen_random 100 
echo $RETVAL + $RETREM/$M

#!/bin/bash
# $Id: reversefile.bash,v 1.1 2004-05-19 18:12:18 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from David N. Welton
tac
#!/bin/bash
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

declare -a LINES ; COUNT=1

while read LINES[$COUNT] ; do let COUNT+=1 ; done

let COUNT-=1 ; until [ "$COUNT" -eq "0" ] ; do
  echo ${LINES[$COUNT]} ; let COUNT-=1
done
#!/bin/bash
# $Id: sieve.bash,v 1.2 2005-05-13 16:24:19 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/
# from Steve Fink 

NUM=${1:-1}

while [ $NUM -gt 0 ]; do
    i=2
    while [ $i -le 8192 ]; do
        eval P$i=true
        i=$[ $i + 1 ]
    done

    count=0
    i=2
    while [ $i -le 8192 ]; do
        if eval \$P$i; then
            # remove all multiples of prime: i
            k=$[ $i + $i ]
            while [ $k -le 8192 ]; do
                eval P$k=false
                k=$[ $k + $i ]
            done
            count=$[ $count + 1 ]
        fi
        i=$[ $i + 1 ]
    done

    NUM=$[ $NUM - 1 ]
done

echo Count: $count
#!/bin/bash
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

DICT="Usr.Dict.Words"

while read WORD ; do
  eval ${WORD}=1
done < $DICT

while read WORD ; do
  eval _WORD=\$$WORD ; if [ -z "${_WORD}" ] ; then echo ${WORD} ; fi
done
#!/bin/bash
# $Id: strcat.bash,v 1.2 2005-05-13 16:24:19 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/ 

NUM=$1
str=""
while [ $NUM -gt 0 ] ; do
    str="${str}hello
"
    NUM=$[ $NUM - 1 ]
done
echo ${#str}
#!/bin/bash
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

SUM=0

while read NEXT ; do
  let SUM+=NEXT
done

echo ${SUM}
#!/bin/bash
# $Id: wc.bash,v 1.1 2004-05-19 18:13:51 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# (use echo to collapse whitespace output from "wc")
echo `wc`
#!/bin/bash
# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

readonly PADDING=1

function words() { echo -n "$#" ; }

IFS= ; LINES=0 ; WORDS=0 ; CHARS=0

while read NEXTLINE ; do
  let LINES+=1 ; let WORDS+=`IFS=' ' ; words $NEXTLINE`
  let CHARS=CHARS+${#NEXTLINE}+PADDING
done

echo "${LINES} ${WORDS} ${CHARS}"
#!/bin/bash
# $Id: wordfreq.bash,v 1.1 2004-05-19 18:14:15 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# data comes on stdin
# first tr lowercases all letters
# second tr turns non-alpha into whitespace
# grep removes lines that do not contain alpha chars
# sort the words
# take a count of each uniq word
# display frequencies in descending order
tr A-Z a-z | tr -cs a-z "[\012*]" | grep "[a-z]" | sort | uniq -c | sort -rn
