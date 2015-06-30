# $Id: ackermann.mawk,v 1.1 2004-05-19 18:09:09 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

function ack(m, n) {
    if (m == 0) return( n + 1 );
    if (n == 0) return( ack(m - 1, 1) );
    return( ack(m - 1, ack(m, (n - 1))) );
}

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];
    printf("Ack(3,%d): %d\n", n, ack(3, n));
    exit;
}
# $Id: ary.mawk,v 1.2 2004-05-22 07:25:00 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];

    for (i = 0; i < n; i++)
	x[i] = i + 1
    for (k = 0; k < 1000; k++) {
	for (j = n-1; j >= 0; j--)
	    y[j] += x[j]
    }

    print y[0], y[n-1]
}
# $Id: fibo.mawk,v 1.3 2005-04-25 19:01:38 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

function fib(n) {
    if (n < 2) return(1);
    return(fib(n-2) + fib(n-1));
}

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];
    printf("%d\n", fib(n));
    exit;
}
# $Id: hash.mawk,v 1.1 2004-05-19 18:09:55 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];

    for (i = 1; i <= n; i++)
	x[sprintf("%x", i)] = i
    for (i = n; i > 0; i--)
	if (i in x)
	    c++
    print c
}
# $Id: hash2.mawk,v 1.1 2004-05-19 18:10:02 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];

    for (i=0; i<10000; i++)
	hash1[sprintf("foo_%d", i)] = i
    for (i=0; i<n; i++)
	for (k in hash1)
	    hash2[k] += hash1[k]
    print hash1["foo_1"], hash1["foo_9999"], hash2["foo_1"], hash2["foo_9999"]
}
# $Id: heapsort.mawk,v 1.1 2004-05-19 18:10:10 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

function gen_random(n) { return( (n * (LAST = (LAST * IA + IC) % IM)) / IM ); }

function heapsort (n, ra) {
    l = int(0.5+n/2) + 1
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
        j = l * 2;
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

BEGIN {
    IM = 139968;
    IA = 3877;
    IC = 29573;
    LAST = 42;

    n = (ARGV[1] < 1) ? 1 : ARGV[1];
    ary[0] = 0;
    for (i=1; i<=n; i++) {
	ary[i] = gen_random(1.0);
    }

    heapsort(n, ary);

    printf("%.10f\n", ary[n]);

    exit;
}
# $Id: hello.mawk,v 1.1 2004-05-19 18:10:16 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN { print "hello world" }
# $Id: matrix.mawk,v 1.1 2004-05-19 18:10:34 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

function mkmatrix(mx, rows, cols) {
    count = 1;
    for (i=0; i<rows; i++) {
	for (j=0; j<cols; j++) {
	    mx[i,j] = count++;
	}
    }
}

function mmult(rows, cols, m1, m2, m3) {
    for (i=0; i<rows; i++) {
	for (j=0; j<cols; j++) {
	    val = 0;
	    for (k=0; k<cols; k++) {
		val += m1[i,k] * m2[k,j];
	    }
	    m3[i,j] = val;
	}
    }
}


BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];
    size = 30;
    m1[0,0] = 0;
    m2[0,0] = 0;
    mkmatrix(m1, size, size);
    mkmatrix(m2, size, size);
    mm[0,0] = 0;
    for (i=0; i<n; i++) {
	mmult(size, size, m1, m2, mm);
    }
    printf("%d %d %d %d\n", mm[0,0], mm[2,3], mm[3,2], mm[4,4]);
    exit;
}
# $Id: moments.mawk,v 1.1 2004-05-19 18:10:47 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN {
    delete ARGV;
    sum = 0;
    n = 0;
}

{
    nums[n++] = $1;
    sum += $1;
}

END {
    mean = sum/n;
    for (num in nums) {
	dev = num - mean;
	if (dev > 0) { avg_dev += dev; } else { avg_dev -= dev; }
	vari += dev^2;
	skew += dev^3;
	kurt += dev^4;
    }
    avg_dev /= n;
    vari /= (n - 1);
    std_dev = sqrt(vari);

    if (vari > 0) {
	skew /= (n * vari * std_dev);
	kurt = kurt/(n * vari * vari) - 3.0;
    }

    nums[n] = nums[0];
    heapsort(n, nums);

    mid = int(n/2)+1;
    median = (n % 2) ? nums[mid] : (nums[mid] + nums[mid-1])/2;

    printf("n:                  %d\n", n);
    printf("median:             %f\n", median);
    printf("mean:               %f\n", mean);
    printf("average_deviation:  %f\n", avg_dev);
    printf("standard_deviation: %f\n", std_dev);
    printf("variance:           %f\n", vari);
    printf("skew:               %f\n", skew);
    printf("kurtosis:           %f\n", kurt);
}

function heapsort (n, ra) {
    l = int(0.5+n/2) + 1
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
        j = l * 2;
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
# $Id: nestedloop.mawk,v 1.1 2004-05-19 18:10:56 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];

    for (a=0; a<n; a++)
	for (b=0; b<n; b++)
	    for (c=0; c<n; c++)
		for (d=0; d<n; d++)
		    for (e=0; e<n; e++)
			for (f=0; f<n; f++)
			    x += 1
    print x
}
# $Id: random.mawk,v 1.1 2004-05-19 18:11:16 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

function gen_random(max) { return( (max * (LAST = (LAST * IA + IC) % IM)) / IM ); }

BEGIN {
    IM = 139968;
    IA = 3877;
    IC = 29573;
    LAST = 42;

    n = ((ARGV[1] < 1) ? 1 : ARGV[1]) - 1;
    while (n--) {
	gen_random(100);
    }
    printf("%.9f\n", gen_random(100));
    exit;
}
# $Id: regexmatch.mawk,v 1.1 2004-05-19 18:11:24 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];
    delete ARGV;
}

{ phones[p++] = $0; }

END {
    for (i=0; i<n; i++) {
	for (j=0; j<p; j++) {
	    line = phones[j];
	    if (match(line, /(^|[^0-9])(\([0-9][0-9][0-9]\)|[0-9][0-9][0-9]) [0-9][0-9][0-9][ -][0-9][0-9][0-9][0-9]($|[^0-9])/)) {
		m1 = substr(line, RSTART, RLENGTH);
		num = ""
		if (match(m1, /[0-9][0-9][0-9] [0-9][0-9][0-9][ -][0-9][0-9][0-9][0-9]/)) {
		    if (substr(m1, RSTART-1, 1) == "(") {
			break;
		    }
		    if (x = split(substr(m1, RSTART, RLENGTH), parts, /[ -]/)) {
			num = "(" parts[1] ") " parts[2] "-" parts[3];
		    }
		} else if (match(m1, /\([0-9][0-9][0-9]\) [0-9][0-9][0-9][ -][0-9][0-9][0-9][0-9]/)) {
		    if (x = split(substr(m1, RSTART, RLENGTH), parts, /[ -]/)) {
			num = parts[1] " " parts[2] "-" parts[3];
		    }
		}
		if (i == (n-1)) {
		    count++;
		    printf("%d: %s\n", count, num);
		}
	    }
	}
    }
}
# $Id: reversefile.mawk,v 1.1 2004-05-19 18:12:18 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN { delete ARGV }
{ x[NR] = $0 }
END { for (i = NR; i >= 1; i--)
    print x[i]
}
# $Id: sieve.mawk,v 1.1 2004-05-19 18:12:27 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];
    while (n--) {
        count=0;
        for(i=2; i <= 8192; flags[i++]=1);
        for (i=2; i <= 8192; i++) {
	    if (flags[i]) {
		# remove all multiples of prime: i
		for (k=i+i; k <= 8192; k+=i) {
                    flags[k] = 0;
		}
		count++;
	    }
        }
    }
    printf("Count: %d\n", count);
    exit;
}
# $Id: spellcheck.mawk,v 1.1 2004-05-19 18:13:26 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN {
    delete ARGV;
    while (getline < "Usr.Dict.Words") {
	dict[$0] = 1;
    }
}
{
    if (!dict[$1]) {
	print $1
    }
}
# $Id: strcat.mawk,v 1.1 2004-05-19 18:13:34 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN {
    n = (ARGV[1] < 1) ? 1 : ARGV[1];

    str = ""
    for (i = 0; i < n; i++)
	str = str "hello\n"

    print length(str)
}
# $Id: sumcol.mawk,v 1.1 2004-05-19 18:13:43 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

BEGIN { delete ARGV; tot = 0 }
{ tot += $1 }
END { print tot }
# $Id: wc.mawk,v 1.1 2004-05-19 18:13:51 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

# this version is a little more efficient than the original via
# use of NR

BEGIN { delete ARGV }
{
    nc += length($0) + 1
    nw += NF
}
END { print NR, nw, nc }
# $Id: wordfreq.mawk,v 1.2 2004-07-03 05:36:11 bfulgham Exp $
# http://shootout.alioth.debian.org/

BEGIN {
    delete ARGV;
    FS = "[^A-Za-z][^A-Za-z]*";
}
{
    for (i=1; i<=NF; i++) {
	freq[tolower($(i))]++;
    }
}
END {
    # gawk doesn't have a builtin sort routine
    # so we have to pipe through the shell sort program
    sort = "sort -nr"
    for (word in freq) {
	if (word) {
	    printf "%7d %s\n", freq[word], word | sort
	}
    }
    close(sort)
}
