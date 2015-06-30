#!/usr/bin/perl
# $Id: ackermann.perl,v 1.3 2005-04-04 14:56:35 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# With help from Ernesto Hernandez-Novich
use integer;

# It's prettier but slower to do this
#sub Ack {
#    my($M, $N) = @_;
#    return( $N + 1 )         if ($M == 0);
#    return( Ack($M - 1, 1) ) if ($N == 0);
#    Ack($M - 1, Ack($M, $N - 1));
#}

# in our quest for speed, we must get ugly:
# it helps reduce stack frame size a little bit
# from Leif Stensson
sub Ack {
    return $_[0] ? ($_[1] ? Ack($_[0]-1, Ack($_[0], $_[1]-1))
		    : Ack($_[0]-1, 1))
	: $_[1]+1;
}

my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);
my $ack = Ack(3, $NUM);
print "Ack(3,$NUM): $ack\n";
#!/usr/bin/perl
# $Id: ackermann.perl-2.perl,v 1.1 2005-04-04 14:56:35 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# With help from Ernesto Hernandez-Novich
use integer;

# Note:  If memoization were allowed in this program, we could
# do so by adding:
use Memoize;
memoize("Ack");

# in our quest for speed, we must get ugly:
# it helps reduce stack frame size a little bit
# from Leif Stensson
sub Ack {
    return $_[0] ? ($_[1] ? Ack($_[0]-1, Ack($_[0], $_[1]-1))
		    : Ack($_[0]-1, 1))
	: $_[1]+1;
}

my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);
my $ack = Ack(3, $NUM);
print "Ack(3,$NUM): $ack\n";
#!/usr/bin/perl
# $Id: ackermann.perl-3.perl,v 1.1 2005-04-27 16:51:28 greg-guest Exp $
# http://www.bagley.org/~doug/shootout/

# We avoid using memoize by inlining the cache
# from Thomas Drugeon

# in our quest for speed, we must get ugly:
# it helps reduce stack frame size a little bit
# from Leif Stensson
sub Ack {
    $_[0] ? ($Ack[$_[0]][$_[1]] ||= $_[1] ? Ack($_[0]-1, Ack($_[0], $_[1]-1))
		    : Ack($_[0]-1, 1))
	: $_[1]+1;
}

my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);
my $ack = Ack(3, $NUM);
print "Ack(3,$NUM): $ack\n";
#!/usr/bin/perl
# The Great Win32 Computer Language Shootout
# http://shootout.alioth.debian.org/
# modified by Isaac Gouy

use integer;

sub Ack {
    my($M, $N) = @_;
    return( $N + 1 )         if ($M == 0);
    return( Ack($M - 1, 1) ) if ($N == 0);
    Ack($M - 1, Ack($M, $N - 1));
}

my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);
my $ack = Ack(3, $NUM);
print "Ack(3,$NUM): $ack\n";
#!/usr/bin/perl 
# $Id: ackermann.perl-2.perl,v 1.1 2004-11-10 06:09:46 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use strict;
use integer;

# cheat by saving intermediate values (memoizing)
# Warning!  This won't work for anything more than
# small values of M, N :-)
my @ACK = ();

sub Ack {
    my($M, $N) = @_;
    return( $ACK[$M][$N] ) if ($ACK[$M][$N]);
    return( $ACK[$M][$N] = $N + 1 ) if ($M == 0);
    return( $ACK[$M][$N] = Ack($M - 1, 1) ) if ($N == 0);
    return( $ACK[$M][$N] = Ack($M - 1, Ack($M, $N - 1)) );
}

my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);
my $ack = Ack(3, $NUM);
print "Ack(3,$NUM): $ack\n";
#!/usr/bin/perl
# $Id: ary.perl,v 1.2 2004-05-22 07:25:00 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program is modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

my $n = @ARGV[0] || 1;
my @X;
my @Y;

my $last = $n - 1;
for my $i (0..$last) {
    $X[$i] = $i + 1;
}
for my $k (0..999) {
    for my $i (reverse 0..$last) {
	$Y[$i] += $X[$i];
    }
}

print "$Y[0] $Y[$last]\n";
#!/usr/bin/perl
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
# Modified: 2005-06-18 Cosimo Streppone

use integer;

my $n = @ARGV[0] || 1;
my(@X, @Y, $i, $k);
my $last = $n - 1;

# Initialize @X list in a single step
@X = (1 .. $n);

# Execute 1000 times
for(0 .. 999) {
    # Use of `$_' aliasing is faster than using a lexical var
    # Also, there is no need to reverse (0 .. $last) list
    $Y[$_] += $X[$_] for 0 .. $last;
}

print $Y[0], ' ', $Y[$last], "\n";

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# 
# contributed by Emanuele Zeppieri

sub bottomup_tree {
    my ($value, $depth) = @_;
    return $value unless $depth;
    my $value2 = $value * 2; $depth--;
    [ bottomup_tree($value2-1, $depth), bottomup_tree($value2, $depth), $value ]
}

sub check_tree {
    my ($left, $right, $value) = @{ $_[0] };
    $value + (
        ref $left ? check_tree($left) - check_tree($right) : $left - $right
    )
}

my $max_depth = shift @ARGV;
my $min_depth = 4;

$max_depth = $min_depth + 2 if $min_depth + 2 > $max_depth;

my $stretch_depth = $max_depth + 1;
my $stretch_tree = bottomup_tree(0, $stretch_depth);
print "stretch tree of depth $stretch_depth\t check: ",
    check_tree($stretch_tree), "\n";
undef $stretch_tree;

my $longlived_tree = bottomup_tree(0, $max_depth);

for ( my $depth = $min_depth; $depth <= $max_depth; $depth += 2 ) {
    my $iterations = 2 << $max_depth - $depth + $min_depth - 1;
    my $check = 0;
    
    foreach (1..$iterations) {
        $check += check_tree( bottomup_tree(0, $depth) );
        $check += check_tree( bottomup_tree(0, $depth) )
    }
    
    print 2*$iterations, "\t trees of depth $depth\t check: ", $check, "\n"
}

print "long lived tree of depth $max_depth\t check: ",
    check_tree($longlived_tree), "\n"
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Doug King
# Corrected by Heiner Marxen
# Tree-building made non-recursive by Steffen Mueller

use integer;

sub item_check {
    my ($tree) = @_;

    return $tree->[2] unless (defined $tree->[0]);
    return $tree->[2] + item_check($tree->[0]) - item_check($tree->[1]);
}


sub bottom_up_tree {
    my($depth) = @_;

    my @pool;
    push @pool, [undef, undef, -$_] foreach 0..2**$depth-1;

    foreach my $exponent (reverse(0..($depth-1))) {
        push @pool, [reverse(splice(@pool, 0, 2)), $_] 
                       foreach reverse(-(2**$exponent-1) .. 0);
    }
    return $pool[0];
}


my $n = shift @ARGV;

my $min_depth = 4;
my $max_depth;

if ( ($min_depth + 2) > $n) {
    $max_depth = $min_depth + 2;
} else {
    $max_depth = $n;
}

{
    my $stretch_depth = $max_depth + 1;
    my $stretch_tree = bottom_up_tree($stretch_depth);
    print "stretch tree of depth $stretch_depth\t check: ",
           item_check($stretch_tree), "\n";
}

my $long_lived_tree = bottom_up_tree($max_depth);

my $depth = $min_depth;
while( $depth <= $max_depth ) {
    my $iterations = 2 ** ($max_depth - $depth + $min_depth);
    my $check = 0;

    foreach my $i (1..$iterations) {
	my $temp_tree = bottom_up_tree($depth);
	$check += item_check($temp_tree);

	$temp_tree = bottom_up_tree($depth);
	$check += item_check($temp_tree);
    }

    print $iterations * 2, "\t trees of depth $depth\t check: ", $check, "\n";
    $depth += 2;
}

print "long lived tree of depth $max_depth\t check: ", 
       item_check($long_lived_tree), "\n";

#########################################
#     The Computer Language Shootout    #
#   http://shootout.alioth.debian.org/  #
#                                       #
#      Contributed by Jesse Millikan    #
#########################################

use threads;
use threads::shared;

# Complement method is numerical based on Haskell version
my ($red, $yellow, $blue, $none) = (0,1,2,3);

# Count, signal and mutex are all $meetings
my $meetings : shared = $ARGV[0];

# Locked and updated by each thread when it ends
my $total_meetings : shared = 0;

# Colour communication variables
my $first : shared = $none;
my $second : shared = $none;

# $_ is thread on the outer map and color on the inner loop
map { $_->join } (map {
 # async starts a new thread running the block given
 async {
  my ($color, $other_color) = ($_,$none);
  my $met = 0; 

  # with 'redo', loop until 'last' is called
  LIVE: { 

   # Meeting place 
   { 
    lock $meetings;
    
    last LIVE if($meetings <= 0); # 'fade' by jumping out of the block

    if($first != $none){
     $other_color = $first;
     $second = $color;
     cond_signal $meetings; 
     $meetings -= 1;
     $first = $none;
    }
    else
    {
     $first = $color; 
     cond_wait $meetings;
     $other_color = $second;
    }
   } # Unlock the meeting place 

   $color = 3 - $color - $other_color if($color != $other_color);
   $met++;

   redo;
  }

  # Lock the total and add own to it before dying
  lock $total_meetings;
  $total_meetings += $met;
 }
} ($blue, $red, $yellow, $blue));

print "$total_meetings\n";
# The Computer Language Benchmark Game
# http://shootout.alioth.debian.org/
# contributed by Daniel Green 2010-4-1
#  a transliteration of Python 3 #2

use 5.10.0;
use strict;
use warnings;
use threads;
use threads::shared;
use Thread::Semaphore;
use List::Util qw(sum);

my @creature_colors = qw(blue red yellow);

sub complement {
    my ($c1, $c2) = @_;

    if ($c1 eq $c2) {
        return $c1;
    } elsif ($c1 eq 'blue') {
        if ($c2 eq 'red') {
            return 'yellow';
        } else {
            return 'red';
        }
    } elsif ($c1 eq 'red') {
        if ($c2 eq 'blue') {
            return 'yellow';
        } else {
            return 'blue';
        }
    } elsif ($c2 eq 'blue') {
        return 'red';
    } else {
        return 'blue';
    }
}

my %compl_dict;
foreach my $c1 (@creature_colors) {
    foreach my $c2 (@creature_colors) {
        $compl_dict{"$c1,$c2"} = complement($c1, $c2);
    }
}

sub check_complement {
    foreach my $c1 (@creature_colors) {
        foreach my $c2 (@creature_colors) {
            say "$c1 + $c2 -> " . $compl_dict{"$c1,$c2"};
        }
    }

    say '';
}

sub spellout {
    my ($n) = @_;

    my @numbers = qw(zero one two three four five six seven eight nine);

    return ' ' . join(' ', map { $numbers[$_] } split //, $n);
}

sub report {
    my ($input_zoo, $met, $self_met) = @_;

    say ' ' . join(' ', @{$input_zoo});

    for (my $x = 0; $x < scalar @{$met}; $x++) {
        say $met->[$x] . spellout($self_met->[$x]);
    }

    say spellout(sum(@{$met})) . "\n";
}

sub creature {
    my ($my_id, $venue, $my_lock, $in_lock, $out_lock) = @_;

    while (1) {
        $my_lock->down();
        $in_lock->down();

        $venue->[0] = $my_id;
        $out_lock->up();
    }
}

sub let_them_meet {
    my ($meetings_left, $input_zoo) = @_;

    my $c_no = scalar @{$input_zoo};
    my @venue :shared = (-1);
    my @met = (0) x $c_no;
    my @self_met = (0) x $c_no;
    my @colors = @{$input_zoo};

    my $in_lock = Thread::Semaphore->new();
    $in_lock->down();
    my $out_lock = Thread::Semaphore->new();
    $out_lock->down();
    
    my @locks;
    for my $ci (0 .. $c_no - 1) {
        $locks[$ci] = Thread::Semaphore->new();
        threads->new(\&creature, $ci, \@venue, $locks[$ci], $in_lock, $out_lock)->detach();
    }

    $in_lock->up();
    $out_lock->down();
    my $id1 = $venue[0];
    while ($meetings_left > 0) {
        $in_lock->up();
        $out_lock->down();
        my $id2 = $venue[0];
        if ($id1 != $id2) {
            my $new_color = $compl_dict{"$colors[$id1],$colors[$id1]"};
            $colors[$id1] = $new_color;
            $colors[$id2] = $new_color;
            $met[$id1] += 1;
            $met[$id2] += 1;
        } else {
            $self_met[$id1] += 1;
            $met[$id1] += 1;
        }
        $meetings_left -= 1;
        if ($meetings_left > 0) {
            $locks[$id1]->up();
            $id1 = $id2;
        } else {
            report($input_zoo, \@met, \@self_met);
        }
    }
}

check_complement();
let_them_meet($ARGV[0], ['blue', 'red', 'yellow']);
let_them_meet($ARGV[0], ['blue', 'red', 'yellow', 'red', 'yellow', 'blue', 'red', 'yellow', 'red', 'blue']);
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Jonathan DePeri 2010/5
# based on an earlier version by Jesse Millikan
# uses Perl interpreter threads with pthreads-like cond_wait and cond_signal

use threads;
use threads::shared;

sub complement
{
   my $_ = join('', @_);
   
   s/BB/B/;
   s/BR/Y/;
   s/BY/R/;
   s/RB/Y/;
   s/RR/R/;
   s/RY/B/;
   s/YB/R/;
   s/YR/B/;
   s/YY/Y/;
   
   return $_;
}

sub color_name
{
   my $_ = shift;

   s/B/blue/;
   s/R/red/;
   s/Y/yellow/;

   return $_;
}

sub display_complements
{
   local $\ = "\n";

   print 'blue + blue -> ', color_name(complement('B','B'));
   print 'blue + red -> ', color_name(complement('B','R'));
   print 'blue + yellow -> ', color_name(complement('B','Y'));
   print 'red + blue -> ', color_name(complement('R','B'));
   print 'red + red -> ', color_name(complement('R','R'));
   print 'red + yellow -> ', color_name(complement('R','Y'));
   print 'yellow + blue -> ', color_name(complement('Y','B'));
   print 'yellow + red -> ', color_name(complement('Y','R'));
   print 'yellow + yellow -> ', color_name(complement('Y','Y'));
   print '';
}

sub num2words
{
   my $_ = shift;

   s/0/ zero/g;
   s/1/ one/g;
   s/2/ two/g;
   s/3/ three/g;
   s/4/ four/g;
   s/5/ five/g;
   s/6/ six/g;
   s/7/ seven/g;
   s/8/ eight/g;
   s/9/ nine/g;

   return $_;
}

sub print_color_names
{
   for (@_) { print ' ', color_name($_); }
}


my @colors : shared;
my $meetings : shared;
my $first : shared = undef;
my $second : shared = undef;
my @met : shared;
my @met_self : shared;

sub chameneos
{
   my $id = shift;
   my $other = undef;
   
   while (1) {
      lock $meetings;
      last if ($meetings <= 0);
   
      if (not defined $first) {
         $first = $id;
         cond_wait $meetings;
      } else {
         cond_signal $meetings;
         
         $colors[$first] = $colors[$id] = complement($colors[$first], $colors[$id]);
         $met_self[$first]++ if ($first == $id);      
         $met[$first]++;  $met[$id]++;
         $meetings -= 1;
         
         $first = undef;
      }
   }
}

sub pall_mall
{
   my $N = shift;
   @colors = @_;
   my @threads;
   
   print_color_names(@colors);

   $meetings = $N;
   for (0..@colors-1) {
      $met[$_] = $met_self[$_] = 0;
      $threads[$_] = threads->create(\&chameneos, $_);
   }
   for (@threads) {
     $_->join();
   }
   
   $meetings = 0;
   for (0..@colors-1) {
      print "\n$met[$_]", num2words($met_self[$_]);
      $meetings += $met[$_];
     }
   print "\n", num2words($meetings), "\n\n";
}


display_complements();
pall_mall($ARGV[0], qw(B R Y));
pall_mall($ARGV[0], qw(B R Y R Y B R Y R B));
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Jonathan DePeri 2010/5
# based on an earlier version by Jesse Millikan
# uses Perl interpreter threads with pthreads-like cond_wait and cond_signal
# Modified by Andrew Rodland, August 2010

use threads;
use threads::shared;

my %color = (
  blue => 1,
  red => 2,
  yellow => 4,
);

my @colors;
@colors[values %color] = keys %color;

my @complement;
for my $triple (
  [qw(blue blue blue)],
  [qw(red red red)],
  [qw(yellow yellow yellow)],
  [qw(blue red yellow)],
  [qw(blue yellow red)],
  [qw(red blue yellow)],
  [qw(red yellow blue)],
  [qw(yellow red blue)],
  [qw(yellow blue red)],
) {
  $complement[ $color{$triple->[0]} | $color{$triple->[1]} ] = $color{$triple->[2]};
}

my @numbers = qw(zero one two three four five six seven eight nine);

sub display_complements
{
  for my $i (1, 2, 4) {
    for my $j (1, 2, 4) {
      print "$colors[$i] + $colors[$j] -> $colors[ $complement[$i | $j] ]\n";
    }
  }
  print "\n";
}

sub num2words {
  join ' ', '', map $numbers[$_], split //, shift;
}

my @creatures : shared;
my $meetings : shared;
my $first : shared = undef;
my $second : shared = undef;
my @met : shared;
my @met_self : shared;

sub chameneos
{
   my $id = shift;

   while (1) {
      lock $meetings;
      last unless $meetings;

      if (defined $first) {
         cond_signal $meetings;
         $creatures[$first] = $creatures[$id] = $complement[$creatures[$first] | $creatures[$id]];
         $met_self[$first]++ if ($first == $id);
         $met[$first]++;  $met[$id]++;
         $meetings --;
         undef $first;
      } else {
         $first = $id;
         cond_wait $meetings;
      }
   }
}

sub pall_mall
{
   my $N = shift;
   @creatures = map $color{$_}, @_;
   my @threads;

   print " ", join(" ", @_);

   $meetings = $N;
   for (0 .. $#creatures) {
      $met[$_] = $met_self[$_] = 0;
      push @threads, threads->create(\&chameneos, $_);
   }
   for (@threads) {
     $_->join();
   }

   $meetings = 0;
   for (0 .. $#creatures) {
      print "\n$met[$_]", num2words($met_self[$_]);
      $meetings += $met[$_];
     }
   print "\n", num2words($meetings), "\n\n";
}


display_complements();
pall_mall($ARGV[0], qw(blue red yellow));
pall_mall($ARGV[0], qw(blue red yellow red yellow blue red yellow red blue));
#!/usr/bin/perl
# $Id: echo.perl,v 1.1 2004-05-19 18:09:37 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use Socket;

my $DATA = "Hello there sailor\n";

sub server_sock {
    local *SS;
    socket(SS, PF_INET, SOCK_STREAM, 0) or
	die "server/socket ($!)";
    setsockopt(SS, SOL_SOCKET, SO_REUSEADDR, pack("l", 1)) or
	die "server/setsockopt ($!)";
    bind(SS, sockaddr_in(0, INADDR_LOOPBACK)) or
	die "server/bind ($!)";
    listen(SS, 2);
    return(*SS);
}

sub get_port {
    local *SK = shift;
    (sockaddr_in(getsockname(SK)))[0];
}

sub client_sock {
    my $port = shift;
    local *CS;
    socket(CS, PF_INET, SOCK_STREAM, getprotobyname('tcp')) or
	die "client/socket ($!)";
    connect(CS, sockaddr_in($port, INADDR_LOOPBACK)) or
	die "client/connect ($!)";
    return(*CS);
}

sub echo_client {
    my($N, $port) = @_;
    local *SOCK = client_sock($port);
    select(SOCK);
    $| = 1;
    for my $i (0..($N-1)) {
	print $DATA;
	my $ans = <SOCK>;
	($ans eq $DATA) or die qq{client: "$DATA" ne "$ans"};
    }
    close SOCK;
}

sub echo_server {
    my($N) = @_;
    local *SSOCK = server_sock();
    my $port = get_port(*SSOCK);
    my $pid = fork;
    defined $pid or die "server/fork ($!)";
    if ($pid) {
	# parent is server
	local *CSOCK;
	accept(CSOCK, SSOCK) or die "server/accept ($!)";
	select(CSOCK);
	$| = 1;
	my $n = 0;
	while (<CSOCK>) {
	    print $_;
	    $n += length($_);
	}
	select(STDOUT);
	print "server processed $n bytes\n";
    } else {
	# child is client
	echo_client($N, $port);
    }
    wait();
}

sub main {
    my $N = $ARGV[0] || 1;
    echo_server($N);
    exit(0);
}

main();
#!/usr/bin/perl
# $Id: except.perl,v 1.1 2004-05-19 18:09:43 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use integer;

my $HI = 0;
my $LO = 0;
my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);

package Lo_Exception;

sub new {
    bless({Val => shift}, __PACKAGE__);
}

package Hi_Exception;

sub new {
    bless({Val => shift}, __PACKAGE__);
}

package main;

sub some_function {
    my $num = shift;
    eval {
	&hi_function($num);
    };
    if ($@) {
	die "We shouldn't get here ($@)";
    }
}

sub hi_function {
    my $num = shift;
    eval {
	&lo_function($num);
    };
    if (ref($@) eq "Hi_Exception") {
	$HI++;		# handle
    } elsif ($@) {
	die $@;		# rethrow
    }
}

sub lo_function {
    my $num = shift;
    eval {
	&blowup($num);
    };
    if (ref($@) eq "Lo_Exception") {
	$LO++;		# handle
    } elsif ($@) {
	die $@;		# rethrow
    }
}

sub blowup {
    my $num = shift;
    if ($num % 2) {
	die Lo_Exception->new(Num => $num);
    } else {
	die Hi_Exception->new(Num => $num);
    }
}

$NUM = $ARGV[0];
while ($NUM--) {
    &some_function($NUM);
}
print "Exceptions: HI=$HI / LO=$LO\n";
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Initial port from C by Steve Clark
# Rewrite by Kalev Soikonen
# Modified by Kuang-che Wu
# Modified by David Golden

use integer;

sub fannkuch {
    my ($n) = shift;
    my ($iter, $flips, $maxflips, $i);
    my ($q, $f, $p, @count);

    $iter = $maxflips = 0;
    @count = (1..$n); 
    $p = pack "c*", @count;
    my $m = $n - 1;

    TRY: {
        if ($iter < 30) {
            print join("", unpack("c*",$p)) . "\n";
            $iter++;
        }

        if (ord(substr($p,0)) != 1 && ord(substr($p,$m)) != $n) {
            $q = $p;
            $flips=0;
            while ( ($f = ord(substr($q,0))) != 1 ) {
                $flips++;
                substr( $q, 0, $f, reverse( substr($q,0,$f) ) );
            }
            $maxflips = $flips if ($flips > $maxflips);
        }

        for my$i(1..$m) {
            substr $p, $i, 0, (substr($p,0,1,""));
            redo TRY if (--$count[$i]);
            $count[$i] = $i + 1;
        }
        return $maxflips;
    }
}

for (shift || 7) {
    print "Pfannkuchen($_) = ".fannkuch($_)."\n";
}
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/

# Initial port from C by Steve Clark
# Rewrite by Kalev Soikonen
# Modified by Kuang-che Wu
# Multi-threaded by Andrew Rodland

use integer;
use threads;

sub fannkuch {
  my ($n, $last) = @_;
  my ($iter, $flips, $maxflips);
  my (@q, @p, @count);

  @p = (1 .. $last - 1, $last + 1 .. $n, $last);
  @count = (1..$n);

  TRY: while (1) {
    if ($p[0] != 1 && $p[-1] != $n) {
      @q = @p;
      for ($flips=0; $q[0] != 1; $flips++) {
        unshift @q, reverse splice @q, 0, $q[0];
      }
      $maxflips = $flips if $flips > $maxflips;
    }

    for my $i (1 .. $n - 2) {
      splice @p, $i, 0, shift @p;
      next TRY if (--$count[$i]);
      $count[$i] = $i + 1;
    }
    return $maxflips;
  }
}

sub print30 {
  my ($n, $iter) = @_;
  @p = @count = (1..$n);

  TRY: while (1) {
    print @p, "\n";
    return if ++$iter >= 30;
    for my $i (1 .. $n - 1) {
      splice @p, $i, 0, shift @p;
      next TRY if (--$count[$i]);
      $count[$i] = $i + 1;
    }
  }
}

my $n = shift || 7;

print30($n);

my @threads;
for my $i (1 .. $n) {
  push @threads, threads->create(\&fannkuch, $n, $i);
}

my $max = 0;
for my $thread (@threads) {
  my $val = $thread->join;
  $max = $val if $val > $max;
}
print "Pfannkuchen($n) = $max\n";
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# initial fannkuch port from C by Steve Clark
#   rewrite by Kalev Soikonen
#   modified by Kuang-che Wu
#   modified by David Golden
# updated for fannkuch-redux by Jonathan DePeri
#   permutations generated using Mike Pall's approach

use integer;

sub fannkuchredux {
    my ($n) = shift;
    my ($m, $checksum, $maxflips, $flips, $sign) = ($n-1, 0, 0, 0, 1);
    my ($p, $q, $f, $i, @count);
    
    @count = (0..$m); 
    $p = pack "c*", @count;

    do {
        if (ord(substr($p,0))) {
            $q = $p;
            $flips = 0;
            while ($f = ord(substr($q,0))) {
                $flips++;
                substr($q, 0, $f+1, reverse(substr($q,0,$f+1)));
            }
            $maxflips = $flips if ($flips > $maxflips);
            $checksum += ($sign * $flips);
        }
        
        return if ($n <= 1);
        if ($sign == 1) {
            $sign = -1;
            substr $p, 1, 0, (substr($p,0,1,""));
        } else {
            return if ($n <= 2);
            $sign = 1;
            substr $p, 1, 0, (substr($p,2,1,""));
            for $i (2..$m) {
	            if ($count[$i]) { $count[$i]--; last; }
	            return ($checksum, $maxflips) if ($i == $m);
	            $count[$i] = $i;
	            substr $p, $i+1, 0, (substr($p,0,1,""));
            }
        }
    } while (1);
}

for (shift) {
    exit -1 if ((not defined $_) || $_ < 1);
    my ($checksum, $maxflips) = fannkuchredux($_);
    print "$checksum\n";
    print "Pfannkuchen($_) = $maxflips\n";
}

# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by David Pyke
# tweaked by Danny Sauer
# Butchered by Jesse Millikan

use constant IM => 139968;
use constant IA => 3877;
use constant IC => 29573;

use constant LINELENGTH => 60;

my $LAST = 42;

sub makeCumulative {
    my($genelist) = @_;
    my $cp = 0.0;

    foreach (@$genelist){
        $_->[1] = $cp += $_->[1];
    }
}

sub makeRandomFasta {
    my($id,$desc,$n,$genelist) = @_;

    print ">$id $desc\n";
    my $pick, $r;

	while($n > 0){
		$pick='';

		# Get LINELENGTH chars or what's left of $n
        CHAR: foreach (1 .. ($n > LINELENGTH ? LINELENGTH : $n)){
    		$rand = ($LAST = ($LAST * IA + IC) % IM) / IM;

			# Select gene and append it
    		foreach (@$genelist){
				if($rand < $_->[1]){
					$pick .= $_->[0];
					next CHAR;
				}
    		}
        }

        print "$pick\n";
		$n -= LINELENGTH;
    }
}

# Print $n characters of $s (repeated if nessary) with newlines every LINELENGTH
sub makeRepeatFasta {
    my($id,$desc,$s,$n) = @_;

    print ">$id $desc\n";

	my $ss;
	while($n > 0){
		# Overfill $ss with $s
		$ss .= $s while length $ss < LINELENGTH;
		# Print LINELENGTH chars or whatever's left of $n
        print substr($ss,0,$n > LINELENGTH ? LINELENGTH : $n,""), "\n";
		$n -= LINELENGTH;
	}
}

my $iub = [
    [a, 0.27],
    [c, 0.12],
    [g, 0.12],
    [t, 0.27],
    [B, 0.02],
    [D, 0.02],
    [H, 0.02],
    [K, 0.02],
    [M, 0.02],
    [N, 0.02],
    [R, 0.02],
    [S, 0.02],
    [V, 0.02],
    [W, 0.02],
    [Y, 0.02]
];

my $homosapiens = [
    [a, 0.3029549426680],
    [c, 0.1979883004921],
    [g, 0.1975473066391],
    [t, 0.3015094502008]
];

$alu =
    'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' .
    'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' .
    'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' .
    'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' .
    'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' .
    'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' .
    'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

#main

my $n = ($ARGV[0] || 1000) ;

makeCumulative $iub;
makeCumulative $homosapiens;

makeRepeatFasta ('ONE', 'Homo sapiens alu', $alu, $n*2);
makeRandomFasta ('TWO', 'IUB ambiguity codes', $n*3, $iub);
makeRandomFasta ('THREE', 'Homo sapiens frequency', $n*5, $homosapiens);

#!/usr/bin/perl

# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by David Pyke
# tweaked by Danny Sauer

use constant IM => 139968;
use constant IA => 3877;
use constant IC => 29573;

use constant LINELENGTH => 60;

my $LAST = 42;
sub gen_random ($) {
    return ( ($_[0] * ($LAST = ($LAST * IA + IC) % IM)) / IM );
}

sub makeCumulative($){
    my($genelist) = @_;
    $cp = 0.0;

    foreach (@$genelist){
        $_->[1] = $cp += $_->[1];
    }
}

sub selectRandom($){
    my($genelist) = @_;
    $r = gen_random (1);

    foreach (@$genelist){
        if ($r < $_->[1]){ return $_->[0]; }
    }
}


sub makeRandomFasta($$$$){
#void makeRandomFasta (const char * id, const char * desc, const struct aminoacids * genelist, int count, int n) {
    my($id,$desc,$n,$genelist) = @_;

    print ">$id $desc\n";
    $pick='';

    # print whole lines
    foreach (1 .. int($n / LINELENGTH) ){
        foreach (1 ..  LINELENGTH ){
            $pick .= selectRandom($genelist);
        }
        print "$pick\n";
        $pick = '';
    }
    #print remaining line (if required)
    if ($n % LINELENGTH){
        foreach (1 ..  $n % LINELENGTH ){
            $pick .= selectRandom($genelist);
        }
        print "$pick\n";
    }
}

sub makeRepeatFasta($$$$){
#void makeRepeatFasta (const char * id, const char * desc, const char * s, int n) {
    # we want to print $n characters of $s (repeated if nessary) with newlines every LINELENGTH
    my($id,$desc,$s,$n) = @_;

    print ">$id $desc\n";

    # what we need, and the extra (if any) will be discarded.
    foreach (1 .. int($n / LINELENGTH) ){
        while (length $ss < LINELENGTH){
            $ss .= $s;
        }
        print substr($ss,0,LINELENGTH), "\n";
        $ss = substr($ss,LINELENGTH);
    }
    #final_line
    while (length $ss < LINELENGTH){
        $ss .= $s;
    }
    print substr($ss, 0, ($n % LINELENGTH)), "\n";
print STDERR "\n";
}


my $iub = [
    [ 'a', 0.27 ],
    [ 'c', 0.12 ],
    [ 'g', 0.12 ],
    [ 't', 0.27 ],
    [ 'B', 0.02 ],
    [ 'D', 0.02 ],
    [ 'H', 0.02 ],
    [ 'K', 0.02 ],
    [ 'M', 0.02 ],
    [ 'N', 0.02 ],
    [ 'R', 0.02 ],
    [ 'S', 0.02 ],
    [ 'V', 0.02 ],
    [ 'W', 0.02 ],
    [ 'Y', 0.02 ]
];

my $homosapiens = [
    [ 'a', 0.3029549426680 ],
    [ 'c', 0.1979883004921 ],
    [ 'g', 0.1975473066391 ],
    [ 't', 0.3015094502008 ]
];

$alu =
    'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' .
    'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' .
    'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' .
    'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' .
    'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' .
    'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' .
    'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

######################################################################
#main

my $n = ($ARGV[0] || 1000) ;

makeCumulative $iub;
makeCumulative $homosapiens;

makeRepeatFasta ('ONE', 'Homo sapiens alu', $alu, $n*2);
makeRandomFasta ('TWO', 'IUB ambiguity codes', $n*3, $iub);
makeRandomFasta ('THREE', 'Homo sapiens frequency', $n*5, $homosapiens);

exit 0;

#END OF FILE
# The Computer Language Benchmarks game
# http://shootout.alioth.debian.org/
#
# contributed by David Pyke
# tweaked by Danny Sauer
# optimized by Steffen Mueller
# tweaked by Kuang-che Wu

use strict;
use warnings;
use constant IM => 139968;
use constant IA => 3877;
use constant IC => 29573;

use constant LINELENGTH => 60;

my $LAST = 42;
sub gen_random {
    return map {( ($_[0] * ($LAST = ($LAST * IA + IC) % IM)) / IM )} 1..($_[1]||1);
}

sub makeCumulative {
    my $genelist = shift;
    my $cp = 0.0;

    $_->[1] = $cp += $_->[1] foreach @$genelist;
}

sub selectRandom {
    my $genelist = shift;
    my $number = shift || 1;
    my @r = gen_random(1, $number);

    my $s;
    foreach my $r (@r) {
        foreach (@$genelist){
            if ($r < $_->[1]) { $s .= $_->[0]; last; }
        }
    }

    return $s;
}


sub makeRandomFasta {
    my ($id, $desc, $n, $genelist) = @_;

    print ">", $id, " ", $desc, "\n";

    # print whole lines
    foreach (1 .. int($n / LINELENGTH) ){
        print selectRandom($genelist, LINELENGTH), "\n";
    }
    # print remaining line (if required)
    if ($n % LINELENGTH){
        print selectRandom($genelist, $n % LINELENGTH), "\n";
    }
}

sub makeRepeatFasta {
    my ($id, $desc, $s, $n) = @_;

    print ">", $id, " ", $desc, "\n";

    my $r = length $s;
    my $ss = $s . $s . substr($s, 0, $n % $r);
    for my $j(0..int($n / LINELENGTH)-1) {
	my $i = $j*LINELENGTH % $r;
	print substr($ss, $i, LINELENGTH), "\n";
    }
    if ($n % LINELENGTH) {
	print substr($ss, -($n % LINELENGTH)), "\n";
    }
}


my $iub = [
    [ 'a', 0.27 ],
    [ 'c', 0.12 ],
    [ 'g', 0.12 ],
    [ 't', 0.27 ],
    [ 'B', 0.02 ],
    [ 'D', 0.02 ],
    [ 'H', 0.02 ],
    [ 'K', 0.02 ],
    [ 'M', 0.02 ],
    [ 'N', 0.02 ],
    [ 'R', 0.02 ],
    [ 'S', 0.02 ],
    [ 'V', 0.02 ],
    [ 'W', 0.02 ],
    [ 'Y', 0.02 ]
];

my $homosapiens = [
    [ 'a', 0.3029549426680 ],
    [ 'c', 0.1979883004921 ],
    [ 'g', 0.1975473066391 ],
    [ 't', 0.3015094502008 ]
];

my $alu =
    'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' .
    'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' .
    'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' .
    'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' .
    'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' .
    'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' .
    'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

######################################################################
#main

my $n = ($ARGV[0] || 1000) ;

makeCumulative($iub);
makeCumulative($homosapiens);

makeRepeatFasta ('ONE', 'Homo sapiens alu', $alu, $n*2);
makeRandomFasta ('TWO', 'IUB ambiguity codes', $n*3, $iub);
makeRandomFasta ('THREE', 'Homo sapiens frequency', $n*5, $homosapiens);

#!/usr/bin/perl
# $Id: fibo.perl,v 1.5 2005-04-25 19:01:38 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

use strict;
use integer;

# from Leif Stensson
sub fib {
    return $_[0] < 2 ? 1 : fib($_[0]-2) + fib($_[0]-1);
}

my $N = ($ARGV[0] < 1) ? 1 : $ARGV[0];
my $fib = fib($N);
print "$fib\n";
#!/usr/bin/perl
# $Id: fibo.perl-2.perl,v 1.2 2005-04-25 19:01:38 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

use strict;
use integer;

# Comments by Ernesto Hernandez-Novich
# If memoization were allowed, we could gain some benefit by
# using:
# use Memoize;
# memoize("fib");

# from Leif Stensson
sub fib {
    return $_[0] < 2 ? 1 : fib($_[0]-2) + fib($_[0]-1);
}

my $N = ($ARGV[0] < 1) ? 1 : $ARGV[0];
my $fib = fib($N);
print "$fib\n";
#!/usr/bin/perl -w
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
# contributed by Greg Buchholz

$sum += 1/$_ for 1..$ARGV[0];
printf "%.9f\n", $sum;
#!/usr/bin/perl
# $Id: hash.perl,v 1.1 2004-05-19 18:09:55 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program is modified from:
#   http:#cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

use strict;

my $n = $ARGV[0] || 1;
my %X = ();
my $c = 0;

for my $i (1..$n) {
    $X{sprintf('%x', $i)} = $i;
}
for my $i (reverse 1..$n) {
    ++$c if exists $X{$i};
}
print "$c\n";
#!/usr/bin/perl
use integer;

$n = $ARGV[0] || 1;
%X = ();
$c = 0;
keys %X=$n;
for ($i=0; $i<$n;) {
    $X{sprintf('%x', $i)} = ++$i;
    $X{sprintf('%x', $i)} = ++$i;
    $X{sprintf('%x', $i)} = ++$i;
    $X{sprintf('%x', $i)} = ++$i;
}
for ($i=$n+1; $i>0;) {
    $c+=exists($X{--$i}) + exists($X{--$i}) + exists($X{--$i}) + exists($X{--$i});
}
print $c,"\n";
#!/usr/bin/perl
# $Id: hash.perl-3.perl,v 1.1 2004-11-10 06:34:44 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program is modified from:
#   http:#cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

use strict;

my $n = $ARGV[0] || 1;
my %X = ();
keys %X = $n / 3;
my $c = 0;

for my $i (1..$n) {
    $X{sprintf('%x', $i)} = $i;
}
for my $i (reverse 1..$n) {
    ++$c if exists $X{$i};
}
print "$c\n";
#!/usr/bin/perl
# $Id: hash2.perl,v 1.1 2004-05-19 18:10:02 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Steve Fink

use strict;

my $n = ($ARGV[0] > 0) ? $ARGV[0] : 1;
my %hash1 = ();
$hash1{"foo_$_"} = $_ for 0..9999;
my %hash2 = ();
my($k, $v);
for (1..$n) {
    $hash2{$_} += $hash1{$_} while (defined ($_ = each %hash1));
}
print "$hash1{foo_1} $hash1{foo_9999} $hash2{foo_1} $hash2{foo_9999}\n";
#!/usr/bin/perl 
# $Id: heapsort.perl,v 1.1 2004-05-19 18:10:10 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# Matt Harris suggested passing the array via typeglob

use strict;

use constant IM => 139968;
use constant IA =>   3877;
use constant IC =>  29573;

use vars qw(@ra);

my $LAST = 42;
sub gen_random { ($_[0] * ($LAST = ($LAST * IA + IC) % IM)) / IM }

sub heapsort ($\@) {
    my $n = shift;
    # use typeglob ra to refer to array.
    local *ra = shift;

    my($rra, $i, $j);

    my $l = ($n >> 1) + 1;
    my $ir = $n;
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
	    $j++ if (($j < $ir) && ($ra[$j] < $ra[$j+1]));
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


my $N = $ARGV[0];
$N = 1 if ($N < 1);

# create an array of N random doubles
my @ary = ();
for (my $i=1; $i<=$N; $i++) {
    $ary[$i] = gen_random(1.0);
}

heapsort($N, @ary);

printf("%.10f\n", $ary[-1]);

#!/usr/bin/perl
# $Id: hello.perl,v 1.1 2004-05-19 18:10:16 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

print "hello world\n";
#  The Computer Language Benchmarks Game
#  http://shootout.alioth.debian.org/

#  contributed by Karl FORNER
# (borrowed fasta loading routine from Kjetil Skotheim, 2005-11-29)
# Corrected again by Jesse Millikan
# revised by Kuang-che Wu
# Multi-threaded by Andrew Rodland

use strict;
use threads;

my $threads = num_cpus() || 1;

my ($sequence, $begin, $end);
$/ = ">";
/^THREE/ and $sequence = uc(join "", grep !/^THREE/, split /\n+/) while <STDIN>;

my ($l,%h,$sum) = (length $sequence);

foreach my $frame (1,2) {
  %h = ();
  update_hash_for_frame($frame);
  $sum = $l - $frame + 1;
  printf "$_ %.3f\n", $h{$_}*100/$sum for sort { $h{$b} <=> $h{$a} || $a cmp $b } keys %h;
  print "\n";
}

foreach my $s (qw(GGT GGTA GGTATT GGTATTTTAATT GGTATTTTAATTTATAGT)) {
  update_hash_for_frame(length($s));
  printf "%d\t$s\n", $h{$s};
}

sub update_hash_for_frame {
  my $frame = $_[0];
  my @threads;
  for my $i (0 .. $threads - 1) {
    use integer;
    my $begin = $l * $i / $threads;
    my $end = $l * ($i + 1) / $threads - 1;
    no integer;
    if ($end > $l - $frame) {
      $end = $l - $frame;
    }
    push @threads, threads->create(\&update_hash_slice, $frame, $begin, $end);
  }
  for my $thread (@threads) {
    my $count = $thread->join;
    $h{$_} += $count->{$_} for keys %$count;
  }
}

sub update_hash_slice {
  my ($frame, $begin, $end) = @_;
  my %local;
  $local{substr($sequence,$_,$frame)}++ for $begin .. $end;
  return \%local;
}

sub num_cpus {
  open my $fh, '</proc/cpuinfo' or return;
  my $cpus;
  while (<$fh>) {
    $cpus ++ if /^processor\s+:/;
  }
  return $cpus;
}
#  The Computer Language Benchmarks Game
#  http://shootout.alioth.debian.org/
#  contributed by Karl FORNER
# (borrowed fasta loading routine from Kjetil Skotheim, 2005-11-29)
# Corrected again by Jesse Millikan
# revised by Kuang-che Wu

my ($sequence);
$/ = ">";
/^THREE/ and $sequence = uc(join "", grep !/^THREE/, split /\n+/) while <STDIN>;

my ($l,%h,$sum) = (length $sequence);
foreach my $frame (1,2) {
  %h = ();
  update_hash_for_frame($frame);
  $sum = $l - $frame + 1;
  printf "$_ %.3f\n", $h{$_}*100/$sum for sort { $h{$b} <=> $h{$a} || $a cmp $b } keys %h;
  print "\n";
}

foreach my $s (qw(GGT GGTA GGTATT GGTATTTTAATT GGTATTTTAATTTATAGT)) {
  update_hash_for_frame(length($s));
  printf "%d\t$s\n", $h{$s};
}

sub update_hash_for_frame {
  my $frame = $_[0];
  $h{substr($sequence,$_,$frame)}++ foreach (0..($l - $frame));
}

#!/usr/bin/perl 
# $Id: lists.perl,v 1.1 2004-05-19 18:10:24 bfulgham Exp $
use strict;

my $SIZE = 10000;

my $ITER = $ARGV[0];
$ITER = 1 if ($ITER < 1);

my $result = 0;
while ($ITER--) {
    $result = &test_lists();
}
print "$result\n";

sub test_lists {
    # create a list of integers (Li1) from 1 to SIZE
    my @Li1 = (1..$SIZE);
    # copy the list to Li2 (not by individual items)
    my @Li2 = @Li1;
    my @Li3 = ();
    # remove each individual item from left side of Li2 and
    # append to right side of Li3 (preserving order)
    push(@Li3, shift @Li2) while (@Li2);
    # Li2 must now be empty
    # remove each individual item from right side of Li3 and
    # append to right side of Li2 (reversing list)
    push(@Li2, pop @Li3) while (@Li3);
    # Li3 must now be empty
    # reverse Li1 in place
    @Li1 = reverse @Li1;
    # check that first item is now SIZE
    return(0) if $Li1[0] != $SIZE;
    # compare Li1 and Li2 for equality
    my $len1 = scalar(@Li1);
    my $len2 = scalar(@Li2);
    my $lists_equal = ($len1 == $len2);
    return(0) if not $lists_equal;
    for my $i (0..($len1-1)) {
	if ($Li1[$i] != $Li2[$i]) {
	    $lists_equal = 0;
	    last;
	}
    }
    return(0) if not $lists_equal;
    # return the length of the list
    return($len1);
}
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# implemented by Greg Buchholz
# streamlined by Kalev Soikonen
# parallelised by Philip Boulain
# modified by Jerry D. Hedden
use warnings; use strict; use threads;

use constant ITER     => 50;
use constant LIMITSQR => 2.0 ** 2;
use constant MAXPIXEL => 524288; # Maximum pixel buffer per thread

my ($w, $h);
$w = $h = shift || 80;
my $threads = 6; # Workers; ideally slightly overshoots number of processors

# Generate pixel data for a single dot
sub dot($$) {
   my ($Zr, $Zi, $Tr, $Ti) = (0.0,0.0,0.0,0.0);
   my $i = ITER;
   my $Cr = 2 * $_[0] / $w - 1.5;
   my $Ci = 2 * $_[1] / $h - 1.0;
   (
      $Zi = 2.0 * $Zr * $Zi + $Ci,
      $Zr = $Tr - $Ti + $Cr,
      $Ti = $Zi * $Zi,
      $Tr = $Zr * $Zr
   ) until ($Tr + $Ti > LIMITSQR || !$i--);
   return ($i == -1);
}

# Generate pixel data for range of lines, inclusive
sub lines($$) {
   map { my $y = $_;
      pack 'B*', pack 'C*', map dot($_, $y), 0..$w-1;
   } $_[0]..$_[1]
}

# Decide upon roughly equal batching of workload, within buffer limits
$threads = $h if $threads > $h;
my $each = int($h / $threads);
$each = int(MAXPIXEL / $w) if ($each * $w) > MAXPIXEL;
$each = 1 if $each < 1;

# Work as long as we have lines to spawn for or threads to collect from
$| = 1;
print "P4\n$w $h\n";
my $y = 0;
my @workers;
while(@workers or ($y < $h)) {
   # Create workers up to requirement
   while((@workers < $threads) and ($y < $h)) {
      my $y2 = $y + $each;
      $y2 = $h if $y2 > $h;
      push(@workers, threads->create('lines', $y, $y2 - 1));
      $y = $y2;
   }
   # Block for result from the leading thread (to keep output in order)
   my $next = shift @workers;
   print $next->join();
}

# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# implemented by Greg Buchholz
# streamlined by Kalev Soikonen

sub ITER ()	{ 50 }
sub LIMITSQR ()	{ 2.0 ** 2 }

my ($w, $h, $i);
my ($Cr, $Ci, $Zr, $Zi, $Tr, $Ti);

sub dot {
    $Cr = 2 * $_[0] / $w - 1.5;
    $Ci = 2 * $_[1] / $h - 1.0;

    $Zr = $Zi = $Tr = $Ti = 0.0;
    $i = ITER;
    (
	$Zi = 2.0 * $Zr * $Zi + $Ci,
	$Zr = $Tr - $Ti + $Cr,
	$Ti = $Zi * $Zi,
	$Tr = $Zr * $Zr
    ) until ($Tr + $Ti > LIMITSQR || !$i--);
    return ($i == -1);
}

$w = $h = shift || 80;
print "P4\n$w $h\n";
for my $y (0..$h-1) {
    print pack 'B*', pack 'C*', map dot($_, $y), 0..$w-1;
}

#!/usr/bin/perl 
# $Id: matrix.perl,v 1.1 2004-05-19 18:10:34 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# This program based on the original from:
# "The What, Why, Who, and Where of Python" By Aaron R. Watters
# http://www.networkcomputing.com/unixworld/tutorial/005/005.html

# modified to pass rows and cols, and avoid matrix size checks
# I've sped up the original quite a bit by removing some loop
# invariants and declaring "use integer"

use strict;
use integer;

my $size = 30;

sub mkmatrix {
    my($rows, $cols) = @_;
    --$rows; --$cols;
    my $count = 1;
    my @mx = ();
    foreach (0 .. $rows) {
	my @row = ();
	$row[$_] = $count++ foreach (0 .. $cols);
	push(@mx, \@row);
    }
    return(\@mx);
}

sub mmult {
    my ($rows, $cols, $m1, $m2) = @_;
    my @m3 = ();
    --$rows; --$cols;
    for my $i (0 .. $rows) {
	my @row = ();
	my $m1i = $m1->[$i];
	for my $j (0 .. $cols) {
	    my $val = 0;
	    for my $k (0 .. $cols) {
		$val += $m1i->[$k] * $m2->[$k]->[$j];
	    }
	    push(@row, $val);
	}
	push(@m3, \@row);
    }
    return(\@m3);
}

my $N = $ARGV[0] || 1;

my $m1 = mkmatrix($size, $size);
my $m2 = mkmatrix($size, $size);
my $mm;
while ($N--) {
    $mm = mmult($size, $size, $m1, $m2);
}
print "$mm->[0]->[0] $mm->[2]->[3] $mm->[3]->[2] $mm->[4]->[4]\n";

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Sean O'Rourke

use threads;
use Thread::Queue;

my $t = 500;
my $n = shift;

my @q : shared;
@q = map { new Thread::Queue } 1..$t;

for my $i (1..$t-1) {
    (async {
        while (!$done) {
            $q[$i]->enqueue(1+$q[$i-1]->dequeue);
        }
    })->detach;
}

for (1..$n) {
    $q[0]->enqueue(0);
    $sum += $q[-1]->dequeue + 1;
}

print "$sum\n";
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by Daniel Green, 2010-04-30
# based on python 3 #3


use 5.10.1;
use warnings;
use strict;
use integer;
use List::Util qw(min);

my ($w, $h) = (5, 10);
my $dir_no = 6;
my ($S, $E) = ($w * $h, 2);
my $SE = $S + ($E / 2);
my $SW = $SE - $E;
my ($W, $NW, $NE) = (-$E, -$SE, -$SW);

my %rd = ($E => $NE, $NE => $NW, $NW => $W, $W => $SW, $SW => $SE, $SE => $E);
my %fd = ($E => $E, $NE => $SE, $NW => $SW, $W => $W, $SW => $NW, $SE => $NE);

my ($na, $nb, $nc);
my ($board, $cti, $pieces) = get_puzzle();
my @fps = get_footprints($board, $cti, $pieces);
my @se_nh = get_senh($board, $cti);

my %free = map { $_ => undef } 0 .. scalar @{$board} - 1;
my @curr_board = (-1) x scalar @{$board};
my @pieces_left = 0 .. scalar @{$pieces} - 1;
my @solutions = ();
my $needed = $ARGV[0];

solve(0, \%free, \@pieces_left);
@solutions = sort @solutions;

say scalar @solutions,  ' solutions found';
print_board($solutions[0]);
print_board($solutions[-1]);
print "\n";


sub rotate {
    return [map {$rd{$_}} @{$_[0]}];
}

sub flip {
    return [map {$fd{$_}} @{$_[0]}];
}

sub permute {
    my ($ido, $r_ido) = @_;

    my @ps = ($ido);
    for my $r (0 .. $dir_no - 2) {
        push @ps, rotate($ps[-1]);

        if (@{$ido} ~~ @{$r_ido}) {
            my $end = min(scalar @ps, int($dir_no/2));
            @ps = @ps[0 .. $end-1];
        }
    }
    
    push @ps, map { flip($_) } @ps;

    return \@ps;
}

sub convert {
    my ($ido) = @_;

    my @out = (0);
    for my $o (@{$ido}) {
        push @out, $out[-1] + $o;
    }
    
    my %unique;
    return [grep { !$unique{$_}++ } @out];
}

sub get_footprints {
    my ($bd, $ct, $ps) = @_;

    my @fp;
    foreach my $p (0 .. scalar @{$ps} - 1) {
        foreach my $ci (0 .. scalar @{$bd} - 1) {
            $fp[$ci]->[$p] = [];
        }
    }

    for my $c (@{$bd}) {
        for (my $pi = 0; $pi < scalar @{$ps}; $pi++) {
            for my $pp (@{$ps->[$pi]}) {
                my %f = ();
                for my $o (@{$pp}) {
                    if (exists $ct->{$c + $o}) {
                        $f{$ct->{$c + $o}}++;
                    }
                }

                if (scalar keys %f == 5) {
                    push @{$fp[min(keys %f)]->[$pi]}, [keys %f];
                }
            }
        }
    }
    
    return @fp;
}

sub get_senh {
    my ($bd, $ct) = @_;
    
    my @se_nh2 = ();
    for my $c (@{$bd}) {
        my %f = ();
        for my $o ($E, $SW, $SE) {
            if (exists $ct->{$c + $o}) {
                $f{$ct->{$c + $o}}++;
            }
        }
        
        push @se_nh2, \%f;
    }
    
    return @se_nh2;
}

sub get_puzzle {

    my @bd;
    for my $y (0 .. $h - 1) {
        for my $x (0 .. $w - 1) {
            push @bd, $E*$x + $S*$y + $y%2;
        }
    }

    my %ct;
    for my $i (0 .. scalar @bd - 1) {
        $ct{$bd[$i]} = $i;
    }

    my @idos = ([$E, $E, $E, $SE],
                [$SE, $SW, $W, $SW],
                [$W, $W, $SW, $SE],
                [$E, $E, $SW, $SE],
                [$NW, $W, $NW, $SE, $SW],
                [$E, $E, $NE, $W],
                [$NW, $NE, $NE, $W],
                [$NE, $SE, $E, $NE],
                [$SE, $SE, $E, $SE],
                [$E, $NW, $NW, $NW]);

    my @ps;
    for my $p (map { permute($_, $idos[3]) } @idos) {
        push @ps, [map {convert($_)} @{$p}];
    }
    
    return (\@bd, \%ct, \@ps);
}

sub print_board {
    my ($bd) = @_;

    print "\n";
    for my $y (0 .. $h - 1) {
        for my $x (0 .. $w - 1) {
            print substr($bd, $x + $y * $w, 1) . ' ';
        }
        
        print "\n";
        
        if ($y % 2 == 0) {
            print ' ';
        }
    }
}

sub solve {
    my ($i_min, $free, $pieces_left) = @_;
    
    my $fp_i_cands = $fps[$i_min];
    
    for my $p (@{$pieces_left}) {
        my $fp_cands = $fp_i_cands->[$p];
        for my $fpa (@{$fp_cands}) {
            
            $na = scalar @{$fpa};
            $nb = scalar keys %{$free};
            $nc = scalar grep { exists $free->{$_} } @{$fpa};

            if (($na == $nc) || ($na == $nc && $nb == $nc)) {
                for my $ci (@{$fpa}) {
                    $curr_board[$ci] = $p;
                }
                
                if (scalar @{$pieces_left} > 1) {
                    
                    my %fp = map { $_ => undef } @{$fpa};
                    my %n_free;
                    @n_free{ grep { !exists $fp{$_} } keys %{$free} } = ();
                    
                    my $n_i_min = min(keys %n_free);
                    if ((scalar grep { exists $se_nh[$n_i_min]->{$_} } keys %n_free) > 0) {
                        my @n_pieces_left = @{$pieces_left};
                        for (my $x = 0; $x < scalar @n_pieces_left; $x++) {
                            if ($n_pieces_left[$x] == $p) {
                                splice(@n_pieces_left, $x, 1);
                                last;
                            }
                        }
                        
                        solve($n_i_min, \%n_free, \@n_pieces_left);
                    }
                } else {
                    my $s = join('', @curr_board);
                    push @solutions, $s;
                    my $rs = reverse $s;
                    push @solutions, $rs;
                    
                    if (scalar @solutions >= $needed) {
                        return;
                    }
                }
            }
        }
        
        if (scalar @solutions >= $needed) {
            return;
        }
    }
    
    return;
}
#!/usr/bin/perl
# $Id: methcall.perl,v 1.1 2004-05-19 18:10:41 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Ben Tilly

package Toggle;

sub new {
    my($class, $start_state) = @_;
    bless( { Bool => $start_state }, $class );
}

sub value {
    (shift)->{Bool};
}

sub activate {
    my $self = shift;
    $self->{Bool} ^= 1;
    return($self);
}


package NthToggle;
our @ISA = qw(Toggle);

sub new {
    my($class, $start_state, $max_counter) = @_;
    my $self = $class->SUPER::new($start_state);
    $self->{CountMax} = $max_counter;
    $self->{Counter} = 0;
    return($self);
}

sub activate {
    my $self = shift;
    if (++$self->{Counter} >= $self->{CountMax}) {
	$self->{Bool} ^= 1;
	$self->{Counter} = 0;
    }
    return($self);
}


package main;

sub main {
    my $NUM = $ARGV[0];
    $NUM = 1 if ($NUM < 1);

    my $val = 1;
    my $toggle = Toggle->new($val);
    for (1..$NUM) {
	$val = $toggle->activate->value;
    }
    print (($val) ? "true\n" : "false\n");

    $val = 1;
    my $ntoggle = NthToggle->new($val, 3);
    for (1..$NUM) {
	$val = $ntoggle->activate->value;
    }
    print (($val) ? "true\n" : "false\n");
}

main();
#!/usr/bin/perl
# $Id: moments.perl,v 1.1 2004-05-19 18:10:48 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use strict;

my @nums = <STDIN>;
my $sum = 0;
foreach (@nums) { $sum += $_ }
my $n = scalar(@nums);
my $mean = $sum/$n;
my $average_deviation = 0;
my $standard_deviation = 0;
my $variance = 0;
my $skew = 0;
my $kurtosis = 0;
foreach (@nums) {
    my $deviation = $_ - $mean;
    $average_deviation += abs($deviation);
    $variance += $deviation**2;
    $skew += $deviation**3;
    $kurtosis += $deviation**4;
}
$average_deviation /= $n;
$variance /= ($n - 1);
$standard_deviation = sqrt($variance);

if ($variance) {
    $skew /= ($n * $variance * $standard_deviation);
    $kurtosis = $kurtosis/($n * $variance * $variance) - 3.0;
}

@nums = sort { $a <=> $b } @nums;
my $mid = int($n/2);
my $median = ($n % 2) ? $nums[$mid] : ($nums[$mid] + $nums[$mid-1])/2;

printf("n:                  %d\n", $n);
printf("median:             %f\n", $median);
printf("mean:               %f\n", $mean);
printf("average_deviation:  %f\n", $average_deviation);
printf("standard_deviation: %f\n", $standard_deviation);
printf("variance:           %f\n", $variance);
printf("skew:               %f\n", $skew);
printf("kurtosis:           %f\n", $kurtosis);
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by Christoph Bauer
# converted into Perl by Márton Papp
# fixed and cleaned up by Danny Sauer
# optimized by Jesse Millikan

use constant PI            => 3.141592653589793;
use constant SOLAR_MASS    => (4 * PI * PI);
use constant DAYS_PER_YEAR => 365.24;

#  Globals for arrays... Oh well.
#  Almost every iteration is a range, so I keep the last index rather than a count.
my (@xs, @ys, @zs, @vxs, @vys, @vzs, @mass, $last);

sub advance($)
{
  my ($dt) = @_;
  my ($mm, $mm2, $j, $dx, $dy, $dz, $distance, $mag);
  
#  This is faster in the outer loop...
  for (0..$last) {
#  But not in the inner loop. Strange.
    for ($j = $_ + 1; $j < $last + 1; $j++) {
      $dx = $xs[$_] - $xs[$j];
      $dy = $ys[$_] - $ys[$j];
      $dz = $zs[$_] - $zs[$j];
      $distance = sqrt($dx * $dx + $dy * $dy + $dz * $dz);
      $mag = $dt / ($distance * $distance * $distance);
      $mm = $mass[$_] * $mag;
      $mm2 = $mass[$j] * $mag;
      $vxs[$_] -= $dx * $mm2;
      $vxs[$j] += $dx * $mm;
      $vys[$_] -= $dy * $mm2;
      $vys[$j] += $dy * $mm;
      $vzs[$_] -= $dz * $mm2;
      $vzs[$j] += $dz * $mm;
    }  

# We're done with planet $_ at this point
# This could be done in a seperate loop, but it's slower
    $xs[$_] += $dt * $vxs[$_];
    $ys[$_] += $dt * $vys[$_];
    $zs[$_] += $dt * $vzs[$_];
  }
}

sub energy
{
  my ($e, $i, $dx, $dy, $dz, $distance);

  $e = 0.0;
  for $i (0..$last) {
    $e += 0.5 * $mass[$i] *
          ($vxs[$i] * $vxs[$i] + $vys[$i] * $vys[$i] + $vzs[$i] * $vzs[$i]);
    for ($i + 1..$last) {
      $dx = $xs[$i] - $xs[$_];
      $dy = $ys[$i] - $ys[$_];
      $dz = $zs[$i] - $zs[$_];
      $distance = sqrt($dx * $dx + $dy * $dy + $dz * $dz);
      $e -= ($mass[$i] * $mass[$_]) / $distance;
    }
  }
  return $e;
}

sub offset_momentum
{
  my ($px, $py, $pz) = (0.0, 0.0, 0.0);

  for (0..$last) {
    $px += $vxs[$_] * $mass[$_];
    $py += $vys[$_] * $mass[$_];
    $pz += $vzs[$_] * $mass[$_];
  }
  $vxs[0] = - $px / SOLAR_MASS;
  $vys[0] = - $py / SOLAR_MASS;
  $vzs[0] = - $pz / SOLAR_MASS;
}

# @ns = ( sun, jupiter, saturn, uranus, neptune )
@xs = (0, 4.84143144246472090e+00, 8.34336671824457987e+00, 1.28943695621391310e+01, 1.53796971148509165e+01);
@ys = (0, -1.16032004402742839e+00, 4.12479856412430479e+00, -1.51111514016986312e+01, -2.59193146099879641e+01);
@zs = (0, -1.03622044471123109e-01, -4.03523417114321381e-01, -2.23307578892655734e-01, 1.79258772950371181e-01);
@vxs = map {$_ * DAYS_PER_YEAR}
  (0, 1.66007664274403694e-03, -2.76742510726862411e-03, 2.96460137564761618e-03, 2.68067772490389322e-03);
@vys = map {$_ * DAYS_PER_YEAR}
  (0, 7.69901118419740425e-03, 4.99852801234917238e-03, 2.37847173959480950e-03, 1.62824170038242295e-03);
@vzs = map {$_ * DAYS_PER_YEAR} 
  (0, -6.90460016972063023e-05, 2.30417297573763929e-05, -2.96589568540237556e-05, -9.51592254519715870e-05);
@mass = map {$_ * SOLAR_MASS} 
  (1, 9.54791938424326609e-04, 2.85885980666130812e-04, 4.36624404335156298e-05, 5.15138902046611451e-05);

$last = @xs - 1;

offset_momentum();
printf ("%.9f\n", energy());

my $n = $ARGV[0];

# This does not, in fact, consume N*4 bytes of memory
for (1..$n){
  advance(0.01);
}

printf ("%.9f\n", energy());

#!/usr/bin/perl
# $Id: nestedloop.perl,v 1.1 2004-05-19 18:10:57 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use strict;

my $n = ($ARGV[0] > 0) ? $ARGV[0] : 1;
my $x = 0;
my $a = $n;
while ($a--) {
    my $b = $n;
    while ($b--) {
	my $c = $n;
	while ($c--) {
	    my $d = $n;
	    while ($d--) {
		my $e = $n;
		while ($e--) {
		    my $f = $n;
		    while ($f--) {
			$x++;
		    }
		}
	    }
	}
    }
}
print "$x\n";
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by David Pyke, March 2005
# optimized by Steffen Mueller, Sept 2007

use integer;
use strict;

sub nsieve {
   my ($m) = @_;
   my @a = (1) x $m;

   my $count = 0;
   foreach my $i (2..$m-1) {
      if ($a[$i]) {
         for (my $j = $i + $i; $j < $m; $j += $i){
            $a[$j] = 0;
         }
         ++$count;
      }
   }
   return $count;
}


sub nsieve_test {
   my($n) = @_;

   my $m = (1<<$n) * 10000;
   my $ncount= nsieve($m);
   printf "Primes up to %8u %8u\n", $m, $ncount;
}


my $N = ($ARGV[0] < 1) ? 1 : $ARGV[0];
nsieve_test($N);
nsieve_test($N-1)  if $N >= 1;
nsieve_test($N-2)  if $N >= 2;


# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by David Pyke, March 2005
# optimized by Steffen Mueller, Sept 2007
# optimized by Laimonas Vėbra, Nov 2007

use integer;
use strict;


sub nsieve {
   my ($m) = @_;
   my $i, my $j, my $a;
   my $count = 0;
   
   $a = 0 x $m;

   for($i = 2; $i < $m; $i++) {
      if (substr($a, $i, 1) ne '1') {
         for ($j = $i + $i; $j < $m; $j += $i){
            substr($a, $j, 1) = '1';
         }
         ++$count;
      }
   }
   return $count;
}


sub nsieve_test {
   my($n) = @_;

   my $m = (1<<$n) * 10000;
   my $ncount= nsieve($m);
   printf "Primes up to %8u %8u\n", $m, $ncount;
}

my $N = ($ARGV[0] < 1) ? 1 : $ARGV[0];

nsieve_test($N);
nsieve_test($N-1)  if $N >= 1;
nsieve_test($N-2)  if $N >= 2;
#!/usr/bin/perl

# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
# nsieve-bits benchmark
# contributed by Joel Hoffman, 2005-03-28

use strict;

sub nsieve {
   my $mx = shift;
   vec(my $vec,$mx,1)=0;
   my $count=0;
   for my $idx (2..$mx) {   
      unless (vec($vec,$idx,1)) {
         $count++;
         for (my $i=2 * $idx; $i<=$mx; $i+=$idx) {
            vec($vec,$i,1)=1;
         }
      }
   }
   $count;
}

sub test {
   my $n = shift;
   my $mx = 10000 * (2**$n);
   printf "Primes up to %8d %8d\n",$mx,nsieve($mx);
}

for (0,1,2) {
   if ($ARGV[0] > $_) {
      test($ARGV[0] - $_)
   }
}


#!/usr/bin/perl 
# $Id: objinst.perl,v 1.1 2004-05-19 18:11:03 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use strict;


package Toggle;

sub new {
    my($class, $start_state) = @_;
    bless( { Bool => $start_state }, $class );
}

sub value {
    my $self = shift;
    return($self->{Bool});
}

sub activate {
    my $self = shift;
    $self->{Bool} ^= 1;
    return($self);
}


package NthToggle;
@NthToggle::ISA = qw(Toggle);

sub new {
    my($class, $start_state, $max_counter) = @_;
    my $self = $class->SUPER::new($start_state);
    $self->{CountMax} = $max_counter;
    $self->{Counter} = 0;
    return($self);
}

sub activate {
    my $self = shift;
    if (++$self->{Counter} >= $self->{CountMax}) {
	$self->{Bool} ^= 1;
	$self->{Counter} = 0;
    }
    return($self);
}


package main;

sub main {
    my $NUM = ($ARGV[0] > 0) ? $ARGV[0] : 1;

    my $toggle = Toggle->new(1);
    for (1..5) {
	print (($toggle->activate->value) ? "true\n" : "false\n");
    }
    for (1..$NUM) {
	$toggle = Toggle->new(1);
    }

    print "\n";

    my $ntoggle = NthToggle->new(1, 3);
    for (1..8) {
	print (($ntoggle->activate->value) ? "true\n" : "false\n");
    }
    for (1..$NUM) {
	$ntoggle = NthToggle->new(1, 3);
    }
}

main();

# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# Contributed by Emanuele Zeppieri

my $N = shift || 2_500_000;

my ($s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $s8, $k2, $k3, $ksin, $kcos);
my $alt = -1;
sub TWO_THIRDS () { 2/3 }

for (1..$N) {
    $s0 += TWO_THIRDS ** ($_ - 1);
    $s1 += 1 / sqrt;
    $s2 += 1 / ( $_ * ($_ + 1));
    $s3 += 1 / ( ($k3 = ($k2 = $_ * $_) * $_) * ($ksin = sin) * $ksin );
    $s4 += 1 / ( $k3 * ($kcos = cos) * $kcos );
    $s5 += 1 / $_;
    $s6 += 1 / $k2;
    $s7 += ($alt = -$alt) / $_;
    $s8 += $alt / (2 * $_ - 1)
}

printf "%0.9f\t(2/3)^k\n"             , $s0;
printf "%0.9f\tk^-0.5\n"              , $s1;
printf "%0.9f\t1/k(k+1)\n"            , $s2;
printf "%0.9f\tFlint Hills\n"         , $s3;
printf "%0.9f\tCookson Hills\n"       , $s4;
printf "%0.9f\tHarmonic\n"            , $s5;
printf "%0.9f\tRiemann Zeta\n"        , $s6;
printf "%0.9f\tAlternating Harmonic\n", $s7;
printf "%0.9f\tGregory\n"             , $s8
# The Computer Language Shootout
#   http://shootout.alioth.debian.org/
#
#   contributed by Robert Bradshaw
#      modified by Ruud H.G.van Tol
#      modified by Emanuele Zeppieri

use strict;

use Math::BigInt lib => 'GMP';

die 'Math::BigInt::GMP missing!'
    if Math::BigInt->config->{lib} ne 'Math::BigInt::GMP';

my $z0 = Math::BigInt->new(1);
my $z1 = Math::BigInt->new(0);
my $z2 = Math::BigInt->new(1);

sub extract_digit { return scalar( ($z0 * $_[0] + $z1) / $z2 ) }

sub compose {
    if ( defined $_[3] ) {
        $z1->bmul( $_[0] )->badd( $_[1] * $z2 )
    } else {
        $z1->bmul( $_[2] )->badd( $_[1] * $z0 )
    }
    $z0->bmul( $_[0] );
    $z2->bmul( $_[2] );
    return
}

my $n = $ARGV[0];
($,, $\) = ("\t", "\n");
my ($i, $s, $d); my $k = 0;

# main loop
for $i (1..$n) {
    while (
        $z0->bcmp($z2) == 1 || ( $d = extract_digit(3) ) != extract_digit(4)
    ) {
        # y not safe
        $k++; compose($k, 4*$k+2, 2*$k+1)
    }
    compose(10, -10*$d, 1, 1);
    $s .= $d;

    unless ( $i % 10 ) { print $s, ":$i"; undef $s }
}

$s .= ' ' x (10-$i) if $i = $n % 10;

print $s, ":$n" if $s
# The Computer Language Benchmarks Game
#   http://shootout.alioth.debian.org/
#
#   contributed by Robert Bradshaw
#   modified by Ruud H.G.van Tol
#   modified by Emanuele Zeppieri
#   modified to use Math:GMP by Kuang-che Wu

use strict;
use Math::GMP;

my($z0, $z1, $z2) = map Math::GMP->new($_),1,0,1;

sub extract_digit { return ($z0*$_[0]+$z1)/$z2; }

sub compose {
    if ( defined $_[3] ) {
        $z1 = $z1*$_[0]+$_[1]*$z2;
    } else {
        $z1 = $z1*$_[2]+$_[1]*$z0;
    }
    $z0 = $z0*$_[0];
    $z2 = $z2*$_[2];
    return;
}

my $n = $ARGV[0];
($,, $\) = ("\t", "\n");
my ($i, $s, $d); my $k = 0;

# main loop
for $i (1..$n) {
    while (
        $z0>$z2 || ( $d = extract_digit(3) ) != extract_digit(4)
    ) {
        # y not safe
        $k++; compose($k, 4*$k+2, 2*$k+1)
    }
    compose(10, -10*$d, 1, 1);
    $s .= $d;

    unless ( $i % 10 ) { print $s, ":$i"; undef $s }
}

$s .= ' ' x (10-$i) if $i = $n % 10;

print $s, ":$n" if $s
#!/usr/bin/perl

# perl thread benchmark for The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
# Contributed by Steve Clark

# perl threads creates an interpreter per thread so this won't
# be pretty.
use threads;

# The thread code blocks til it gets a message, then sends it
# Argument is reference to last thread
sub inc_thread {
    my $thr = shift;
    my ($num) = $thr->join;
    $num++;
    return ($num);
}

# special thread to kick off the chain
sub zero_thread {
    return (0);
}


# Algorithm:
#   Create n threads from 1 to n
#   Each thread x has ref to thread x-1
#   Creates extra zero_thread to send 0 to start of chain
#   prints return of last thread created

sub dothread {
    my $n = shift;
    my $thread = threads->new(\&zero_thread);

    for ($i = 1; $i <= $n; $i++) {
	$thread = threads->new(\&inc_thread, $thread);
    }

    # Now wait for end
    my $num = $thread->join;

    # print the result
    print "$num\n";
}


my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);
dothread ($NUM);
#!/usr/bin/perl
# $Id: prodcons.perl,v 1.3 2005-05-13 16:24:18 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/ 

use strict;
use Thread qw(cond_wait cond_signal);

my $count = 0;
my $data = 0;
my $produced = 0;
my $consumed = 0;

sub consumer {
    my $n = shift;
    while (1) {
	lock($count);
	cond_wait($count) while ($count == 0);
	my $i = $data;
	$count = 0;
	$consumed++;
	last if ($i == $n);
	cond_signal($count);
    }
}

sub producer {
    my $n = shift;
    for (my $i=1; $i<=$n; $i++) {
	lock($count);
	cond_wait($count) while ($count == 1);
	$data = $i;
	$count = 1;
	$produced++;
	cond_signal($count);
    }
}

sub main {
    my $n = ($ARGV[0] < 1) ? 1 : $ARGV[0];
    my $p = Thread->new(\&producer, $n);
    my $c = Thread->new(\&consumer, $n);
    $p->join;
    $c->join;
    print "$produced $consumed\n";
}

&main();
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# Contributed by Kjetil Skotheim

use constant {IM => 139968, IA => 3877, IC => 29573};
my $LAST=42;

sub gen_random {
  my ($n,$max) = @_;
  use integer;
  $LAST = ($LAST * IA + IC) % IM for 1..$n;
  no integer;
  return $max * $LAST / IM;
}

printf "%.9f\n", gen_random($ARGV[0] || 1, 100.0);

# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# recursive test, by Andreas Koenig, Sep 24 2006

### Uses temp variables to help perl free memory earlier

use strict;

sub Ack
{
	my ($x, $y) = @_;

	return $y + 1         if $x == 0;
	return Ack($x - 1, 1) if $y == 0;

        my $y2 = Ack($x, $y - 1);
	my $ret = Ack($x - 1, $y2);
        return $ret;
}

sub Fib
{
	my ($n) = @_;

	return 1 if $n < 2;

        my $f1 = Fib($n - 1);
        my $f2 = Fib($n - 2);
	return $f2 + $f1;
}

sub Tak
{
	my ($x, $y, $z) = @_;

        if ($y < $x) {
          my $z1 = Tak($x - 1.0, $y, $z);
          my $z2 = Tak($y - 1.0, $z, $x);
          my $z3 = Tak($z - 1.0, $x, $y);
          my $ret = Tak($z1, $z2, $z3);
          return $ret;
        } else {
          return $z;
        }
}

my $n = ($ARGV[0] || 0) - 1;
printf "Ack(%d,%d): %d\n",
	3, $n + 1, Ack(3, $n + 1);
printf "Fib(%.1f): %.1f\n",
	28.0 + $n, Fib(28.0 + $n);
printf "Tak(%d,%d,%d): %d\n",
	$n * 3, $n * 2, $n, Tak($n * 3, $n * 2, $n);
printf "Fib(%d): %d\n",
	3, Fib(3);
printf "Tak(%.1f,%.1f,%.1f): %.1f\n",
	3.0,2.0,1.0, Tak(3.0,2.0,1.0);
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Danny Sauer
# completely rewritten and
# cleaned up for speed and fun by Mirco Wahab
# improved STDIN read, regex clean up by Jake Berner
# More speed and multithreading by Andrew Rodland

use strict;
use warnings;

my $l_file  = -s STDIN;
my $content; read STDIN, $content, $l_file;
# this is significantly faster than using <> in this case

$content =~ s/^>.*//mg;
$content =~ tr/\n//d;
my $l_code  =  length $content;

my @seq = ( 'agggtaaa|tttaccct',
        '[cgt]gggtaaa|tttaccc[acg]',
        'a[act]ggtaaa|tttacc[agt]t',
        'ag[act]gtaaa|tttac[agt]ct',
        'agg[act]taaa|ttta[agt]cct',
        'aggg[acg]aaa|ttt[cgt]ccct',
        'agggt[cgt]aa|tt[acg]accct',
        'agggta[cgt]a|t[acg]taccct',
        'agggtaa[cgt]|[acg]ttaccct' );

my @procs;
for my $s (@seq) {
  my $pat = qr/$s/;
  my $pid = open my $fh, '-|';
  defined $pid or die "Error creating process";
  unless ($pid) {
    my $cnt = 0;
    ++$cnt while $content =~ /$pat/gi;
    print "$s $cnt\n";
    exit 0;
  }
  push @procs, $fh;
}

for my $proc (@procs) {
  print <$proc>;
  close $proc;
}

my %iub = (         B => '(c|g|t)',  D => '(a|g|t)',
  H => '(a|c|t)',   K => '(g|t)',    M => '(a|c)',
  N => '(a|c|g|t)', R => '(a|g)',    S => '(c|g)',
  V => '(a|c|g)',   W => '(a|t)',    Y => '(c|t)' );

# We could cheat here by using $& in the subst and doing it inside a string
# eval to "hide" the fact that we're using $& from the rest of the code... but
# it's only worth 0.4 seconds on my machine.
my $findiub = '(['.(join '', keys %iub).'])';

$content =~ s/$findiub/$iub{$1}/g;

printf "\n%d\n%d\n%d\n", $l_file, $l_code, length $content;
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Danny Sauer
# completely rewritten and
# cleaned up for speed and fun by Mirco Wahab
# improved STDIN read, regex clean up by Jake Berner

use strict;
use warnings;

my $l_file  = -s STDIN;
my $content; read STDIN, $content, $l_file;
# this is significantly faster than using <> in this case

my $dispose =  qr/(^>.*)?\n/m; # slight performance gain here
   $content =~ s/$dispose//g;
my $l_code  =  length $content;

my @seq = ( 'agggtaaa|tttaccct',
        '[cgt]gggtaaa|tttaccc[acg]',
        'a[act]ggtaaa|tttacc[agt]t',
        'ag[act]gtaaa|tttac[agt]ct',
        'agg[act]taaa|ttta[agt]cct',
        'aggg[acg]aaa|ttt[cgt]ccct',
        'agggt[cgt]aa|tt[acg]accct',
        'agggta[cgt]a|t[acg]taccct',
        'agggtaa[cgt]|[acg]ttaccct' );

my @cnt = (0) x @seq;
for my $k (0..$#seq) {
  ++$cnt[$k] while $content=~/$seq[$k]/gi;
  printf "$seq[$k] $cnt[$k]\n"
}

my %iub = (         B => '(c|g|t)',  D => '(a|g|t)',
  H => '(a|c|t)',   K => '(g|t)',    M => '(a|c)',
  N => '(a|c|g|t)', R => '(a|g)',    S => '(c|g)',
  V => '(a|c|g)',   W => '(a|t)',    Y => '(c|t)' );

# using $& and no submatch marginally improves the
# speed here, but mentioning $& causes perl to 
# define that value for the @seq patterns too, which
# slows those down considerably. No change.

my $findiub = '(['.(join '', keys %iub).'])';

$content =~ s/$findiub/$iub{$1}/g;

printf "\n%d\n%d\n%d\n", $l_file, $l_code, length $content;
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Danny Sauer
# modified by Mirco Wahab
# modified by Steffen Mueller
# modified by Kuang-che Wu

use strict;
use warnings;

my $content =  do { local $/; <STDIN> };
my $l_file  =  length $content;
$content =~ s/^>.*$//mg;
$content =~ s/\n//g;
my $l_code  =  length $content;

my @seq = ( 'agggtaaa|tttaccct',
        '[cgt]gggtaaa|tttaccc[acg]',
        'a[act]ggtaaa|tttacc[agt]t',
        'ag[act]gtaaa|tttac[agt]ct',
        'agg[act]taaa|ttta[agt]cct',
        'aggg[acg]aaa|ttt[cgt]ccct',
        'agggt[cgt]aa|tt[acg]accct',
        'agggta[cgt]a|t[acg]taccct',
        'agggtaa[cgt]|[acg]ttaccct' );

my @cnt = (0) x @seq;
for (0..$#seq) {
  my ($l, $r) = map {qr/$_/} split /\|/, $seq[$_];
  $cnt[$_] += (() = ($content=~/$l/gi, $content=~/$r/gi));
  print $seq[$_], ' ', $cnt[$_], "\n";
}

my %iub = (         B => '(c|g|t)',  D => '(a|g|t)',
  H => '(a|c|t)',   K => '(g|t)',    M => '(a|c)',
  N => '(a|c|g|t)', R => '(a|g)',    S => '(c|g)',
  V => '(a|c|g)',   W => '(a|t)',    Y => '(c|t)' );

my $findiub = '(['.(join '', keys %iub).'])';

$content =~ s/$findiub/$iub{$1}/g;
print "\n", $l_file, "\n", $l_code, "\n", length($content), "\n";

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Danny Sauer
# completely rewritten and cleaned up for speed and fun by Mirco Wahab
# improved STDIN read, regex clean up by Jake Berner
# more speed and multithreading by Andrew Rodland
# moved alternation out of the regexes into the program logic for speed by Daniel Green

use strict;
use warnings;

my $l_file  = -s STDIN;
my $content; read STDIN, $content, $l_file;
# this is significantly faster than using <> in this case

$content =~ s/^>.*//mg;
$content =~ tr/\n//d;
my $l_code  =  length $content;

my @seq = ( ['agggtaaa', 'tttaccct'],
        ['[cgt]gggtaaa', 'tttaccc[acg]'],
        ['a[act]ggtaaa', 'tttacc[agt]t'],
        ['ag[act]gtaaa', 'tttac[agt]ct'],
        ['agg[act]taaa', 'ttta[agt]cct'],
        ['aggg[acg]aaa', 'ttt[cgt]ccct'],
        ['agggt[cgt]aa', 'tt[acg]accct'],
        ['agggta[cgt]a', 't[acg]taccct'],
        ['agggtaa[cgt]', '[acg]ttaccct'] );

my @procs;
for my $s (@seq) {
  my ($pat_l, $pat_r) = (qr/$s->[0]/, qr/$s->[1]/);
  my $pid = open my $fh, '-|';
  defined $pid or die "Error creating process";
  unless ($pid) {
    my $cnt = 0;
    ++$cnt while $content =~ /$pat_l/gi;
    ++$cnt while $content =~ /$pat_r/gi;
    print "$s->[0]|$s->[1] $cnt\n";
    exit 0;
  }
  push @procs, $fh;
}

for my $proc (@procs) {
  print <$proc>;
  close $proc;
}

my %iub = (         B => '(c|g|t)',  D => '(a|g|t)',
  H => '(a|c|t)',   K => '(g|t)',    M => '(a|c)',
  N => '(a|c|g|t)', R => '(a|g)',    S => '(c|g)',
  V => '(a|c|g)',   W => '(a|t)',    Y => '(c|t)' );

# We could cheat here by using $& in the subst and doing it inside a string
# eval to "hide" the fact that we're using $& from the rest of the code... but
# it's only worth 0.4 seconds on my machine.
my $findiub = '(['.(join '', keys %iub).'])';

$content =~ s/$findiub/$iub{$1}/g;

printf "\n%d\n%d\n%d\n", $l_file, $l_code, length $content;
#!/usr/bin/perl 
# $Id: regexmatch.perl,v 1.1 2004-05-19 18:11:25 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use strict;

my $re = qr{
    (?: ^ | [^\d\(])		# must be preceeded by non-digit
    ( \( )?			# match 1: possible initial left paren
    (\d\d\d)			# match 2: area code is 3 digits
    (?(1) \) )			# if match1 then match right paren
    [ ]				# area code followed by one space
    (\d\d\d)			# match 3: prefix of 3 digits
    [ -]			# separator is either space or dash
    (\d\d\d\d)			# match 4: last 4 digits
    \D				# must be followed by a non-digit
}x;

my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);

my @phones = <STDIN>;
my $count = 0;
my $num;
while ($NUM--) {
    foreach (@phones) {
	if (/$re/o) {
	    $num = "($2) $3-$4";
	    if (0 == $NUM) {
		$count++;
		print "$count: $num\n";
	    }
	}
    }
}
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Bradford Powell
# Fixed slow print substr-solution, by Kjetil Skotheim
# Changed input reading method and avoid a sub call by Bruno Vecchi

use strict;
use feature 'say';

local $/ = ">";
while (my $entry = <STDIN>) {
    chomp $entry;

    my ($header, $seq) = split /\n/, $entry, 2;
    next unless $header;

    {
        local $/ = "\n";
        say ">", $header;

        $seq =~ s/\n//g;
        $seq =  reverse $seq;
        $seq =~ tr{wsatugcyrkmbdhvnATUGCYRKMBDHV}
                  {WSTAACGRYMKVHDBNTAACGRYMKVHDB};

        my $lines   = length($seq)/60;
        my $current = 0;

        say substr($seq, $current++*60, 60)
            while $current < $lines;
    }
}
#!/usr/bin/perl

# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Bradford Powell
# Fixed slow print substr-solution, by Kjetil Skotheim


use strict;

sub print_revcomp {
    my ($desc, $s) = @_;
    return if not $desc;
    print $desc, "\n";
    $s =  reverse $s;
    $s =~ tr{wsatugcyrkmbdhvnATUGCYRKMBDHVN}
            {WSTAACGRYMKVHDBNTAACGRYMKVHDBN};
    my($i,$stop)=(0,length($s)/60);
    print substr($s,$i++*60,60),"\n"  while $i<$stop;
}

my($desc,$seq);
while (<STDIN>) {
    chomp;
    if (/^>/) {
        print_revcomp($desc, $seq);
        $desc = $_;
        $seq = '';
    } else {
        $seq .= $_;
    }
}
print_revcomp($desc, $seq);
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Andrew Rodland

use strict;

sub print_reverse {
  while (my $chunk = substr $_[0], -60, 60, '') {
    print scalar reverse($chunk), "\n";
  }
}

my $data;

while (<STDIN>) {
  if (/^>/) {
    print_reverse $data;
    print;
  } else {
    chomp;
    tr{wsatugcyrkmbdhvnATUGCYRKMBDHVN}
      {WSTAACGRYMKVHDBNTAACGRYMKVHDBN};
    $data .= $_;
  }
}
print_reverse $data;
#!/usr/bin/perl
# $Id: reversefile.perl,v 1.2 2004-11-23 08:08:45 bfulgham Exp $
# http://shootout.alioth.debian.org/
# Revised by Soren Morton

undef($/);
print reverse( split(/^/, <STDIN>));
#print join("\n", reverse split(/\n/, <STDIN>)),"\n";
#!/usr/bin/perl 
# $Id: sieve.perl,v 1.1 2004-05-19 18:12:27 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Tony Bowden

use strict;
use integer;

my $NUM = ($ARGV[0] < 1) ? 1 : $ARGV[0];
my $count;
my @flags = ();
while ($NUM--) {
    $count = 0; 
    my @flags = (0 .. 8192);
    for my $i (2 .. 8192 ) {
	next unless defined $flags[$i];
	# remove all multiples of prime: i
	my $k = $i;
	undef $flags[$k] while (($k+=$i) < 8193);
	$count++;
    }
}
print "Count: $count\n";
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Markus Peter
# Modified by Daniel Green

use warnings;
use strict;

sub A {
    return 1.0 / ( ( $_[0] + $_[1] ) * ( $_[0] + $_[1] + 1 ) / 2 + $_[0] + 1 );
}

sub Av {
    my ( $u ) = @_;
    my $len = $#$u;
    my @v = ( 0 ) x ( $len+1 );
    for my $i ( 0..$len ) {
        for my $j ( 0..$len ) {
            $v[$i] += A( $i, $j ) * $u->[$j];
        }
    }
    return \@v;
}

sub Atv {
    my ( $u ) = @_;
    my $len = $#$u;
    my @v = ( 0 ) x ( $len+1 );
    for my $i ( 0..$len ) {
        for my $j ( 0..$len ) {
            $v[$i] += A( $j, $i ) * $u->[$j];
        }
    }
    return \@v;
}

sub AtAv {
    return Atv( Av( $_[0] ) );
}

my $N = @ARGV ? $ARGV[0] : 500;

my $v;
my $u = [ ( 1 ) x $N ];
for my $i ( 0..9 ) {
    $v = AtAv( $u );
    $u = AtAv( $v );
}

my ($vBv, $vv) = (0, 0);
for my $i ( 0..$N-1 ) {
    $vBv += $u->[$i] * $v->[$i];
    $vv += $v->[$i] ** 2;
}
printf( "%0.9f\n", sqrt( $vBv / $vv ) );

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Andrew Rodland

use strict;

sub eval_A {
  use integer;
  my $div = ( ($_[0] + $_[1]) * ($_[0] + $_[1] + 1) / 2) + $_[0] + 1;
  no integer;
  1 / $div;
}

sub multiplyAv {
  return map {
    my ($i, $sum) = ($_);
    $sum += eval_A($i, $_) * $_[$_] for 0 .. $#_;
    $sum;
  } 0 .. $#_;
}

sub multiplyAtv {
  return map {
    my ($i, $sum) = ($_);
    $sum += eval_A($_, $i) * $_[$_] for 0 .. $#_;
    $sum;
  } 0 .. $#_;
}

sub multiplyAtAv {
  return multiplyAtv( multiplyAv( @_ ) );
}

my $n = @ARGV ? shift : 500;
my @u = (1) x $n;
my @v;
for (0 .. 9) {
  @v = multiplyAtAv( @u );
  @u = multiplyAtAv( @v );
}

my ($vBv, $vv);
for my $i (0 .. $#u) {
  $vBv += $u[$i] * $v[$i];
  $vv += $v[$i] ** 2;
}

printf( "%0.9f\n", sqrt( $vBv / $vv ) );

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Andrew Rodland

use strict;
use IO::Select;

our ($n, $size_of_float, $threads, @ranges, $begin, $end);

sub eval_A {
  use integer;
  my $div = ( ($_[0] + $_[1]) * ($_[0] + $_[1] + 1) / 2) + $_[0] + 1;
  no integer;
  1 / $div;
}

sub multiplyAv {
  return map {
    my ($i, $sum) = ($_);
    $sum += eval_A($i, $_) * $_[$_] for 0 .. $#_;
    $sum;
  } $begin .. $end;
}

sub multiplyAtv {
  return map {
    my ($i, $sum) = ($_);
    $sum += eval_A($_, $i) * $_[$_] for 0 .. $#_;
    $sum;
  } $begin .. $end;
}

sub do_parallel {
  my $func = shift;

  my @out;
  my (@fd, @ptr, %fh2proc);
  for my $proc (0 .. $threads - 1) {
    ($begin, $end) = @{ $ranges[$proc] };
    my $pid = open $fd[$proc], "-|";
    if ($pid == 0) {
      print pack "F*", $func->( @_ );
      exit 0;
    } else {
      $fh2proc{ $fd[$proc] } = $proc;
      $ptr[$proc] = $begin;
    }
  }

  my $select = IO::Select->new(@fd);

  while ($select->count) {
    for my $fh ($select->can_read) {
      my $proc = $fh2proc{$fh};
      while (read $fh, my $data, $size_of_float) {
        $out[ $ptr[$proc] ++ ] = unpack "F", $data;
      }
      $select->remove($fh) if eof($fh);
    }
  }

  return @out;
}

sub multiplyAtAv {
  my @array = do_parallel(\&multiplyAv, @_);
  return do_parallel(\&multiplyAtv, @array);
}

sub num_cpus {
  open my $fh, '</proc/cpuinfo' or return;
  my $cpus;
  while (<$fh>) {
    $cpus ++ if /^processor\s+:/;
  }
  return $cpus;
}

sub init {
  $size_of_float = length pack "F", 0;

  $n = @ARGV ? $ARGV[0] : 500;
  $threads = num_cpus() || 1;

  if ($threads > $n) {
    $threads = $n;
  }

  for my $i (0 .. $threads - 1) {
    use integer;
    $ranges[$i][0] = $n * $i / $threads;
    $ranges[$i][1] = $n * ($i + 1) / $threads - 1;
    no integer;
  }
}

init();

my @u = (1) x $n;
my @v;
for (0 .. 9) {
  @v = multiplyAtAv( @u );
  @u = multiplyAtAv( @v );
}

my ($vBv, $vv);
for my $i (0 .. $#u) {
  $vBv += $u[$i] * $v[$i];
  $vv += $v[$i] ** 2;
}

printf( "%0.9f\n", sqrt( $vBv / $vv ) );

#!/usr/bin/perl
# $Id: spellcheck.perl,v 1.2 2004-07-31 09:19:06 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# 
# Updated per suggestions by Alan Post

use strict;

# read dictionary
open(DICT, "<Usr.Dict.Words") or
    die "Error, unable to open Usr.Dict.Words\n";

my %dict;
while (<DICT>) {
    chomp;
    $dict{$_} = undef;
}
close(DICT);

$\ = "\n";
while (<STDIN>) {
    chomp;
    print unless exists $dict{$_};
}
#!/usr/bin/perl 
# $Id: strcat.perl,v 1.1 2004-05-19 18:13:35 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use strict;

my $NUM = $ARGV[0];
$NUM = 1 if ($NUM < 1);

my $str = "";
$str .= "hello\n" foreach (1..$NUM);
print length($str),"\n";

#!/usr/bin/perl
# $Id: sumcol.perl,v 1.1 2004-05-19 18:13:44 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

use integer;
shift;
while (<>) { $tot += $_ }
print "$tot\n";
#!/usr/bin/perl -w 
# http://shootout.alioth.debian.org
# 
# Perl dies from lack of memory while computing the recursive
# tak function.  So here's a version that can compute the benchmark
# in constant time.
#
# by Greg Buchholz


$n=shift;
printf "%.1f\n", tak($n);

sub tak
{   
    my $z=shift;

    return $z   if($z<0);
    return 2*$z if(int($z)==$z && $z%2);
    return $z+1 if(int($z)==$z && !($z%2));
    return 2*$z if(!(int($z)%2));
    return int($z)+2*($z-int($z));
}
#!/usr/bin/perl
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# by Greg Buchholz
# memory consumption fixed by Danny Sauer

my $n = $ARGV[0];
sub takfp($$$);

printf "%.1f\n", takfp(3*$n, 2*$n, $n);

sub takfp($$$)
{
   return ($_[1] >= $_[0]) ? 
           $_[2] : 
           takfp( takfp($_[0]-1.0, $_[1], $_[2]),
                  takfp($_[1]-1.0, $_[2], $_[0]),
                  takfp($_[2]-1.0, $_[0], $_[1])
              )
}
#!/usr/bin/perl

# The Great Computer Language Shootout
#  http://shootout.alioth.debian.org/
#
#   contributed by John O'Hare 03 August 2005.

use strict;
use warnings;
use Socket;

use constant PORT_NUMBER => 	12330;
use constant M => 		6400;
use constant REPLY_SIZE => 	64;
use constant REQUEST_SIZE => 	64;

my $n = 1;
$n = $ARGV[0] if (defined($ARGV[0]));
$n *= M;

unless (fork) { #client
	sleep 2; #wait for the server to start

	my $cbuf;
	my $request = chr(60)x REQUEST_SIZE;
	my $bytes = 0;
	my $replies = 0;

	socket(CSOCK, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die $!;
	connect(CSOCK, sockaddr_in(PORT_NUMBER, INADDR_LOOPBACK)) or die $!;

	while ($n--) {
		my $tmpbytes = 0;
		defined(send(CSOCK, $request, 0)) or die $!;
		while (($tmpbytes += sysread(CSOCK, $cbuf, REPLY_SIZE)) < REPLY_SIZE) {}
		$bytes += $tmpbytes;
		$replies++;
	}

	shutdown(CSOCK, 2);
	print "replies: $replies\tbytes: $bytes\n";
	exit(0);
}

#server
my $reply = chr(62)x REPLY_SIZE;
my $sbuf;

socket(SSOCK, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die $!;
bind (SSOCK, sockaddr_in(PORT_NUMBER, INADDR_LOOPBACK)) || die $!;

listen (SSOCK, 1);

accept (CONN, SSOCK) || die $!;

while (sysread(CONN, $sbuf, REQUEST_SIZE)) {
	defined(send(CONN, $reply, 0)) or die $!;
}

shutdown(CONN, 2);
shutdown(SSOCK, 2);
#!/usr/bin/perl

# The Great Computer Language Shootout
#  http://shootout.alioth.debian.org/
#
#   contributed by John O'Hare 03 August 2005.

use strict;
use warnings;
use Socket;

use constant PORT_NUMBER => 	12331;
use constant M => 		100;
use constant REPLY_SIZE => 	4096;
use constant REQUEST_SIZE => 	64;

my $n = 1;
$n = $ARGV[0] if (defined($ARGV[0]));
$n *= M;

unless (fork) { #client
	sleep 2; #wait for the server to start

	my $cbuf;
	my $request = chr(60)x REQUEST_SIZE;
	my $bytes = 0;
	my $replies = 0;

	socket(CSOCK, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die $!;
	connect(CSOCK, sockaddr_in(PORT_NUMBER, INADDR_LOOPBACK)) or die $!;

	while ($n--) {
		my $tmpbytes = 0;
		defined(send(CSOCK, $request, 0)) or die $!;
		while (($tmpbytes += sysread(CSOCK, $cbuf, REPLY_SIZE)) < REPLY_SIZE) {}
		$bytes += $tmpbytes;
		$replies++;
	}

	shutdown(CSOCK, 2);
	print "replies: $replies\tbytes: $bytes\n";
	exit(0);
}

#server
my $reply = chr(62)x REPLY_SIZE;
my $sbuf;

socket(SSOCK, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die $!;
bind (SSOCK, sockaddr_in(PORT_NUMBER, INADDR_LOOPBACK)) || die $!;

listen (SSOCK, 1);

accept (CONN, SSOCK) || die $!;

while (sysread(CONN, $sbuf, REQUEST_SIZE)) {
	defined(send(CONN, $reply, 0)) or die $!;
}

shutdown(CONN, 2);
shutdown(SSOCK, 2);
#!/usr/bin/perl

# The Great Computer Language Shootout
#  http://shootout.alioth.debian.org/
#
#   contributed by John O'Hare 03 August 2005.

use strict;
use warnings;
use Socket;

use constant PORT_NUMBER => 	12332;
use constant M => 		1;
use constant REPLY_SIZE => 	409600;
use constant REQUEST_SIZE => 	64;

my $n = 1;
$n = $ARGV[0] if (defined($ARGV[0]));
$n *= M;

unless (fork) { #client
	sleep 2; #wait for the server to start

	my $cbuf;
	my $request = chr(60)x REQUEST_SIZE;
	my $bytes = 0;
	my $replies = 0;

	socket(CSOCK, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die $!;
	connect(CSOCK, sockaddr_in(PORT_NUMBER, INADDR_LOOPBACK)) or die $!;

	while ($n--) {
		my $tmpbytes = 0;
		defined(send(CSOCK, $request, 0)) or die $!;
		while (($tmpbytes += sysread(CSOCK, $cbuf, REPLY_SIZE)) < REPLY_SIZE) {}
		$bytes += $tmpbytes;
		$replies++;
	}

	shutdown(CSOCK, 2);
	print "replies: $replies\tbytes: $bytes\n";
	exit(0);
}

#server
my $reply = chr(62)x REPLY_SIZE;
my $sbuf;

socket(SSOCK, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die $!;
bind (SSOCK, sockaddr_in(PORT_NUMBER, INADDR_LOOPBACK)) || die $!;

listen (SSOCK, 1);

accept (CONN, SSOCK) || die $!;

while (sysread(CONN, $sbuf, REQUEST_SIZE)) {
	defined(send(CONN, $reply, 0)) or die $!;
}

shutdown(CONN, 2);
shutdown(SSOCK, 2);
#  The Computer Language Benchmarks Game
#  http://shootout.alioth.debian.org/

#  contributed by Richard Cottrill


use strict;
use warnings;
use threads;
use threads::shared;
use Thread::Semaphore;

my $numThreads	:shared;
my $data	:shared;
my $result	:shared;
my @mutex	:shared;

$numThreads = 503;

sub thr_func {
  my ($nextThread, $thr_name);
  $thr_name = threads->tid();
  threads->detach();
  if ($thr_name == $numThreads) {
    $nextThread = 1;
  }
  else {
    $nextThread = $thr_name + 1;
  }
  while (1) {
    $mutex[$thr_name]->down();
    if ($data) {
      $data = --$data;
      $mutex[$nextThread]->up();
    }
    else {
      $result = $thr_name;
      $mutex[0]->up();
    }
  } 
}

$data = $ARGV[0];

$mutex[0] = new Thread::Semaphore(0);
{
  for (1 .. $numThreads) {
    $mutex[$_] = new Thread::Semaphore(0);
    threads->create(\&thr_func);
  }
}
$mutex[1]->up();
$mutex[0]->down();
print "$result\n";
exit(0);
# The Computer Language Benchmarks Game
#   http://shootout.alioth.debian.org/
#  contributed by Peter Corlett 

# This is really more a classic fork() and Unix IPC implementation, but it
# uses threads purely to satisfy the rules of the game. This makes it quite
# nippy as it doesn't have to worry about any sort of locking because we
# essentially have 503 independent processes that just happen to share an
# address space.
#
# Almost all of the time appears to be consumed by the thread library doing
# all the deep copying required to create a clone and then tearing it down
# afterwards. A fork() implementation is thus likely to be very fast as it'd
# use copy-on-write pages in the kernel.
#
# As a minor aside, IO::Pipe wasn't used here because it expects one to fork()
# and use ->reader and ->writer in different processes to set which side of
# the pipe the IO::Pipe object will now refer to.
#
# It requires at least perl 5.10.0, although it could be easily rewritten to
# use an earlier version.

use 5.010;
use warnings;
use strict;
use threads;
use IO::Handle; # for autoflush

use constant THREADS => 503;
# stack size may need tuning for your arch, default of 8MB is likely to not
# work well on 32 bit systems or those with limited memory.
use constant THREAD_STACK_SIZE => 512 * 1024;

my $passes = shift;
die "Usage: $0 [passes]\n"
  unless defined $passes && int($passes) > 0;
$passes = int($passes);

my(@pipes, @threads);

@pipes = map {
  pipe my($r, $w) or die "pipe() failed";
  { read => $r, write => $w }
} (0 .. THREADS-1);

@threads = map {
  my $in = $pipes[$_]{read};
  $in->autoflush;
  my $out = $pipes[($_ + 1) % THREADS]{write};
  $out->autoflush;
  my $thread_id = $_ + 1;
  threads->create
    ({ stack_size => THREAD_STACK_SIZE, },
     sub {	     # $in, $out and $thread_id are captured in this closure
       while(my $msg = <$in>) { # receive message
	 chomp $msg;
	 if($msg eq 'EXIT') {	# asked to exit
	   last;
	 } elsif($msg > 0) {	# still work to do
	   say $out --$msg;	# send message
	 } else {		# no more work to do
	   say $thread_id;	# output result
	   # tell all threads to exit
	   say $_ 'EXIT' foreach map { $_->{write} } @pipes;
	   last;
	 }
       }
     });
} (0 .. THREADS-1);

# inject initial message
my $start_fh = $pipes[0]{write};
say $start_fh $passes;

# collect exited threads
$_->join foreach @threads;

#!/usr/bin/perl
# $Id: wc.perl,v 1.1 2004-05-19 18:13:51 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

# this program is modified from:
#   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
# Timing Trials, or, the Trials of Timing: Experiments with Scripting
# and User-Interface Languages</a> by Brian W. Kernighan and
# Christopher J. Van Wyk.

use strict;

my($nl, $nw, $nc);
while (read(STDIN, $_, 4095)) {
    $_ .= <STDIN>;
    $nl += scalar(split(/\n/));
    $nc += length;
    $nw += scalar(split);
}
print "$nl $nw $nc\n";
#!/usr/bin/perl
#   The Computer Language Shootout
#   http://shootout.alioth.debian.org/
#   contributed by Cosimo Streppone

use strict;
my($nl, $nw, $nc);
while (read(STDIN, $_, 4095)) {
    $_ .= <STDIN>;
    $nc += length;
    $nw += scalar split;
    $nl += tr/\n/\n/;
}
print "$nl $nw $nc\n";
#!/usr/bin/perl
# $Id: wordfreq.perl,v 1.2 2004-07-03 05:36:11 bfulgham Exp $
# http://shootout.alioth.debian.org/

# Tony Bowden suggested using tr versus lc and split(/[^a-z]/)
# Some corrections to comply with tr/wc/sort "master" implementation

use strict;

my %count = ();
while (read(STDIN, $_, 4095) and $_ .= <STDIN>) {
    tr/A-Za-z/\n/cs;
    ++$count{$_} foreach split('\n', lc $_);
}

my @lines = ();
my ($w, $c);
while (($w, $c) = each(%count)) {
    next if ("$w" eq "");
    push(@lines, sprintf("%7d %s\n", $c, $w));
}
print sort { $b cmp $a } @lines;
