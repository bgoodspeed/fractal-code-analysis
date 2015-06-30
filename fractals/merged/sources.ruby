#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: ackermann.ruby,v 1.2 2005-06-10 00:57:22 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/ 

def ack(m, n)
    if m == 0 then
	n + 1
    elsif n == 0 then
	ack(m - 1, 1)
    else
	ack(m - 1, ack(m, n - 1))
    end
end

NUM = Integer(ARGV.shift || 1)
print "Ack(3,", NUM, "): ", ack(3, NUM), "\n"
#!/usr/bin/ruby
# The Computer Language Benchmark Shootout
# http://shootout.alioth.debian.org
#
# original code by Martin DeMello
# modified by Jabari Zakiya 3/20/05
# modified by Glenn Parker 3/28/05 (format results)

include Math

SOLAR_MASS = 4*PI*PI
DAYS_PER_YEAR = 365.24

Vector3D = Struct.new("Vector3D", :x, :y, :z)

class Vector3D

  def *(val)
    Vector3D.new(*self.map {|i| i * val})
  end

  def /(val)
    Vector3D.new(*self.map {|i| i / val})
  end

  #in-place add with scale
  # a.adds(b, s) -> a += b*s

  def adds(other, scale)
    self[0] += other[0]*scale; self[1] += other[1]*scale
    self[2] += other[2]*scale
  end

  def subs(other, scale)
    self[0] -= other[0]*scale; self[1] -= other[1]*scale
    self[2] -= other[2]*scale
  end

  def magnitude
    x=self[0]; y=self[1]; z=self[2]
    sqrt(x*x + y*y + z*z)
  end

  # |self - other|
  def dmag(other)
    x=self[0]-other[0]; y=self[1]-other[1]; z=self[2]-other[2]
    sqrt(x*x + y*y + z*z)
  end
end

class Planet
  attr_accessor :pos, :v, :mass

  def initialize(x, y, z, vx, vy, vz, mass)
    @pos = Vector3D.new(x, y, z)
    @v = Vector3D.new(vx, vy, vz) * DAYS_PER_YEAR
    @mass = mass * SOLAR_MASS
  end

  def distance(other)
    self.pos.dmag(other.pos)
  end
end

jupiter = Planet.new(
   4.84143144246472090e+00,
   -1.16032004402742839e+00,
   -1.03622044471123109e-01,
   1.66007664274403694e-03,
   7.69901118419740425e-03,
   -6.90460016972063023e-05,
   9.54791938424326609e-04)

saturn = Planet.new(
   8.34336671824457987e+00,
   4.12479856412430479e+00,
   -4.03523417114321381e-01,
   -2.76742510726862411e-03,
   4.99852801234917238e-03,
   2.30417297573763929e-05,
   2.85885980666130812e-04)

uranus = Planet.new(
   1.28943695621391310e+01,
   -1.51111514016986312e+01,
   -2.23307578892655734e-01,
   2.96460137564761618e-03,
   2.37847173959480950e-03,
   -2.96589568540237556e-05,
   4.36624404335156298e-05)

neptune = Planet.new(
   1.53796971148509165e+01,
   -2.59193146099879641e+01,
   1.79258772950371181e-01,
   2.68067772490389322e-03,
   1.62824170038242295e-03,
   -9.51592254519715870e-05,
   5.15138902046611451e-05)

sun = Planet.new(0, 0, 0, 0, 0, 0, 1)

class Array
  def each_pair
    a = []
    each_index {|i|
      ((i+1)...length).each {|j|
        yield at(i), at(j)
      }
    }
  end
end

bodies = [sun, jupiter, saturn, uranus, neptune]

class << bodies
  def advance(dt)
    mag = m1 = m2 = nil
    each_pair {|b1, b2|
      d = b1.distance(b2)
      mag = dt/(d*d*d)

      m1 = b1.mass * mag
      m2 = b2.mass * mag

      b1.v.adds(b2.pos, m2)
      b1.v.subs(b1.pos, m2)
      b2.v.adds(b1.pos, m1)
      b2.v.subs(b2.pos, m1)
    }

    each {|b| b.pos.adds(b.v, dt)}
  end

  def energy
    e = 0
    each {|b| e += 0.5 * b.mass * (b.v.magnitude ** 2) }
    each_pair {|b1, b2| e -= (b1.mass * b2.mass) / b1.distance(b2) }
    e
  end

  def offset_momentum
    p = Vector3D.new(0,0,0)
    sun = self[0]
    each {|b| p.adds(b.v, b.mass) }
    sun.v.subs(p, 1.0/sun.mass)
  end
end

bodies.offset_momentum
puts "%.9f" % bodies.energy
Integer(ARGV[0]).times { bodies.advance(0.01) }
puts "%.9f" % bodies.energy
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: ary.ruby,v 1.3 2004-06-20 08:39:45 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Paul Brannan and Mark Hubbart

n = Integer(ARGV.shift || 1)

x = Array.new(n)
y = Array.new(n, 0)

for i in 0 ... n
  x[i] = i + 1
end

(0 .. 999).each do
  (n-1).step(0,-1) do |i|
    y[i] += x.at(i)
  end
end

puts "#{y.first} #{y.last}"
# The Computer Language Shootout Benchmarks
# http://shootout.alioth.debian.org
#
# contributed by Jesse Millikan
# Modified by Wesley Moxam


def item_check(left, item, right)
  return item if left.nil?
  item + item_check(*left) - item_check(*right)
end

def bottom_up_tree(item, depth)
  return [nil, item, nil] unless depth > 0
  item_item = 2 * item
  depth -= 1
  [bottom_up_tree(item_item - 1, depth), item, bottom_up_tree(item_item, depth)]
end

max_depth = ARGV[0].to_i
min_depth = 4

max_depth = min_depth + 2 if min_depth + 2 > max_depth

stretch_depth = max_depth + 1
stretch_tree = bottom_up_tree(0, stretch_depth)

puts "stretch tree of depth #{stretch_depth}\t check: #{item_check(*stretch_tree)}"
stretch_tree = nil

long_lived_tree = bottom_up_tree(0, max_depth)

min_depth.step(max_depth + 1, 2) do |depth|
  iterations = 2**(max_depth - depth + min_depth)

  check = 0

  for i in 1..iterations
    temp_tree = bottom_up_tree(i, depth)
    check += item_check(*temp_tree)

    temp_tree = bottom_up_tree(-i, depth)
    check += item_check(*temp_tree)
  end

  puts "#{iterations * 2}\t trees of depth #{depth}\t check: #{check}"
end

puts "long lived tree of depth #{max_depth}\t check: #{item_check(*long_lived_tree)}"

#########################################
#     The Computer Language Shootout    #
#   http://shootout.alioth.debian.org/  #
#                                       #
#      Contributed by Jesse Millikan    #
#    Based on version by Gordon Innes   #
#########################################

require 'thread'

creature_meetings = Queue.new
meeting_point = Mutex.new
wait_signal = ConditionVariable.new
meetings_left = ARGV[0].to_i
waiting_colour, incoming_colour = nil, nil

# Each chameneo is represented here by a thread
# and its colour variable, rather than explicitly
# by an object
#
# This is all packed into one place for speed and
# clarity (It's clear to *me* :)
[:blue, :red, :yellow, :blue].each { |colour|
  Thread.new {
    met = 0
    while true
      # The form meeting_point.synchronize { } is slow
      meeting_point.lock

      if meetings_left <= 0
        meeting_point.unlock
	# colour = :faded
	break 
      end

      # Both threads emerge with variable other_colour set
      if waiting_colour
        other_colour = waiting_colour
        incoming_colour = colour
        wait_signal.signal
        meetings_left-=1
        waiting_colour = nil
      else
        waiting_colour = colour
        wait_signal.wait(meeting_point)
        other_colour = incoming_colour
      end
      meeting_point.unlock

      met += 1

      # Take the complement colour
      colour = 
        case other_colour
          when :blue
           colour == :red ? :yellow : :red
          when :red
           colour == :blue ? :yellow : :blue
          when :yellow
           colour == :blue ? :red : :blue
        end
    end

    # Leave the total on the queue for the main thread
    creature_meetings.push(met)
  }
}

total = 0
4.times { total += creature_meetings.pop }
puts total
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/

#   contributed by Michael Barker
#   based on a Java contribution by Luzius Meisser
#   converted to C by dualamd
#   converted to Ruby by Eugene Pimenov

require 'thread'

COLORS     = [:blue, :red, :yellow, :invalid].freeze
COMPLIMENT = {
  :blue => {:blue => :blue, :red => :yellow, :yellow => :red}.freeze,
  :red => {:blue => :yellow, :red => :red, :yellow => :blue}.freeze,
  :yellow => {:blue => :red, :red => :blue, :yellow => :yellow}.freeze
}.freeze

$creature_id = 0

NUMBERS = %w{zero one two three four five six seven eight nine}.freeze

# convert integer to number string: 1234 -> "one two three four"
def format_number(num)
  out = []
  begin
    out << NUMBERS[num%10]
    num /= 10
  end while num > 0
  out.reverse.join(" ")
end

class MeetingPlace
  attr_reader :mutex
  attr_accessor :meetings_left, :first_creature

  def initialize(meetings)
    @mutex = Mutex.new
    @meetings_left = meetings
  end
end

class Creature
  attr_accessor :place, :thread, :count, :same_count, :color, :id, :two_met, :sameid

  def initialize(place, color)
    @place = place
    @count = @same_count = 0

    @id = ($creature_id += 1)
    @color = color
    @two_met = FALSE

    @thread = Thread.new do
      loop do
        if meet
          Thread.pass while @two_met == false

          @same_count += 1 if @sameid
          @count += 1
        else
          break
        end
      end
    end
  end

  def meet
    @place.mutex.lock

    if @place.meetings_left > 0
      if @place.first_creature
        first = @place.first_creature
        new_color = COMPLIMENT[@color][first.color]

        @sameid  = first.sameid  = @id == first.id
        @color   = first.color   = new_color
        @two_met = first.two_met = true

        @place.first_creature = nil
        @place.meetings_left -= 1
      else
        @two_met = false
        @place.first_creature = self
      end
      true
    else
      false
    end
  ensure
    @place.mutex.unlock
  end

  def result
    '' << @count.to_s << ' ' << format_number(@same_count)
  end
end

def run_game(n_meeting, colors)
  place = MeetingPlace.new(n_meeting)

  creatures = []
  colors.each do |color|
    print color, " "
    creatures << Creature.new(place, color)
  end
  puts

  # wait for them to meet
  creatures.each { |c| c.thread.join}

  total = 0
  # print meeting times of each creature
  creatures.each do |c|
    puts c.result
    total += c.count
  end

  # print total meeting times, should be equal n_meeting
  print ' ', format_number(total), "\n\n"
end

def print_colors_table
  [:blue, :red, :yellow].each do |c1|
    [:blue, :red, :yellow].each do |c2|
      puts "#{c1} + #{c2} -> #{COMPLIMENT[c1][c2]}"
    end
  end
end

n = (ARGV[0] || 600).to_i


print_colors_table
puts

run_game n, [:blue, :red, :yellow]
run_game n, [:blue, :red, :yellow, :red, :yellow, :blue, :red, :yellow, :red, :blue]
#!/usr/bin/ruby
#
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org
# 
# contributed by Jesse Millikan

class BottleState
 attr_reader :tag
 private_class_method :new

 def initialize(tag)
  @tag = tag
 end

 def BottleState.initial; @@empty; end

 def BottleState.pressurized_initial; @@unpressurized_empty; end

# Thanks to dbrock on #ruby-lang on freenode for some tips on this.

 @@empty = new(1)

 def @@empty.next(bottle); bottle.state = @@full; end

 @@full = new(2)
 
 def @@full.next(bottle); bottle.state = @@sealed; end

 @@sealed = new(3)

 def @@sealed.next(bottle); bottle.state = @@empty; end

 @@unpressurized_empty = new(4)

 def @@unpressurized_empty.next(bottle); bottle.state = @@unpressurized_full; end

 @@unpressurized_full = new(5)
 
 def @@unpressurized_full.next(bottle); bottle.state = @@pressurized_unsealed; end

 @@pressurized_unsealed = new(6)
 
 def @@pressurized_unsealed.next(bottle); bottle.state = @@pressurized_sealed; end

 @@pressurized_sealed = new(7)
 
 def @@pressurized_sealed.next(bottle); bottle.state = @@unpressurized_empty; end
end

#Someone with judgement on style could pare this down a bit.
class Bottle
  attr_writer :state

 def initialize(id)
  @id = id
  @state = initial
 end

 def initial; BottleState.initial; end

 def cycle; fill; seal; empty; end
 
 def next; @state.next(self); end

 alias_method :empty, :next
 alias_method :fill, :next
 alias_method :seal, :next

 def check(c); @state.tag + @id + c; end
end

class PressurizedBottle < Bottle
 def initial; BottleState.pressurized_initial; end
 
 alias_method :pressurize, :next

 def cycle; fill; pressurize; seal; empty; end
end

def bottle_check(a1, a2, a3, a4, a5, i)
 a1.cycle; a2.cycle; a3.cycle; a4.cycle; a5.cycle

 c = i % 2

 a1.check(c) + a2.check(c) + a3.check(c) + a4.check(c) + a5.check(c)
end

n = 0
n = ARGV[0].to_i unless ARGV.empty?

b1 = Bottle.new(1); b2 = Bottle.new(2)
b3 = Bottle.new(3); b4 = Bottle.new(4)
b5 = Bottle.new(5); b6 = Bottle.new(6)
b7 = Bottle.new(7); b8 = Bottle.new(8)
b9 = Bottle.new(9); b0 = Bottle.new(0)

p1 = PressurizedBottle.new(1); p2 = PressurizedBottle.new(2)
p3 = PressurizedBottle.new(3); p4 = PressurizedBottle.new(4)
p5 = PressurizedBottle.new(5); p6 = PressurizedBottle.new(6)
p7 = PressurizedBottle.new(7); p8 = PressurizedBottle.new(8)
p9 = PressurizedBottle.new(9); p0 = PressurizedBottle.new(0)

check = 0

for i in 1..n
 check += bottle_check(b1, b2, b3, b4, b5, i)
 check += bottle_check(b6, b7, b8, b9, b0, i)

 check += bottle_check(p1, p2, p3, p4, p5, i)
 check -= bottle_check(p6, p7, p8, p9, p0, i)
end

puts "#{check}"
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: echo.ruby,v 1.1 2004-05-19 18:09:37 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

require "socket"

DATA = "Hello there sailor\n"

def echo_client(n, port)
    sock = TCPsocket.open('127.0.0.1', port)
    n.times do
	sock.write(DATA)
	ans = sock.readline
	if ans != DATA then
	    raise sprintf("client: \"%s\" \"%s\"", DATA, ans)
	end
    end
    sock.close
end


def echo_server(n)
    ssock = TCPserver.open('127.0.0.1', 0)
    port = ssock.addr[1]
    if pid = fork then
	# parent is server
	csock = ssock.accept
	n = 0
	while str = csock.gets
	    n += csock.write(str)
	end
	Process.wait
        printf "server processed %d bytes\n", n
    else
	# child is client
	echo_client(n, port)
    end
end

echo_server(Integer(ARGV.shift || 1))
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: except.ruby,v 1.1 2004-05-19 18:09:43 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

$HI = 0
$LO = 0
NUM = Integer(ARGV[0] || 1)


class Lo_Exception < Exception
    def initialize(num)
        @value = num
        return self
    end
end

class Hi_Exception < Exception
    def initialize(num)
        @value = num
        return self
    end
end

def some_function(num)
    begin
	hi_function(num)
    rescue
        print "We shouldn't get here, exception is: #{$!.type}\n"
    end
end

def hi_function(num)
    begin
	lo_function(num)
    rescue Hi_Exception
	$HI = $HI + 1
    end
end

def lo_function(num)
    begin
	blowup(num)
    rescue Lo_Exception
	$LO = $LO + 1
    end
end

def blowup(num)
    if num % 2 == 0
	raise Lo_Exception.new(num)
    else
	raise Hi_Exception.new(num)
    end
end


for iter in 1 .. NUM
    some_function(iter)
end
print "Exceptions: HI=", $HI, " / LO=", $LO, "\n"
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: except.ruby-2.ruby,v 1.1 2004-11-10 06:26:50 bfulgham Exp $
# http://shootout.alioth.debian.org/

$HI = 0
$LO = 0
NUM = Integer(ARGV[0] || 1)


class Lo_Exception < Exception
    def initialize(num)
        @value = num
        return self
    end
end

class Hi_Exception < Exception
    def initialize(num)
        @value = num
        return self
    end
end

def some_function(num)
    begin
	hi_function(num)
    rescue
        print "We shouldn't get here, exception is: #{$!.type}\n"
    end
end

def hi_function(num)
    begin
	lo_function(num)
    rescue Hi_Exception
	$HI = $HI + 1
    end
end

def lo_function(num)
    begin
	blowup(num)
    rescue Lo_Exception
	$LO = $LO + 1
    end
end

def blowup(num)
    if num & 2 == 0
	raise Lo_Exception.new(num)
    else
	raise Hi_Exception.new(num)
    end
end


for iter in 1 .. NUM
    some_function(iter)
end
print "Exceptions: HI=", $HI, " / LO=", $LO, "\n"
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Sokolov Yura
# Modified by Ryan Williams
# Modified by Isaac Gouy
# Modified by Artem Vorozhtsov
# Modified by Wesley Moxam


def fannkuch(n)
   maxFlips, m, r, check = 0, n-1, n, 0
   count = (1..n).to_a
   perm = (1..n).to_a

   while true
      if check < 30
         puts "#{perm}"
         check += 1
      end

      while r != 1
         count[r-1] = r
         r -= 1
      end

      if perm[0] != 1 and perm[m] != n
         perml = perm.clone #.dup
         flips = 0
         while (k = perml.first ) != 1
           last =  perml.slice!(k, n + 1)
           perml.reverse!.concat last
#            perml = perml.slice!(0, k).reverse + perml
           flips += 1
         end
         maxFlips = flips if flips > maxFlips
      end
      while true
        return maxFlips if r == n
        perm.insert r, perm.shift
        break if (count[r] -= 1) > 0
        r += 1
      end
   end
end

N = (ARGV[0] || 1).to_i
puts "Pfannkuchen(#{N}) = #{fannkuch(N)}"

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Wesley Moxam

def fannkuch(n)
  sign, maxflips, sum = 1, 0, 0

  p = [nil].concat((1..n).to_a)
  q = p.dup
  s = p.dup

  while(true)
    # Copy and flip.
    q1 = p[1]				# Cache 1st element.
    if q1 != 1
      q = p.dup
      flips = 1
      while(true)
	      qq = q[q1]
	      if qq == 1				# ... until 1st element is 1.
	        sum = sum + sign * flips
	        maxflips = flips if flips > maxflips # New maximum?
	        break
	      end
	      q[q1] = q1
	      if q1 >= 4
	        i, j = 2, q1 - 1
	        begin
            q[i], q[j] = q[j], q[i]
            i = i + 1
            j = j - 1
          end while i < j
	      end
	      q1 = qq
        flips = flips + 1
      end
    end
    # Permute.
    if sign == 1
      # Rotate 1<-2.
      p[1], p[2] = p[2], p[1]
      sign = -1	
    else
      # Rotate 1<-2 and 1<-2<-3.
      p[2], p[3] = p[3], p[2]
      sign = 1
      3.upto(n) do |i|
        (s[i] =  s[i] - 1) && break unless s[i] == 1
	      return [sum, maxflips] if i == n 	# Out of permutations.
	      s[i] = i
        # Rotate 1<-...<-i+1.
	      t = p[1]
        1.upto(i) do |j|
          p[j] = p[j+1]
        end
        p[i+1] = t
      end
    end
  end
end

n = (ARGV[0] || 1).to_i
sum, flips = fannkuch(n)
printf "%d\nPfannkuchen(%d) = %d\n", sum, n, flips

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Sokolov Yura
# Modified by Joseph LaFata

$last = 42.0
IM=139968
IA=3877
IC=29573
def gen_random (max)
    (max * ($last = ($last * IA + IC) % IM)) / IM
end

alu =
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"+
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"+
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"+
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"+
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"+
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"+
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [
    ["a", 0.27],
    ["c", 0.12],
    ["g", 0.12],
    ["t", 0.27],

    ["B", 0.02],
    ["D", 0.02],
    ["H", 0.02],
    ["K", 0.02],
    ["M", 0.02],
    ["N", 0.02],
    ["R", 0.02],
    ["S", 0.02],
    ["V", 0.02],
    ["W", 0.02],
    ["Y", 0.02],
]
homosapiens = [
    ["a", 0.3029549426680],
    ["c", 0.1979883004921],
    ["g", 0.1975473066391],
    ["t", 0.3015094502008],
]

def make_repeat_fasta(id, desc, src, n)
    puts ">#{id} #{desc}"
    l = src.length
    s = src * ((n / l) + 1)
    s.slice!(n, l)
    0.step(s.length-1,60) {|x| print s[x,60] , "\n"}
end

def make_random_fasta(id, desc, table, n)
    puts ">#{id} #{desc}"
    rand, v = nil,nil
    prob = 0.0
    table.each{|v| v[1]= (prob += v[1])}
    output = ""
    n.times do
      rand = gen_random(1.0)
      table.each do |v|
	if v[1] > rand then
	  output << v[0]
	  break
	end
      end
    end
    0.step(output.length-1,60) {|x| print output[x,60] , "\n"}
end


n = (ARGV[0] or 27).to_i

make_repeat_fasta('ONE', 'Homo sapiens alu', alu, n*2)
make_random_fasta('TWO', 'IUB ambiguity codes', iub, n*3)
make_random_fasta('THREE', 'Homo sapiens frequency', homosapiens, n*5)
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Sokolov Yura
# Modified by Joseph LaFata
# Modified by Philip (flip) Kromer (used lookup table in place of search, unrolled print loop)

alu =
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"+
  "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"+
  "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"+
  "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"+
  "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"+
  "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"+
  "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [
  ["a", 0.27],
  ["c", 0.12],
  ["g", 0.12],
  ["t", 0.27],

  ["B", 0.02],
  ["D", 0.02],
  ["H", 0.02],
  ["K", 0.02],
  ["M", 0.02],
  ["N", 0.02],
  ["R", 0.02],
  ["S", 0.02],
  ["V", 0.02],
  ["W", 0.02],
  ["Y", 0.02],
]
homosapiens = [
  ["a", 0.3029549426680],
  ["c", 0.1979883004921],
  ["g", 0.1975473066391],
  ["t", 0.3015094502008],
]

$lasti = 42
IM=139968
IA=3877
IC=29573
# Generates an integer in 0 <= rx < IM
def gen_random_int
  $lasti = ($lasti * IA + IC) % IM
end

def print_in_rows_of row_len, str, fh=$stdout
  0.step(str.length-1,row_len){|x| fh.print str[x,row_len], "\n" }
end

def make_repeat_fasta(id, desc, src, n)
  puts ">#{id} #{desc}"
  l = src.length
  s = src * ((n / l) + 1) # enough duplicates to be longer than n chars
  s.slice!(n, l)          # remove characters past nth
  print_in_rows_of 60, s
end

# The given random number generator only creates integers in 0...IM (exclusive)
# Calculate the result for each rather than do linear search.
def make_lut_flat prob_dist
  lut = []
  pt = 0.0
  prob_dist.map do |ch,pr|
    lut << ((pt*IM).ceil ... ((pt+pr)*IM).ceil).map{ ch }
    pt += pr
  end
  lut.flatten!
end

def make_random_fasta_lut_chunked(id, desc, prob_dist, n)
  puts ">#{id} #{desc}"
  lut = make_lut_flat(prob_dist)
  rx = $lasti
  # ruby <= 1.8x's memory manager is an Achilles heel.  Let's print 60 chars as
  # we go along rather than accumulate n chars to print all at once.
  full_lines = (n/60); extra_lines = n - 60*full_lines
  full_lines.times do
    60.times       { print lut[rx = (rx * IA + IC) % IM] } ; print "\n"
  end
  extra_lines.times{ print lut[rx = (rx * IA + IC) % IM] } ; print "\n" unless extra_lines == 0
  $lasti = rx
end

n = (ARGV[0] || 27).to_i

make_repeat_fasta('ONE', 'Homo sapiens alu', alu, n*2)
make_random_fasta_lut_chunked('TWO', 'IUB ambiguity codes', iub, n*3)
make_random_fasta_lut_chunked('THREE', 'Homo sapiens frequency', homosapiens, n*5)
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Sokolov Yura
# Modified by Rick Branson

$last = 42.0

GR_IM = 139968.0
GR_IA = 3877.0
GR_IC = 29573.0

alu =
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"+
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"+
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"+
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"+
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"+
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"+
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [
    ["a", 0.27],
    ["c", 0.12],
    ["g", 0.12],
    ["t", 0.27],

    ["B", 0.02],
    ["D", 0.02],
    ["H", 0.02],
    ["K", 0.02],
    ["M", 0.02],
    ["N", 0.02],
    ["R", 0.02],
    ["S", 0.02],
    ["V", 0.02],
    ["W", 0.02],
    ["Y", 0.02],
]
homosapiens = [
    ["a", 0.3029549426680],
    ["c", 0.1979883004921],
    ["g", 0.1975473066391],
    ["t", 0.3015094502008],
]

def generate_rand_finder(tbl)
  rb = "lambda do |n| \n"
  
  tbl.each do |va, vb|
    rb += "return #{va.inspect} if #{vb.inspect} > n\n"
  end
  
  rb += "end\n"
  
  eval rb
end

def make_repeat_fasta(id, desc, src, n)
    puts ">#{id} #{desc}"
    v = nil
    width = 60
    l = src.length
    s = src * ((n / l) + 1)
    s.slice!(n, l)
    puts (s.scan(/.{1,#{width}}/).join("\n"))
end

def make_random_fasta(id, desc, table, n)
    puts ">#{id} #{desc}"
    rand, v = nil,nil
    width = 60
    chunk = 1 * width
    prob = 0.0
    rwidth = (1..width)
    table.each{|v| v[1]= (prob += v[1])}
    f = generate_rand_finder(table)
    
    if RUBY_PLATFORM == "java" 
      collector = lambda do |x|
        rand = ($last = ($last * GR_IA + GR_IC) % GR_IM) / GR_IM
        table.find { |va, vb| vb > rand }[0]
      end
    else
      collector = lambda do |x|
        rand = ($last = ($last * GR_IA + GR_IC) % GR_IM) / GR_IM
        f.call(rand)
      end
    end
    
    for i in 1..(n/width)
      puts rwidth.collect(&collector).join
    end
    if n%width != 0
      puts (1..(n%width)).collect(&collector).join
    end
end


n = (ARGV[0] or 27).to_i

make_repeat_fasta('ONE', 'Homo sapiens alu', alu, n*2)
make_random_fasta('TWO', 'IUB ambiguity codes', iub, n*3)
make_random_fasta('THREE', 'Homo sapiens frequency', homosapiens, n*5)
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Sokolov Yura
# Modified by Rick Branson, Andy Fingerhut

$last = 42.0

GR_IM = 139968.0
GR_IA = 3877.0
GR_IC = 29573.0

alu =
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"+
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"+
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"+
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"+
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"+
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"+
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [
    ["a", 0.27],
    ["c", 0.12],
    ["g", 0.12],
    ["t", 0.27],

    ["B", 0.02],
    ["D", 0.02],
    ["H", 0.02],
    ["K", 0.02],
    ["M", 0.02],
    ["N", 0.02],
    ["R", 0.02],
    ["S", 0.02],
    ["V", 0.02],
    ["W", 0.02],
    ["Y", 0.02],
]
homosapiens = [
    ["a", 0.3029549426680],
    ["c", 0.1979883004921],
    ["g", 0.1975473066391],
    ["t", 0.3015094502008],
]

def generate_rand_finder(tbl)
  rb = "lambda do |n| \n"

  tbl.each do |va, vb|
    rb += "return #{va.inspect} if #{vb.inspect} > n\n"
  end

  rb += "end\n"

  eval rb
end

def make_repeat_fasta(id, desc, src, n)
    puts ">#{id} #{desc}"
    v = nil
    width = 60
    l = src.length
    s = src * (((width + l - 1) / l) + 1)
    i = 0
    p = []
    p[i] = s.slice(i,width)
    i = (i + width) % l
    while i != 0 do
      p[i] = s[i,width]
      i = (i + width) % l
    end
    i = 0
    printed = 0
    while printed <= (n - width) do
      puts "#{p[i]}"
      printed += width
      i = (i + width) % l
    end
    if printed < n
      puts "#{p[i].slice(0, n-printed)}"
    end
end

def make_random_fasta(id, desc, table, n)
    puts ">#{id} #{desc}"
    rand, v = nil,nil
    width = 60
    chunk = 1 * width
    prob = 0.0
    rwidth = (1..width)
    table.each{|v| v[1]= (prob += v[1])}
    f = generate_rand_finder(table)

    if RUBY_PLATFORM == "java"
      collector = lambda do |x|
        rand = ($last = ($last * GR_IA + GR_IC) % GR_IM) / GR_IM
        table.find { |va, vb| vb > rand }[0]
      end
    else
      collector = lambda do |x|
        rand = ($last = ($last * GR_IA + GR_IC) % GR_IM) / GR_IM
        f.call(rand)
      end
    end

    for i in 1..(n/width)
      puts rwidth.collect(&collector).join
    end
    if n%width != 0
      puts (1..(n%width)).collect(&collector).join
    end
end


n = (ARGV[0] or 27).to_i

make_repeat_fasta('ONE', 'Homo sapiens alu', alu, n*2)
make_random_fasta('TWO', 'IUB ambiguity codes', iub, n*3)
make_random_fasta('THREE', 'Homo sapiens frequency', homosapiens, n*5)
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: fibo.ruby,v 1.3 2005-04-25 19:01:38 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

def fib(n)
    if n < 2 then
	   1
    else
	   fib(n-2) + fib(n-1)
    end
end

N = Integer(ARGV.shift || 1)
puts fib(N)
#!/usr/bin/ruby
# http://shootout.alioth.debian.org/
#
# Contributed by Christopher Williams
# modified by Daniel South
# modified by Doug King

n = (ARGV[0] || 10000000).to_i

partialSum = 0.0
for i in (1..n)
  partialSum += (1.0 / i)
end

printf("%.9f\n", partialSum)
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: hash.ruby,v 1.1 2004-05-19 18:09:55 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Aristarkh A Zagorodnikov

n = (ARGV.shift || 1).to_i

hash = {}
for i in 1..n
    hash['%x' % i] = 1
end

c = 0
n.downto 1 do |i|
    c += 1 if hash.has_key? i.to_s
end

puts c
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: hash2.ruby,v 1.2 2004-11-10 06:36:29 bfulgham Exp $
# http://shootout.alioth.debian.org/
# Revised by Dave Anderson

n = Integer(ARGV.shift || 1)

hash1 = {}
i = 0
for i in 0 .. 9999
    hash1["foo_" << i.to_s] = i
end

hash2 = Hash.new(0)
n.times do
    for i in hash1.keys
	hash2[i] += hash1[i]
    end
end

printf "%d %d %d %d\n",
    hash1["foo_1"], hash1["foo_9999"], hash2["foo_1"], hash2["foo_9999"]
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: heapsort.ruby,v 1.5 2005-04-14 15:59:37 igouy-guest Exp $
#
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# modified by Jabari Zakiya

IM = 139968
IA =   3877
IC =  29573

$last = 42.0
def gen_random (max) (max * ($last = ($last * IA + IC) % IM)) / IM end

def heapsort(n, ra)
    j = i = rra = 0
    l = (n >> 1) + 1
    ir = n - 1

    while (1) do
	if (l > 1) then
	    rra = ra.at(l -= 1)
	else
	    rra = ra.at(ir)
	    ra[ir] = ra.at(1)
	    if ((ir -= 1) == 1) then
		ra[1] = rra
		return
	    end
	end
	i = l
	j = l << 1
	while (j <= ir) do
	    if ((j < ir) and (ra.at(j) < ra.at(j+1))) then
		j += 1
	    end
	    if (rra < ra.at(j)) then
		ra[i] = ra.at(j)
		j += (i = j)
	    else
		j = ir + 1
	    end
	end
	ra[i] = rra
    end
end

N = Integer(ARGV.shift || 1)
ary = Array.new(N) { gen_random(1.0) }

heapsort(N, ary)

printf "%.10f\n", ary.last
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: hello.ruby,v 1.1 2004-05-19 18:10:16 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

puts "hello world"
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org
#
# contributed by jose fco. gonzalez
# modified by Sokolov Yura
# Parallelism by Rick Branson

$seq = String.new

def frequency(seq, length)
  frequencies = Hash.new(0)
  ns          = seq.length + 1 - length
  
  for i in (0 ... ns)
    frequencies[seq[i, length]] += 1
  end
  
  [ns, frequencies]
end

def sort_by_freq(seq, length)
  ret       = ""
  n, table  = frequency(seq, length)

  table.sort{|a,b| b[1] <=> a[1]}.each do |v|
      ret += "%s %.3f\n" % [v[0].upcase,((v[1]*100).to_f/n)]
  end
  
  ret += "\n"
end

def find_seq(seq, s)
  n, table = frequency(seq, s.length)
  "#{table[s].to_s}\t#{s.upcase}\n"
end

line = STDIN.gets while line !~ /^>THREE/
line = STDIN.gets
while (line !~ /^>/) & line do
    $seq << line.chomp
    line = STDIN.gets
end

class Worker
  def initialize(&block)
    if RUBY_PLATFORM == "java"
      @t = Thread.new do
        Thread.current[:result] = yield
      end
    else
      @r, @w = IO.pipe
      @p = Process.fork do
        @r.close
        @w.write yield
        @w.close
      end
      
      @w.close
    end
  end
  
  def result
    if RUBY_PLATFORM == "java"
      @t.join
      @t[:result]
    else
      ret = @r.read
      @r.close
      Process.wait(@p)
      ret
    end
  end
end

FREQS   = [1, 2]
NUCLEOS = %w(ggt ggta ggtatt ggtattttaatt ggtattttaatttatagt)

workers =   FREQS.map   { |i| Worker.new { sort_by_freq($seq, i) } }
workers +=  NUCLEOS.map { |s| Worker.new { find_seq($seq, s) } }
  
results = workers.map { |w| w.result }
print results.join
# The Computer Language Shootout
# http://shootout.alioth.debian.org
#
# contributed by jose fco. gonzalez
# modified by Sokolov Yura

seq = String.new

def frecuency( seq,length )
    n, table = seq.length - length + 1, Hash.new(0)
    f, i = nil, nil
    (0 ... length).each do |f|
        (f ... n).step(length) do |i|
            table[seq[i,length]] += 1
        end
    end
    [n,table]

end

def sort_by_freq( seq,length )
    n,table = frecuency( seq,length )
    a, b, v = nil, nil, nil
    table.sort{|a,b| b[1] <=> a[1]}.each do |v|
        puts "%s %.3f" % [v[0].upcase,((v[1]*100).to_f/n)]
    end
    puts
end

def find_seq( seq,s )
    n,table = frecuency( seq,s.length )
    puts "#{table[s].to_s}\t#{s.upcase}"
end

line = STDIN.gets while line !~ /^>THREE/
line = STDIN.gets
while (line !~ /^>/) & line do
    seq << line.chomp
    line = STDIN.gets
end

[1,2].each {|i| sort_by_freq( seq,i ) }

%w(ggt ggta ggtatt ggtattttaatt ggtattttaatttatagt).each{|s| find_seq( seq,s) }
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: lists.ruby,v 1.3 2005-06-10 16:59:56 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

NUM = Integer(ARGV.shift || 1)

SIZE = 10000

def test_lists()
    # create a list of integers (Li1) from 1 to SIZE
    li1 = (1..SIZE).to_a
    # copy the list to li2 (not by individual items)
    li2 = li1.dup
    # remove each individual item from left side of li2 and
    # append to right side of li3 (preserving order)
    li3 = Array.new
    while (not li2.empty?)
	li3.push(li2.shift)
    end
    # li2 must now be empty
    # remove each individual item from right side of li3 and
    # append to right side of li2 (reversing list)
    while (not li3.empty?)
	li2.push(li3.pop)
    end
    # li3 must now be empty
    # reverse li1 in place
    li1.reverse!
    # check that first item is now SIZE
    if li1[0] != SIZE then
	p "not SIZE"
	return(0)
    end
    # compare li1 and li2 for equality
    if li1 != li2 then
	return(0)
    end
    # return the length of the list
    return(li1.length)
end

for iter in 1 .. NUM
    result = test_lists()
end
print result, "\n"
#  The Computer Language Benchmarks Game
#  http://shootout.alioth.debian.org/
#
#  contributed by Karl von Laudermann
#  modified by Jeremy Echols
#  modified by Detlef Reichl
#  modified by Joseph LaFata

PAD = "\\\\__MARSHAL_RECORD_SEPARATOR__//" # silly, but works

class Worker
  
  attr_reader :reader
  
  def initialize(enum, index, total, &block)
    @enum             = enum
    @index            = index
    @total            = total
    @reader, @writer  = IO.pipe
    
    if RUBY_PLATFORM == "java"
      @t = Thread.new do
        self.execute(&block)
      end
    else
      @p = Process.fork do
        @reader.close
        self.execute(&block)
        @writer.close
      end
      
      @writer.close
    end
  end
  
  def execute(&block)
    (0 ... @enum.size).step(@total) do |bi|
      idx = bi + @index
      if item = @enum[idx]
        res = yield(item)
        @writer.write(Marshal.dump([idx, res]) + PAD)
      end
    end
    
    @writer.write(Marshal.dump(:end) + PAD)
  end
end

def parallel_map(enum, worker_count = 8, &block)
  count = [enum.size, worker_count].min
  
  Array.new(enum.size).tap do |res|  
    workers = (0 ... count).map do |idx|
      Worker.new(enum, idx, count, &block)
    end
  
    ios = workers.map { |w| w.reader }

    while ios.size > 0 do
      sr, sw, se = IO.select(ios, nil, nil, 0.01)

      if sr
        sr.each do |io|
          buf = ""
          
          while sbuf = io.readpartial(4096)
            buf += sbuf
            break if sbuf.size < 4096
          end
          
          msgs = buf.split(PAD)
          
          msgs.each do |msg|
            m = Marshal.load(msg)
            if m == :end
              ios.delete(io)
            else
              idx, content = m
              res[idx] = content
            end
          end
        end
      end      
    end
    
    Process.waitall
  end
end

$size = (ARGV[0] || 100).to_i
csize = $size - 1

puts "P4"
puts "#{$size} #{$size}"

set = (0 ... $size).to_a

results = parallel_map(set, 8) do |y|
  res = ""
  
  byte_acc = 0
  bit_num  = 0
  
  ci = (2.0 * y / $size) - 1.0

  $size.times do |x|
    zrzr = zr = 0.0
    zizi = zi = 0.0
    cr = (2.0 * x / $size) - 1.5
    escape = 0b1
  
    50.times do
      tr = zrzr - zizi + cr
      ti = 2.0 * zr * zi + ci
      zr = tr
      zi = ti
      # preserve recalculation
      zrzr = zr * zr
      zizi = zi * zi
      if zrzr + zizi > 4.0
        escape = 0b0
        break
      end
    end
  
    byte_acc = (byte_acc << 1) | escape
    bit_num  += 1
    
    if (bit_num == 8)
      res += byte_acc.chr
      byte_acc = 0
      bit_num = 0
    elsif (x == csize)
      byte_acc <<= (8 - bit_num)
      res += byte_acc.chr
      byte_acc = 0
      bit_num = 0
    end
  end

  res
end

print results.join
#  The Computer Language Benchmarks Game
#  http://shootout.alioth.debian.org/
#
#  contributed by Karl von Laudermann
#  modified by Jeremy Echols
#  modified by Detlef Reichl
#  modified by Joseph LaFata
#  modified by Peter Zotov

size = ARGV.shift.to_i

puts "P4\n#{size} #{size}"

byte_acc = 0
bit_num = 0

y = 0
while y < size
  ci = (2.0*y/size)-1.0

  x = 0
  while x < size
    zrzr = zr = 0.0
    zizi = zi = 0.0
    cr = (2.0*x/size)-1.5
    escape = 0b1

    z = 0
    while z < 50
      tr = zrzr - zizi + cr
      ti = 2.0*zr*zi + ci
      zr = tr
      zi = ti
      # preserve recalculation
      zrzr = zr*zr
      zizi = zi*zi
      if zrzr+zizi > 4.0
        escape = 0b0
        break
      end
      z += 1
    end

    byte_acc = (byte_acc << 1) | escape
    bit_num += 1

    # Code is very similar for these cases, but using separate blocks
    # ensures we skip the shifting when it's unnecessary, which is most cases.
    if (bit_num == 8)
      print byte_acc.chr
      byte_acc = 0
      bit_num = 0
    elsif (x == size - 1)
      byte_acc <<= (8 - bit_num)
      print byte_acc.chr
      byte_acc = 0
      bit_num = 0
    end
    x += 1
  end
  y += 1
end
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: matrix.ruby,v 1.2 2005-03-23 06:11:41 bfulgham Exp $
# http://shootout.alioth.debian.org/
#
# Contributed by Christopher Williams

n = (ARGV[0] || 60).to_i
size = 30

def mkmatrix(rows, cols)
  count = 0
  Array.new(rows) do |i| 
    Array.new(cols) {|j| count +=1 }
  end
end

def mmult(rows, cols, m1, m2)
  m3 = []
  for i in 0 .. (rows - 1)
    row = []
    for j in 0 .. (cols - 1)
      val = 0
      for k in 0 .. (cols - 1)
        val += m1[i][k] * m2[k][j]
      end
      row << val
    end
    m3 << row
  end
  m3
end

m1 = mkmatrix(size, size)
m2 = mkmatrix(size, size)
mm = []
n.times do
  mm = mmult(size, size, m1, m2)
end
puts "#{mm[0][0]} #{mm[2][3]} #{mm[3][2]} #{mm[4][4]}"
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: matrix.ruby-2.ruby,v 1.1 2005-03-23 06:11:41 bfulgham Exp $
# http://shootout.alioth.debian.org/
#
# Contributed by Christopher Williams

n = (ARGV[0] || 60).to_i
size = 30
require 'matrix'
n = (ARGV[0] || 600).to_i
size = 30

def mkmatrix(rows,cols)
  count = 0
  the_rows = Array.new(rows) do |i| 
    Array.new(cols) {|j| count +=1 }
  end
  Matrix[*the_rows]
end

m1 = mkmatrix(size,size)
m2 = mkmatrix(size,size)
mm = []
n.times do
  mm = m1 * m2
end
puts "#{mm[0,0]} #{mm[2,3]} #{mm[3,2]} #{mm[4,4]}"


# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Jesse Millikan

require 'thread'

N = ARGV[0].to_i
next_q = last_q = SizedQueue.new(1)

500.times {
 q = SizedQueue.new(1)
 q2 = next_q
 Thread.new{
  i = N
  while i > 0
   q2.push(q.pop+1)
   i -= 1
  end
 }
 next_q = q
}

Thread.new{N.times{next_q.push(0)}}

t = 0
N.times{t+=last_q.pop}
puts t

#!/usr/bin/env ruby
#
# Gonzalo Garramuno -- Dec.31 2006
#

def coroutine(n)
  if n > 1
    coroutine(n-1) { |x| yield x + 1 }
  else
    yield 1 while true
  end
end

iter  = 0
last  = ARGV[0].to_i
count = 0

coroutine( 500 ) { |x|
  break if iter >= last
  count += x
  iter  += 1
}

puts count
#!/usr/bin/env ruby
#
# The Computer Language Shootout
#   http://shootout.alioth.debian.org
#   contributed by Kevin Barnes (Ruby novice)

def blank_board
  0b111111100000100000100000100000100000100000100000100000100000100000
end

def is_even( location)
  (location % 12) < 6
end

def create_collector_support
    odd_map = [0b11, 0b110, 0b1100, 0b11000, 0b10000] 
    even_map = [0b1, 0b11, 0b110, 0b1100, 0b11000] 
    
    all_odds = Array.new(0b100000)
    all_evens = Array.new(0b100000)
    bit_counts = Array.new(0b100000)
    new_regions = Array.new(0b100000)
    0.upto(0b11111) do | i |
      bit_count = odd = even = 0
      0.upto(4) do | bit |
        if (i[bit] == 1) then
          bit_count += 1
          odd |= odd_map[bit]
          even |= even_map[bit]
        end
      end
      all_odds[i] = odd
      all_evens[i] = even
      bit_counts[i] = bit_count
      new_regions[i] = create_regions( i)
    end

    @@converter = []
    10.times { | row | @@converter.push((row % 2 == 0) ? all_evens : all_odds) }
    @@bit_counts = bit_counts
    @@regions = new_regions.collect { | set | set.collect { | value | [ value, bit_counts[value], value] } }

  end
  
def prunable( board, location, slotting = false)
  collectors = []
  (location / 6).to_i.upto(9) do | row_on | 
    regions = @@regions[(board >> (row_on * 6)) & 0b11111 ^ 0b11111]
    converter = @@converter[row_on]
    initial_collector_count = collectors.length
    regions.each do | region |
      collector_found = nil
      region_mask = region[0]
      initial_collector_count.times do | collector_num |
        collector = collectors[collector_num]
        if (collector) then
          collector_mask = collector[0]
          if (collector_mask & region_mask != 0) then
            if (collector_found) then
              collector_found[0] |= collector_mask
              collector_found[1] += collector[1]
              collector_found[2] |= collector[2]
              collectors[collector_num] = nil
            else
              collector_found = collector
              collector[1] += region[1]
              collector[2] |= region_mask
            end
          end
        end
      end
      if (collector_found == nil) then
        collectors.push(Array.new(region))
      end
    end
    collectors.length.times do | collector_num |
      collector = collectors[collector_num]
      if (collector) then
        if (collector[2] == 0) then
          return true if (collector[1] % 5 != 0)
          collectors[collector_num] = nil
        else
          return false if (collector[2] == 0b11111 && !slotting)
          collector[0] = converter[collector[2]]
          collector[2] = 0
        end
      end
    end
    collectors.compact!
  end
  return false if (collectors.length <= 1) 
  collectors.any? { | collector | (collector[1] % 5) != 0 }
end
  
def as_binary( value)
  rtn = ""
  5.times do | i |
    rtn += "#{value[i]}"
  end
  rtn
end
  
def create_regions( value )
  regions = []
  cur_region = 0
  5.times do | bit |
    if (value[bit] == 1) then
      cur_region |= 1 << bit
    else
      if (cur_region !=0 ) then
        regions.push( cur_region)
        cur_region = 0;
      end
    end
  end
  regions.push(cur_region) if (cur_region != 0)
  regions
end

def print_board( board, padding = "", rows = 10, row_offset = 0)
  rows.times do | row |
    rtn = padding
    rtn = "#{rtn} " if ((row + row_offset) % 2) == 1 
    6.times do | col | 
      rtn = "#{rtn}#{board[row*6+col]} " 
    end
    print "#{rtn}\n"
  end
end  

class Rotation
  attr_reader :start_masks
  
  @@rotation_even_adder = { :west => -1, :east => 1, :nw => -7, :ne => -6, :sw => 5, :se => 6 }
  @@rotation_odd_adder = { :west => -1, :east => 1, :nw => -6, :ne => -5, :sw => 6, :se => 7 }
  
  def initialize( directions )
    values, min = get_values( directions )
    @even_offsets, @odd_offsets = normalize_offsets( values, min)
      
    @even_mask = mask_for_offsets( @even_offsets)
    @odd_mask = mask_for_offsets( @odd_offsets)

    @start_masks = Array.new(60)
    
    0.upto(59) do | offset |
      mask = is_even(offset) ? (@even_mask << offset) : (@odd_mask << offset)
      if (blank_board & mask == 0 && !prunable(blank_board | mask, 0, true)) then
        @start_masks[offset] = mask
      else
        @start_masks[offset] = false 
      end
    end
  end
  
  def offsets( location)
    if is_even( location) then
      @even_offsets.collect { | value | value + location }
    else
      @odd_offsets.collect { | value | value + location }
    end
  end
  
  def normalize_offsets( values, min)
    even_min = is_even(min)
    other_min = even_min ? min + 6 : min + 7
    other_values = values.collect do | value | 
      if is_even(value) then 
        value + 6 - other_min 
      else 
        value + 7 - other_min 
      end
    end
    values.collect! { | value | value - min }
    
    if even_min then
      [values, other_values]
    else
      [other_values, values]
    end
  end
  
  def mask_for_offsets( offsets )
    mask = 0
    offsets.each { | value | mask = mask + ( 1 << value ) }
    mask
  end

  def start_adjust( directions )
    south = east = 0;
    directions.each do | direction |
      east += 1 if ( direction == :sw || direction == :nw || direction == :west )
      south += 1 if ( direction == :nw || direction == :ne )   
    end
    [south, east]
  end

  def get_values ( directions )
    south, east = start_adjust(directions)
    min = start = south * 6 + east
    values = [ start ]
    directions.each do | direction |
      if (start % 12 >= 6) then 
        start += @@rotation_odd_adder[direction]
      else 
        start += @@rotation_even_adder[direction]
      end
      min = start if (start < min)
      values += [ start ]
    end
    
    if (values.length != 5)
      values.uniq!
    end
    
    [ values, min ]
  end
end

class Piece
  attr_reader :rotations, :type, :masks
  attr_accessor :placed
  
  @@flip_converter = { :west => :west, :east => :east, :nw => :sw, :ne => :se, :sw => :nw, :se => :ne }
  @@rotate_converter = { :west => :nw, :east => :se, :nw => :ne, :ne => :east, :sw => :west, :se => :sw }
  
  def initialize( directions, type )
    @type = type
    @rotations = Array.new();
    @map = {}
    generate_rotations( directions )
    directions.collect! { | value | @@flip_converter[value] }
    generate_rotations( directions )
    
    @masks = Array.new();
    0.upto(59) do | i |
      @masks[i] = @rotations.collect do | rotation | 
        mask = rotation.start_masks[i]
        @map[mask] = [ i, rotation ] if (mask) 
        mask || nil
      end
      @masks[i].compact!
    end
  end
  
  def generate_rotations( directions ) 
    6.times do
      rotations.push( Rotation.new(directions))
      directions.collect! { | value | @@rotate_converter[value] }
    end
  end
  
  def fill_array( board_array)
    location, rotation = @map[@placed]
    rotation.offsets(location).each do | offset |
      row, col = offset.divmod(6)
      board_array[ row*5 + col ] = @type.to_s
    end
  end
end

class Processor 
  attr :pieces, :board
  
  def initialize() 
    create_collector_support
    @pieces = [ 
      Piece.new( [ :east, :east, :east, :se ], 0),
      Piece.new( [ :ne, :east, :ne, :nw ], 1),
      Piece.new( [ :nw, :ne, :east, :east ], 2),
      Piece.new( [ :east, :east, :sw, :se ], 3),
      Piece.new( [ :ne, :nw, :se, :east, :se ], 4),
      Piece.new( [ :east, :ne, :se, :ne ], 5),
      Piece.new( [ :east, :sw, :sw, :se ], 6),
      Piece.new( [ :ne, :se, :east, :ne ], 7),
      Piece.new( [ :se, :se, :east, :se ], 8),
      Piece.new( [ :se, :se, :se, :west ], 9) ];
      
    @all_pieces = Array.new( @pieces)

    @min_board = "99999999999999999999999999999999999999999999999999"
    @max_board = "00000000000000000000000000000000000000000000000000"
    @stop_count = ARGV[0].to_i || 2089
    @all_boards = {}
    @boards_found = 0
  end
  
  def find_all
    find_top( 0)
    find_top( 1)
    print_results
  end

  def print_results
    print "#{@boards_found} solutions found\n\n"
    print_full_board( @min_board)
    print "\n"
    print_full_board( @max_board)
    print "\n"
  end

  def find_top( rotation_skip) 
    board = blank_board
    @pieces.length.times do
      piece = @pieces.shift
      piece.masks[0].each do | mask |
        if ((rotation_skip += 1) % 2 == 0) then
          piece.placed = mask
          find( 1, 1, board | mask) 
        end
      end
      @pieces.push(piece)
    end
  end

  def find( start_location, placed, board) 
    while board[start_location] == 1
      start_location += 1 
    end

    return if (start_location < 28 && prunable( board, start_location))
    
    @pieces.length.times do
      piece = @pieces.shift
      piece.masks[start_location].each do | mask |
        if (mask & board == 0) then
          piece.placed = mask
          if (placed == 9) then
            add_board
          else
            find( start_location + 1, placed + 1, board | mask) 
          end
        end
      end
      @pieces.push(piece)
    end
  end
  
  def print_full_board( board_string)
    10.times do | row |
      print " " if (row % 2 == 1) 
      5.times do | col |
        print "#{board_string[row*5 + col,1]} "
      end
      print "\n"
    end
  end
  
  def add_board
    board_array = Array.new(50)
    @all_pieces.each do | piece |
      piece.fill_array( board_array )
    end
    start_board = board_string = board_array.join("")
    save( board_string)
    board_string = flip( board_string)
    save( board_string)
  end

  def flip( board_string)
    new_string = ""
    50.times do | i |
      row, col = i.divmod(5)
      new_string += board_string[((9 - row) * 5) + (4 - col), 1]
    end
    new_string
  end
      
  def save( board_string)
    if (@all_boards[board_string] == nil) then
      @min_board = board_string if (board_string < @min_board)
      @max_board = board_string if (board_string > @max_board)
      @all_boards.store(board_string,true)
      @boards_found += 1

      if (@boards_found == @stop_count) then
        print_results
        exit(0)
      end
    end
  end
  
end

proc = Processor.new.find_all

#!/usr/bin/env ruby
#
# The Computer Language Shootout
#   http://shootout.alioth.debian.org
#   contributed by Kevin Barnes (Ruby novice)

# PROGRAM:  the main body is at the bottom.  
#   1) read about the problem here: http://www-128.ibm.com/developerworks/java/library/j-javaopt/
#   2) see how I represent a board as a bitmask by reading the blank_board comments
#   3) read as your mental paths take you

# class to represent all information about a particular rotation of a particular piece
class Rotation
  # an array (by location) containing a bit mask for how the piece maps at the given location.
  # if the rotation is illegal at that location the mask will contain false
  attr_reader :start_masks
  
  # maps a direction to a relative location.  these differ depending on whether it is an even or
  # odd row being mapped from
  @@rotation_even_adder = { :west => -1, :east => 1, :nw => -7, :ne => -6, :sw => 5, :se => 6 }
  @@rotation_odd_adder = { :west => -1, :east => 1, :nw => -6, :ne => -5, :sw => 6, :se => 7 }
  
  def initialize( directions )
    @even_offsets, @odd_offsets = normalize_offsets( get_values( directions ))
      
    @even_mask = mask_for_offsets( @even_offsets)
    @odd_mask = mask_for_offsets( @odd_offsets)

    @start_masks = Array.new(60)
    
    # create the rotational masks by placing the base mask at the location and seeing if
    # 1) it overlaps the boundries and 2) it produces a prunable board.  if either of these
    # is true the piece cannot be placed
    0.upto(59) do | offset |
      mask = is_even(offset) ? (@even_mask << offset) : (@odd_mask << offset)
      if (blank_board & mask == 0 && !prunable(blank_board | mask, 0, true)) then
        imask = compute_required( mask, offset)
        @start_masks[offset] = [ mask, imask, imask | mask ]
      else
        @start_masks[offset] = false 
      end
    end
  end
  
  def compute_required( mask, offset )
    board = blank_board
    0.upto(offset) { | i | board |= 1 << i }
    board |= mask
    return 0 if (!prunable(board | mask, offset))
    board = flood_fill(board,58)
    count = 0
    imask = 0
    0.upto(59) do | i |
      if (board[i] == 0) then
        imask |= (1 << i) 
        count += 1
      end
    end
    (count > 0 && count < 5) ? imask : 0
  end
  
  def flood_fill( board, location)
    return board if (board[location] == 1)
    board |= 1 << location
    row, col = location.divmod(6)
    board = flood_fill( board, location - 1) if (col > 0)
    board = flood_fill( board, location + 1) if (col < 4)
    if (row % 2 == 0) then
      board = flood_fill( board, location - 7) if (col > 0 && row > 0)
      board = flood_fill( board, location - 6) if (row > 0)
      board = flood_fill( board, location + 6) if (row < 9)
      board = flood_fill( board, location + 5) if (col > 0 && row < 9)
    else
      board = flood_fill( board, location - 5) if (col < 4 && row > 0)
      board = flood_fill( board, location - 6) if (row > 0)
      board = flood_fill( board, location + 6) if (row < 9)
      board = flood_fill( board, location + 7) if (col < 4 && row < 9)
    end
    board
  end
  
  # given a location, produces a list of relative locations covered by the piece at this rotation
  def offsets( location)
    if is_even( location) then
      @even_offsets.collect { | value | value + location }
    else
      @odd_offsets.collect { | value | value + location }
    end
  end
  
  # returns a set of offsets relative to the top-left most piece of the rotation (by even or odd rows)
  # this is hard to explain. imagine we have this partial board:
  #   0 0 0 0 0 x        [positions 0-5]
  #    0 0 1 1 0 x       [positions 6-11]
  #   0 0 1 0 0 x        [positions 12-17]
  #    0 1 0 0 0 x       [positions 18-23]
  #   0 1 0 0 0 x        [positions 24-29]
  #    0 0 0 0 0 x       [positions 30-35]
  #       ...
  # The top-left of the piece is at position 8, the
  # board would be passed as a set of positions (values array) containing [8,9,14,19,25] not necessarily in that
  # sorted order.  Since that array starts on an odd row, the offsets for an odd row are: [0,1,6,11,17] obtained 
  # by subtracting 8 from everything.  Now imagine the piece shifted up and to the right so it's on an even row:
  #   0 0 0 1 1 x        [positions 0-5]
  #    0 0 1 0 0 x       [positions 6-11]
  #   0 0 1 0 0 x        [positions 12-17]
  #    0 1 0 0 0 x       [positions 18-23]
  #   0 0 0 0 0 x        [positions 24-29]
  #    0 0 0 0 0 x       [positions 30-35]
  #       ...
  # Now the positions are [3,4,8,14,19] which after subtracting the lowest value (3) gives [0,1,5,11,16] thus, the 
  # offsets for this particular piece are (in even, odd order) [0,1,5,11,16],[0,1,6,11,17] which is what
  # this function would return
  def normalize_offsets( values)
    min = values.min
    even_min = is_even(min)
    other_min = even_min ? min + 6 : min + 7
    other_values = values.collect do | value | 
      if is_even(value) then 
        value + 6 - other_min 
      else 
        value + 7 - other_min 
      end
    end
    values.collect! { | value | value - min }
    
    if even_min then
      [values, other_values]
    else
      [other_values, values]
    end
  end
  
  # produce a bitmask representation of an array of offset locations
  def mask_for_offsets( offsets )
    mask = 0
    offsets.each { | value | mask = mask + ( 1 << value ) }
    mask
  end

  # finds a "safe" position that a position as described by a list of directions can be placed
  # without falling off any edge of the board.  the values returned a location to place the first piece
  # at so it will fit after making the described moves
  def start_adjust( directions )
    south = east = 0;
    directions.each do | direction |
      east += 1 if ( direction == :sw || direction == :nw || direction == :west )
      south += 1 if ( direction == :nw || direction == :ne )   
    end
    south * 6 + east
  end

  # given a set of directions places the piece (as defined by a set of directions) on the board at 
  # a location that will not take it off the edge
  def get_values ( directions )
    start = start_adjust(directions)
    values = [ start ]
    directions.each do | direction |
      if (start % 12 >= 6) then 
        start += @@rotation_odd_adder[direction]
      else 
        start += @@rotation_even_adder[direction]
      end
      values += [ start ]
    end
    
    # some moves take you back to an existing location, we'll strip duplicates
    values.uniq
  end
end

# describes a piece and caches information about its rotations to as to be efficient for iteration
# ATTRIBUTES:
#   rotations -- all the rotations of the piece
#   type -- a numeic "name" of the piece
#   masks -- an array by location of all legal rotational masks (a n inner array) for that location
#   placed -- the mask that this piece was last placed at (not a location, but the actual mask used)
class Piece
  attr_reader :rotations, :type, :masks 
  attr_accessor :placed 
  
  # transform hashes that change one direction into another when you either flip or rotate a set of directions
  @@flip_converter = { :west => :west, :east => :east, :nw => :sw, :ne => :se, :sw => :nw, :se => :ne }
  @@rotate_converter = { :west => :nw, :east => :se, :nw => :ne, :ne => :east, :sw => :west, :se => :sw }
  
  def initialize( directions, type )
    @type = type
    @rotations = Array.new();
    @map = {}
    
    generate_rotations( directions )
    directions.collect! { | value | @@flip_converter[value] }
    generate_rotations( directions )
    
    # creates the masks AND a map that returns [location, rotation] for any given mask
    # this is used when a board is found and we want to draw it, otherwise the map is unused
    @masks = Array.new();
    0.upto(59) do | i |
      even = true
      @masks[i] = @rotations.collect do | rotation | 
        mask = rotation.start_masks[i]
        @map[mask[0]] = [ i, rotation ] if (mask) 
        mask || nil
      end
      @masks[i].compact!
    end
  end
  
  # rotates a set of directions through all six angles and adds a Rotation to the list for each one
  def generate_rotations( directions ) 
    6.times do
      rotations.push( Rotation.new(directions))
      directions.collect! { | value | @@rotate_converter[value] }
    end
  end
  
  # given a board string, adds this piece to the board at whatever location/rotation
  # important: the outbound board string is 5 wide, the normal location notation is six wide (padded)
  def fill_string( board_string)
    location, rotation = @map[@placed]
    rotation.offsets(location).each do | offset |
      row, col = offset.divmod(6)
      board_string[ row*5 + col, 1 ] = @type.to_s
    end
  end
end

# a blank bit board having this form:
#
#    0 0 0 0 0 1
#     0 0 0 0 0 1
#    0 0 0 0 0 1
#     0 0 0 0 0 1
#    0 0 0 0 0 1
#     0 0 0 0 0 1
#    0 0 0 0 0 1
#     0 0 0 0 0 1
#    0 0 0 0 0 1
#     0 0 0 0 0 1
#    1 1 1 1 1 1
#
# where left lest significant bit is the top left and the most significant is the lower right 
# the actual board only consists of the 0 places, the 1 places are blockers to keep things from running 
# off the edges or bottom
def blank_board
  0b111111100000100000100000100000100000100000100000100000100000100000
end

def full_board
  0b111111111111111111111111111111111111111111111111111111111111111111
end

# determines if a location (bit position) is in an even row
def is_even( location)
  (location % 12) < 6
end

# support function that create three utility maps:
#  @@converter -- for each row an array that maps a five bit row (via array mapping) 
#                 to the a a five bit representation of the bits below it
#  @@bit_count -- maps a five bit row (via array mapping) to the number of 1s in the row
#  @@new_regions -- maps a five bit row (via array mapping) to an array of "region" arrays
#                   a region array has three values the first is a mask of bits in the region, 
#                   the second is the count of those bits and the third is identical to the first
#                   examples:
#                           0b10010 => [ 0b01100, 2, 0b01100 ], [ 0b00001, 1, 0b00001]
#                           0b01010 => [ 0b10000, 1, 0b10000 ], [ 0b00100, 1, 0b00100 ], [ 0b00001, 1, 0b00001]
#                           0b10001 => [ 0b01110, 3, 0b01110 ]
def create_collector_support
  odd_map = [0b11, 0b110, 0b1100, 0b11000, 0b10000] 
  even_map = [0b1, 0b11, 0b110, 0b1100, 0b11000] 
  
  all_odds = Array.new(0b100000)
  all_evens = Array.new(0b100000)
  bit_counts = Array.new(0b100000)
  new_regions = Array.new(0b100000)
  0.upto(0b11111) do | i |
    bit_count = odd = even = 0
    0.upto(4) do | bit |
      if (i[bit] == 1) then
        bit_count += 1
        odd |= odd_map[bit]
        even |= even_map[bit]
      end
    end
    all_odds[i] = odd
    all_evens[i] = even
    bit_counts[i] = bit_count
    new_regions[i] = create_regions( i)
  end

  @@converter = []
  10.times { | row | @@converter.push((row % 2 == 0) ? all_evens : all_odds) }
  @@bit_counts = bit_counts
  @@regions = new_regions.collect { | set | set.collect { | value | [ value, bit_counts[value], value] } }
end
 
# determines if a board is punable, meaning that there is no possibility that it 
# can be filled up with pieces.  A board is prunable if there is a grouping of unfilled spaces
# that are not a multiple of five.  The following board is an example of a prunable board:
#    0 0 1 0 0
#     0 1 0 0 0
#    1 1 0 0 0
#     0 1 0 0 0
#    0 0 0 0 0
#       ...
#
# This board is prunable because the top left corner is only 3 bits in area, no piece will ever fit it
# parameters:
#   board -- an initial bit board (6 bit padded rows, see blank_board for format)
#   location -- starting location, everything above and to the left is already full
#   slotting -- set to true only when testing initial pieces, when filling normally
#               additional assumptions are possible
#
# Algorithm:
#    The algorithm starts at the top row (as determined by location) and iterates a row at a time
#    maintainng counts of active open areas (kept in the collector array) each collector contains
#    three values at the start of an iteration: 
#          0: mask of bits that would be adjacent to the collector in this row
#          1: the number of bits collected so far
#          2: a scratch space starting as zero, but used during the computation to represent
#             the empty bits in the new row that are adjacent (position 0)
#  The exact procedure is described in-code 
def prunable( board, location, slotting = false)
  collectors = []
  # loop accross the rows
  (location / 6).to_i.upto(9) do | row_on | 
    # obtain a set of regions representing the bits of the curent row.  
    regions = @@regions[(board >> (row_on * 6)) & 0b11111]
    converter = @@converter[row_on]
    
    # track the number of collectors at the start of the cycle so that
    # we don't compute against newly created collectors, only existing collectors
    initial_collector_count = collectors.length
    
    # loop against the regions.  For each region of the row
    # we will see if it connects to one or more existing collectors.
    # if it connects to 1 collector, the bits from the region are added to the 
    # bits of the collector and the mask is placed in collector[2]
    # If the region overlaps more than one collector then all the collectors
    # it overlaps with are merged into the first one (the others are set to nil in the array) 
    # if NO collectors are found then the region is copied as a new collector
    regions.each do | region |
      collector_found = nil
      region_mask = region[2]
      initial_collector_count.times do | collector_num |
        collector = collectors[collector_num]
        if (collector) then
          collector_mask = collector[0]
          if (collector_mask & region_mask != 0) then
            if (collector_found) then
              collector_found[0] |= collector_mask
              collector_found[1] += collector[1]
              collector_found[2] |= collector[2]
              collectors[collector_num] = nil
            else
              collector_found = collector
              collector[1] += region[1]
              collector[2] |= region_mask
            end
          end
        end
      end
      if (collector_found == nil) then
        collectors.push(Array.new(region))
      end
    end
    
    # check the existing collectors, if any collector overlapped no bits in the region its [2] value will
    # be zero.  The size of any such reaason is tested if it is not a muliple of five true is returned since
    # the board is prunable.  if it is a multiple of five it is removed.
    # Collector that are still active have a new adjacent value [0] set based n the matched bits 
    # and have [2] cleared out for the next cycle.
    collectors.length.times do | collector_num |
      collector = collectors[collector_num]
      if (collector) then
        if (collector[2] == 0) then
          return true if (collector[1] % 5 != 0)
          collectors[collector_num] = nil
        else
          # if a collector matches all bits in the row then we can return unprunable early for the 
          # follwing reasons:
          #    1) there can be no more unavailable bits bince we fill from the top left downward
          #    2) all previous regions have been closed or joined so only this region can fail
          #    3) this region must be good since there can never be only 1 region that is nuot
          #       a multiple of five
          # this rule only applies when filling normally, so we ignore the rule if we are "slotting"
          # in pieces to see what configurations work for them (the only other time this algorithm is used).
          return false if (collector[2] == 0b11111 && !slotting)
          collector[0] = converter[collector[2]]
          collector[2] = 0
        end
      end
    end
    
    # get rid of all the empty converters for the next round
    collectors.compact!
  end
  return false if (collectors.length <= 1) # 1 collector or less and the region is fine
  collectors.any? { | collector | (collector[1] % 5) != 0 } # more than 1 and we test them all for bad size
end
  
# creates a region given a row mask.  see prunable for what a "region" is
def create_regions( value )
  regions = []
  cur_region = 0
  5.times do | bit |
    if (value[bit] == 0) then
      cur_region |= 1 << bit
    else
      if (cur_region != 0 ) then
        regions.push( cur_region)
        cur_region = 0;
      end
    end
  end
  regions.push(cur_region) if (cur_region != 0)
  regions
end

# find up to the counted number of solutions (or all solutions) and prints the final result
def find_all
  find_top( 1)
  find_top( 0)
  print_results
end

# show the board
def print_results
  print "#{@boards_found} solutions found\n\n"
  print_full_board( @min_board)
  print "\n"
  print_full_board( @max_board)
  print "\n"
end

# finds solutions.  This special version of the main function is only used for the top level
# the reason for it is basically to force a particular ordering on how the rotations are tested for
# the first piece.  It is called twice, first looking for placements of the odd rotations and then 
# looking for placements of the even locations.
# 
# WHY?
#   Since any found solution has an inverse we want to maximize finding solutions that are not already found 
#   as an inverse.  The inverse will ALWAYS be 3 one of the piece configurations that is exactly 3 rotations away 
#   (an odd number).  Checking even vs odd then produces a higher probability of finding more pieces earlier 
#   in the cycle.  We still need to keep checking all the permutations, but our probability of finding one will
#   diminsh over time.  Since we are TOLD how many to search for this lets us exit before checking all pieces
#   this bennifit is very great when seeking small numbers of solutions and is 0 when looking for more than the 
#   maximum number 
def find_top( rotation_skip) 
  board = blank_board
  (@pieces.length-1).times do
    piece = @pieces.shift
    piece.masks[0].each do | mask, imask, cmask |
      if ((rotation_skip += 1) % 2 == 0) then
        piece.placed = mask
        find( 1, 1, board | mask) 
      end
    end
    @pieces.push(piece)
  end
  piece = @pieces.shift
  @pieces.push(piece)
end

# the normail find routine, iterates through the available pieces, checks all rotations at the current location
# and adds any boards found.  depth is acheived via recursion.  the overall approach is described 
# here: http://www-128.ibm.com/developerworks/java/library/j-javaopt/
# parameters:
#  start_location -- where to start looking for place for the next piece at
#  placed -- number of pieces placed
#  board -- current state of the board
#
# see in-code comments
def find( start_location, placed, board) 
  # find the next location to place a piece by looking for an empty bit
  while board[start_location] == 1
    start_location += 1 
  end
  
  @pieces.length.times do
    piece = @pieces.shift
    piece.masks[start_location].each do | mask, imask, cmask |
      if ( board & cmask == imask) then
        piece.placed = mask
        if (placed == 9) then
          add_board
        else
          find( start_location + 1, placed + 1, board | mask) 
        end
      end
    end
    @pieces.push(piece)
  end
end

# print the board
def print_full_board( board_string)
  10.times do | row |
    print " " if (row % 2 == 1) 
    5.times do | col |
      print "#{board_string[row*5 + col,1]} "
    end
    print "\n"
  end
end

# when a board is found we "draw it" into a string and then flip that string, adding both to
# the list (hash) of solutions if they are unique.  
def add_board
  board_string = "99999999999999999999999999999999999999999999999999"
  @all_pieces.each {  | piece | piece.fill_string( board_string ) }
  save( board_string)
  save( board_string.reverse)
end

# adds a board string to the list (if new) and updates the current best/worst board
def save( board_string)
  if (@all_boards[board_string] == nil) then
    @min_board = board_string if (board_string < @min_board)
    @max_board = board_string if (board_string > @max_board)
    @all_boards.store(board_string,true)
    @boards_found += 1

    # the exit motif is a time saver.  Ideally the function should return, but those tests
    # take noticable time (performance).
    if (@boards_found == @stop_count) then
      print_results
      exit(0)
    end
  end
end


##
## MAIN BODY :)
##
create_collector_support
@pieces = [ 
  Piece.new( [ :nw, :ne, :east, :east ], 2),
  Piece.new( [ :ne, :se, :east, :ne ], 7),
  Piece.new( [ :ne, :east, :ne, :nw ], 1),
  Piece.new( [ :east, :sw, :sw, :se ], 6),
  Piece.new( [ :east, :ne, :se, :ne ], 5),
  Piece.new( [ :east, :east, :east, :se ], 0),
  Piece.new( [ :ne, :nw, :se, :east, :se ], 4),
  Piece.new( [ :se, :se, :se, :west ], 9), 
  Piece.new( [ :se, :se, :east, :se ], 8),
  Piece.new( [ :east, :east, :sw, :se ], 3)
  ];
  
@all_pieces = Array.new( @pieces)

@min_board = "99999999999999999999999999999999999999999999999999"
@max_board = "00000000000000000000000000000000000000000000000000"
@stop_count = ARGV[0].to_i || 2089
@all_boards = {}
@boards_found = 0

find_all ######## DO IT!!!

#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: methcall.ruby,v 1.1 2004-05-19 18:10:41 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Aristarkh Zagorodnikov

class Toggle
    def initialize(start_state)
	@bool = start_state
    end

    def value
	@bool
    end

    def activate
	@bool = !@bool
	self
    end
end

class NthToggle < Toggle
    def initialize(start_state, max_counter)
	super start_state
	@count_max = max_counter
	@counter = 0
    end

    def activate
	@counter += 1
	if @counter >= @count_max
	    @bool = !@bool
	    @counter = 0
	end
	self
    end
end

def main()
    n = Integer(ARGV.shift || 1)

    val = 1
    toggle = Toggle.new(val)
    n.times do
	val = toggle.activate().value()
    end
    if val then puts "true" else puts "false" end

    val = 1
    ntoggle = NthToggle.new(val, 3)
    n.times do
	val = ntoggle.activate().value()
    end
    if val then puts "true" else puts "false" end
end

main()

#!/usr/bin/tclsh
# $Id: methcall.ruby-2.ruby,v 1.1 2005-04-16 15:11:10 igouy-guest Exp $

# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Hemang Lavana
# This program is based on object.tcl

package require Itcl

::itcl::class Toggle {
    variable _state
    constructor {start_state} {set _state $start_state}
    public method value {} { return [expr {$_state ? true : false}]}
    public method activate {} { 
        set _state [expr {!$_state}] 
        return $this
    }
}

::itcl::class NthToggle {
    inherit Toggle
    variable _counter
    variable _count_max

    constructor {start_state max_counter} {Toggle::constructor $start_state} {
        set _counter 0
        set _count_max $max_counter
    }
    method activate {} {
        incr _counter 1
        if {$_counter >= $_count_max} {
            set _state [expr {!$_state}]
            set _counter 0
        }
        return $this
    }
}

proc main {n} {
    Toggle toggle TRUE
    for {set i 0} {$i<$n} {incr i} {
        set value [[toggle activate] value]
    }
    puts $value

    NthToggle ntoggle TRUE 3
    for {set i 0} {$i<$n} {incr i} {
        set value [[ntoggle activate] value]
    }
    puts $value
}
main [lindex $argv 0]
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: moments.ruby,v 1.2 2005-06-10 00:57:22 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/ 

# throw away unused parameter sent by benchmark framework
ARGV.shift()

def main ()
    sum = 0.0
    nums = []
    num = nil

    for line in STDIN.readlines()
	num = Float(line)
	nums << num
	sum += num
    end

    n = nums.length()
    mean = sum/n;
    deviation = 0.0
    average_deviation = 0.0
    standard_deviation = 0.0
    variance = 0.0
    skew = 0.0
    kurtosis = 0.0
    
    for num in nums
	deviation = num - mean
	average_deviation += deviation.abs()
	variance += deviation**2;
	skew += deviation**3;
	kurtosis += deviation**4
    end
    average_deviation /= n
    variance /= (n - 1)
    standard_deviation = Math.sqrt(variance)

    if (variance > 0.0)
	skew /= (n * variance * standard_deviation)
	kurtosis = kurtosis/(n * variance * variance) - 3.0
    end

    nums.sort()
    mid = n / 2
    
    if (n % 2) == 0
	median = (nums.at(mid) + nums.at(mid-1))/2
    else
	median = nums.at(mid)
    end
    
    printf("n:                  %d\n", n)
    printf("median:             %f\n", median)
    printf("mean:               %f\n", mean)
    printf("average_deviation:  %f\n", average_deviation)
    printf("standard_deviation: %f\n", standard_deviation)
    printf("variance:           %f\n", variance)
    printf("skew:               %f\n", skew)
    printf("kurtosis:           %f\n", kurtosis)
end

main()
# The Computer Language Shootout
# http://shootout.alioth.debian.org
#
# Optimized for Ruby by Jesse Millikan
# From version ported by Michael Neumann from the C gcc version, 
# which was written by Christoph Bauer. 

SOLAR_MASS = 4 * Math::PI**2
DAYS_PER_YEAR = 365.24

class Planet
 attr_accessor :x, :y, :z, :vx, :vy, :vz, :mass

 def initialize(x, y, z, vx, vy, vz, mass)
  @x, @y, @z = x, y, z
  @vx, @vy, @vz = vx * DAYS_PER_YEAR, vy * DAYS_PER_YEAR, vz * DAYS_PER_YEAR 
  @mass = mass * SOLAR_MASS
 end

 def move_from_i(bodies, nbodies, dt, i)
  while i < nbodies
   b2 = bodies[i]
   dx = @x - b2.x
   dy = @y - b2.y
   dz = @z - b2.z

   distance = Math.sqrt(dx * dx + dy * dy + dz * dz)
   mag = dt / (distance * distance * distance)
   b_mass_mag, b2_mass_mag = @mass * mag, b2.mass * mag

   @vx -= dx * b2_mass_mag
   @vy -= dy * b2_mass_mag
   @vz -= dz * b2_mass_mag
   b2.vx += dx * b_mass_mag
   b2.vy += dy * b_mass_mag
   b2.vz += dz * b_mass_mag
   i += 1 
  end

  @x += dt * @vx
  @y += dt * @vy
  @z += dt * @vz
 end
end

def energy(bodies)
  e = 0.0
  nbodies = bodies.size
 
  for i in 0 ... nbodies 
    b = bodies[i]
    e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz)
    for j in (i + 1) ... nbodies
      b2 = bodies[j]
      dx = b.x - b2.x
      dy = b.y - b2.y
      dz = b.z - b2.z
      distance = Math.sqrt(dx * dx + dy * dy + dz * dz)
      e -= (b.mass * b2.mass) / distance
    end
  end
  e
end

def offset_momentum(bodies)
  px, py, pz = 0.0, 0.0, 0.0

  for b in bodies
    m = b.mass
    px += b.vx * m
    py += b.vy * m
    pz += b.vz * m
  end

  b = bodies[0]
  b.vx = - px / SOLAR_MASS
  b.vy = - py / SOLAR_MASS
  b.vz = - pz / SOLAR_MASS
end

BODIES = [
  # sun
  Planet.new(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),

  # jupiter
  Planet.new(
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03,
    7.69901118419740425e-03,
    -6.90460016972063023e-05,
    9.54791938424326609e-04),

  # saturn
  Planet.new(
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03,
    4.99852801234917238e-03,
    2.30417297573763929e-05,
    2.85885980666130812e-04),

  # uranus
  Planet.new(
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03,
    2.37847173959480950e-03,
    -2.96589568540237556e-05,
    4.36624404335156298e-05),

  # neptune
  Planet.new(
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03,
    1.62824170038242295e-03,
    -9.51592254519715870e-05,
    5.15138902046611451e-05)
]


n = Integer(ARGV[0])

offset_momentum(BODIES)

puts "%.9f" % energy(BODIES)

nbodies = BODIES.size
dt = 0.01

n.times do
  i = 0
  while i < nbodies
    b = BODIES[i]
    b.move_from_i(BODIES, nbodies, dt, i + 1)
    i += 1
  end
end

puts "%.9f" % energy(BODIES)
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: nestedloop.ruby,v 1.1 2004-05-19 18:10:57 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from Avi Bryant

n = Integer(ARGV.shift || 1)
x = 0
n.times do
    n.times do
	n.times do
	    n.times do
		n.times do
		    n.times do
			x += 1
		    end
		end
	    end
	end
    end
end
puts x
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by Glenn Parker, March 2005
# modified by Evan Phoenix, Sept 2006

def sieve(m)
  flags = Flags.dup[0,m]
  count = 0
  pmax = m - 1
  p = 2
  while p <= pmax
    unless flags[p].zero?
      count += 1
      mult = p
      while mult <= pmax
        flags[mult] = 0
        mult += p
      end
    end
    p += 1
  end
  count
end

n = (ARGV[0] || 2).to_i

Flags = "\x1" * ( 2 ** n * 10_000)

n.downto(n-2) do |exponent|
  break if exponent < 0
  m = (1 << exponent) * 10_000
  # m = (2 ** exponent) * 10_000
  count = sieve(m)
  printf "Primes up to %8d %8d\n", m, count
end
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# contributed by Pavel Valodzka

def nsieve(m)
  is_prime = Array.new(m, true)
  count = 0
  2.upto(m){|i|
    if is_prime[i]
      (2 * i).step(m, i){|v|
        is_prime[v] = false
      }
      count += 1
    end
  }
  return count
end

n = (ARGV[0] || 2).to_i
n = 2 if (n < 2)

3.times {|t|
  m = (1<<n-t)*10000
  printf("Primes up to %8d%9d\n", m, nsieve(m))  
}
#!/usr/bin/ruby
#
# The Great Computer Language Shootout 
# http://shootout.alioth.debian.org/
#
# nsieve-bits in Ruby
# Contributed by Glenn Parker, March 2005

CharExponent = 3
BitsPerChar = 1 << CharExponent
LowMask = BitsPerChar - 1

def sieve(m)
  items = "\xFF" * ((m / BitsPerChar) + 1)
  masks = ""
  BitsPerChar.times do |b|
    masks << (1 << b).chr
  end

  count = 0
  pmax = m - 1
  2.step(pmax, 1) do |p|
    if items[p >> CharExponent][p & LowMask] == 1
      count += 1
      p.step(pmax, p) do |mult|
	a = mult >> CharExponent
	b = mult & LowMask
	items[a] -= masks[b] if items[a][b] != 0
      end
    end
  end
  count
end

n = (ARGV[0] || 2).to_i
n.step(n - 2, -1) do |exponent|
  break if exponent < 0
  m = 2 ** exponent * 10_000
  count = sieve(m)
  printf "Primes up to %8d %8d\n", m, count
end

#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: objinst.ruby,v 1.1 2004-05-19 18:11:03 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Aristarkh Zagorodnikov

class Toggle
    def initialize(start_state)
	@bool = start_state
    end

    def value
	@bool
    end

    def activate
	@bool = !@bool
	self
    end
end

class NthToggle < Toggle
    def initialize(start_state, max_counter)
	super start_state
	@count_max = max_counter
	@counter = 0
    end

    def activate
	@counter += 1
	if @counter >= @count_max
	    @bool = !@bool
	    @counter = 0
	end
	self
    end
end

n = (ARGV.shift || 1).to_i

toggle = Toggle.new 1
5.times do
    puts toggle.activate.value ? 'true' : 'false'
end
n.times do
    toggle = Toggle.new 1
end

puts

ntoggle = NthToggle.new 1, 3
8.times do
    puts ntoggle.activate.value ? 'true' : 'false'
end
n.times do
    ntoggle = NthToggle.new 1, 3
end

# ----------------------------------------------------------------------
# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Based on D language implementation by Dave Fladebo
#
# Contributed by Anthony Borla
# ----------------------------------------------------------------------

n = (ARGV.shift || 1).to_i

alt = 1.0 ; s0 = s1 = s2 = s3 = s4 = s5 = s6 = s7 = s8 = 0.0

1.upto(n) do |d|
  d = d.to_f ; d2 = d * d ; d3 = d2 * d ; ds = Math.sin(d) ; dc = Math.cos(d)

  s0 += (2.0 / 3.0) ** (d - 1.0)
  s1 += 1.0 / Math.sqrt(d)
  s2 += 1.0 / (d * (d + 1.0))
  s3 += 1.0 / (d3 * ds * ds)
  s4 += 1.0 / (d3 * dc * dc)
  s5 += 1.0 / d
  s6 += 1.0 / d2
  s7 += alt / d
  s8 += alt / (2.0 * d - 1.0)

  alt = -alt
end

printf("%.9f\t(2/3)^k\n", s0)
printf("%.9f\tk^-0.5\n", s1)
printf("%.9f\t1/k(k+1)\n", s2)
printf("%.9f\tFlint Hills\n", s3)
printf("%.9f\tCookson Hills\n", s4)
printf("%.9f\tHarmonic\n", s5)
printf("%.9f\tRiemann Zeta\n", s6)
printf("%.9f\tAlternating Harmonic\n", s7)
printf("%.9f\tGregory\n", s8)

# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by Gabriele Renzi

class PiDigitSpigot 
 
    def initialize()
        @z = Transformation.new 1,0,0,1
        @x = Transformation.new 0,0,0,0
        @inverse = Transformation.new 0,0,0,0
    end

    def next!
        @y = @z.extract(3) 
        if safe? @y
            @z = produce(@y)
            @y
        else 
            @z = consume @x.next!() 
            next!()
        end
    end

    def safe?(digit)
        digit == @z.extract(4)
    end

    def produce(i)
        @inverse.qrst(10,-10*i,0,1).compose(@z)
    end

    def consume(a)
        @z.compose(a)
    end
end


class Transformation 
    attr_reader :q, :r, :s, :t
    def initialize (q, r, s, t)
        @q,@r,@s,@t,@k = q,r,s,t,0
    end
    
    def next!()
        @q = @k = @k + 1
        @r = 4 * @k + 2
        @s = 0
        @t = 2 * @k + 1
        self
    end

    def extract(j)
        (@q * j + @r) / (@s * j + @t)
    end
    
    def compose(a)
        self.class.new( @q * a.q,
                        @q * a.r + r * a.t,
                        @s * a.q + t * a.s,
                        @s * a.r + t * a.t
                    ) 
    end
    
    def qrst *args
        initialize *args
        self
    end

    
end


WIDTH = 10
n = Integer(ARGV[0])
j = 0

digits = PiDigitSpigot.new

while n > 0
    if n >= WIDTH
        WIDTH.times {print digits.next!}
        j += WIDTH
    else 
        n.times {print digits.next!}
        (WIDTH-n).times {print " "} 
        j += n
    end
    puts "\t:"+j.to_s
    n -= WIDTH
end

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Use libgmp-ruby_1.0 
#
# contributed by Gabriele Renzi
# modified by Pilho Kim

require 'gmp'

class PiDigitSpigot
    def initialize()
        @ZERO = GMP::Z.new(0)
        @ONE = GMP::Z.new(1)
        @THREE = GMP::Z.new(3)
        @FOUR = GMP::Z.new(4)
        @TEN = GMP::Z.new(10)
        @z = Transformation.new @ONE,@ZERO,@ZERO,@ONE
        @x = Transformation.new @ZERO,@ZERO,@ZERO,@ZERO
        @inverse = Transformation.new @ZERO,@ZERO,@ZERO,@ZERO
    end

    def next!
        @y = @z.extract(@THREE)
        if safe? @y
            @z = produce(@y)
            @y
        else
            @z = consume @x.next!()
            next!()
        end
    end

    def safe?(digit)
        digit == @z.extract(@FOUR)
    end

    def produce(i)
        @inverse.qrst(@TEN,-@TEN*i,@ZERO,@ONE).compose(@z)
    end

    def consume(a)
        @z.compose(a)
    end
end


class Transformation
    attr_reader :q, :r, :s, :t
    def initialize (q, r, s, t)
        @ZERO = GMP::Z.new(0)
        @ONE = GMP::Z.new(1)
        @TWO = GMP::Z.new(2)
        @FOUR = GMP::Z.new(4)
        @q,@r,@s,@t,@k = q,r,s,t,@ZERO
    end

    def next!()
        @q = @k = @k + @ONE
        @r = @FOUR * @k + @TWO
        @s = @ZERO
        @t = @TWO * @k + @ONE
        self
    end

    def extract(j)
        (@q * j + @r).tdiv( @s * j + @t )
    end

    def compose(a)
        self.class.new( @q * a.q,
                        @q * a.r + r * a.t,
                        @s * a.q + t * a.s,
                        @s * a.r + t * a.t
                    )
    end

    def qrst *args
        initialize *args
        self
    end

end


@zero = GMP::Z.new(0)
@one = GMP::Z.new(1)
@two = GMP::Z.new(2)
@four = GMP::Z.new(4)
@ten = GMP::Z.new(10)

WIDTH = 10
n = Integer(ARGV[0] || "27")
j = 0

digits = PiDigitSpigot.new

while n > 0
    if n >= WIDTH
        WIDTH.times {print digits.next!}
        j += WIDTH
    else
        n.times {print digits.next!}
        (WIDTH-n).times {print " "}
        j += n
    end
    puts "\t:"+j.to_s
    n -= WIDTH
end
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/

# transliterated from Mario Pernici's Python program
# contributed by Rick Branson

N = (ARGV[0] || 100).to_i

i = k = ns = 0
k1 = 1
n,a,d,t,u = [1,0,1,0,0]

loop do
  k += 1
  t = n<<1
  n *= k
  a += t
  k1 += 2
  a *= k1
  d *= k1
  if a >= n
    t,u = (n*3 +a).divmod(d)
    u += n
    if d > u
      ns = ns*10 + t
      i += 1
      if i % 10 == 0
        puts "#{ns.to_s.rjust(10, '0')}\t:#{i.to_s}"
        ns = 0
      end
      break if i >= N
   
      a -= d*t
      a *= 10
      n *= 10
    end
  end
end
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: prodcons.ruby,v 1.3 2005-06-10 00:57:22 igouy-guest Exp $
# http://www.bagley.org/~doug/shootout/

require 'thread'

def main(n)
    mutex = Mutex.new
    access = ConditionVariable.new
    count = data = consumed = produced = 0
    consumer = Thread.new do
	i = 0
	loop do
	    mutex.synchronize {
		while count == 0 do access.wait(mutex) end
		i = data
		count = 0
		access.signal
	    }
	    consumed += 1
	    if i == n then break end
	end
    end
    producer = Thread.new do
	for i in 1 .. n do
	    mutex.synchronize {
		while count == 1 do access.wait(mutex) end
		data = i
		count = 1
		access.signal
	    }
	    produced += 1
	end
    end
    producer.join
    consumer.join
    puts "#{produced} #{consumed}"
end

main(Integer(ARGV.shift || 1))
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: random.ruby,v 1.1 2004-05-19 18:11:16 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

IM = 139968
IA = 3877
IC = 29573

$last = 42.0
def gen_random (max) (max * ($last = ($last * IA + IC) % IM)) / IM end

N = Integer(ARGV.shift || 1) - 1
N.times do
    gen_random(100.0)
end
printf "%.9f\n", gen_random(100.0)
# ----------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Code based on / inspired by existing, relevant Shootout submissions
#
# Contributed by Anthony Borla
# Optimized by Jesse Millikan
# ----------------------------------------------------------------------

def ack(m, n)
  if m == 0 then 
    n + 1
  else if n == 0 then
    ack(m - 1, 1)
   else 
     ack(m - 1, ack(m, n - 1))
   end
  end
end

# ---------------------------------

def fib(n)
   if n > 1 then
     fib(n - 2) + fib(n - 1) 
   else 
     1
   end
end

# ---------------------------------

def tak(x, y, z)
  if y < x then
   tak(tak(x - 1.0, y, z), tak(y - 1.0, z, x), tak(z - 1.0, x, y))
  else z
  end
end

# ---------------------------------

n = (ARGV.shift || 1).to_i

printf("Ack(3,%d): %d\n", n, ack(3, n));
printf("Fib(%.1f): %.1f\n", 27.0 + n, fib(27.0 + n));

n -= 1;
printf("Tak(%d,%d,%d): %d\n", n * 3, n * 2, n, tak(n * 3, n * 2, n));

printf("Fib(3): %d\n", fib(3));
printf("Tak(3.0,2.0,1.0): %.1f\n", tak(3.0, 2.0, 1.0));

# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by jose fco. gonzalez
seq = STDIN.readlines.join
ilen = seq.size

seq.gsub!(/>.*\n|\n/,"")
clen = seq.length

[
  /agggtaaa|tttaccct/i,
  /[cgt]gggtaaa|tttaccc[acg]/i,
  /a[act]ggtaaa|tttacc[agt]t/i,
  /ag[act]gtaaa|tttac[agt]ct/i,
  /agg[act]taaa|ttta[agt]cct/i,
  /aggg[acg]aaa|ttt[cgt]ccct/i,
  /agggt[cgt]aa|tt[acg]accct/i,
  /agggta[cgt]a|t[acg]taccct/i,
  /agggtaa[cgt]|[acg]ttaccct/i
].each {|f| puts "#{f.source} #{seq.scan(f).size}" }

{
'B' => '(c|g|t)', 'D' => '(a|g|t)', 'H' => '(a|c|t)', 'K' => '(g|t)',
'M' => '(a|c)', 'N' => '(a|c|g|t)', 'R' => '(a|g)', 'S' => '(c|t)',
'V' => '(a|c|g)', 'W' => '(a|t)', 'Y' => '(c|t)'
}.each { |f,r| seq.gsub!(f,r) }

puts
puts ilen
puts clen
puts seq.length
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by jose fco. gonzalez
# optimized & parallelized by Rick Branson

seq = STDIN.readlines.join
ilen = seq.size

seq.gsub!(/>.*\n|\n/,"")
clen = seq.length

MATCHERS = [
  /agggtaaa|tttaccct/,
  /[cgt]gggtaaa|tttaccc[acg]/,
  /a[act]ggtaaa|tttacc[agt]t/,
  /ag[act]gtaaa|tttac[agt]ct/,
  /agg[act]taaa|ttta[agt]cct/,
  /aggg[acg]aaa|ttt[cgt]ccct/,
  /agggt[cgt]aa|tt[acg]accct/,
  /agggta[cgt]a|t[acg]taccct/,
  /agggtaa[cgt]|[acg]ttaccct/
]

if RUBY_PLATFORM == "java"
  threads = MATCHERS.map do |f|
    Thread.new do
      Thread.current[:result] = "#{f.source} #{seq.scan(f).size}"
    end
  end

  threads.each do |t|
    t.join
  end

  threads.each do |t|
    puts t[:result]
  end
else
  children = MATCHERS.map do |f|
    r, w = IO.pipe
    p = Process.fork do
      r.close
      w.write "#{f.source} #{seq.scan(f).size}"
      w.close
    end
  
    w.close
    [p, r, w]
  end

  children.each do |p, r, w|
    puts r.read
    r.close
  end

  Process.waitall
end

{
'B' => '(c|g|t)', 'D' => '(a|g|t)', 'H' => '(a|c|t)', 'K' => '(g|t)',
'M' => '(a|c)', 'N' => '(a|c|g|t)', 'R' => '(a|g)', 'S' => '(c|t)',
'V' => '(a|c|g)', 'W' => '(a|t)', 'Y' => '(c|t)'
}.each { |f,r| seq.gsub!(f,r) }

puts
puts ilen
puts clen
puts seq.length
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: regexmatch.ruby,v 1.2 2005-05-17 05:20:31 bfulgham Exp $
# http://shootout.alioth.debian.org/
# modified by: Jon-Carlos Rivera

re = Regexp.new(
    '(?:^|[^\d\(])' +			# must be preceeded by non-digit
    '(?:\((\d\d\d)\)|(\d\d\d))' +	# match 1 or 2: area code is 3 digits
    '[ ]' +				# area code followed by one space
    '(\d\d\d)' +			# match 3: prefix of 3 digits
    '[ -]' +				# separator is either space or dash
    '(\d\d\d\d)' +			# match 4: last 4 digits
    '\D'				# must be followed by a non-digit
)

num = Integer(ARGV[0] || 1)

phones = STDIN.readlines

phonenum, count = "", 0

(1..num).each do |iter|
  phones.each do |line|
	  if line =~ re 
	    phonenum = "(#{($1 || $2)}) #{$3}-#{$4}";
	    if iter == num
		    count += 1
		    puts "#{count}: #{phonenum}"
	    end
	  end
  end
end
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Peter Bjarke Olsen
# Modified by Doug King
# Modified by Joseph LaFata

seq=""

def revcomp(seq)
  seq.reverse!.tr!('wsatugcyrkmbdhvnATUGCYRKMBDHVN','WSTAACGRYMKVHDBNTAACGRYMKVHDBN')
  stringlen=seq.length-1
  0.step(stringlen,60) {|x| print seq[x,60] , "\n"}
end

STDIN.each do |line|
  if line.include? '>'
    if !seq.empty?
      revcomp(seq)
      seq=""
    end
    puts line
  else
    line.chomp!
    seq << line
  end
end
revcomp(seq)
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: reversefile.ruby,v 1.1 2004-05-19 18:12:18 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

print STDIN.readlines().reverse()

#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: sieve.ruby,v 1.4 2004-11-10 06:48:59 bfulgham Exp $
# http://shootout.alioth.debian.org/
#
# Revised implementation by Paul Sanchez

NUM = Integer(ARGV.shift || 1)

max, flags = 8192, nil
flags0 = Array.new(max+1)
for i in 2 .. max
  flags0[i] = i
end

i=j=0

NUM.times do
    flags = flags0.dup
    #for i in 2 .. Math.sqrt(max)	#<-- This is much faster
    for i in 2 .. 8192 
	next unless flags[i]
	# remove all multiples of prime: i
	(i+i).step(max, i) do |j|
	    flags[j] = nil
	end
    end
end

print "Count: ", flags.compact.size, "\n"
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# Contributed by Sokolov Yura

def eval_A(i,j)
	return 1.0/((i+j)*(i+j+1)/2+i+1)
end

def eval_A_times_u(u)
        v, i = nil, nil
	(0..u.length-1).collect { |i|
                v = 0
		for j in 0..u.length-1
			v += eval_A(i,j)*u[j]
                end
                v
        }
end

def eval_At_times_u(u)
	v, i = nil, nil
	(0..u.length-1).collect{|i|
                v = 0
		for j in 0..u.length-1
			v += eval_A(j,i)*u[j]
                end
                v
        }
end

def eval_AtA_times_u(u)
	return eval_At_times_u(eval_A_times_u(u))
end

n = ARGV[0].to_i
u=[1]*n
for i in 1..10
        v=eval_AtA_times_u(u)
        u=eval_AtA_times_u(v)
end
vBv=0
vv=0
for i in 0..n-1
        vBv += u[i]*v[i]
        vv += v[i]*v[i]
end
print "%0.9f" % (Math.sqrt(vBv/vv)), "\n"

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by Rick Branson

require "thread"

def eval_a(i, j)
  1.0/((i+j)*(i+j+1.0)/2.0+i+1.0)
end

class Barrier  
  def initialize(count)
    @mutex = Mutex.new
    @count = count
    reset_generation
  end
  
  def reset_generation
    @generation = { :waiting => 0 }    
  end
  
  def wait
    generation = nil
    
    @mutex.synchronize do
      generation = @generation
    end
    
    @mutex.synchronize do
      generation[:waiting] += 1
      
      if generation[:waiting] == @count
        reset_generation
      end
    end
    
    loop do
      @mutex.synchronize do
        if generation[:waiting] == @count
          return
        end
      end
      
      Thread.pass
    end
  end
end

class SpectralNorm
  class Worker
    def initialize(sn, range)
      @u, @v, @mtx, @tmp, @range = sn.u, sn.v, sn.mtx, sn.tmp, range

      for i in (1..10)
        multiply_at_av(@u, @tmp, @v)
        multiply_at_av(@v, @tmp, @u)
      end
      
      @vBv = 0
      @vv  = 0
      
      for i in @range
        @vBv += @u[i] * @v[i]
        @vv  += @v[i] * @v[i]
      end
    end

    def values
      [ @vBv, @vv ]
    end
    
    private
    
    def multiply_atv(v, atv)
      for i in @range
        sum = 0.0
        
        for j in (0 .. (v.size - 1))
          sum += eval_a(j, i) * v[j]
        end
        
        atv[i] = sum
      end      
    end
    
    def multiply_av(v, av)
      for i in @range
        sum = 0.0
        
        for j in (0 .. (v.size - 1))
          sum += eval_a(i, j) * v[j]
        end
        
        av[i] = sum
      end
    end
    
    def multiply_at_av(v, tmp, at_av)
      multiply_av(v, tmp)
      @mtx.wait
      multiply_atv(tmp, at_av)
      @mtx.wait
    end
  end
  
  attr_reader :u
  attr_reader :v
  attr_reader :tmp
  attr_reader :mtx

  def initialize(n, threads = 4)
    @n        = n
    @u        = [1.0] * n
    @v        = Array.new(n)
    @tmp      = Array.new(n)
    @threads  = threads
    @mtx      = Barrier.new(threads)
  end
  
  def run
    vBv = 0
    vv  = 0
    ths = []
    chk = @n / @threads
    
    @threads.times do |i|
      r = ((i * chk) .. ((i < (@threads - 1) ? (i * chk) + chk : @n) - 1))

      ths << Thread.new do
        Thread.current[:worker] = Worker.new(self, r)
      end
    end
    
    ths.each do |t|
      t.join
      t_vBv, t_vv = t[:worker].values
      vBv += t_vBv
      vv  += t_vv
    end
    
    Math.sqrt(vBv / vv)
  end
end

print "%0.9f" % SpectralNorm.new(ARGV[0].to_i).run, "\n"
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Rick Branson
#
# There is currently a bug in JRuby as of 11/16/2010 that causes
# Marshal to read IOs in a blocking manner, which will cause this
# script to hang.

IS_THREADED = RUBY_PLATFORM == "java"

if IS_THREADED
  require "fcntl"
end

class Worker
  
  attr_reader :reader
  
  def initialize(enum, index, total, &block)
    @enum   = enum
    @index  = index
    @total  = total
    
    @reader, @writer = IO.pipe
      
    if IS_THREADED
      @thread = Thread.new do
        self.execute(&block)
      end
    else
      @p = Process.fork do
        @reader.close
        self.execute(&block)
        @writer.close
      end
      
      @writer.close
    end
  end
  
  def execute(&block)
    chk   = @enum.size / @total
    
    (0 ... @enum.size).step(@total) do |i|
      idx = i + @index
      d = @enum[idx]
      to_parent([idx, yield(d)]) unless d == nil
    end
  end

  def to_parent(msg)
    Marshal.dump(msg, @writer)
  end
  
  def self.gather(workers)
    res = []
    ios = workers.map { |w| w.reader }

    while ios.size > 0 do
      sr, sw, se = IO.select(ios, nil, nil)

      if sr
        sr.each do |io|
          begin
            loop do
              msg = Marshal.load(io)          
              idx, content = msg
              res[idx] = content
            end
          rescue EOFError
            ios.delete(io)
          end
        end
      end
    end
    
    Process.waitall
    res
  end

  def self.map(enum, worker_count = 6, &block)
    count = [enum.size, worker_count].min

    workers = (0 ... count).map do |idx|
      Worker.new(enum, idx, count, &block)
    end

    Worker.gather(workers)    
  end
end

def eval_A_times_u(u)
  usz     = u.size
  urange  = (0 ... usz)
  umap    = urange.to_a
  
  Worker.map(umap) do |i|
    urange.inject(0) do |sum, j|
      sum += (1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)) * u[j]
    end
  end
end

def eval_At_times_u(u)
  usz     = u.size
  urange  = (0 ... usz)
  umap    = urange.to_a
  
  Worker.map(umap) do |i|
    urange.inject(0) do |sum, j|
      sum += (1.0 / ((j + i) * (j + i + 1) / 2 + j + 1)) * u[j]      
    end
  end
end

def eval_AtA_times_u(u)
  eval_At_times_u(eval_A_times_u(u))
end

n = ARGV[0].to_i
u = [1] * n
v = nil

10.times do
  v = eval_AtA_times_u(u)
  u = eval_AtA_times_u(v)
end

vBv = 0
vv  = 0

(0 ... n).each do |i|
  vBv += u[i] * v[i]
  vv  += v[i] * v[i]
end

print "%0.9f" % (Math.sqrt(vBv / vv)), "\n"
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: spellcheck.ruby,v 1.3 2005-06-21 05:36:55 igouy-guest Exp $
# http://shootout.alioth.debian.org/
# Revised by Dave Anderson 

dict = Hash.new
l = ""

IO.foreach("Usr.Dict.Words") do |l|
  dict[l.chomp!] = 1
end 

STDIN.each do |l|
  unless dict.has_key? l.chomp!
    puts l
  end
end
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: strcat.ruby,v 1.1 2004-05-19 18:13:35 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# based on code from Aristarkh A Zagorodnikov and Dat Nguyen

STUFF = "hello\n"
hello = ''
(ARGV.first.to_i || 1).times do
    hello << STUFF
end
puts hello.length
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: strcat.ruby-2.ruby,v 1.1 2004-11-10 06:44:59 bfulgham Exp $
# http://shootout.alioth.debian.org/

n = Integer(ARGV.shift || 1)

str = ''
for i in 1 .. n
    str += "hello\n"
end
puts str.length
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: sumcol.ruby,v 1.2 2004-11-10 06:43:14 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from: Mathieu Bouchard, revised by Dave Anderson

count = 0
l=""
STDIN.each{ |l|
    count += l.to_i
}
puts count
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Pavel Valodzka
 
 puts STDIN.inject(0){|a,v| a + v.to_i }
#!/usr/bin/ruby
#  The Great Computer Language Shootout
#  http://shootout.alioth.debian.org/
#
#  contributed by Gabriele Renzi 

def takfp x, y, z
  return z unless y < x
  takfp( takfp(x-1.0, y, z),
    takfp(y-1.0, z, x),
    takfp(z-1.0, x, y))
end

n=Float(ARGV[0])
puts takfp(n*3.0, n*2.0, n*1.0)
#!/usr/bin/ruby
#### The Great Computer Language Shootout
#### http://shootout.alioth.debian.org/
#### 
#### Contributed by Robbert Haarman
#### Modified by Ian Osgood

require 'socket'

N = Integer(ARGV[0] || 10)
M = 6400
REPLY_SIZE = 64
REQUEST_SIZE = 1
Host = 'localhost'
Port = 12345

sock = TCPServer.new Host, Port
if fork
	# Parent process
	conn = sock.accept
	reply = 'x' * REPLY_SIZE
	while true
		request = conn.read REQUEST_SIZE
		break if request == nil
		conn.write reply
	end
else
	# Child process
	conn = TCPSocket.new Host, Port
	replies = 0
	bytes = 0
	n = N * M
	request = 'x' * REQUEST_SIZE
	while n > 0
		n = n - 1
		conn.write request
		reply = conn.read REPLY_SIZE
		replies = replies + 1
		bytes = bytes + reply.length
	end
	conn.close
	puts "replies: #{replies}\tbytes: #{bytes}"
end

sock.close
#!/usr/bin/ruby
#### The Great Computer Language Shootout
#### http://shootout.alioth.debian.org/
#### 
#### Contributed by Robbert Haarman 
#### Modified by Ian Osgood

require 'socket'

N = Integer(ARGV[0] || 10)
M = 100
REPLY_SIZE = 4096
REQUEST_SIZE = 1
Host = 'localhost'
Port = 12345

sock = TCPServer.new Host, Port
if fork
	# Parent process
	conn = sock.accept
	reply = 'x' * REPLY_SIZE
	while true
		request = conn.read REQUEST_SIZE
		break if request == nil
		conn.write reply
	end
else
	# Child process
	conn = TCPSocket.new Host, Port
	replies = 0
	bytes = 0
	n = N * M
	request = 'x' * REQUEST_SIZE
	while n > 0
		n = n - 1
		conn.write request
		reply = conn.read REPLY_SIZE
		replies = replies + 1
		bytes = bytes + reply.length
	end
	conn.close
	puts "replies: #{replies}\tbytes: #{bytes}"
end

sock.close
#!/usr/bin/ruby
#### The Great Computer Language Shootout
#### http://shootout.alioth.debian.org/
#### 
#### Contributed by Robbert Haarman
#### Modified by Ian Osgood

require 'socket'

N = Integer(ARGV[0] || 10)
M = 1
REPLY_SIZE = 409600
REQUEST_SIZE = 1
Host = 'localhost'
Port = 12345

sock = TCPServer.new Host, Port
if fork
	# Parent process
	conn = sock.accept
	reply = 'x' * REPLY_SIZE
	while true
		request = conn.read REQUEST_SIZE
		break if request == nil
		conn.write reply
	end
else
	# Child process
	conn = TCPSocket.new Host, Port
	replies = 0
	bytes = 0
	n = N * M
	request = 'x' * REQUEST_SIZE
	while n > 0
		n = n - 1
		conn.write request
		reply = conn.read REPLY_SIZE
		replies = replies + 1
		bytes = bytes + reply.length
	end
	conn.close
	puts "replies: #{replies}\tbytes: #{bytes}"
end

sock.close
#
# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Klaus Friedel
#

require "thread"

THREAD_COUNT = 503

class Receiver
  @next
  @mailbox

  def initialize(name)
    @name = name
    @mailbox = Queue.new
  end

  def next=(n)
    @next = n
  end

  def put(msg)
    @mailbox.push(msg)
  end

  def messageloop
    while true
      hopsRemaining = @mailbox.pop
      if(hopsRemaining == 0)
        print @name, "\n"
        exit(0)
      end
      @next.put(hopsRemaining - 1)
    end
  end
end

##########
#  Main  #
##########
receivers = []
for i in 0..THREAD_COUNT-1
  receivers[i] = Receiver.new(i+1)
  if(i > 0)
    receivers[i-1].next = receivers[i]
  end
end
#close the ring
receivers[THREAD_COUNT-1].next = receivers[0]

# start the threads
for i in 0..THREAD_COUNT-1
  Thread.start(i){|k| receivers[k].messageloop}
end

receivers[0].put(ARGV[0].to_i)

sleep

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# contributed by Serhiy Boiko


require 'thread'
THREAD_NUM = 503
number = ARGV.first.to_i

threads = []
for i in 1..THREAD_NUM
   threads << Thread.new(i) do |thr_num|
      while true
         Thread.stop
         if number > 0
            number -= 1
         else
            puts thr_num
            exit 0
         end
      end
   end
end

prev_thread = threads.last
while true
   for thread in threads
      Thread.pass until prev_thread.stop?
      thread.run
      prev_thread = thread
   end
end

#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: wc.ruby,v 1.1 2004-05-19 18:13:51 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Paul Brannan

nl = nw = nc = 0
loop do
  data = (STDIN.read(4096) or break) << (STDIN.gets || "")
  nc += data.length
  nl += data.count("\n")
  ((data.strip! || data).tr!("\n", " ") || data).squeeze!
  nw += data.count(" ") + 1
end
puts "#{nl} #{nw} #{nc}"
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: wordfreq.ruby,v 1.2 2004-07-03 05:36:11 bfulgham Exp $
# http://shootout.alioth.debian.org/

freq = Hash.new(0)
loop {
    data = (STDIN.read(4095) or break) << (STDIN.gets || "")
    for word in data.downcase.tr_s('^A-Za-z',' ').split(' ')
	freq[word] += 1
    end
}
freq.delete("")

lines = Array.new
freq.each{|w,c| lines << sprintf("%7d %s\n", c, w) }
print lines.sort.reverse
