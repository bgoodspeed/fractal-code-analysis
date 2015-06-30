--- The Great Computer Lanuage Shootout
--  http://shootout.alioth.debian.org
--
-- Contributed by ???
-- Modified by Mike Pall (email withheld by request)
-- Submitted by Matthew Burke <shooutout@bluedino.net>
--
local function Ack(m, n)
  if m == 0 then return n+1 end
  if n == 0 then return Ack(m-1, 1) end
  return Ack(m-1, Ack(m, n-1))
end

local N = tonumber(arg and arg[1]) or 1
io.write("Ack(3,", N ,"): ", Ack(3,N), "\n")
-- $Id: ary.lua,v 1.2 2004-05-22 07:25:00 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/

local n = tonumber((arg and arg[1]) or 1)

local x, y = {}, {}
local last = n - 1

for i=0,last do
  x[i] = i + 1
  y[i] = 0
end
for k=1,1000 do
  for j=last,0,-1 do
    y[j] = y[j] + x[j]
  end
end

io.write(y[0], " ", y[last], "\n")
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item }
  end
end

local function ItemCheck(tree)
  if tree[2] then
    return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3])
  else
    return tree[1]
  end
end

local N = tonumber(arg and arg[1]) or 0
local mindepth = 4
local maxdepth = mindepth + 2
if maxdepth < N then maxdepth = N end

do
  local stretchdepth = maxdepth + 1
  local stretchtree = BottomUpTree(0, stretchdepth)
  io.write(string.format("stretch tree of depth %d\t check: %d\n",
    stretchdepth, ItemCheck(stretchtree)))
end

local longlivedtree = BottomUpTree(0, maxdepth)

for depth=mindepth,maxdepth,2 do
  local iterations = 2 ^ (maxdepth - depth + mindepth)
  local check = 0
  for i=1,iterations do
    check = check + ItemCheck(BottomUpTree(1, depth)) +
            ItemCheck(BottomUpTree(-1, depth))
  end
  io.write(string.format("%d\t trees of depth %d\t check: %d\n",
    iterations*2, depth, check))
end

io.write(string.format("long lived tree of depth %d\t check: %d\n",
  maxdepth, ItemCheck(longlivedtree)))
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- modified by Sokolov yura

collectgarbage("setstepmul", 0) -- sometimes it helps much. For this benchmark ~ 10%

local function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item } -- Faster for LuaJIT: return { item, false }
  end
end

local function ItemCheck(tree)
  if #tree == 3 then -- Faster for LuaJIT: if tree[2] then
    return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3])
  else
    return tree[1]
  end
end

local N = tonumber(arg and arg[1]) or 0
local mindepth = 4
local maxdepth = mindepth + 2
if maxdepth < N then maxdepth = N end

do
  local stretchdepth = maxdepth + 1
  local stretchtree = BottomUpTree(0, stretchdepth)
  io.write(string.format("stretch tree of depth %d\t check: %d\n",
    stretchdepth, ItemCheck(stretchtree)))
end

local longlivedtree = BottomUpTree(0, maxdepth)

for depth=mindepth,maxdepth,2 do
  local iterations = 2 ^ (maxdepth - depth + mindepth)
  local check = 0
  for i=1,iterations do
    check = check + ItemCheck(BottomUpTree(1, depth)) +
            ItemCheck(BottomUpTree(-1, depth))
  end
  io.write(string.format("%d\t trees of depth %d\t check: %d\n",
    iterations*2, depth, check))
end

io.write(string.format("long lived tree of depth %d\t check: %d\n",
  maxdepth, ItemCheck(longlivedtree)))

-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local co = coroutine
local create, resume, yield = co.create, co.resume, co.yield

local N = tonumber(arg and arg[1]) or 10
local first, second

-- Meet another creature.
local function meet(me)
  while second do yield() end -- Wait until meeting place clears.
  local other = first
  if other then -- Hey, I found a new friend!
    first = nil
    second = me
  else -- Sniff, nobody here (yet).
    local n = N - 1
    if n < 0 then return end -- Uh oh, the mall is closed.
    N = n
    first = me
    repeat yield(); other = second until other -- Wait for another creature.
    second = nil
    yield() -- Be nice and let others meet up.
  end
  return other
end

-- Create a very social creature.
local function creature(color)
  return create(function()
    local me = color
    for met=0,1e9 do
      local other = meet(me)
      if not other then return met end
      if me ~= other then
        if me == "blue" then me = other == "red" and "yellow" or "red"
        elseif me == "red" then me = other == "blue" and "yellow" or "blue"
        else me = other == "blue" and "red" or "blue" end
      end
    end
  end)
end

-- Trivial round-robin scheduler.
local function schedule(threads)
  local resume = resume
  local nthreads, meetings = table.getn(threads), 0
  repeat
    for i=1,nthreads do
      local thr = threads[i]
      if not thr then return meetings end
      local ok, met = resume(thr)
      if met then
        meetings = meetings + met
        threads[i] = nil
      end
    end
  until false
end

-- A bunch of colorful creatures.
local threads = {
  creature("blue"),
  creature("red"),
  creature("yellow"),
  creature("blue"),
}

io.write(schedule(threads), "\n")
-- $Id: except.lua,v 1.2 2004-05-23 04:36:29 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- from Roberto Ierusalimschy

-- uses `call' to catch errors; return the error message
-- (or nil if there are no errors)

function try (f, arg)
  local status, err = pcall(f, arg)
  if not status then return err end
end

local HI = 0
local LO = 0

function some_function (n)
  local res = try(hi_function, n)
  if res then print("We shouldn't get here: " .. res) end
end


function hi_function (n)
  local res = try(lo_function, n)
  if res == "Hi_Exception" then HI = HI+1 
  elseif res then error(res, 0)  -- rethrow
  end
end


function lo_function (n)
  local res = try(blowup, n)
  if res == "Lo_Exception" then LO = LO+1 
  elseif res then error(res, 0)  -- rethrow
  end
end


function blowup (n)
  if math.mod(n,2) ~= 0 then error("Lo_Exception", 0)
  else error("Hi_Exception",0)
  end
end


N = (arg and arg[1]) or 1
for i=1,N do
  some_function(i)
end

print(string.format("Exceptions: HI=%d / LO=%d", HI, LO))
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local function fannkuch(n)
  local p, q, s, odd, check, maxflips = {}, {}, {}, true, 0, 0
  for i=1,n do p[i] = i; q[i] = i; s[i] = i end
  repeat
    -- Print max. 30 permutations.
    if check < 30 then
      if not p[n] then return maxflips end	-- Catch n = 0, 1, 2.
      io.write(unpack(p)); io.write("\n")
      check = check + 1
    end
    -- Copy and flip.
    local q1 = p[1]				-- Cache 1st element.
    if p[n] ~= n and q1 ~= 1 then		-- Avoid useless work.
      for i=2,n do q[i] = p[i] end		-- Work on a copy.
      for flips=1,1000000 do			-- Flip ...
	local qq = q[q1]
	if qq == 1 then				-- ... until 1st element is 1.
	  if flips > maxflips then maxflips = flips end -- New maximum?
	  break
	end
	q[q1] = q1
	if q1 >= 4 then
	  local i, j = 2, q1 - 1
	  repeat q[i], q[j] = q[j], q[i]; i = i + 1; j = j - 1; until i >= j
	end
	q1 = qq
      end
    end
    -- Permute.
    if odd then
      p[2], p[1] = p[1], p[2]; odd = false	-- Rotate 1<-2.
    else
      p[2], p[3] = p[3], p[2]; odd = true	-- Rotate 1<-2 and 1<-2<-3.
      for i=3,n do
	local sx = s[i]
	if sx ~= 1 then s[i] = sx-1; break end
	if i == n then return maxflips end	-- Out of permutations.
	s[i] = i
	-- Rotate 1<-...<-i+1.
	local t = p[1]; for j=1,i do p[j] = p[j+1] end; p[i+1] = t
      end
    end
  until false
end

local n = tonumber(arg and arg[1]) or 1
io.write("Pfannkuchen(", n, ") = ", fannkuch(n), "\n")
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- modified by Geoff Leyland

local function fannkuch(n)
  local p, q, s, odd, check, maxflips = {}, {}, {}, true, 0, 0
  for i=1,n do p[i] = i; q[i] = i; s[i] = i end
  repeat
    -- Print max. 30 permutations.
    if check < 30 then
      if not p[n] then return maxflips end	-- Catch n = 0, 1, 2.
      io.write(unpack(p)); io.write("\n")
      check = check + 1
    end
    -- Copy and flip.
    local q1 = p[1]				-- Cache 1st element.
    if p[n] ~= n and q1 ~= 1 then		-- Avoid useless work.
      for i=2,n do q[i] = p[i] end		-- Work on a copy.
      local flips = 1			-- Flip ...
      while true do
        local qq = q[q1]
        if qq == 1 then				-- ... until 1st element is 1.
          if flips > maxflips then maxflips = flips end -- New maximum?
          break
        end
        q[q1] = q1
        if q1 >= 4 then
          local i, j = 2, q1 - 1
          repeat q[i], q[j] = q[j], q[i]; i = i + 1; j = j - 1; until i >= j
        end
        q1 = qq
        flips=flips+1
      end
    end
    -- Permute.
    if odd then
      p[2], p[1] = p[1], p[2]; odd = false	-- Rotate 1<-2.
    else
      p[2], p[3] = p[3], p[2]; odd = true	-- Rotate 1<-2 and 1<-2<-3.
      for i=3,n do
        local sx = s[i]
        if sx ~= 1 then s[i] = sx-1; break end
        if i == n then return maxflips end	-- Out of permutations.
        s[i] = i
        -- Rotate 1<-...<-i+1.
        local t=p[1]; for j=i+1,1,-1 do p[j],t=t,p[j] end
      end
    end
  until false
end

local n = tonumber(arg and arg[1]) or 1
io.write("Pfannkuchen(", n, ") = ", fannkuch(n), "\n")
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local function fannkuch(n)
  local p, q, s, sign, maxflips, sum = {}, {}, {}, 1, 0, 0
  for i=1,n do p[i] = i; q[i] = i; s[i] = i end
  repeat
    -- Copy and flip.
    local q1 = p[1]				-- Cache 1st element.
    if q1 ~= 1 then
      for i=2,n do q[i] = p[i] end		-- Work on a copy.
      local flips = 1
      repeat
	local qq = q[q1]
	if qq == 1 then				-- ... until 1st element is 1.
	  sum = sum + sign*flips
	  if flips > maxflips then maxflips = flips end -- New maximum?
	  break
	end
	q[q1] = q1
	if q1 >= 4 then
	  local i, j = 2, q1 - 1
	  repeat q[i], q[j] = q[j], q[i]; i = i + 1; j = j - 1; until i >= j
	end
	q1 = qq; flips = flips + 1
      until false
    end
    -- Permute.
    if sign == 1 then
      p[2], p[1] = p[1], p[2]; sign = -1	-- Rotate 1<-2.
    else
      p[2], p[3] = p[3], p[2]; sign = 1		-- Rotate 1<-2 and 1<-2<-3.
      for i=3,n do
	local sx = s[i]
	if sx ~= 1 then s[i] = sx-1; break end
	if i == n then return sum, maxflips end	-- Out of permutations.
	s[i] = i
	-- Rotate 1<-...<-i+1.
	local t = p[1]; for j=1,i do p[j] = p[j+1] end; p[i+1] = t
      end
    end
  until false
end

local n = tonumber(arg and arg[1]) or 1
local sum, flips = fannkuch(n)
io.write(sum, "\nPfannkuchen(", n, ") = ", flips, "\n")
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local Last = 42
local function random(max)
  local y = (Last * 3877 + 29573) % 139968
  Last = y
  return (max * y) / 139968
end

local function make_repeat_fasta(id, desc, s, n)
  local write, sub = io.write, string.sub
  write(">", id, " ", desc, "\n")
  local p, sn, s2 = 1, #s, s..s
  for i=60,n,60 do
    write(sub(s2, p, p + 59), "\n")
    p = p + 60; if p > sn then p = p - sn end
  end
  local tail = n % 60
  if tail > 0 then write(sub(s2, p, p + tail-1), "\n") end
end

local function make_random_fasta(id, desc, bs, n)
  io.write(">", id, " ", desc, "\n")
  loadstring([=[
    local write, char, unpack, n, random = io.write, string.char, unpack, ...
    local buf, p = {}, 1
    for i=60,n,60 do
      for j=p,p+59 do ]=]..bs..[=[ end
      buf[p+60] = 10; p = p + 61
      if p >= 2048 then write(char(unpack(buf, 1, p-1))); p = 1 end
    end
    local tail = n % 60
    if tail > 0 then
      for j=p,p+tail-1 do ]=]..bs..[=[ end
      p = p + tail; buf[p] = 10; p = p + 1
    end
    write(char(unpack(buf, 1, p-1)))
  ]=], desc)(n, random)
end

local function bisect(c, p, lo, hi)
  local n = hi - lo
  if n == 0 then return "buf[j] = "..c[hi].."\n" end
  local mid = math.floor(n / 2)
  return "if r < "..p[lo+mid].." then\n"..bisect(c, p, lo, lo+mid)..
         "else\n"..bisect(c, p, lo+mid+1, hi).."end\n"
end

local function make_bisect(tab)
  local c, p, sum = {}, {}, 0
  for i,row in ipairs(tab) do
    c[i] = string.byte(row[1])
    sum = sum + row[2]
    p[i] = sum
  end
  return "local r = random(1)\n"..bisect(c, p, 1, #tab)
end

local alu =
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"..
  "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"..
  "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"..
  "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"..
  "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"..
  "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"..
  "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

local iub = make_bisect{
  { "a", 0.27 },
  { "c", 0.12 },
  { "g", 0.12 },
  { "t", 0.27 },
  { "B", 0.02 },
  { "D", 0.02 },
  { "H", 0.02 },
  { "K", 0.02 },
  { "M", 0.02 },
  { "N", 0.02 },
  { "R", 0.02 },
  { "S", 0.02 },
  { "V", 0.02 },
  { "W", 0.02 },
  { "Y", 0.02 },
}

local homosapiens = make_bisect{
  { "a", 0.3029549426680 },
  { "c", 0.1979883004921 },
  { "g", 0.1975473066391 },
  { "t", 0.3015094502008 },
}

local N = tonumber(arg and arg[1]) or 1000
make_repeat_fasta('ONE', 'Homo sapiens alu', alu, N*2)
make_random_fasta('TWO', 'IUB ambiguity codes', iub, N*3)
make_random_fasta('THREE', 'Homo sapiens frequency', homosapiens, N*5)
-- $Id: fibo.lua,v 1.3 2005-04-25 19:01:38 igouy-guest Exp $
-- http://www.bagley.org/~doug/shootout/

function fib(n)
    if (n < 2) then return(1) end
    return( fib(n-2) + fib(n-1) )
end

N = tonumber((arg and arg[1])) or 1
io.write(fib(N), "\n")
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- contributed by Isaac Gouy


local n = tonumber(arg and arg[1]) or 10000000
local partialSum = 0.0

for d = 1,n do partialSum = partialSum + (1.0/d) end

io.write(string.format("%0.9f",partialSum), "\n")
-- $Id: hash.lua,v 1.2 2004-05-22 04:31:50 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- Author: Roberto Ierusalimschy

local n = tonumber((arg and arg[1]) or 1)

local X={}
for i=1,n do
  X[string.format("%x", i)] = i
end

local c = 0

for i=n,1,-1 do
  if X[i..''] then c = c+1 end
end

print(c)
-- $Id: hash2.lua,v 1.2 2004-05-20 02:39:49 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- with help from Roberto Ierusalimschy

local n = tonumber((arg and arg[1]) or 1)

local hash1={}
for i=0,10000 do
    hash1["foo_"..i] = i
end
local hash2={}
for i=1,n do
    for k,v in hash1 do
	hash2[k] = v + (hash2[k] or 0)
    end
end

io.write(string.format("%d %d %d %d\n", hash1["foo_1"], hash1["foo_9999"],
	     hash2["foo_1"], hash2["foo_9999"]))
#!/usr/bin/lua
-- $Id: heapsort.lua,v 1.2 2004-06-12 16:19:43 bfulgham Exp $
-- http://shootout.alioth.debian.org
-- contributed by Roberto Ierusalimschy

local IM = 139968
local IA =   3877
local IC =  29573

local LAST = 42
function gen_random(max)
    LAST = math.mod((LAST * IA + IC), IM)
    return( (max * LAST) / IM )
end

function heapsort(n, ra)
    local j, i, rra
    local l = math.floor(n/2) + 1
    local ir = n;
    while 1 do
	if l > 1 then
	    l = l - 1
	    rra = ra[l]
	else
	    rra = ra[ir]
	    ra[ir] = ra[1]
	    ir = ir - 1
	    if (ir == 1) then
		ra[1] = rra
		return
	    end
	end
	i = l
	j = l * 2
	while j <= ir do
	    if (j < ir) and (ra[j] < ra[j+1]) then
		j = j + 1
	    end
	    if rra < ra[j] then
		ra[i] = ra[j]
		i = j
		j = j + i
	    else
		j = ir + 1
	    end
	end
	ra[i] = rra
    end
end

local ary = {}
local N = (tonumber((arg and arg[1])) or 1)

for i=1, N do
    ary[i] = gen_random(1.0)
end

heapsort(N, ary)

io.write(string.format("%0.10f\n", ary[N]))
-- $Id: hello.lua,v 1.1 2004-05-19 18:10:16 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/

io.write("hello world\n")
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local function kfrequency(seq, freq, k, frame)
  local sub = string.sub
  local k1 = k - 1
  for i=frame,string.len(seq)-k1,k do
    local c = sub(seq, i, i+k1)
    freq[c] = freq[c] + 1
  end
end

local function freqdefault()
  return 0
end

local function count(seq, frag)
  local k = string.len(frag)
  local freq = setmetatable({}, { __index = freqdefault })
  for frame=1,k do kfrequency(seq, freq, k, frame) end
  io.write(freq[frag] or 0, "\t", frag, "\n")
end

local function frequency(seq, k)
  local freq = setmetatable({}, { __index = freqdefault })
  for frame=1,k do kfrequency(seq, freq, k, frame) end
  local sfreq, sn = {}, 1
  for c,v in pairs(freq) do sfreq[sn] = c; sn = sn + 1 end
  table.sort(sfreq, function(a, b)
    local fa, fb = freq[a], freq[b]
    return fa == fb and a > b or fa > fb
  end)
  sum = string.len(seq)-k+1
  for _,c in ipairs(sfreq) do
    io.write(string.format("%s %0.3f\n", c, (freq[c]*100)/sum))
  end
  io.write("\n")
end

local function readseq()
  local sub = string.sub
  for line in io.lines() do
    if sub(line, 1, 1) == ">" and sub(line, 2, 6) == "THREE" then break end
  end
  local lines, ln = {}, 0
  for line in io.lines() do
    local c = sub(line, 1, 1)
    if c == ">" then
      break
    elseif c ~= ";" then
      ln = ln + 1
      lines[ln] = line
    end
  end
  return string.upper(table.concat(lines, "", 1, ln))
end

local seq = readseq()
frequency(seq, 1)
frequency(seq, 2)
count(seq, "GGT")
count(seq, "GGTA")
count(seq, "GGTATT")
count(seq, "GGTATTTTAATT")
count(seq, "GGTATTTTAATTTATAGT")
-- $Id: lists.lua,v 1.1 2004-05-19 18:10:23 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- implemented by: Roberto Ierusalimschy

--------------------------------------------------------------
-- List module
-- defines a prototipe for lists
--------------------------------------------------------------

List = {first = 0, last = -1}

function List:new ()
  local n = {}
  for k,v in self do n[k] = v end
  return n
end

function List:length ()
  return self.last - self.first + 1
end

function List:pushleft (value)
  local first = self.first - 1
  self.first = first
  self[first] = value
end

function List:pushright (value)
  local last = self.last + 1
  self.last = last
  self[last] = value
end

function List:popleft ()
  local first = self.first
  if first > self.last then error"list is empty" end
  local value = self[first]
  self[first] = nil  -- to allow collection
  self.first = first+1
  return value
end

function List:popright ()
  local last = self.last
  if self.first > last then error"list is empty" end
  local value = self[last]
  self[last] = nil  -- to allow collection
  self.last = last-1
  return value
end

function List:reverse ()
  local i, j = self.first, self.last
  while i<j do
    self[i], self[j] = self[j], self[i]
    i = i+1
    j = j-1
  end
end

function List:equal (otherlist)
  if self:length() ~= otherlist:length() then return nil end
  local diff = otherlist.first - self.first
  for i1=self.first,self.last do
    if self[i1] ~= otherlist[i1+diff] then return nil end
  end
  return 1
end

-----------------------------------------------------------
-----------------------------------------------------------

-- Some tests

function test ()
  local SIZE = 10000
  -- create a list with elements 1..SIZE
  local l1 = List:new()
  for i=1,SIZE do
    l1:pushright(i)
  end
  -- creates a copy of l1
  local l2 = l1:new()
  -- remove each individual item from left side of l2 and
  -- append to right side of l3 (preserving order)
  local l3 = List:new()
  while l2:length() > 0 do
    l3:pushright(l2:popleft())  
  end
  -- remove each individual item from right side of l3 and
  -- append to right side of l2 (reversing list)
  while l3:length() > 0 do
    l2:pushright(l3:popright())
  end
  -- reverse l1 in place
  l1:reverse()
  -- compare Li1 and Li2 for equality
  -- and return length of the list
  if not l1:equal(l2) then return nil
  else return l1:length()
  end
end

N = tonumber((arg and arg[1])) or 1
for i=1, N do
  result = test()
end
print(result)
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local write, char, unpack = io.write, string.char, unpack
local N = tonumber(arg and arg[1]) or 100
local M, ba, bb, buf = 2/N, 2^(N%8+1)-1, 2^(8-N%8), {}
write("P4\n", N, " ", N, "\n")
for y=0,N-1 do
  local Ci, b, p = y*M-1, 1, 0
  for x=0,N-1 do
    local Cr = x*M-1.5
    local Zr, Zi, Zrq, Ziq = Cr, Ci, Cr*Cr, Ci*Ci
    b = b + b
    for i=1,49 do
      Zi = Zr*Zi*2 + Ci
      Zr = Zrq-Ziq + Cr
      Ziq = Zi*Zi
      Zrq = Zr*Zr
      if Zrq+Ziq > 4.0 then b = b + 1; break; end
    end
    if b >= 256 then p = p + 1; buf[p] = 511 - b; b = 1; end
  end
  if b ~= 1 then p = p + 1; buf[p] = (ba-b)*bb; end
  write(char(unpack(buf, 1, p)))
end
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local width = tonumber(arg and arg[1]) or 100
local height, wscale = width, 2/width
local m, limit2 = 50, 4.0
local write, char = io.write, string.char

write("P4\n", width, " ", height, "\n")

for y=0,height-1 do
  local Ci = 2*y / height - 1
  for xb=0,width-1,8 do
    local bits = 0
    local xbb = xb+7
    for x=xb,xbb < width and xbb or width-1 do
      bits = bits + bits
      local Zr, Zi, Zrq, Ziq = 0.0, 0.0, 0.0, 0.0
      local Cr = x * wscale - 1.5
      for i=1,m do
        local Zri = Zr*Zi
        Zr = Zrq - Ziq + Cr
        Zi = Zri + Zri + Ci
        Zrq = Zr*Zr
        Ziq = Zi*Zi
        if Zrq + Ziq > limit2 then
          bits = bits + 1
          break
        end
      end
    end
    if xbb >= width then
      for x=width,xbb do bits = bits + bits + 1 end
    end
    write(char(255-bits))
  end
end
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- modified by Rob Kendrick to be parallel
-- modified by Isaac Gouy

-- called with the following arguments on the command line;
-- 1: size of mandelbrot to generate
-- 2: number of children to spawn (defaults to 6, which works well on 4-way)
-- If this is a child, then there will be additional parameters;
-- 3: start row
-- 4: end row
--
-- Children buffer up their output and emit it to stdout when
-- finished, to avoid stalling due to a full pipe.

local width = tonumber(arg and arg[1]) or 100
local children = tonumber(arg and arg[2]) or 6
local srow = tonumber(arg and arg[3])
local erow = tonumber(arg and arg[4])

local height, wscale = width, 2/width
local m, limit2 = 50, 4.0
local write, char = io.write, string.char

if not srow then
   -- we are the parent process.  emit the header, and then spawn children
   
   local workunit = math.floor(width / (children + 1))
   local handles = { }
   
   write("P4\n", width, " ", height, "\n")
   
   children = children - 1
   
   for i = 0, children do
      local cs, ce
      
      if i == 0 then
         cs = 0
         ce = workunit
      elseif i == children then
         cs = (workunit * i) + 1
         ce = width - 1
      else
         cs = (workunit * i) + 1
         ce = cs + workunit - 1
      end
      
      handles[i + 1] = io.popen(("%s %s %d %d %d %d"):format(
         arg[-1], arg[0], width, children + 1, cs, ce))
   end
   
   -- collect answers, and emit
   for i = 0, children do
      write(handles[i + 1]:read "*a")
   end
   
else
   -- we are a child process.  do the work allocated to us.
   local obuff = { }
   for y=srow,erow do
     local Ci = 2*y / height - 1
     for xb=0,width-1,8 do
      local bits = 0
      local xbb = xb+7
      for x=xb,xbb < width and xbb or width-1 do
        bits = bits + bits
        local Zr, Zi, Zrq, Ziq = 0.0, 0.0, 0.0, 0.0
        local Cr = x * wscale - 1.5
        for i=1,m do
         local Zri = Zr*Zi
         Zr = Zrq - Ziq + Cr
         Zi = Zri + Zri + Ci
         Zrq = Zr*Zr
         Ziq = Zi*Zi
         if Zrq + Ziq > limit2 then
           bits = bits + 1
           break
         end
        end
      end
      if xbb >= width then
        for x=width,xbb do bits = bits + bits + 1 end
      end
      obuff[#obuff + 1] = char(255 - bits)
     end
   end
   
   write(table.concat(obuff))
end
-- $Id: matrix.lua,v 1.2 2004-07-04 07:29:51 bfulgham Exp $
-- http://shootout.alioth.debian.org/
-- with help from Roberto Ierusalimschy

local n = tonumber((arg and arg[1]) or 1)

local size = 30

function mkmatrix(rows, cols)
    local count = 1
    local mx = {}
    for i=1,rows do
	local row = {}
	for j=1,cols do
	    row[j] = count
	    count = count + 1
	end
	mx[i] = row
    end
    return(mx)
end

function mmult(rows, cols, m1, m2)
    local m3 = {}
    for i=1,rows do
        local m3i = {}
        m3[i] = m3i
        local m1i = m1[i]              -- "cache" m1[i]
        for j=1,cols do
            local rowj = 0
            for k=1,cols do
                rowj = rowj + m1i[k] * m2[k][j]
            end
            m3i[j] = rowj
        end
    end
    return(m3)
end

local m1 = mkmatrix(size, size)
local m2 = mkmatrix(size, size)
for i=1,n do
    mm = mmult(size, size, m1, m2)
end
io.write(string.format("%d %d %d %d\n", mm[1][1], mm[3][4], mm[4][3], mm[5][5]))
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local co = coroutine
local wrap, yield = co.wrap, co.yield
if co.cstacksize then co.cstacksize(1) end -- Use minimum C stack.

local function link(n)
  if n > 1 then
    local cofunc = wrap(link)
    cofunc(n-1)
    yield()
    repeat yield(cofunc() + 1) until false
  else
    repeat yield(1) until false
  end
end

local N = tonumber(arg and arg[1]) or 10
local cofunc = wrap(link)
cofunc(500)
local count = 0
for i = 1,N do count = count + cofunc() end
io.write(count, "\n")

-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

-- Generate a decision tree based solver for the meteor puzzle.
local function generatesolver(countinit)
  local pairs, ipairs, format = pairs, ipairs, string.format
  local byte, min, sort = string.byte, math.min, table.sort

  -- Cached position to distance lookup.
  local dist = setmetatable({}, { __index = function(t, xy)
    local x = xy%10; local y = (xy-x)/10
    if (x+y)%2 == 1 then y = y + 1; x = 10 - x end
    local d = xy + 256*x*x + 1024*y*y; t[xy] = d; return d
  end})

  -- Generate an optimized decision tree (within 4% of a hand-tuned tree).
  local dtree = {}
  local rot = { nil, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
  for k=0,9 do
    -- Generate 10 initial pieces from line noise. :-)
    local t = { 60, 62, byte("@BMBIK@KT@GPIKR@IKIKT@GK@KM@BG", k*3+1, k*3+3) }
    rot[1] = t
    for i,xy in ipairs(t) do
      local x = xy%10; local y = (xy-x-60)/10
      -- Add 11 more variations by rotating and flipping.
      for j=2,12 do
	if j == 7 then y = -y else x,y = (x+3*y)/2, (y-x)/2 end
	rot[j][i] = x+10*y
      end
    end
    for _,v in ipairs(rot) do
      -- Normalize to origin, add distance, sort by distance from origin.
      local m = min(v[1], v[2], v[3], v[4], v[5])
      for i=1,5 do v[i] = dist[v[i]-m] end
      sort(v)
      -- Insert into decision tree in distance order.
      local tt = dtree
      for i=2,4 do
	local xy = v[i]%256
	local tn = tt[xy]
	if not tn then tn = {}; tt[xy] = tn end -- Create nodes as needed.
	tt = tn
      end
      tt[v[5]%256] = k -- Leaves hold the piece numbers.
    end
  end

  -- Lookup table to validate a cell and to find its successor.
  local ok = {}
  for i=0,150 do ok[i] = false end
  for i=99,0,-1 do
    local x = i%10
    if ((i-x)/10+x)%2 == 0 then
      ok[i] = i + (ok[i+1] and 1 or (ok[i+2] and 2 or 3))
    end
  end

  local s = "local u0,u1,u2,u3,u4,u5,u6,u7,u8,u9" -- Piece use flags.
  for p=0,99 do if ok[p] then s = s..",b"..p end end -- Board cells.
  s = s.."\n"..[[
local countinit = ...
local count = countinit
local b0a, b0b, pcs = 9, 0, {}
local smin, smax
local write = io.write

-- Print min/max boards.
local function printboard(s)
  local flip = true
  for x in string.gmatch(string.gsub(s, ".", "%1 "), "..........") do
    write(x, flip and "\n " or "\n")
    flip = not flip
  end
  write("\n")
end

-- Print result.
local function printresult()
  write(countinit-count, " solutions found\n\n")
  printboard(smin)
  printboard(smax)
end

-- Generate piece lookup array from the order of use.
local function genp()
  local p = pcs
  p[u0] = "0" p[u1] = "1" p[u2] = "2" p[u3] = "3" p[u4] = "4"
  p[u5] = "5" p[u6] = "6" p[u7] = "7" p[u8] = "8" p[u9] = "9"
  return p
end

-- Goal function.
local function f91(k)
  if k ~= 10 then return end
  count = count - 1
  repeat
    -- Quick precheck before constructing the string.
    local b0 = b0
    if b0 <= b0a then b0a = b0 elseif b0 >= b0b then b0b = b0 else break end
    -- Translate the filled board to a string.
    local p = genp()
    local s = p[b0] ]]
  for p=2,99 do if ok[p] then s = s.."..p[b"..p.."]" end end
  s = s..[[
    -- Remember min/max boards.
    if not smin then smin = s; smax = s
    elseif s < smin then smin = s elseif s > smax then smax = s end
  until true
  if count == 0 then error("") end -- Early abort if max count given.
end
local f93 = f91
]]

  -- Recursively prune the decision tree and convert it to Lua code.
  local function codetree(tree, d, p, pn)
    local found, s = false, ""
    d = d + 1
    for a,t in pairs(tree) do
      local b = p+a
      local pp = ok[b]
      if pp then -- Prune the tree on-the-fly.
	if b ~= pn then pp = pn end -- Find maximum successor function.
	if d == 5 then -- Try to place the last cell of a piece and advance.
	  found = true
	  s = format("%sif not u%d and not b%d then b%d=k u%d=k f%d(k) u%d=N b%d=N end\n",
		     s, t, b, b, t, pp, t, b)
	else -- Try to place an intermediate cell.
	  local st = codetree(t, d, p, pp)
	  if st then -- But only if the subtree is not empty.
	    found = true
	    s = format("%sif not b%d then b%d=k\n%sb%d=N end\n", s, b, b, st, b)
	  end
	end
      end
    end
    return found and s
  end

  -- Embed the decision tree into a function hierarchy.
  for p=88,0,-1 do
    local pn = ok[p]
    if pn then
      s = format("%slocal function f%d(k)\nlocal N if b%d then return f%d(k) end k=k+1 b%d=k\n%sb%d=N end\n",
	    s, p, p, pn, p, codetree(dtree, 1, p, pn), p)
    end
  end

  -- Compile and return solver function and result getter.
  return loadstring(s.."return f0, printresult\n", "solver")(countinit)
end

-- Run the solver protected to get partial results (max count or ctrl-c).
local solver, printresult = generatesolver(tonumber(arg and arg[1]) or 0)
pcall(solver, 0)
printresult()

-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

-- Generate a decision tree based solver for the meteor puzzle.
local function generatesolver(countinit)
  local pairs, ipairs, format = pairs, ipairs, string.format
  local byte, min, sort = string.byte, math.min, table.sort

  -- Cached position to distance lookup.
  local dist = setmetatable({}, { __index = function(t, xy)
    local x = xy%10; local y = (xy-x)/10
    if (x+y)%2 == 1 then y = y + 1; x = 10 - x end
    local d = xy + 256*x*x + 1024*y*y; t[xy] = d; return d
  end})

  -- Generate an optimized decision tree (within 4% of a hand-tuned tree).
  local dtree = {}
  local rot = { nil, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
  for k=0,9 do
    -- Generate 10 initial pieces from line noise. :-)
    local t = { 60, 62, byte("@BMBIK@KT@GPIKR@IKIKT@GK@KM@BG", k*3+1, k*3+3) }
    rot[1] = t
    for i,xy in ipairs(t) do
      local x = xy%10; local y = (xy-x-60)/10
      -- Add 11 more variations by rotating and flipping.
      for j=2,12 do
	if j == 7 then y = -y else x,y = (x+3*y)/2, (y-x)/2 end
	rot[j][i] = x+10*y
      end
    end
    for i,v in ipairs(rot) do
      -- Exploit symmetry and leave out half of the orientations of one piece.
      -- The selected piece gives the best reduction of the solution space.
      if k ~= 3 or i%2 == 0 then
	-- Normalize to origin, add distance, sort by distance from origin.
	local m = min(v[1], v[2], v[3], v[4], v[5])
	for i=1,5 do v[i] = dist[v[i]-m] end
	sort(v)
	-- Insert into decision tree in distance order.
	local tt = dtree
	for i=2,4 do
	  local xy = v[i]%256
	  local tn = tt[xy]
	  if not tn then tn = {}; tt[xy] = tn end -- Create nodes as needed.
	  tt = tn
	end
	tt[v[5]%256] = k -- Leaves hold the piece numbers.
      end
    end
  end

  -- Lookup table to validate a cell and to find its successor.
  local ok = {}
  for i=0,150 do ok[i] = false end
  for i=99,0,-1 do
    local x = i%10
    if ((i-x)/10+x)%2 == 0 then
      ok[i] = i + (ok[i+1] and 1 or (ok[i+2] and 2 or 3))
    end
  end

  local s = "local u0,u1,u2,u3,u4,u5,u6,u7,u8,u9" -- Piece use flags.
  for p=0,99 do if ok[p] then s = s..",b"..p end end -- Board cells.
  s = s.."\n"..[[
local countinit = ...
local count = countinit
local bmin, bmax, pcs = 9, 0, {}
local smin, smax
local write, reverse = io.write, string.reverse

-- Print min/max boards.
local function printboard(s)
  local flip = true
  for x in string.gmatch(string.gsub(s, ".", "%1 "), "..........") do
    write(x, flip and "\n " or "\n")
    flip = not flip
  end
  write("\n")
end

-- Print result.
local function printresult()
  write(countinit-count, " solutions found\n\n")
  printboard(smin)
  printboard(smax)
end

-- Generate piece lookup array from the order of use.
local function genp()
  local p = pcs
  p[u0] = "0" p[u1] = "1" p[u2] = "2" p[u3] = "3" p[u4] = "4"
  p[u5] = "5" p[u6] = "6" p[u7] = "7" p[u8] = "8" p[u9] = "9"
  return p
end

-- Goal function.
local function f91(k)
  if k ~= 10 then return end
  count = count - 2 -- Need to count the symmetric solution, too.
  repeat
    -- Quick precheck before constructing the string.
    local b0, b99 = b0, b99
    if b0 <= bmin then bmin = b0 elseif b0 >= bmax then bmax = b0
    elseif b99 <= bmin then bmin = b99 elseif b99 >= bmax then bmax = b99
    else break end
    -- Translate the filled board to a string.
    local p = genp()
    local s = p[b0] ]]
  for p=2,99 do if ok[p] then s = s.."..p[b"..p.."]" end end
  s = s..[[
    -- Remember min/max boards, dito for the symmetric board.
    if not smin then smin = s; smax = s
    elseif s < smin then smin = s elseif s > smax then smax = s end
    s = reverse(s)
    if s < smin then smin = s elseif s > smax then smax = s end
  until true
  if count <= 0 then error("") end -- Early abort if max count given.
end
local f93 = f91
]]

  -- Recursively prune the decision tree and convert it to Lua code.
  local function codetree(tree, d, p, pn)
    local found, s = false, ""
    d = d + 1
    for a,t in pairs(tree) do
      local b = p+a
      local pp = ok[b]
      if pp then -- Prune the tree on-the-fly.
	if b ~= pn then pp = pn end -- Find maximum successor function.
	if d == 5 then -- Try to place the last cell of a piece and advance.
	  found = true
	  s = format("%sif not u%d and not b%d then b%d=k u%d=k f%d(k) u%d=N b%d=N end\n",
		     s, t, b, b, t, pp, t, b)
	else -- Try to place an intermediate cell.
	  local st = codetree(t, d, p, pp)
	  if st then -- But only if the subtree is not empty.
	    found = true
	    s = format("%sif not b%d then b%d=k\n%sb%d=N end\n", s, b, b, st, b)
	  end
	end
      end
    end
    return found and s
  end

  -- Embed the decision tree into a function hierarchy.
  for p=88,0,-1 do
    local pn = ok[p]
    if pn then
      s = format("%slocal function f%d(k)\nlocal N if b%d then return f%d(k) end k=k+1 b%d=k\n%sb%d=N end\n",
	    s, p, p, pn, p, codetree(dtree, 1, p, pn), p)
    end
  end

  -- Compile and return solver function and result getter.
  return loadstring(s.."return f0, printresult\n", "solver")(countinit)
end

-- Run the solver protected to get partial results (max count or ctrl-c).
local solver, printresult = generatesolver(tonumber(arg and arg[1]) or 10000)
pcall(solver, 0)
printresult()

-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

-- Generate a decision tree based solver for the meteor puzzle.
local function generatesolver(countinit)
  local pairs, ipairs, format = pairs, ipairs, string.format
  local byte, min, sort = string.byte, math.min, table.sort

  -- Cached position to distance lookup.
  local dist = setmetatable({}, { __index = function(t, xy)
    local x = xy%10; local y = (xy-x)/10
    if (x+y)%2 == 1 then y = y + 1; x = 10 - x end
    local d = xy + 256*x*x + 1024*y*y; t[xy] = d; return d
  end})

  -- Generate an optimized decision tree (within 4% of a hand-tuned tree).
  local dtree = {}
  local rot = { nil, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
  for k=0,9 do
    -- Generate 10 initial pieces from line noise. :-)
    local t = { 60, 62, byte("@BMBIK@KT@GPIKR@IKIKT@GK@KM@BG", k*3+1, k*3+3) }
    rot[1] = t
    for i,xy in ipairs(t) do
      local x = xy%10; local y = (xy-x-60)/10
      -- Add 11 more variations by rotating and flipping.
      for j=2,12 do
	if j == 7 then y = -y else x,y = (x+3*y)/2, (y-x)/2 end
	rot[j][i] = x+10*y
      end
    end
    for i,v in ipairs(rot) do
      -- Exploit symmetry and leave out half of the orientations of one piece.
      -- The selected piece gives the best reduction of the solution space.
      if k ~= 3 or i%2 == 0 then
	-- Normalize to origin, add distance, sort by distance from origin.
	local m = min(v[1], v[2], v[3], v[4], v[5])
	for i=1,5 do v[i] = dist[v[i]-m] end
	sort(v)
	-- Insert into decision tree in distance order.
	local tt = dtree
	for i=2,4 do
	  local xy = v[i]%256
	  local tn = tt[xy]
	  if not tn then tn = {}; tt[xy] = tn end -- Create nodes as needed.
	  tt = tn
	end
	tt[v[5]%256] = k -- Leaves hold the piece numbers.
      end
    end
  end

  -- Lookup table to validate a cell and to find its successor.
  local ok = {}
  for i=0,150 do ok[i] = false end
  for i=99,0,-1 do
    local x = i%10
    if ((i-x)/10+x)%2 == 0 then
      ok[i] = i + (ok[i+1] and 1 or (ok[i+2] and 2 or 3))
    end
  end

  local s = "local u0,u1,u2,u3,u4,u5,u6,u7,u8,u9" -- Piece use flags.
  for p=0,99 do if ok[p] then s = s..",b"..p end end -- Board cells.
  s = s.."\n"..[[
local countinit = ...
local count = countinit
local bmin, bmax, pcs = 9, 0, {}
local smin, smax
local write, reverse = io.write, string.reverse

-- Print min/max boards.
local function printboard(s)
  local flip = true
  for x in string.gmatch(string.gsub(s, ".", "%1 "), "..........") do
    write(x, flip and "\n " or "\n")
    flip = not flip
  end
  write("\n")
end

-- Print result.
local function printresult()
  write(countinit-count, " solutions found\n\n")
  printboard(smin)
  printboard(smax)
end

-- Generate piece lookup array from the order of use.
local function genp()
  local p = pcs
  p[u0] = "0" p[u1] = "1" p[u2] = "2" p[u3] = "3" p[u4] = "4"
  p[u5] = "5" p[u6] = "6" p[u7] = "7" p[u8] = "8" p[u9] = "9"
  return p
end

-- Goal function.
local function f91(k)
  if k ~= 10 then return end
  count = count - 2 -- Need to count the symmetric solution, too.
  repeat
    -- Quick precheck before constructing the string.
    local b0, b99 = b0, b99
    if b0 <= bmin then bmin = b0 elseif b0 >= bmax then bmax = b0
    elseif b99 <= bmin then bmin = b99 elseif b99 >= bmax then bmax = b99
    else break end
    -- Translate the filled board to a string.
    local p = genp()
    local s = p[b0] ]]
  for p=2,99 do if ok[p] then s = s.."..p[b"..p.."]" end end
  s = s..[[
    -- Remember min/max boards, dito for the symmetric board.
    if not smin then smin = s; smax = s
    elseif s < smin then smin = s elseif s > smax then smax = s end
    s = reverse(s)
    if s < smin then smin = s elseif s > smax then smax = s end
  until true
  if count <= 0 then error("") end -- Early abort if max count given.
end
local f93 = f91
]]

  -- Recursively prune the decision tree and convert it to Lua code.
  local function codetree(tree, d, p, pn)
    local found, s = false, ""
    d = d + 1
    for a,t in pairs(tree) do
      local b = p+a
      local pp = ok[b]
      if pp then -- Prune the tree on-the-fly.
	if b ~= pn then pp = pn end -- Find maximum successor function.
	if d == 5 then -- Try to place the last cell of a piece and advance.
	  found = true
	  s = format("%sif not u%d and not b%d then b%d=k u%d=k f%d(k) u%d=N b%d=N end\n",
		     s, t, b, b, t, pp, t, b)
	else -- Try to place an intermediate cell.
	  local st = codetree(t, d, p, pp)
	  if st then -- But only if the subtree is not empty.
	    found = true
	    s = format("%sif not b%d then b%d=k\n%sb%d=N end\n", s, b, b, st, b)
	  end
	end
      end
    end
    return found and s
  end

  -- Embed the decision tree into a function hierarchy.
  for p=88,0,-1 do
    local pn = ok[p]
    if pn then
      s = format("%slocal function f%d(k)\nlocal N if b%d then return f%d(k) end k=k+1 b%d=k\n%sb%d=N end\n",
	    s, p, p, pn, p, codetree(dtree, 1, p, pn), p)
    end
  end

  -- Compile and return solver function and result getter.
  return loadstring(s.."return f0, printresult\n", "solver")(countinit)
end

-- The optimizer for LuaJIT 1.1.x is not helpful here, so turn it off.
if jit and jit.opt and jit.version_num < 10200 then
  jit.opt.start(0)
  jit.off(generatesolver)
end

-- Run the solver protected to get partial results (max count or ctrl-c).
local solver, printresult = generatesolver(tonumber(arg and arg[1]) or 10000)
pcall(solver, 0)
printresult()

-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

-- Generate a decision tree based solver for the meteor puzzle.
local function generatesolver(countinit)
  local pairs, ipairs, format = pairs, ipairs, string.format
  local byte, min, sort = string.byte, math.min, table.sort

  -- Cached position to distance lookup.
  local dist = setmetatable({}, { __index = function(t, xy)
    local x = xy%10; local y = (xy-x)/10
    if (x+y)%2 == 1 then y = y + 1; x = 10 - x end
    local d = xy + 256*x*x + 1024*y*y; t[xy] = d; return d
  end})

  -- Lookup table to validate a cell and to find its successor.
  local ok = {}
  for i=0,150 do ok[i] = false end
  for i=99,0,-1 do
    local x = i%10
    if ((i-x)/10+x)%2 == 0 then
      ok[i] = i + (ok[i+1] and 1 or (ok[i+2] and 2 or 3))
    end
  end

  -- Temporary board state for the island checks.
  local islands, slide = {}, {20,22,24,26,28,31,33,35,37,39}
  local bbc, bb = 0, {}
  for i=0,19 do bb[i] = false; bb[i+80] = false end
  for i=20,79 do bb[i] = ok[i] end

  -- Recursive flood fill algorithm.
  local function fill(bb, p)
    bbc = bbc + 1
    local n = p+2; if bb[n] then bb[n] = false; fill(bb, n) end
    n = p-2; if bb[n] then bb[n] = false; fill(bb, n) end
    n = p-9; if bb[n] then bb[n] = false; fill(bb, n) end
    n = p-11; if bb[n] then bb[n] = false; fill(bb, n) end
    n = p+9; if bb[n] then bb[n] = false; fill(bb, n) end
    n = p+11; if bb[n] then bb[n] = false; fill(bb, n) end
  end

  -- Generate pruned, sliding decision trees.
  local dtrees = {{}, {}, {}, {}, {}, {}, {}, {}, {}, {}}
  local rot = { nil, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
  for k=0,9 do
    -- Generate 10 initial pieces from line noise. :-)
    local t = { 60, 62, byte("@BMBIK@KT@GPIKR@IKIKT@GK@KM@BG", k*3+1, k*3+3) }
    rot[1] = t
    for i,xy in ipairs(t) do
      local x = xy%10; local y = (xy-x-60)/10
      -- Add 11 more variations by rotating and flipping.
      for j=2,12 do
	if j == 7 then y = -y else x,y = (x+3*y)/2, (y-x)/2 end
	rot[j][i] = x+10*y
      end
    end
    for r,v in ipairs(rot) do
      -- Exploit symmetry and leave out half of the orientations of one piece.
      -- The selected piece gives the best reduction of the solution space.
      if k ~= 3 or r%2 == 0 then
	-- Normalize to origin, add distance, sort by distance from origin.
	local m = min(v[1], v[2], v[3], v[4], v[5])
	for i=1,5 do v[i] = dist[v[i]-m] end
	sort(v)
	local v2, v3, v4, v5 = v[2]%256, v[3]%256, v[4]%256, v[5]%256
	-- Slide the piece across 2 rows, prune the tree, check for islands.
	for j,p in ipairs(slide) do
	  bb[p] = false
	  if ok[p+v2] and ok[p+v3] and ok[p+v4] and ok[p+v5] then -- Prune.
	    for i=p+1,79 do bb[i] = ok[i] end -- Clear remaining board.
	    bb[p+v2] = false; bb[p+v3] = false -- Add piece.
	    bb[p+v4] = false; bb[p+v5] = false
	    bbc = j -- Flood fill and count the filled positions.
	    if bb[71] then bb[71] = false; fill(bb, 71) end -- Lower left.
	    if bb[79] then bb[79] = false; fill(bb, 79) end -- Lower right.
	    local di = 0
	    if bbc < 22 then bbc = 26
	    elseif bbc < 26 then -- Island found, locate it, fill from above.
	      for i=p+2,79 do if bb[i] then di = i-p; break end end
	      for i=p-9,p-1 do if ok[i] then fill(bb, i) bbc = bbc - 1 end end
	    end
	    if bbc == 26 then -- Prune boards with static islands.
	      local tb = dtrees[j] -- Build decision tree in distance order.
	      local ta = tb[v2]; if not ta then ta = {}; tb[v2] = ta end
	      tb = ta[v3]; if not tb then tb = {}; ta[v3] = tb end
	      ta = tb[v4]; if not ta then ta = {}; tb[v4] = ta; islands[ta] = di
	      elseif islands[ta] ~= di then islands[ta] = 0 end
	      ta[v5] = di*10+k -- Leaves hold island check and piece number.
	    end
	  end
	end
      end
    end
  end

  local s = "local u0,u1,u2,u3,u4,u5,u6,u7,u8,u9" -- Piece use flags.
  for p=0,99 do if ok[p] then s = s..",b"..p end end -- Board cells.
  s = s.."\n"..[[
local countinit = ...
local count = countinit
local bmin, bmax, pcs = 9, 0, {}
local smin, smax
local write, reverse = io.write, string.reverse

-- Print min/max boards.
local function printboard(s)
  local flip = true
  for x in string.gmatch(string.gsub(s, ".", "%1 "), "..........") do
    write(x, flip and "\n " or "\n")
    flip = not flip
  end
  write("\n")
end

-- Print result.
local function printresult()
  write(countinit-count, " solutions found\n\n")
  printboard(smin)
  printboard(smax)
end

-- Generate piece lookup array from the order of use.
local function genp()
  local p = pcs
  p[u0] = "0" p[u1] = "1" p[u2] = "2" p[u3] = "3" p[u4] = "4"
  p[u5] = "5" p[u6] = "6" p[u7] = "7" p[u8] = "8" p[u9] = "9"
  return p
end

-- Goal function.
local function f91(k)
  if k ~= 10 then return end
  count = count - 2 -- Need to count the symmetric solution, too.
  repeat
    -- Quick precheck before constructing the string.
    local b0, b99 = b0, b99
    if b0 <= bmin then bmin = b0 elseif b0 >= bmax then bmax = b0
    elseif b99 <= bmin then bmin = b99 elseif b99 >= bmax then bmax = b99
    else break end
    -- Translate the filled board to a string.
    local p = genp()
    local s = p[b0] ]]
  for p=2,99 do if ok[p] then s = s.."..p[b"..p.."]" end end
  s = s..[[
    -- Remember min/max boards, dito for the symmetric board.
    if not smin then smin = s; smax = s
    elseif s < smin then smin = s elseif s > smax then smax = s end
    s = reverse(s)
    if s < smin then smin = s elseif s > smax then smax = s end
  until true
  if count <= 0 then error() end -- Early abort if max count given.
end
local f93 = f91
]]

  -- Recursively convert the decision tree to Lua code.
  local function codetree(tree, d, p, pn)
    local found, s = false, ""
    d = d + 1
    for a,t in pairs(tree) do
      local b = p+a
      if b < 100 then -- Prune the tree at the lower border.
	local pp = b ~= pn and pn or ok[b] -- Find maximum successor function.
	if d >= 5 then -- Try to place the last cell of a piece and advance.
	  found = true
	  local u = t%10
	  local di = (t-u)/10
	  if di ~= 0 and d == 5 then
	    di = di + p; if pp == di then pp = ok[di] end
	    s = format("%sif b%d and not u%d and not b%d then b%d=k u%d=k f%d(k) u%d=N b%d=N end\n",
		       s, di, u, b, b, u, pp, u, b)
	  else
	    s = format("%sif not u%d and not b%d then b%d=k u%d=k f%d(k) u%d=N b%d=N end\n",
		       s, u, b, b, u, pp, u, b)
	  end
	else -- Try to place an intermediate cell.
	  local di = d ~= 4 and 0 or islands[t]
	  if di == 0 then
	    local st = codetree(t, d, p, pp)
	    if st then
	      found = true
	      s = format("%sif not b%d then b%d=k\n%sb%d=N end\n", s, b, b, st, b)
	    end
	  else -- Combine island checks.
	    di = di + p; if pp == di then pp = ok[di] end
	    local st = codetree(t, 6, p, pp)
	    if st then
	      found = true
	      s = format("%sif b%d and not b%d then b%d=k\n%sb%d=N end\n", s, di, b, b, st, b)
	    end
	  end
	end
      end
    end
    return found and s
  end

  -- Embed the decision tree into a function hierarchy.
  local j = 5
  for p=88,0,-1 do
    local pn = ok[p]
    if pn then
      s = format("%slocal function f%d(k)\nlocal N if b%d then return f%d(k) end k=k+1 b%d=k\n%sb%d=N end\n",
	    s, p, p, pn, p, codetree(dtrees[j], 1, p, pn), p)
      j = j - 1; if j == 0 then j = 10 end
    end
  end

  -- Compile and return solver function and result getter.
  return loadstring(s.."return f0, printresult\n", "solver")(countinit)
end

-- Generate the solver function hierarchy.
local solver, printresult = generatesolver(tonumber(arg and arg[1]) or 10000)

-- The optimizer for LuaJIT 1.1.x is not helpful here, so turn it off.
if jit and jit.opt and jit.version_num < 10200 then jit.opt.start(0) end

-- Run the solver protected to get partial results (max count or ctrl-c).
pcall(solver, 0)
printresult()

-- $Id: methcall.lua,v 1.2 2004-06-12 16:19:43 bfulgham Exp $
-- http://shootout.alioth.debian.org
-- contributed by Roberto Ierusalimschy

--------------------------------------------------------------
-- Toggle class
--------------------------------------------------------------

Toggle = {}

function Toggle:value ()
  return self.state
end

function Toggle:activate ()
  self.state = not self.state
  return self
end

function Toggle:new (start_state)
  local o = {state = start_state}
  self.__index =self
  setmetatable(o, self)
  return o
end


--------------------------------------------------------------
-- NthToggle class
--------------------------------------------------------------

NthToggle = Toggle:new()

function NthToggle:activate ()
  self.counter = self.counter + 1
  if self.counter >= self.count_max then
    self.state = not self.state
    self.counter = 0
  end
  return self
end

function NthToggle:new (start_state, max_counter)
  local o = Toggle.new(self, start_state)
  o.count_max = max_counter
  o.counter = 0
  return o
end


-----------------------------------------------------------
-- main
-----------------------------------------------------------

function main ()
  local N = tonumber((arg and arg[1])) or 1

  local val = 1
  local toggle = Toggle:new(val)
  for i=1,N do
    val = toggle:activate():value()
  end
  print(val and "true" or "false")
    
  val = 1
  local ntoggle = NthToggle:new(val, 3)
  for i=1,N do
    val = ntoggle:activate():value()
  end
  print(val and "true" or "false")
end

main()


-- $Id: moments.lua,v 1.2 2004-05-24 02:23:25 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- implemented by: Roberto Ierusalimschy

local nums = {}
local n = 0
local sum = 0
while 1 do
  local line = io.read()
  if line == nil then break end
  line = line+0        -- convert line to number
  sum = sum + line
  n = n + 1
  nums[n] = line
end

local mean = sum/n

local average_deviation, variance, skew, kurtosis = 0, 0, 0, 0

for i = 1, n do
  local deviation = nums[i] - mean
  average_deviation = average_deviation + math.abs(deviation)
  variance = variance + deviation^2
  skew = skew + deviation^3
  kurtosis = kurtosis + deviation^4
end

average_deviation = average_deviation/n
variance = variance/(n-1)
local standard_deviation = math.sqrt(variance)
if variance ~= 0 then
  skew = skew / (n * variance * standard_deviation)
  kurtosis = kurtosis/(n * variance * variance) - 3.0
end

table.sort(nums)
local mid = math.floor(n/2)
local median
if math.mod(n,2) == 1 then
  median = nums[mid+1]
else
  median = (nums[mid] + nums[mid+1])/2
end

io.write(string.format("n:                  %d\n", n))
io.write(string.format("median:             %f\n", median))
io.write(string.format("mean:               %f\n", mean))
io.write(string.format("average_deviation:  %f\n", average_deviation))
io.write(string.format("standard_deviation: %f\n", standard_deviation))
io.write(string.format("variance:           %f\n", variance))
io.write(string.format("skew:               %f\n", skew))
io.write(string.format("kurtosis:           %f\n", kurtosis))
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local sqrt = math.sqrt

local PI = 3.141592653589793
local SOLAR_MASS = 4 * PI * PI
local DAYS_PER_YEAR = 365.24
local bodies = {
  { -- Sun
    x = 0,
    y = 0,
    z = 0,
    vx = 0,
    vy = 0,
    vz = 0,
    mass = SOLAR_MASS
  },
  { -- Jupiter
    x = 4.84143144246472090e+00,
    y = -1.16032004402742839e+00,
    z = -1.03622044471123109e-01,
    vx = 1.66007664274403694e-03 * DAYS_PER_YEAR,
    vy = 7.69901118419740425e-03 * DAYS_PER_YEAR,
    vz = -6.90460016972063023e-05 * DAYS_PER_YEAR,
    mass = 9.54791938424326609e-04 * SOLAR_MASS
  },
  { -- Saturn
    x = 8.34336671824457987e+00,
    y = 4.12479856412430479e+00,
    z = -4.03523417114321381e-01,
    vx = -2.76742510726862411e-03 * DAYS_PER_YEAR,
    vy = 4.99852801234917238e-03 * DAYS_PER_YEAR,
    vz = 2.30417297573763929e-05 * DAYS_PER_YEAR,
    mass = 2.85885980666130812e-04 * SOLAR_MASS
  },
  { -- Uranus
    x = 1.28943695621391310e+01,
    y = -1.51111514016986312e+01,
    z = -2.23307578892655734e-01,
    vx = 2.96460137564761618e-03 * DAYS_PER_YEAR,
    vy = 2.37847173959480950e-03 * DAYS_PER_YEAR,
    vz = -2.96589568540237556e-05 * DAYS_PER_YEAR,
    mass = 4.36624404335156298e-05 * SOLAR_MASS
  },
  { -- Neptune
    x = 1.53796971148509165e+01,
    y = -2.59193146099879641e+01,
    z = 1.79258772950371181e-01,
    vx = 2.68067772490389322e-03 * DAYS_PER_YEAR,
    vy = 1.62824170038242295e-03 * DAYS_PER_YEAR,
    vz = -9.51592254519715870e-05 * DAYS_PER_YEAR,
    mass = 5.15138902046611451e-05 * SOLAR_MASS
  }
}

local function advance(bodies, nbody, dt)
  for i=1,nbody do
    local bi = bodies[i]
    local bix, biy, biz, bimass = bi.x, bi.y, bi.z, bi.mass
    local bivx, bivy, bivz = bi.vx, bi.vy, bi.vz
    for j=i+1,nbody do
      local bj = bodies[j]
      local dx, dy, dz = bix-bj.x, biy-bj.y, biz-bj.z
      local distance = sqrt(dx*dx + dy*dy + dz*dz)
      local mag = dt / (distance * distance * distance)
      local bim, bjm = bimass*mag, bj.mass*mag
      bivx = bivx - (dx * bjm)
      bivy = bivy - (dy * bjm)
      bivz = bivz - (dz * bjm)
      bj.vx = bj.vx + (dx * bim)
      bj.vy = bj.vy + (dy * bim)
      bj.vz = bj.vz + (dz * bim)
    end
    bi.vx = bivx
    bi.vy = bivy
    bi.vz = bivz
  end
  for i=1,nbody do
    local bi = bodies[i]
    bi.x = bi.x + (dt * bi.vx)
    bi.y = bi.y + (dt * bi.vy)
    bi.z = bi.z + (dt * bi.vz)
  end
end

local function energy(bodies, nbody)
  local e = 0
  for i=1,nbody do
    local bi = bodies[i]
    local vx, vy, vz, bim = bi.vx, bi.vy, bi.vz, bi.mass
    e = e + (0.5 * bim * (vx*vx + vy*vy + vz*vz))
    for j=i+1,nbody do
      local bj = bodies[j]
      local dx, dy, dz = bi.x-bj.x, bi.y-bj.y, bi.z-bj.z
      local distance = sqrt(dx*dx + dy*dy + dz*dz)
      e = e - ((bim * bj.mass) / distance)
    end
  end
  return e
end

local function offsetMomentum(b, nbody)
  local px, py, pz = 0, 0, 0
  for i=1,nbody do
    local bi = b[i]
    local bim = bi.mass
    px = px + (bi.vx * bim)
    py = py + (bi.vy * bim)
    pz = pz + (bi.vz * bim)
  end
  b[1].vx = -px / SOLAR_MASS
  b[1].vy = -py / SOLAR_MASS
  b[1].vz = -pz / SOLAR_MASS
end

local N = tonumber(arg and arg[1]) or 1000
local nbody = #bodies

offsetMomentum(bodies, nbody)
io.write( string.format("%0.9f",energy(bodies, nbody)), "\n")
for i=1,N do advance(bodies, nbody, 0.01) end
io.write( string.format("%0.9f",energy(bodies, nbody)), "\n")
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- modified by Geoff Leyland

local sqrt = math.sqrt

local PI = 3.141592653589793
local SOLAR_MASS = 4 * PI * PI
local DAYS_PER_YEAR = 365.24
local bodies = {
  { -- Sun
    x = 0,
    y = 0,
    z = 0,
    vx = 0,
    vy = 0,
    vz = 0,
    mass = SOLAR_MASS
  },
  { -- Jupiter
    x = 4.84143144246472090e+00,
    y = -1.16032004402742839e+00,
    z = -1.03622044471123109e-01,
    vx = 1.66007664274403694e-03 * DAYS_PER_YEAR,
    vy = 7.69901118419740425e-03 * DAYS_PER_YEAR,
    vz = -6.90460016972063023e-05 * DAYS_PER_YEAR,
    mass = 9.54791938424326609e-04 * SOLAR_MASS
  },
  { -- Saturn
    x = 8.34336671824457987e+00,
    y = 4.12479856412430479e+00,
    z = -4.03523417114321381e-01,
    vx = -2.76742510726862411e-03 * DAYS_PER_YEAR,
    vy = 4.99852801234917238e-03 * DAYS_PER_YEAR,
    vz = 2.30417297573763929e-05 * DAYS_PER_YEAR,
    mass = 2.85885980666130812e-04 * SOLAR_MASS
  },
  { -- Uranus
    x = 1.28943695621391310e+01,
    y = -1.51111514016986312e+01,
    z = -2.23307578892655734e-01,
    vx = 2.96460137564761618e-03 * DAYS_PER_YEAR,
    vy = 2.37847173959480950e-03 * DAYS_PER_YEAR,
    vz = -2.96589568540237556e-05 * DAYS_PER_YEAR,
    mass = 4.36624404335156298e-05 * SOLAR_MASS
  },
  { -- Neptune
    x = 1.53796971148509165e+01,
    y = -2.59193146099879641e+01,
    z = 1.79258772950371181e-01,
    vx = 2.68067772490389322e-03 * DAYS_PER_YEAR,
    vy = 1.62824170038242295e-03 * DAYS_PER_YEAR,
    vz = -9.51592254519715870e-05 * DAYS_PER_YEAR,
    mass = 5.15138902046611451e-05 * SOLAR_MASS
  }
}

local function advance(bodies, nbody, dt)
  for i=1,nbody do
    local bi = bodies[i]
    local bix, biy, biz, bimass = bi.x, bi.y, bi.z, bi.mass
    local bivx, bivy, bivz = bi.vx, bi.vy, bi.vz
    for j=i+1,nbody do
      local bj = bodies[j]
      local dx, dy, dz = bix-bj.x, biy-bj.y, biz-bj.z
      local mag = sqrt(dx*dx + dy*dy + dz*dz)
      mag = dt / (mag * mag * mag)
      local bm = bj.mass*mag
      bivx = bivx - (dx * bm)
      bivy = bivy - (dy * bm)
      bivz = bivz - (dz * bm)
      bm = bimass*mag
      bj.vx = bj.vx + (dx * bm)
      bj.vy = bj.vy + (dy * bm)
      bj.vz = bj.vz + (dz * bm)
    end
    bi.vx = bivx
    bi.vy = bivy
    bi.vz = bivz
    bi.x = bix + dt * bivx
    bi.y = biy + dt * bivy
    bi.z = biz + dt * bivz
  end
end

local function energy(bodies, nbody)
  local e = 0
  for i=1,nbody do
    local bi = bodies[i]
    local vx, vy, vz, bim = bi.vx, bi.vy, bi.vz, bi.mass
    e = e + (0.5 * bim * (vx*vx + vy*vy + vz*vz))
    for j=i+1,nbody do
      local bj = bodies[j]
      local dx, dy, dz = bi.x-bj.x, bi.y-bj.y, bi.z-bj.z
      local distance = sqrt(dx*dx + dy*dy + dz*dz)
      e = e - ((bim * bj.mass) / distance)
    end
  end
  return e
end

local function offsetMomentum(b, nbody)
  local px, py, pz = 0, 0, 0
  for i=1,nbody do
    local bi = b[i]
    local bim = bi.mass
    px = px + (bi.vx * bim)
    py = py + (bi.vy * bim)
    pz = pz + (bi.vz * bim)
  end
  b[1].vx = -px / SOLAR_MASS
  b[1].vy = -py / SOLAR_MASS
  b[1].vz = -pz / SOLAR_MASS
end

local N = tonumber(arg and arg[1]) or 1000
local nbody = #bodies

offsetMomentum(bodies, nbody)
io.write( string.format("%0.9f",energy(bodies, nbody)), "\n")
for i=1,N do advance(bodies, nbody, 0.01) end
io.write( string.format("%0.9f",energy(bodies, nbody)), "\n")
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- modified by Geoff Leyland
-- modified by Mario Pernici

sun = {}
jupiter = {}
saturn = {}
uranus = {}
neptune = {}

local sqrt = math.sqrt

local PI = 3.141592653589793
local SOLAR_MASS = 4 * PI * PI
local DAYS_PER_YEAR = 365.24
sun.x = 0.0
sun.y = 0.0
sun.z = 0.0
sun.vx = 0.0
sun.vy = 0.0
sun.vz = 0.0
sun.mass = SOLAR_MASS
jupiter.x = 4.84143144246472090e+00
jupiter.y = -1.16032004402742839e+00
jupiter.z = -1.03622044471123109e-01
jupiter.vx = 1.66007664274403694e-03 * DAYS_PER_YEAR
jupiter.vy = 7.69901118419740425e-03 * DAYS_PER_YEAR
jupiter.vz = -6.90460016972063023e-05 * DAYS_PER_YEAR
jupiter.mass = 9.54791938424326609e-04 * SOLAR_MASS
saturn.x = 8.34336671824457987e+00
saturn.y = 4.12479856412430479e+00
saturn.z = -4.03523417114321381e-01
saturn.vx = -2.76742510726862411e-03 * DAYS_PER_YEAR
saturn.vy = 4.99852801234917238e-03 * DAYS_PER_YEAR
saturn.vz = 2.30417297573763929e-05 * DAYS_PER_YEAR
saturn.mass = 2.85885980666130812e-04 * SOLAR_MASS
uranus.x = 1.28943695621391310e+01
uranus.y = -1.51111514016986312e+01
uranus.z = -2.23307578892655734e-01
uranus.vx = 2.96460137564761618e-03 * DAYS_PER_YEAR
uranus.vy = 2.37847173959480950e-03 * DAYS_PER_YEAR
uranus.vz = -2.96589568540237556e-05 * DAYS_PER_YEAR
uranus.mass = 4.36624404335156298e-05 * SOLAR_MASS
neptune.x = 1.53796971148509165e+01
neptune.y = -2.59193146099879641e+01
neptune.z = 1.79258772950371181e-01
neptune.vx = 2.68067772490389322e-03 * DAYS_PER_YEAR
neptune.vy = 1.62824170038242295e-03 * DAYS_PER_YEAR
neptune.vz = -9.51592254519715870e-05 * DAYS_PER_YEAR
neptune.mass = 5.15138902046611451e-05 * SOLAR_MASS

local bodies = {sun,jupiter,saturn,uranus,neptune}

local function advance(bodies, nbody, dt)
  for i=1,nbody do
    local bi = bodies[i]
    local bix, biy, biz, bimass = bi.x, bi.y, bi.z, bi.mass
    local bivx, bivy, bivz = bi.vx, bi.vy, bi.vz
    for j=i+1,nbody do
      local bj = bodies[j]
      local dx, dy, dz = bix-bj.x, biy-bj.y, biz-bj.z
      local dist2 = dx*dx + dy*dy + dz*dz
      local mag = sqrt(dist2)
      mag = dt / (mag * dist2)
      local bm = bj.mass*mag
      bivx = bivx - (dx * bm)
      bivy = bivy - (dy * bm)
      bivz = bivz - (dz * bm)
      bm = bimass*mag
      bj.vx = bj.vx + (dx * bm)
      bj.vy = bj.vy + (dy * bm)
      bj.vz = bj.vz + (dz * bm)
    end
    bi.vx = bivx
    bi.vy = bivy
    bi.vz = bivz
    bi.x = bix + dt * bivx
    bi.y = biy + dt * bivy
    bi.z = biz + dt * bivz
  end
end

local function energy(bodies, nbody)
  local e = 0
  for i=1,nbody do
    local bi = bodies[i]
    local vx, vy, vz, bim = bi.vx, bi.vy, bi.vz, bi.mass
    e = e + (0.5 * bim * (vx*vx + vy*vy + vz*vz))
    for j=i+1,nbody do
      local bj = bodies[j]
      local dx, dy, dz = bi.x-bj.x, bi.y-bj.y, bi.z-bj.z
      local distance = sqrt(dx*dx + dy*dy + dz*dz)
      e = e - ((bim * bj.mass) / distance)
    end
  end
  return e
end

local function offsetMomentum(b, nbody)
  local px, py, pz = 0, 0, 0
  for i=1,nbody do
    local bi = b[i]
    local bim = bi.mass
    px = px + (bi.vx * bim)
    py = py + (bi.vy * bim)
    pz = pz + (bi.vz * bim)
  end
  b[1].vx = -px / SOLAR_MASS
  b[1].vy = -py / SOLAR_MASS
  b[1].vz = -pz / SOLAR_MASS
end

local N = tonumber(arg and arg[1]) or 1000
local nbody = #bodies

offsetMomentum(bodies, nbody)
io.write( string.format("%0.9f",energy(bodies, nbody)), "\n")
for i=1,N do advance(bodies, nbody, 0.01) end
io.write( string.format("%0.9f",energy(bodies, nbody)), "\n")
-- $Id: nestedloop.lua,v 1.1 2004-05-19 18:10:56 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/

local n = tonumber((arg and arg[1]) or 1)
local x = 0
for a=1,n do
    for b=1,n do
	for c=1,n do
	    for d=1,n do
		for e=1,n do
		    for f=1,n do
			x = x + 1
		    end
		end
	    end
	end
    end
end
io.write(x,"\n")
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Isaac Gouy
-- modified by Mike Pall


local function nsieve(m,isPrime)
   for i=2,m do
      isPrime[i] = true
   end
   local count = 0

   for i=2,m do
      if isPrime[i] then
         for k=i+i, m, i do
--            if isPrime[k] then isPrime[k] = false end
            isPrime[k] = false
         end
         count = count + 1
      end
   end
   return count
end
 

local n = tonumber(arg and arg[1]) or 1
if n < 2 then n = 2 end
local flags = {}

local m = (2^n)*10000 
io.write( string.format("Primes up to %8d %8d", m, nsieve(m,flags)), "\n")

m = (2^(n-1))*10000
io.write( string.format("Primes up to %8d %8d", m, nsieve(m,flags)), "\n")

m = (2^(n-2))*10000 
io.write( string.format("Primes up to %8d %8d", m, nsieve(m,flags)), "\n")
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local floor, ceil = math.floor, math.ceil

local precision = 50 -- Maximum precision of lua_Number (minus safety margin).
local onebits = (2^precision)-1

local function nsieve(p, m)
  local cm = ceil(m/precision)
  do local onebits = onebits; for i=0,cm do p[i] = onebits end end
  local count, idx, bit = 0, 2, 2
  for i=2,m do
    local r = p[idx] / bit
    if r - floor(r) >= 0.5 then -- Bit set?
      local kidx, kbit = idx, bit
      for k=i+i,m,i do
        kidx = kidx + i
        while kidx >= cm do kidx = kidx - cm; kbit = kbit + kbit end
        local x = p[kidx]
        local r = x / kbit
        if r - floor(r) >= 0.5 then p[kidx] = x - kbit*0.5 end -- Clear bit.
      end
      count = count + 1
    end
    idx = idx + 1
    if idx >= cm then idx = 0; bit = bit + bit end
  end
  return count
end

local N = tonumber(arg and arg[1]) or 1
if N < 2 then N = 2 end
local primes = {}

for i=0,2 do
  local m = (2^(N-i))*10000
  io.write(string.format("Primes up to %8d %8d\n", m, nsieve(primes, m)))
end
-- $Id: objinst.lua,v 1.4 2004-07-04 22:26:59 bfulgham Exp $
-- http://shootout.alioth.debian.org/
-- contributed by Roberto Ierusalimschy

--------------------------------------------------------------
-- Toggle class
--------------------------------------------------------------

Toggle = {}

function Toggle:value ()
  return self.state
end

function Toggle:activate ()
  self.state = not self.state
  return self
end

function Toggle:new (start_state)
  local o = {state = start_state}
  self.__index = self
  setmetatable(o, self)
  return o
end

--------------------------------------------------------------
-- NthToggle class
--------------------------------------------------------------

NthToggle = Toggle:new()

function NthToggle:activate ()
  self.counter = self.counter + 1
  if self.counter >= self.count_max then
    self.state = not self.state
    self.counter = 0
  end
  return self
end

function NthToggle:new (start_state, max_counter)
  local o = Toggle.new(self, start_state)
  o.count_max = max_counter
  o.counter = 0
  return o
end

-----------------------------------------------------------
-- main
-----------------------------------------------------------
function main ()
    local N = tonumber((arg and arg[1])) or 1
    local toggle = Toggle:new(1)
    for i=1,5 do
      toggle:activate()
      print(toggle:value() and "true" or "false")
    end
    for i=1,N do
      toggle = Toggle:new(1)
    end

    print("")

    local ntoggle = NthToggle:new(1, 3)
    for i=1,8 do
      ntoggle:activate()
      print(ntoggle:value() and "true" or "false")
    end
    for i=1,N do
      ntoggle = NthToggle:new(1, 3)
    end
end

main()


-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local n = tonumber(arg[1])
local function pr(fmt, x) io.write(string.format(fmt, x)) end

do
  local sum1, sum2, sum3, sum4, sum5, sum6, sum7 = 1, 0, 0, 0, 0, 0, 0
  local twothirds, sqrt, sin, cos = 2/3, math.sqrt, math.sin, math.cos
  for k=1,n do
    local k2, sk, ck = k*k, sin(k), cos(k)
    local k3 = k2*k
    sum1 = sum1 + twothirds^k
    sum2 = sum2 + 1/sqrt(k)
    sum3 = sum3 + 1/(k2+k)
    sum4 = sum4 + 1/(k3*sk*sk)
    sum5 = sum5 + 1/(k3*ck*ck)
    sum6 = sum6 + 1/k
    sum7 = sum7 + 1/k2
  end
  pr("%.9f\t(2/3)^k\n", sum1)
  pr("%.9f\tk^-0.5\n", sum2)
  pr("%.9f\t1/k(k+1)\n", sum3)
  pr("%.9f\tFlint Hills\n", sum4)
  pr("%.9f\tCookson Hills\n", sum5)
  pr("%.9f\tHarmonic\n", sum6)
  pr("%.9f\tRiemann Zeta\n", sum7)
end

do local sum = 0; for k=1,n-1,2 do sum = sum + 1/k end
for k=2,n,2 do sum = sum - 1/k end
pr("%.9f\tAlternating Harmonic\n", sum) end

do local sum = 0; for k=1,2*n-1,4 do sum = sum + 1/k end
for k=3,2*n,4 do sum = sum - 1/k end
pr("%.9f\tGregory\n", sum) end

-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local n = tonumber(arg[1])
local function pr(fmt, x) io.write(string.format(fmt, x)) end

local a1, a2, a3, a4, a5, a6, a7, a8, a9, alt = 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
local sqrt, sin, cos = math.sqrt, math.sin, math.cos
for k=1,n do
  local k2, sk, ck = k*k, sin(k), cos(k)
  local k3 = k2*k
  a1 = a1 + (2/3)^k
  a2 = a2 + 1/sqrt(k)
  a3 = a3 + 1/(k*(k+1.0))
  a4 = a4 + 1/(k3*sk*sk)
  a5 = a5 + 1/(k3*ck*ck)
  a6 = a6 + 1/k
  a7 = a7 + 1/k2
  a8 = a8 + alt/k
  a9 = a9 + alt/(k+k-1)
  alt = -alt
end
pr("%.9f\t(2/3)^k\n", a1)
pr("%.9f\tk^-0.5\n", a2)
pr("%.9f\t1/k(k+1)\n", a3)
pr("%.9f\tFlint Hills\n", a4)
pr("%.9f\tCookson Hills\n", a5)
pr("%.9f\tHarmonic\n", a6)
pr("%.9f\tRiemann Zeta\n", a7)
pr("%.9f\tAlternating Harmonic\n", a8)
pr("%.9f\tGregory\n", a9)

-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- requires LGMP "A GMP package for Lua 5.1"

local g, aux = {}, {}
require"c-gmp"(g, aux)
local add, mul, div = g.mpz_add, g.mpz_mul_si, g.mpz_tdiv_q
local init, get = g.mpz_init_set_d, g.mpz_get_d

local q, r, s, t, u, v, w

-- Compose matrix with numbers on the right.
local function compose_r(bq, br, bs, bt)
  mul(r, bs, u)
  mul(r, bq, r)
  mul(t, br, v)
  add(r, v, r)
  mul(t, bt, t)
  add(t, u, t)
  mul(s, bt, s)
  mul(q, bs, u)
  add(s, u, s)
  mul(q, bq, q)
end

-- Compose matrix with numbers on the left.
local function compose_l(bq, br, bs, bt)
  mul(r, bt, r)
  mul(q, br, u)
  add(r, u, r)
  mul(t, bs, u)
  mul(t, bt, t)
  mul(s, br, v)
  add(t, v, t)
  mul(s, bq, s)
  add(s, u, s)
  mul(q, bq, q)
end

-- Extract one digit.
local function extract(j)
  mul(q, j, u)
  add(u, r, u)
  mul(s, j, v)
  add(v, t, v)
  return get(div(u, v, w))
end

-- Generate successive digits of PI.
local function pidigits(N)
  local write = io.write
  local k = 1
  q, r, s, t = init(1), init(0), init(0), init(1)
  u, v, w = init(0), init(0), init(0)
  local i = 0
  while i < N do
    local y = extract(3)
    if y == extract(4) then
      write(y)
      i = i + 1; if i % 10 == 0 then write("\t:", i, "\n") end
      compose_r(10, -10*y, 0, 1)
    else
      compose_l(k, 4*k+2, 0, 2*k+1)
      k = k + 1
    end
  end
  if i % 10 ~= 0 then write(string.rep(" ", 10 - N % 10), "\t:", N, "\n") end
end

local N = tonumber(arg and arg[1]) or 27
pidigits(N)
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

-- Start of dynamically compiled chunk.
local chunk = [=[

-- Factory function for multi-precision number (mpn) operations.
local function fmm(fa, fb)
  return loadstring([[
    return function(y, a, ka, b, kb)
      local carry, n = 0, #a ]]..(fb == 0 and "" or [[
      local na, nb = n, #b -- Need to adjust lengths. 1 element suffices here.
      if na > nb then b[na] = 0 elseif na < nb then a[nb] = 0; n = nb end
    ]])..[[
      for i=1,n do -- Sum up all elements and propagate carry.
        local x = a[i] ]]..(fa == 2 and "*ka" or "")..
          (fb == 2 and "+b[i]*kb" or (fb == 1 and "+b[i]" or ""))..[[ + carry
        if x < RADIX and x >= 0 then carry = 0; y[i] = x -- Check for overflow.
        else local d = x % RADIX; carry = (x-d) / RADIX; y[i] = d end
      end
      y[n+1] = nil -- Truncate target. 1 element suffices here.
      if carry == 0 then while n > 0 and y[n] == 0 do y[n] = nil end
      elseif carry == -1 then y[n] = y[n] - RADIX else y[n+1] = carry end
    ]]..(fb == 0 and "" or [[ -- Undo length adjustment.
      if na > nb then b[na] = nil elseif na < nb and y ~= a then a[nb] = nil end
    ]])..[[
      return y
    end]])()
end

-- Generate needed mpn functions.
local mm_kk, mm_k1, mm_k0, mm_11 = fmm(2, 2), fmm(2, 1), fmm(2, 0), fmm(1, 1)

-- Choose the most efficient mpn function for y = a*ka + b*kb at run-time.
local function mm(y, a, ka, b, kb)
  local f = mm_kk
  if kb == 0 or #b == 0 then if ka == 1 then return a else f = mm_k0 end
  elseif kb == 1 then if ka == 1 then f = mm_11 else f = mm_k1 end end
  return f(y, a, ka, b, kb)
end

-- Compose matrix with numbers on the right.
local function compose_r(aq,ar,as,at, bq,br,bs,bt)
  mm(ar, ar,bq, at,br) mm(at, at,bt, ar,bs)
  mm(as, as,bt, aq,bs) mm(aq, aq,bq, nil,0)
end

-- Compose matrix with numbers on the left.
local function compose_l(aq,ar,as,at, bq,br,bs,bt)
  mm(ar, ar,bt, aq,br) mm(at, at,bt, as,br)
  mm(as, as,bq, at,bs) mm(aq, aq,bq, nil,0)
end

-- Extract one digit.
local u, v, jj = {}, {}, 0
local function extract(q,r,s,t, j)
  local u = j == jj + 1 and mm(u, u,1, q,1) or mm(u, q,j, r,1); jj = j
  local v = mm(v, t,1, s,j)
  local nu, nv, y = #u, #v
  if nu == nv then
    if nu == 1 then y = u[1] / v[1]
    else y = (u[nu]*RADIX + u[nu-1]) / (v[nv]*RADIX + v[nv-1]) end
  elseif nu == nv+1 then y = (u[nu]*RADIX + u[nv]) / v[nv]
  else return 0 end
  return math.floor(y)
end

-- Coroutine which yields successive digits of PI.
return coroutine.wrap(function()
  local q, r, s, t, k = {1}, {}, {}, {1}, 1
  repeat
    local y = extract(q,r,s,t, 3)
    if y == extract(q,r,s,t, 4) then
      coroutine.yield(y)
      compose_r(q,r,s,t,  10, -10*y, 0, 1)
    else
      compose_l(q,r,s,t,   k, 4*k+2, 0, 2*k+1)
      k = k + 1
    end
  until false
end)

]=] -- End of dynamically compiled chunk.

local N = tonumber(arg and arg[1]) or 27
local RADIX = N < 6500 and 2^36 or 2^32 -- Avoid overflow.

-- Substitute radix and compile chunk.
local pidigit = loadstring(string.gsub(chunk, "RADIX", tostring(RADIX)))()

-- Print lines with 10 digits.
for i=10,N,10 do
  for j=1,10 do io.write(pidigit()) end
  io.write("\t:", i, "\n")
end

-- Print remaining digits (if any).
local n10 = N % 10
if n10 ~= 0 then
  for i=1,n10 do io.write(pidigit()) end
  io.write(string.rep(" ", 10-n10), "\t:", N, "\n")
end

-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- requires LGMP "A GMP package for Lua 5.1"

local g = {}; require"c-gmp"(g, {})
local add, mul, submul = g.mpz_add, g.mpz_mul_si, g.mpz_submul_ui
local mul2x, div, cmp = g.mpz_mul_2exp, g.mpz_fdiv_qr, g.mpz_cmp
local init, get, write = g.mpz_init_set_d, g.mpz_get_d, io.write

local N = tonumber(arg and arg[1]) or 100
local i, n, a, d, t, u = 0, init(1), init(0), init(1), init(0), init(0)
for k=1,1000000 do
  mul2x(n, 1, t) mul(n, k, n) add(a, t, a) mul(a, k+k+1, a) mul(d, k+k+1, d)
  if cmp(a, n) >= 0 then
    mul2x(n, 1, t) add(t, n, t) add(t, a, t) div(t, d, t, u) add(u, n, u)
    if cmp(d, u) > 0 then
      local y = get(t)
      write(y); i = i + 1; if i % 10 == 0 then write("\t:", i, "\n") end
      if i >= N then break end
      submul(a, d, y) mul(a, 10, a) mul(n, 10, n)
    end
  end
end
if i % 10 ~= 0 then write(string.rep(" ", 10 - N % 10), "\t:", N, "\n") end

-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- Contributed by Wim Couwenberg


-- This is a pure lua implementation of the spigot algorithm for calculating
-- the digits of pi.  It combines the production step and the calculation of
-- the image of the interval [3, 4] into a single computation.  This is based
-- on the fact that for any integer n >= 1 the following equation between
-- matrix products holds:
--
--              [ n  4*n + 2][4  3]   [4  3][2*n - 1  n - 1]
--              [ 0  2*n + 1][1  1] = [1  1][   2     n + 2]
--
-- 1 September 2008


-- the requested number of digits
local N = tonumber(...)

-- Large numbers are expanded in base 2^exp.  Assumption: arithmetic in the Lua
-- interpreter is based on IEEE doubles and we don't need more than 4*N
-- productions to obtain the first N digits of pi.
local exp = 50 - math.ceil(math.log(N)/math.log(2))
local base = 2^exp

-- hardwiring the base in the large number computations (instead of using it as
-- an upvalue) saves quite some time!  Therefore the core algorithms are
-- compiled dynamically for the base that is computed above.  (Idea courtesy of
-- Mike Pall.)
local algo = [[
local function produce(n1, n2, d, n)
    local c1, c2, c3 = 0, 0, 0
    local f = 2*n + 1
    local m11, m12 = 2*n - 1, n - 1
    local      m22 =          n + 2
    for i = 1, #n1 do
        local n1i, n2i = n1[i], n2[i]
        local x = m11*n1i + 2*n2i + c1
        if x < base then
            n1[i], c1 = x, 0
        else
            c1 = x%base
            n1[i], c1 = c1, (x - c1)/base
        end
        x = m12*n1i + m22*n2i + c2
        if x < base then
            n2[i], c2 = x, 0
        else
            c2 = x%base
            n2[i], c2 = c2, (x - c2)/base
        end
        x = f*d[i] + c3
        if x < base then
            d[i], c3 = x, 0
        else
            c3 = x%base
            d[i], c3 = c3, (x - c3)/base
        end
    end
    if c1 ~= 0 or c3 ~= 0 then
        local nn1 = #n1 + 1
        n1[nn1], n2[nn1], d[nn1] = c1, c2, c3
    end
end

local function extract(n1, n2, d, n)
    local c1, c2 = 0, 0
    local f = -10*n
    for i = 1, #n1 do
        local fdi = f*d[i]
        local x = 10*n1[i] + fdi + c1
        if x < base and x >= 0 then
            n1[i], c1 = x, 0
        else
            c1 = x%base
            n1[i], c1 = c1, (x - c1)/base
        end
        x = 10*n2[i] + fdi + c2
        if x < base and x >= 0 then
            n2[i], c2 = x, 0
        else
            c2 = x%base
            n2[i], c2 = c2, (x - c2)/base
        end
    end
    if c1 ~= 0 then
        local nn = #n1 + 1
        n1[nn], n2[nn], d[nn] = c1, c2, 0
    end
end

return produce, extract
]]

local produce, extract = loadstring(string.gsub(algo, "base", tostring(base)))()

local function digit(n1, n2, d)
    local nn = #n1
    local dnn = d[nn]
    if dnn ~= 0 then
        local n1nn, n2nn = n1[nn], n2[nn]
        local r1, r2 = n1nn%dnn, n2nn%dnn
        local p1, p2 = (n1nn - r1)/dnn, (n2nn - r2)/dnn
        if p1 == p2 and p1 <= r1 and p2 <= r2 then return p1 end
    end
end

-- first approximants are 4/1 and 3/1
-- these are expressed in large numbers n1/d, n2/d
local n1 = {4}
local n2 = {3}
local d  = {1}

-- next production step index 
local n = 1

-- here goes...
local write = io.write
local digits = 0
while digits < N do
    local g = digit(n1, n2, d)
    if g then
        write(g)
        extract(n1, n2, d, g)
        digits = digits + 1
        if digits%10 == 0 then write("\t:", digits, "\n") end
    else
        produce(n1, n2, d, n)
        n = n + 1
    end
end

if N%10 ~= 0 then
    write(string.rep(" ", 10 - N%10), "\t:", N, "\n")
end
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- requires LGMP "A GMP package for Lua 5.1"
-- with matrix optimization, courtesy of Wim Couwenberg

local g, aux = {}, {}
require"c-gmp"(g, aux)
local add, mul, div = g.mpz_add, g.mpz_mul_si, g.mpz_tdiv_q
local init, get = g.mpz_init_set_d, g.mpz_get_d

local u, v, w

local function produce(n1, n2, d, k)
  mul(n1, 2*k-1, u)
  add(n2, n2, v)
  mul(n1, k-1, w)
  add(u, v, n1)
  mul(n2, k+2, u)
  add(w, u, n2)
  mul(d, 2*k+1, d)
end

local function extract(n1, n2, d, y)
  mul(d, -10*y, u)
  mul(n1, 10, n1)
  add(n1, u, n1)
  mul(n2, 10, n2)
  add(n2, u, n2)
end

local function digit(n1, n2, d)
  local y = get(div(n1, d, u))
  if y == get(div(n2, d, v)) then return y end
end

-- Generate successive digits of PI.
local function pidigits(N)
  local write = io.write
  local k = 1
  local n1, n2, d = init(4), init(3), init(1)
  u, v, w = init(0), init(0), init(0)
  local i = 0
  while i < N do
    local y = digit(n1, n2, d)
    if y then
      write(y)
      i = i + 1; if i % 10 == 0 then write("\t:", i, "\n") end
      extract(n1, n2, d, y)
    else
      produce(n1, n2, d, k)
      k = k + 1
    end
  end
  if i % 10 ~= 0 then write(string.rep(" ", 10 - N % 10), "\t:", N, "\n") end
end

local N = tonumber(arg and arg[1]) or 27
pidigits(N)

-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Wim Couwenberg
--
-- requires LGMP "A GMP package for Lua 5.1"
--
-- 21 September 2008

local gmp, aux = {}, {}
require "c-gmp" (gmp, aux)
local add, mul, div = gmp.mpz_add, gmp.mpz_mul_ui, gmp.mpz_fdiv_q
local addmul, submul = gmp.mpz_addmul_ui, gmp.mpz_submul_ui
local init, get, set = gmp.mpz_init_set_d, gmp.mpz_get_d, gmp.mpz_set

--
-- Production:
--
-- [m11 m12]     [m11 m12][k  4*k+2]
-- [ 0  m22] <-- [ 0  m22][0  2*k+1]
--
local function produce(m11, m12, m22, k)
  local p = 2*k + 1
  mul(m12, p, m12)
  addmul(m12, m11, 2*p)
  mul(m11, k, m11)
  mul(m22, p, m22)
end

--
-- Extraction:
--
-- [m11 m12]     [10 -10*d][m11 m12]
-- [ 0  m22] <-- [ 0   1  ][ 0  m22]
--
local function extract(m11, m12, m22, d)
  submul(m12, m22, d)
  mul(m11, 10, m11)
  mul(m12, 10, m12)
end

--
-- Get integral part of p/q where
--
-- [p]   [m11 m12][d]
-- [q] = [ 0  m22][1]
--
local function digit(m11, m12, m22, d, tmp)
  set(tmp, m12)
  addmul(tmp, m11, d)
  div(tmp, m22, tmp)
  return get(tmp)
end

-- Generate successive digits of PI.
local function pidigits(N)
  local write = io.write
  local m11, m12, m22, tmp = init(1), init(0), init(1), init(0)
  local k, i = 1, 0
  while i < N do
    local d = digit(m11, m12, m22, 3, tmp)
    if d == digit(m11, m12, m22, 4, tmp) then
      write(d)
      extract(m11, m12, m22, d)
      i = i + 1; if i % 10 == 0 then write("\t:", i, "\n") end
    else
      produce(m11, m12, m22, k)
      k = k + 1
    end
  end
  if i % 10 ~= 0 then write(string.rep(" ", 10 - N % 10), "\t:", N, "\n") end
end

local N = tonumber(arg and arg[1]) or 27
pidigits(N)

-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Wim Couwenberg
--
-- requires LGMP "A GMP package for Lua 5.1"
--
-- This is still the step-by-step unbounded spigot algorithm but a number of
-- optimizations make it an interesting alternative implementation.  :-)
--
-- 21 September 2008

local gmp, aux = {}, {}
require "c-gmp" (gmp, aux)
local add, mul, divmod = gmp.mpz_add, gmp.mpz_mul_ui, gmp.mpz_fdiv_qr
local addmul, submul = gmp.mpz_addmul_ui, gmp.mpz_submul_ui
local init, get, set = gmp.mpz_init_set_d, gmp.mpz_get_d, gmp.mpz_set
local cmp, divx, gcd = gmp.mpz_cmp, gmp.mpz_divexact, gmp.mpz_gcd

-- used to store various intermediate results
local t1, t2

--
-- Production:
--
-- [m11 m12]     [m11 m12][k  4*k+2]
-- [ 0  m22] <-- [ 0  m22][0  2*k+1]
--
local function produce(m11, m12, m22, k)
  local p = 2*k + 1
  mul(m12, p, m12)
  addmul(m12, m11, 2*p)
  mul(m11, k, m11)
  mul(m22, p, m22)
end

--
-- Extraction:
--
-- [m11 m12]     [10 -10*d][m11 m12]
-- [ 0  m22] <-- [ 0   1  ][ 0  m22]
--
local function extract(m11, m12, m22, d)
  submul(m12, m22, d)
  mul(m11, 10, m11)
  mul(m12, 10, m12)
end

--
-- Test for a new digit.  Note that
--
-- [m11 m12][4]   [m11 m12][3]   [m11]
-- [ 0  m22][1] = [ 0  m22][1] + [ 0 ]
--
-- so we can extract an extra digit exactly if (3*m11 + m12)%m22 + m11 is
-- smaller than m22.  (Here % denotes the remainder.)  Then m11 is itself
-- necessarily smaller than m22.
--
local function digit(m11, m12, m22)
  if cmp(m11, m22) < 0 then
    set(t1, m12)
    addmul(t1, m11, 3)
    divmod(t1, m22, t1, t2)
    add(t2, m11, t2)
    if cmp(t2, m22) < 0 then return get(t1) end
  end
end

--
-- Divide out common factors in the matrix coefficients m11, m12, m22.
--
local function reduce(m11, m12, m22)
  gcd(m11, m12, t1)
  gcd(t1, m22, t1)
  divx(m11, t1, m11)
  divx(m12, t1, m12)
  divx(m22, t1, m22)
end

-- Generate successive digits of PI.
local function pidigits(N)
  local write = io.write
  local floor = math.floor
  local k = 1
  local m11, m12, m22 = init(1), init(0), init(1)
  t1, t2 = init(0), init(0)
  local i = 0
  local r = 256
  while i < N do
    local d = digit(m11, m12, m22)
    if d then
      write(d)
      extract(m11, m12, m22, d)
      i = i + 1
      if i == r then reduce(m11, m12, m22); r = floor(1.0625*r) end
      if i % 10 == 0 then write("\t:", i, "\n") end
    else
      produce(m11, m12, m22, k)
      k = k + 1
    end
  end
  if i % 10 ~= 0 then write(string.rep(" ", 10 - N % 10), "\t:", N, "\n") end
end

local N = tonumber(arg and arg[1]) or 27
pidigits(N)
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- contributed by Isaac Gouy


local function link(n)
   local message, next = 0

   if n-1 > 0 then
      next = coroutine.create(link) 
      _,message = coroutine.resume(next,n-1)
   end   
   coroutine.yield(message + 1)   
end


local n = tonumber(arg[1]) or 1000
local message = 0
local chain = coroutine.create(link)

_,message = coroutine.resume(chain,n)
io.write(message, "\n")
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org
-- contributed by Isaac Gouy (Lua novice)

n = tonumber(arg[1]) or 1

produced = 0
consumed = 0
buffer = 0

function producer()
   return coroutine.create( 

      function()
         while produced < n do
            produced = produced + 1
            buffer = produced
               -- io.write(buffer)
            coroutine.yield()
         end
      end

   )
end
   

function consumer(p)
   return coroutine.create( 

      function()
         local value = 0
         while consumed < n do
            coroutine.resume(p)
            value = buffer
               -- io.write(" -> ", value, "\n")
            consumed = consumed + 1
         end
      end

   )
end


coroutine.resume( consumer( producer() ))
io.write(produced, " ", consumed, "\n")
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Roberto Ierusalimschy, tuned by Mike Pall

local IM = 139968
local IA = 3877
local IC = 29573
local LAST = 42

local function gen_random(max)
  local y = (LAST * IA + IC) % IM
  LAST = y
  return (max * y) / IM
end

local N = tonumber(arg and arg[1]) or 1
for i=2,N do gen_random(100) end
io.write(string.format("%.9f\n", gen_random(100)))
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local function ack(m, n)
  if m == 0 then return n+1 end
  if n == 0 then return ack(m-1, 1) end
  return ack(m-1, (ack(m, n-1))) -- The parentheses are deliberate.
end

local function fib(n)
  if n < 2 then return 1 end
  return fib(n-2) + fib(n-1)
end

local function tak(x, y, z)
  if y >= x then return z end
  return tak(tak(x-1, y, z), tak(y-1, z, x), (tak(z-1, x, y)))
end

local write, format = io.write, string.format
local n = tonumber(arg[1]) - 1
write(format("Ack(3,%d): %d\n", n+1, ack(3, n+1)))
write(format("Fib(%.1f): %.1f\n", n+28.0, fib(n+28.0)))
write(format("Tak(%d,%d,%d): %d\n", 3*n, 2*n, n, tak(3*n, 2*n, n)))
write(format("Fib(3): %d\n", fib(3)))
write(format("Tak(3.0,2.0,1.0): %.1f\n", tak(3.0, 2.0, 1.0)))

-- $Id: regexmatch.lua,v 1.2 2004-06-12 16:19:44 bfulgham Exp $
-- http://shootout.alioth.debian.org
-- contributed by Roberto Ierusalimschy

local text = io.read("*a")

-- make sure text does not start with a number
text = "\n" .. text

-- pattern is: not a digit, optional (, 3 digits, optional ),
-- space, 3 digits, space or hyphen, 4 digits, not a digit
local pattern = "%D(%(?)(%d%d%d)(%)?) (%d%d%d)[- ](%d%d%d%d)%f[%D]"

local N = tonumber((arg and arg[1])) or 1
local count = 0
for i=N,1,-1 do
  for open,area,close,exch,digits in string.gfind(text, pattern) do
      if (open == '(') == (close == ')') then
        local tel = "("..area..") "..exch.."-"..digits
        if i == 1 then
          count = count+1
          io.write(count, ": ", tel, "\n")
        end
      end
    end
end


-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall (with ideas from Rici Lake)

local sub = string.sub
iubc = setmetatable({
  A="T", C="G", B="V", D="H", K="M", R="Y",
  a="T", c="G", b="V", d="H", k="M", r="Y",
  T="A", G="C", V="B", H="D", M="K", Y="R", U="A",
  t="A", g="C", v="B", h="D", m="K", y="R", u="A",
  N="N", S="S", W="W", n="N", s="S", w="W",
}, { __index = function(t, s)
  local r = t[sub(s, 2)]..t[sub(s, 1, 1)]; t[s] = r; return r end })

local wcode = [=[
return function(t, n)
  if n == 1 then return end
  local iubc, sub, write = iubc, string.sub, io.write
  local s = table.concat(t, "", 1, n-1)
  for i=#s-59,1,-60 do
    write(]=]
for i=59,3,-4 do wcode = wcode.."iubc[sub(s, i+"..(i-3)..", i+"..i..")], " end
wcode = wcode..[=["\n")
  end
  local r = #s % 60
  if r ~= 0 then
    for i=r,1,-4 do write(iubc[sub(s, i-3 < 1 and 1 or i-3, i)]) end
    write("\n")
  end
end
]=]
local writerev = loadstring(wcode)()

local t, n = {}, 1
for line in io.lines() do
  local c = sub(line, 1, 1)
  if c == ">" then writerev(t, n); io.write(line, "\n"); n = 1
  elseif c ~= ";" then t[n] = line; n = n + 1 end
end
writerev(t, n)
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall (with ideas from Rici Lake)
-- modified by Sokolov Yura

local len, sub, concat = string.len, string.sub, table.concat
local write, mod = io.write, math.mod

local function writerev(p, np, f, lo, hi)
  if lo <= hi then np = np + 1; p[np] = concat(f, "", lo, hi) end
  if np ~= 0 then
    local r = ""
    for i=np,1,-1 do
      local s = r..p[i]
      local sn = len(s)
      for i=1,sn-59,60 do write(sub(s, i, i+59), "\n") end
      r = sub(s, sn-mod(sn, 60)+1)
    end
    if r ~= "" then write(r, "\n") end
  end
end

local iubc = setmetatable({
  A="T", C="G", B="V", D="H", K="M", R="Y",
  a="T", c="G", b="V", d="H", k="M", r="Y",
  T="A", G="C", V="B", H="D", M="K", Y="R", U="A",
  t="A", g="C", v="B", h="D", m="K", y="R", u="A",
  N="N", S="S", W="W", n="N", s="S", w="W",
}, { __index = function(t, s)
  local r = t[sub(s, 2)]..t[sub(s, 1, 1)]; t[s] = r; return r end })

local p, f, np, nf = {}, {}, 0, 1631
for line in io.lines() do
  local c = sub(line, 1, 1)
  if c == ">" then
    writerev(p, np, f, nf, 1630); np = 0; nf = 1631
    write(line, "\n")
  elseif c ~= ";" then
    for i=1,len(line),4 do nf = nf - 1; f[nf] = iubc[sub(line, i, i+3)] end
    if nf <= 30 then
      np = np + 1; p[np] = concat(f, "", nf, 1630); nf = 1631
    end
  end
end
writerev(p, np, f, nf, 1630)
#!/usr/bin/lua
-- $Id: reversefile.lua,v 1.2 2004-06-12 16:19:44 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/

local lines = {}
local nl = 0

for l in io.lines() do
    nl = nl + 1
    lines[nl] = l
end

for i=nl,1,-1 do
    io.write(lines[i], "\n")
end
-- $Id: sieve.lua,v 1.1 2004-05-19 18:12:27 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
--
-- Roberto Ierusalimschy pointed out the for loop is much
-- faster for our purposes here than using a while loop.

function main(num)
    local flags = {}
    for num=num,1,-1 do
	count = 0
	for i=2,8192 do
	    flags[i] = 1
	end
	for i=2,8192 do
	    if flags[i] == 1 then
	        for k=i+i, 8192, i do
		    flags[k] = 0
		end
	        count = count + 1	
	    end
	end
    end
end

NUM = tonumber((arg and arg[1])) or 1
count = 0
main(NUM)
io.write("Count: ", count, "\n")
-- $Id: sieve.lua-2.lua,v 1.1 2004-11-10 06:48:59 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
--
-- Roberto Ierusalimschy pointed out the for loop is much
-- faster for our purposes here than using a while loop.

function main(num)
    for num=num,1,-1 do
        local flags = {}
        count = 0
        for i=2,8192 do
            if not flags[i] then
                for k=i+i, 8192, i do
                    flags[k] = 1
                end
                count = count + 1
            end
        end
    end
end

NUM = tonumber((arg and arg[1])) or 1
count = 0
main(NUM)
io.write("Count: ", count, "\n")

-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall

local function A(i, j)
  local ij = i+j-1
  return 1.0 / (ij * (ij-1) * 0.5 + i)
end

local function Av(x, y, N)
  for i=1,N do
    local a = 0
    for j=1,N do a = a + x[j] * A(i, j) end
    y[i] = a
  end
end

local function Atv(x, y, N)
  for i=1,N do
    local a = 0
    for j=1,N do a = a + x[j] * A(j, i) end
    y[i] = a
  end
end

local function AtAv(x, y, t, N)
  Av(x, t, N)
  Atv(t, y, N)
end

local N = tonumber(arg and arg[1]) or 100
local u, v, t = {}, {}, {}
for i=1,N do u[i] = 1 end

for i=1,10 do AtAv(u, v, t, N) AtAv(v, u, t, N) end

local vBv, vv = 0, 0
for i=1,N do
  local ui, vi = u[i], v[i]
  vBv = vBv + ui*vi
  vv = vv + vi*vi
end
io.write(string.format("%0.9f\n", math.sqrt(vBv / vv)))
-- $Id: spellcheck.lua-2.lua,v 1.1 2004-11-10 06:47:52 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- implemented by: Roberto Ierusalimschy

assert(readfrom("Usr.Dict.Words"))
local dict = {}
-- reads the whole file at once, and then break it into words
gsub(read("*a"), "(%w+)", function (w)
  %dict[w] = 1
end)
readfrom()    -- closes dictionary

-- reads the whole file at once, and then break it into words
gsub(read("*a"), "(%w+)", function (w)
  if not %dict[w] then print(w) end
end)
-- $Id: spellcheck.lua,v 1.2 2004-05-25 02:26:33 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- based on code from Roberto Ierusalimschy

assert(io.open("Usr.Dict.Words"))
local dict = {}
for line in io.lines("Usr.Dict.Words") do
  dict[line] = true
end

for word in io.lines() do
  if not dict[word] then print(word) end
end

-- $Id: strcat.lua,v 1.3 2004-07-08 03:47:07 bfulgham Exp $
-- http://shootout.alioth.debian.org
-- contributed by Roberto Ierusalimschy

local n = tonumber((arg and arg[1]) or 1)
local buff = {}
for i=1,n do
  table.insert(buff, "hello\n")
end
local s = table.concat(buff)
print(string.len(s))

-- $Id: strcat.lua-2.lua,v 1.1 2004-11-10 06:44:59 bfulgham Exp $
-- http://shootout.alioth.debian.org

-- this version uses the native string concatenation operator
-- Modified for Lua 5 by Brent Fulgham

local n = tonumber((arg and arg[1]) or 1)
local str = ""
for i=1,n do
    str = str.."hello\n"
end
print(string.len(str))
-- $Id: sumcol.lua,v 1.3 2004-07-08 03:47:07 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/

local sum = 0
for line in io.lines() do
    sum = sum + line
end
print(sum)
--- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- implemented by: Isaac Gouy
-- Modified by Mike Pall

local function Tak(x,y,z)
	if y>=x then return z end
	return Tak( Tak(x-1.0,y,z), Tak(y-1.0,z,x), Tak(z-1.0,x,y) )
end
	
n = tonumber(arg and arg[1]) or 1
io.write( string.format("%.1f\n", Tak(n*3.0,n*2.0,n*1.0)) )

local socket = require "socket"
local posix = require "posix"

local M, reply_size, request_size = 100, 4096, 64
local N = tonumber(arg and arg[1]) or 1

local function client(sk)
  local repsize = reply_size
  local request = string.rep(" ", request_size)
  local replies = 0
  for i=1,N*M do
    sk:send(request)
    sk:receive(repsize)
    replies = replies + 1
  end
  io.write("replies: ", replies, "\tbytes: ", sk:getstats(), "\n")
  io.flush()
  sk:close()
end

local function server(sk)
  local reqsize = request_size
  local reply = string.rep(" ", reply_size)
  for i=1,N*M do
    sk:receive(reqsize)
    sk:send(reply)
  end
  sk:close()
end

local ls = socket.bind("127.0.0.1", 0)
if posix.fork() == 0 then		-- Child is client
  client(socket.connect(ls:getsockname()))
  os.exit()
else					-- Parent is server
  server(ls:accept())
  ls:close()
  posix.wait()
end
local socket = require "socket" -- LuaSocket-2.0-beta2 or newer required

local M, reply_size, request_size = 100, 4096, 64
local N = tonumber(arg and arg[1]) or 1

local function client(ls, coserver)
  local repsize = reply_size
  local request = string.rep(" ", request_size)
  local resume, len = coroutine.resume, string.len
  resume(coserver, ls)
  local sk = socket.connect(ls:getsockname())
  sk:settimeout(0)
  local replies = 0
  for i=1,N*M do
    local ok, err, sent = sk:send(request)
    while not ok do
      resume(coserver)
      ok, err, sent = sk:send(request, 1+sent)
    end
    resume(coserver)
    local msg, err, part = sk:receive(repsize)
    while not msg do
      resume(coserver)
      msg, err, part = sk:receive(repsize-len(part), part)
    end
    replies = replies + 1
  end
  io.write("replies: ", replies, "\tbytes: ", sk:getstats(), "\n")
  sk:close()
end

local function server(ls)
  local reqsize = request_size
  local reply = string.rep(" ", reply_size)
  local yield, len = coroutine.yield, string.len
  ls:settimeout(0)
  local sk = ls:accept()
  while not sk do
    yield()
    sk = ls:accept()
  end
  sk:settimeout(0)
  for i=1,N*M do
    yield()
    local msg, err, part = sk:receive(reqsize)
    while not msg do
      yield()
      msg, err, part = sk:receive(reqsize-len(part), part)
    end
    local ok, err, sent = sk:send(reply)
    while not ok do
      yield()
      ok, err, sent = sk:send(reply, 1+sent)
    end
  end
  sk:close()
end

client(socket.bind("127.0.0.1", 0), coroutine.create(server))
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Sam Roberts
-- reviewed by Bruno Massa

require"coroutine"

-- first and only argument is number of token passes
local n         = assert(tonumber(arg[1]))

-- fixed size pool
local poolsize  = 503
local threads   = {}

-- cache these to avoid global environment lookups
local create    = coroutine.create
local resume    = coroutine.resume
local yield     = coroutine.yield

local id        = 1
local token     = 0
local ok

local body = function(token)
  while true do
    token = yield(token + 1)
  end
end

-- create all threads
for id = 1, poolsize do
  threads[id] = create(body)
end

-- send the token
repeat
  if id == poolsize then
    id = 1
  else
    id = id + 1
  end
  ok, token = resume(threads[id], token)
until token == n

print(id)
--- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Contributed by Roberto Ierusalimschy
-- Modified by Mike Pall

BUFSIZE = 2^12
local read, len, gsub = io.read, string.len, string.gsub

local cc,lc,wc = 0,0,0
while true do
    local lines, rest = read(BUFSIZE, "*l")
    if lines == nil then break end
    if rest then lines = lines..rest..'\n' end
    cc = cc+len(lines)
    local _,t = gsub(lines, "%S+", "")   -- count words in the line
    wc = wc+t
    _,t = gsub(lines, "\n", "\n")   -- count newlines in the line
    lc = lc+t
end

io.write(lc, " ", wc, " ", cc, "\n")
-- $Id: wordfreq.lua,v 1.1 2004-05-19 18:14:18 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- implemented by: Roberto Ierusalimschy

-- this version reads input line by line and so it is
-- noticably slower than the version that reads the
-- input in blocks.

local words = {}   -- list of all words (for sorting)
local count = {}   -- count occurrences of each word

while 1 do
  local line = read()
  if line == nil then break end
  gsub(strlower(line), "(%l+)", function (w)
    local cw = %count[w]
    if cw == nil then     -- first occurrence?
      cw = 0
      tinsert(%words, w)
    end
    %count[w] = cw + 1
  end)
end

sort(words, function (a,b)
  return  %count[a] > %count[b]  or
         (%count[a] == %count[b] and a > b)
end)

for i=1,getn(words) do
  local w = words[i]
  io.write(format("%7d\t%s\n", count[w], w))
end
-- $Id: wordfreq.lua-2.lua,v 1.1 2004-11-10 06:40:32 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- implemented by: Roberto Ierusalimschy

-- this version reads the files in all at once, versus
-- line by line or in blocks.

local words = {}   -- list of all words (for sorting)
local count = {}   -- count occurrences of each word

-- read the whole input at once, and break it in words
gsub(strlower(read("*a")), "(%l+)", function (w)
  local cw = %count[w]
  if cw == nil then     -- first occurrence?
    cw = 0
    tinsert(%words, w)
  end
  %count[w] = cw + 1
end)

sort(words, function (a,b)
  return  %count[a] > %count[b]  or
         (%count[a] == %count[b] and a > b)
end)

for i=1,getn(words) do
  local w = words[i]
  io.write(format("%7d\t%s\n", count[w], w))
end
-- $Id: wordfreq.lua,v 1.3 2004-07-03 05:36:11 bfulgham Exp $
-- http://shootout.alioth.debian.org
-- contributed by Roberto Ierusalimschy

-- this version reads 4K chunks of input at a time

local words = {}   -- list of all words (for sorting)
local count = {}   -- count occurrences of each word

local BUFSIZE = 2^12

while true do
  local lines, rest = io.read(BUFSIZE, "*l")
  if lines == nil then break end
  lines = lines..(rest or '')    -- ensures whole lines
  for w in string.gfind(string.lower(lines), "(%l+)") do
    local cw = count[w]
    if not cw then     -- first occurrence?
      cw = 0
      table.insert(words, w)
    end
    count[w] = cw + 1
  end
end

table.sort(words, function (a,b)
  return  count[a] > count[b]  or (count[a] == count[b] and a > b)
end)

for i=1,table.getn(words) do
  local w = words[i]
  io.write(string.format("%7d %s\n", count[w], w))
end

--- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- contributed by Mike Pall


local BUFSIZE = 2^12

local lower, gfind, format = string.lower, string.gfind, string.format
local read, write = io.read, io.write

local nwords = 0
local words = {}
local count = setmetatable({}, { __index = function(t, w)
  local n = nwords + 1
  nwords = n
  words[n] = w
  return 0
end })

while true do
  local lines, rest = read(BUFSIZE, "*l")
  if lines == nil then break end
  if rest then lines = lines..rest end
  for w in gfind(lower(lines), "%l+") do count[w] = count[w] + 1 end
end

table.sort(words, function (a, b)
  local ca, cb = count[a], count[b]
  return ca == cb and a > b or ca > cb -- we know a ~= b
end)

for i=1,nwords do
  local w = words[i]
  write(format("%7d %s\n", count[w], w))
end
