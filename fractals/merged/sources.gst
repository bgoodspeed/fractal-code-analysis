"*  The Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im ackermann.st -a 8
*"

!Integer methodsFor: 'shootout'!

ackermann: anInteger
   ^self = 0
      ifTrue: [anInteger + 1]
      ifFalse: [
         anInteger = 0
            ifTrue: [self - 1 ackermann:  1]
            ifFalse: [self - 1 ackermann: (self ackermann: anInteger - 1)] ] ! !

| n |
n := Smalltalk arguments first asInteger.

Transcript show: 'Ack(3,'; show: n printString; show: '): '; 
           show: (3 ackermann: n) printString; nl!
"  The Great Computer Language Shootout
   contributed by Paolo Bonzini
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im ary3.st -a 7000
"

| n x y |
n := Smalltalk arguments first asInteger.

x := (1 to: n) asArray.
y := Array new: n withAll: 0.

1000 timesRepeat: [ 
   n to: 1 by: -1 do: [:i| y at: i put: (y at: i) + (x at: i)] ].
   
(y at: 1) display. ' ' display. (y at: n) displayNl !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Object subclass: #TreeNode   instanceVariableNames: 'left right item'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!Tests class methodsFor: 'benchmarking'!binarytrees: n to: output   | minDepth maxDepth stretchDepth check longLivedTree iterations |   minDepth := 4.   maxDepth := minDepth + 2 max: n.   stretchDepth := maxDepth + 1.   check := (TreeNode bottomUpTree: 0 depth: stretchDepth) itemCheck.   output      nextPutAll: 'stretch tree of depth '; print: stretchDepth; tab;      nextPutAll: ' check: '; print: check; nl.   longLivedTree := TreeNode bottomUpTree: 0 depth: maxDepth.   minDepth to: maxDepth by: 2 do: [:depth|      iterations := 1 bitShift: maxDepth - depth + minDepth.      check := 0.      1 to: iterations do: [:i|         check := check + (TreeNode bottomUpTree: i depth: depth) itemCheck.         check := check + (TreeNode bottomUpTree: -1*i depth: depth) itemCheck         ].      output         print:  (2*iterations); tab;         nextPutAll: ' trees of depth '; print: depth; tab;         nextPutAll: ' check: '; print: check; nl      ].   output      nextPutAll: 'long lived tree of depth '; print: maxDepth; tab;      nextPutAll: ' check: '; print: longLivedTree itemCheck; nl! !!Tests class methodsFor: 'benchmark scripts'!binarytrees   self binarytrees: self arg to: self stdout.   ^''! !!TreeNode methodsFor: 'initialize-release'!left: leftChild right: rightChild item: anItem   left := leftChild.   right := rightChild.   item := anItem! !!TreeNode methodsFor: 'accessing'!itemCheck   ^left isNil       ifTrue: [item] ifFalse: [item + (left itemCheck - right itemCheck)]! !!TreeNode class methodsFor: 'instance creation'!bottomUpTree: anItem depth: anInteger   ^(anInteger > 0)       ifTrue: [         self             left: (self bottomUpTree: 2*anItem - 1 depth: anInteger - 1)             right: (self bottomUpTree: 2*anItem depth: anInteger - 1)              item: anItem         ]      ifFalse: [self left: nil right: nil item: anItem]! !!TreeNode class methodsFor: 'instance creation'!left: leftChild right: rightChild item: anItem         ^(super new) left: leftChild right: rightChild item: anItem! !

Tests binarytrees!
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini
    modified by Eliot Miranda *"!

Object subclass: #Chameleon
   instanceVariableNames: 'meetings color semaphore waitingForPair'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

Object subclass: #MeetingPlace
   instanceVariableNames: 'mutex first total max'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

!Chameleon methodsFor: 'initialize-release'!
initialize
    meetings := 0.
    waitingForPair := Semaphore new.
    semaphore := Semaphore new! !

!Chameleon methodsFor: 'changing colours'!
color: c
    color := c! !

!Chameleon methodsFor: 'changing colours'!
fade
    color := #faded.
    waitingForPair signal.
    semaphore signal! !

!Chameleon methodsFor: 'changing colours'!
met: other
    | newColor |
    meetings := meetings + 1.
    color == #red ifTrue: [
    newColor := other == #yellow ifTrue: [ #blue ] ifFalse: [ #yellow ] ].
    color == #yellow ifTrue: [
    newColor := other == #red ifTrue: [ #blue ] ifFalse: [ #red ] ].
    color == #blue ifTrue: [
    newColor := other == #red ifTrue: [ #yellow ] ifFalse: [ #red ] ].
    color := newColor.
    waitingForPair signal! !

!Chameleon methodsFor: 'running'!
fork: meetingPlace
    ^[ self run: meetingPlace ] fork! !

!Chameleon methodsFor: 'running'!
run: meetingPlace
    [ color == #faded ] whileFalse: [
        meetingPlace reachedBy: self.
        waitingForPair wait ]! !

!Chameleon methodsFor: 'accessing'!
color
    ^color! !

!Chameleon methodsFor: 'accessing'!
meetings
    ^meetings! !

!Chameleon methodsFor: 'accessing'!
wait
    semaphore wait! !

!Chameleon class methodsFor: 'instance creation'!
color: c
    ^self new
        initialize;
        color: c! !

!MeetingPlace methodsFor: 'running'!
max: maxMeetings
    max := maxMeetings! !

!MeetingPlace methodsFor: 'running'!
organizeMeetingWith: second
    total >= max
        ifTrue: [
            first fade.
            second fade ]
        ifFalse: [
            first met: second color.
            second met: first color ].
    total := total + 1! !

!MeetingPlace methodsFor: 'running'!
reachedBy: chameleon
    mutex critical: [
        first isNil
            ifTrue: [ first := chameleon ]
            ifFalse: [ self organizeMeetingWith: chameleon. first := nil ] ]! !

!MeetingPlace methodsFor: 'initialize-release'!
initialize
    mutex := Semaphore forMutualExclusion.
    total := 0! !

!MeetingPlace class methodsFor: 'instance creation'!
forMeetings: maxMeetings
    ^super new
        initialize;
        max: maxMeetings;
        yourself! !

!Tests class methodsFor: 'benchmarking'!
chameneos: n
    | c1 c2 c3 c4 mp |
    c1 := Chameleon color: #blue.
    c2 := Chameleon color: #red.
    c3 := Chameleon color: #yellow.
    c4 := Chameleon color: #blue.
    mp := MeetingPlace forMeetings: n.
    c1 fork: mp.
    c2 fork: mp.
    c3 fork: mp.
    c4 fork: mp.
    c1 wait.
    c2 wait.
    c3 wait.
    c4 wait.
    ^c1 meetings + c2 meetings + c3 meetings + c4 meetings! !

!Tests class methodsFor: 'benchmark scripts'!
chameneos
    self stdout print: (self chameneos: self arg); nl.
    ^''! !

Tests chameneos!
"  The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy

   To run: gst -QI /usr/share/gnu-smalltalk/gst.im dispatch.gst -a 1000 
"

Object subclass: #BottleState
instanceVariableNames: 'tag'
classVariableNames: 'Empty Full Sealed'
poolDictionaries: ''
category: nil !


!BottleState class methodsFor: 'class initialization'!

initialize
   Empty := EmptyState new tag: 1.  
   Full := FullState new tag: 2.
   Sealed := SealedState new tag: 3 ! !
   
!BottleState class methodsFor: 'accessing'!

initialState
   ^Empty ! !   
   
!BottleState methodsFor: 'accessing'!      

tag
   ^tag !
   
tag: anInteger
   "only exists for checksum"
   tag := anInteger ! !

!BottleState methodsFor: 'controlling'!      

next: aBottle
   self subclassResponsibility ! !


BottleState subclass: #EmptyState
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !

!EmptyState methodsFor: 'controlling'!      

next: aBottle
   aBottle state: Full ! !  



BottleState subclass: #FullState
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !

!FullState methodsFor: 'controlling'!      

next: aBottle
   aBottle state: Sealed ! !



BottleState subclass: #SealedState
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !

!SealedState methodsFor: 'controlling'!      

next: aBottle
   aBottle state: Empty ! !
   


BottleState subclass: #PressurizedBottleState
instanceVariableNames: ''
classVariableNames: 'UnpressurizedEmpty UnpressurizedFull PressurizedUnsealed PressurizedSealed'
poolDictionaries: ''
category: nil !

!PressurizedBottleState class methodsFor: 'class initialization'!

initialize
   UnpressurizedEmpty := UnpressurizedEmptyState new tag: 4.   
   UnpressurizedFull := UnpressurizedFullState new tag: 5.
   PressurizedUnsealed := PressurizedUnsealedState new tag: 6.   
   PressurizedSealed := PressurizedSealedState new tag: 7 ! !

!PressurizedBottleState class methodsFor: 'accessing'!   

initialState
   ^UnpressurizedEmpty ! !



PressurizedBottleState subclass: #UnpressurizedEmptyState
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !

!UnpressurizedEmptyState methodsFor: 'controlling'!      

next: aBottle
   aBottle state: UnpressurizedFull ! !



PressurizedBottleState subclass: #UnpressurizedFullState
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !

!UnpressurizedFullState methodsFor: 'controlling'!      

next: aBottle
   aBottle state: PressurizedUnsealed ! !



PressurizedBottleState subclass: #PressurizedUnsealedState
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !

!PressurizedUnsealedState methodsFor: 'controlling'!      

next: aBottle
   aBottle state: PressurizedSealed ! !



PressurizedBottleState subclass: #PressurizedSealedState
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !

!PressurizedSealedState methodsFor: 'controlling'!      

next: aBottle
   aBottle state: UnpressurizedEmpty ! !



Object subclass: #Bottle
instanceVariableNames: 'state id'
classVariableNames: ''
poolDictionaries: ''
category: nil !

!Bottle class methodsFor: 'instance creation'!      

new: anInteger
   ^super new initialize: anInteger ! !
   
!Bottle class methodsFor: 'private'!    

initialState
   ^BottleState initialState! !   

!Bottle methodsFor: 'controlling'!      

empty
   state next: self !

fill
   state next: self !

seal
   state next: self !
   
cycle
   self fill; seal; empty ! !      
   
   
!Bottle methodsFor: 'accessing'!      

check: anInteger
   ^state tag + id + anInteger !

checkWith: aBottle2 with: aBottle3 with: aBottle4 with: aBottle5 with: anInteger
   | c |
   self cycle.
   aBottle2 cycle.
   aBottle3 cycle.
   aBottle4 cycle.
   aBottle5 cycle.

   c := anInteger rem: 2. 
   ^(self check: c) + (aBottle2 check: c) + (aBottle3 check: c) + 
      (aBottle4 check: c) + (aBottle5 check: c) !
   
state: aBottleState
   state := aBottleState ! !
   
!Bottle methodsFor: 'initialize-release'!              

initialize: anInteger
   state := self class initialState.
   id := anInteger ! !
   
   
   
Bottle subclass: #PressurizedBottle
instanceVariableNames: ''
classVariableNames: ''
poolDictionaries: ''
category: nil !   

!PressurizedBottle class methodsFor: 'private'!    

initialState
   ^PressurizedBottleState initialState! !
   
!PressurizedBottle methodsFor: 'controlling'!     

cycle
   self fill; pressurize; seal; empty ! 

pressurize
   state next: self ! !

                     
   
| n b1 b2 b3 b4 b5 b6 b7 b8 b9 b0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p0 check |

BottleState initialize.
PressurizedBottleState initialize.

n := Smalltalk arguments first asInteger.

b1 := Bottle new: 1. b2 := Bottle new: 2. 
b3 := Bottle new: 3. b4 := Bottle new: 4.
b5 := Bottle new: 5. b6 := Bottle new: 6. 
b7 := Bottle new: 7. b8 := Bottle new: 8.
b9 := Bottle new: 9. b0 := Bottle new: 0.

p1 := PressurizedBottle new: 1. p2 := PressurizedBottle new: 2. 
p3 := PressurizedBottle new: 3. p4 := PressurizedBottle new: 4.
p5 := PressurizedBottle new: 5. p6 := PressurizedBottle new: 6. 
p7 := PressurizedBottle new: 7. p8 := PressurizedBottle new: 8.
p9 := PressurizedBottle new: 9. p0 := PressurizedBottle new: 0.

check := 0.
1 to: n do: [:i|
   check := check + (b1 checkWith: b2 with: b3 with: b4 with: b5 with: i).
   check := check + (b6 checkWith: b7 with: b8 with: b9 with: b0 with: i).  
   
   check := check + (p1 checkWith: p2 with: p3 with: p4 with: p5 with: i).   
   check := check - (p6 checkWith: p7 with: p8 with: p9 with: p0 with: i).         
].

check printString displayNl!

"  The Great Computer Language Shootout
   contributed by Isaac Gouy
  
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im ackermann.st -a 8
"

!Integer methodsFor: 'shootout'!

ackermann: anInteger
   ^self = 0
      ifTrue: [anInteger + 1]
      ifFalse: [
         anInteger = 0
            ifTrue: [self - 1 ackermann:  1]
            ifFalse: [self - 1 ackermann: (self ackermann: anInteger - 1)] ] ! !

| n |
n := Smalltalk arguments first asInteger.

Transcript show: 'Ack(3,'; show: n printString; show: '): '; 
           show: (3 ackermann: n) printString; nl!
"  The Great Computer Language Shootout
   contributed by Paolo Bonzini
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im except.st -a 20000
"

Notification subclass: #MyException
   instanceVariableNames: ''
   classVariableNames: ''
   poolDictionaries: ''
   category: 'testing'!
   
MyException class instanceVariableNames: 'count'!


!MyException class methodsFor: 'counting'!

count
   ^count!
   
increment
   count := count + 1!
   
initialize
   count := 0! !
   
MyException subclass: #LoException
   instanceVariableNames: ''
   classVariableNames: ''
   poolDictionaries: ''
   category: 'testing'!
   
MyException subclass: #HiException
   instanceVariableNames: ''
   classVariableNames: ''
   poolDictionaries: ''
   category: 'testing'!
   
LoException initialize.
HiException initialize!


!SmallInteger methodsFor: 'testing'!

someFunction
   ^self hiFunction!
   
hiFunction
   ^[ self loFunction ] on: HiException do: [ :ex | ex class increment ]!
   
loFunction
   ^[ self blowup ] on: LoException do: [ :ex | ex class increment ]!
   
blowup
   ^(self odd ifTrue: [ HiException ] ifFalse: [ LoException ]) signal: self! !
   
| n |
n := Smalltalk arguments isEmpty
   ifTrue: [ 20000 ]
   ifFalse: [ 1 max: Smalltalk arguments first asInteger ].
   
1 to: n do: [ :each | each someFunction ].

('Exceptions: HI=%1 / LO=%2'
   bindWith: HiException count with: LoException count) displayNl !
   
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!


Object subclass: #PermGenerator
   instanceVariableNames: 'timesRotated perm atEnd'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!


!Array methodsFor: 'benchmarking'!
pfannkuchen
   | first complement a b k |
   k := 0.
   [ (first := self at: 1) == 1 ] whileFalse: [
      k := k + 1.
      complement := first + 1.
      1 to: first // 2 do: [ :i |
         a := self at: i.
         b := self at: complement - i.
         self at: i put: b.
         self at: complement - i put: a.
      ]
   ].
   ^k! !



!PermGenerator methodsFor: 'initialize-release'!
initialize: size
   perm := (1 to: size) asArray.
   timesRotated := Array new: size withAll: 0.
   atEnd := false! !



!PermGenerator methodsFor: 'initialize-release'!
makeNext
   | temp remainder |
   "* Generate the next permutation. *"
   2 to: perm size do: [ :r |
      "* Rotate the first r items to the left. *"
      temp := perm at: 1.
      1 to: r - 1 do: [ :i | perm at: i put: (perm at: i + 1) ].
      perm at: r put: temp.

      remainder := timesRotated at: r put: ((timesRotated at: r) + 1) \\ r.
      remainder = 0 ifFalse: [ ^self ].

      "* After r rotations, the first r items are in their original positions.
      Go on rotating the first r+1 items. *"
   ].

   "* We are past the final permutation. *"
   atEnd := true! !



!PermGenerator methodsFor: 'benchmarks'!
maxPfannkuchenTo: output
   | max permutation check |
   max := 0.
   check := 0.
   [self atEnd] whileFalse:
      [permutation := self next.
      check < 30 ifTrue:
         [permutation do: [:each | output print: each].
         output nl.
         check := check + 1].
      max := max max: permutation pfannkuchen].
   ^max! !



!PermGenerator methodsFor: 'accessing'!
atEnd
   ^atEnd! !



!PermGenerator methodsFor: 'accessing'!
next
   | result |
   result := perm copy.
   self makeNext.
   ^result! !



!PermGenerator class methodsFor: 'instance creation'!
new: size
   ^self new
      initialize: size;
      yourself! !



!Tests class methodsFor: 'benchmarking'!
fannkuch: n to: output
   ^(PermGenerator new: n) maxPfannkuchenTo: output! !



!Tests class methodsFor: 'benchmark scripts'!
fannkuch
   | n f |
   n := self arg.
   f := self fannkuch: n to: self stdout.
   self stdout
      nextPutAll: 'Pfannkuchen(', n printString, ') = ';
      print: f; nl.
   ^''! !

Tests fannkuch!
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Paolo Bonzini
    modified by Eliot Miranda *"!

Object subclass: #RandomNumber   instanceVariableNames: 'seed scale'   classVariableNames: 'FModulus Increment Modulus Multiplier'   poolDictionaries: ''   category: 'Shootout'!

ReadStream subclass: #RepeatStream   instanceVariableNames: 'repeatPtr repeatLimit'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

RepeatStream subclass: #RandomStream   instanceVariableNames: 'random percentages'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!RandomNumber methodsFor: 'private'!to: anInteger   seed := 42.   scale := anInteger! !!RandomNumber methodsFor: 'accessing'!next   seed := (seed * Multiplier + Increment) \\ Modulus.   ^(seed * scale) / FModulus! !!RandomNumber class methodsFor: 'class initialization'!initialize   FModulus := 139968.0d0.   Increment := 29573.   Modulus := 139968.   Multiplier := 3877.! !!RandomNumber class methodsFor: 'initialize-release'!to: anInteger   ^self basicNew to: anInteger! !!RepeatStream methodsFor: 'accessing'!next   | element |
   ptr > endPtr ifTrue: [ self position: 0 ].
   element := collection at: ptr.
   ptr := ptr + 1. repeatPtr := repeatPtr + 1.
   ^element! !!RepeatStream methodsFor: 'testing'!atEnd   ^repeatPtr >= repeatLimit! !!RepeatStream methodsFor: 'initialize-release'!to: anInteger   repeatPtr := 0.   repeatLimit := anInteger! !!RandomStream methodsFor: 'accessing'!next   | r |   r := random next.   repeatPtr := repeatPtr + 1.   1 to: percentages size do: [:i|      (r < (percentages at: i)) ifTrue: [^collection at: i]]! !!RandomStream methodsFor: 'accessing'!random: aRandomNumber"* Share the random number generator so we can get the expected results. *"   random := aRandomNumber! !!RandomStream methodsFor: 'private methods'!initCollection: aCollection limit: size   | cp |   repeatPtr := 0.   random := RandomNumber to: 1.0d0.   percentages := Array new: size.   collection := Array new: size.   cp := 0.0d0.
   1 to: size do: [:i|      collection at: i put: (aCollection at: i) first.      percentages at: i put: (cp := cp + (aCollection at: i) last).
   ]! !!RepeatStream class methodsFor: 'instance creation'!to: anInteger on: aCollection   ^(super on: aCollection) to: anInteger! !


!Tests class methodsFor: 'benchmarking'!
writeFasta: aString from: inStream to: outStream lineLength: lineLength
   | i |
   outStream nextPut: $>; nextPutAll: aString; nl.
   i := 0.
   [inStream atEnd] whileFalse:
      [i == lineLength ifTrue: [outStream nl. i := 0].
      outStream nextPut: inStream next.
      i := i + 1].
   outStream nl! !

!Tests class methodsFor: 'benchmarking'!
fasta: n to: out
   | r lineLength |
   lineLength := 60.

   self
      writeFasta: 'ONE Homo sapiens alu'
      from:
         ( RepeatStream
            to: n*2
            on:'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG',
               'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA',
               'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT',
               'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA',
               'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG',
               'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC',
               'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA' )
      to: out
      lineLength: lineLength.

   r := RandomNumber to: 1. "Shared random sequence"

   self
      writeFasta: 'TWO IUB ambiguity codes'
      from:
         (( RandomStream
            to: n*3
            on: #(   #($a 0.27d0)
                  #($c 0.12d0)
                  #($g 0.12d0)
                  #($t 0.27d0)

                  #($B 0.02d0)
                  #($D 0.02d0)
                  #($H 0.02d0)
                  #($K 0.02d0)
                  #($M 0.02d0)
                  #($N 0.02d0)
                  #($R 0.02d0)
                  #($S 0.02d0)
                  #($V 0.02d0)
                  #($W 0.02d0)
                  #($Y 0.02d0)))
         random: r;
         yourself)
      to: out
      lineLength: lineLength.

   self
      writeFasta: 'THREE Homo sapiens frequency'
      from:
         (( RandomStream
            to: n*5
            on: #(   #($a 0.3029549426680d0)
                  #($c 0.1979883004921d0)
                  #($g 0.1975473066391d0)
                  #($t 0.3015094502008d0)))
            random: r;
            yourself)
      to: out
      lineLength: lineLength.

   out flush. ! !
!Tests class methodsFor: 'benchmark scripts'!
fasta   self fasta: self arg to: self stdout.   ^''! !

RandomNumber initialize!


Tests fasta!
"* The Computer Language Shootout
 http://shootout.alioth.debian.org/
 contributed by Isaac Gouy 
 modified by Paolo Bonzini *"

ReadStream subclass: #RepeatStream  instanceVariableNames: 'repeatPtr repeatLimit' classVariableNames: '' poolDictionaries: '' category: nil !

!RepeatStream class methodsFor: 'instance creation '!

to: anInteger on: aCollection
   ^(super on: aCollection) to: anInteger ! !

!RepeatStream methodsFor: 'initialize-release'!

to: anInteger
   repeatPtr := 0.
   repeatLimit := anInteger ! !

!RepeatStream methodsFor: 'accessing-reading'!

next
    | element |
    ptr > endPtr ifTrue: [ self position: 0 ].
    element := collection at: ptr.
    ptr := ptr + 1. repeatPtr := repeatPtr + 1.
    ^element ! !

!RepeatStream methodsFor: 'testing'!

atEnd
   ^repeatPtr >= repeatLimit ! !


RepeatStream subclass: #RandomStream instanceVariableNames: 'random percentages'
classVariableNames: '' poolDictionaries: '' category: nil !

!RandomStream methodsFor: 'private methods'!

initCollection: aCollection limit: size
   | cp |
   repeatPtr := 0.
   random := RandomNumber to: 1.0.
   percentages := Array new: size.
   collection := Array new: size.
   cp := 0.0.
   1 to: size do: [:i|
      cp := cp + (aCollection at: i) value.
      collection at: i put: (aCollection at: i) key.
      percentages at: i put: (cp * RandomNumber scale) ceiling.
   ] ! !

!RandomStream methodsFor: 'accessing'!

next
   | r |
   r := random next.
   repeatPtr := repeatPtr + 1.
   1 to: percentages size do: [:i|
      (r < (percentages at: i)) ifTrue: [^collection at: i]].
self halt !

random: aRandomNumber
"* Share the random number generator so we can get the expected results. *"
   random := aRandomNumber ! !


! FileStream methodsFor: 'accessing'!

writeFasta: aString sequence: aStream
   | i |
   self nextPut: $>; nextPutAll: aString; nl.

   i := 0.
   [aStream atEnd] whileFalse: [
      (i == 60) ifTrue: [self nl. i := 0].
      self nextPut: aStream next.
      i := i + 1.
      ].
   self nl ! !


Object subclass: #RandomNumber
instanceVariableNames: 'seed scale'
classVariableNames: 'Increment Multiplier Modulus '
poolDictionaries: '' category: nil !

!RandomNumber class methodsFor: 'instance creation'!

scale
   ^Modulus!

to: anInteger
   Increment := 29573.
   Multiplier := 3877.
   Modulus := 139968.
   ^self basicNew to: anInteger ! !

!RandomNumber methodsFor: 'accessing'!

next
   ^seed := seed * Multiplier + Increment \\ Modulus! !

!RandomNumber methodsFor: 'private'!

to: anInteger
   seed := 42.
   scale := anInteger ! !


| n r s x |
n := Smalltalk arguments first asInteger.
s := FileStream stdout bufferSize: 4096.

s writeFasta: 'ONE Homo sapiens alu' sequence:
   ( RepeatStream to: n*2 on:
      'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG',
      'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA',
      'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT',
      'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA',
      'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG',
      'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC',
      'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA' ).

r := RandomNumber to: 1. "Shared random sequence"

s writeFasta: 'TWO IUB ambiguity codes' sequence:
   (( RandomStream to: n*3 on: (
      OrderedCollection new
         add: (Association key: $a value: 0.27);
         add: (Association key: $c value: 0.12);
         add: (Association key: $g value: 0.12);
         add: (Association key: $t value: 0.27);

         add: (Association key: $B value: 0.02);
         add: (Association key: $D value: 0.02);
         add: (Association key: $H value: 0.02);
         add: (Association key: $K value: 0.02);
         add: (Association key: $M value: 0.02);
         add: (Association key: $N value: 0.02);
         add: (Association key: $R value: 0.02);
         add: (Association key: $S value: 0.02);
         add: (Association key: $V value: 0.02);
         add: (Association key: $W value: 0.02);
         add: (Association key: $Y value: 0.02);
         yourself )) random: r).

s writeFasta: 'THREE Homo sapiens frequency' sequence:
   (( RandomStream to: n*5 on: (
      OrderedCollection new
         add: (Association key: $a value: 0.3029549426680);
         add: (Association key: $c value: 0.1979883004921);
         add: (Association key: $g value: 0.1975473066391);
         add: (Association key: $t value: 0.3015094502008);
         yourself )) random: r).

s flush; close !
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im fibo.st -a 32
"

!Integer methodsFor: 'shootout'!

fibonacci
   ^self < 2 
      ifTrue: [1] 
      ifFalse: [(self - 2) fibonacci + (self - 1) fibonacci] ! !

Transcript show: Smalltalk arguments first asInteger fibonacci printString; nl !
"  The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy
 
   To run: gst -QI /usr/local/share/smalltalk/gst.im harmonic.st -a 10000000
"

!Float methodsFor: 'printing'!

printStringRoundedTo: anInteger
   | n s |
   n := 0.5d * (10 raisedToInteger: anInteger negated).
   s := ((self sign < 0) ifTrue: [self - n] ifFalse: [self + n]) printString.
   ^s copyFrom: 1 to: (s indexOf: $.) + anInteger ! !


| n partialSum |
n := Smalltalk arguments first asInteger.
partialSum := 0.0.
1 to: n do: [:i| partialSum := partialSum + (1.0/i)].

(partialSum printStringRoundedTo: 9) displayNl !
"  The Great Computer Language Shootout
   contributed by Isaac Gouy (with improvements by Paolo Bonzini)
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im hash.st -a 80000
"

| n count table |
n := Smalltalk arguments first asInteger.

count := 0.
table := Set new: n + (n // 5).

1 to: n do: [:each| table add: (each printString: 16)].

1 to: n do: [:each | 
   (table includes: each printString) ifTrue: [count := count + 1] ].
   
Transcript show: count printString; nl !
"  The Great Computer Language Shootout
   contributed by Isaac Gouy (with improvements by Paolo Bonzini)
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im hash2.st -a 150
"

| n table1 table2 |
n := Smalltalk arguments first asInteger.

table1 := Dictionary new: 12000. 
table2 := Dictionary new: n + (n // 5).

0 to: 9999 do: [:each| table1 at: 'foo_', each printString put: each].

n timesRepeat: [ 
   table1 keysAndValuesDo: [ :key :value |  | assoc |
      (assoc := table2 associationAt: key ifAbsent: []) isNil 
         ifTrue: [table2 at: key put: value]
         ifFalse: [assoc value: assoc value + value]   
   ]
]. 

(table1 at: 'foo_1')    display. ' ' display. 
(table1 at: 'foo_9999') display. ' ' display.
(table2 at: 'foo_1')    display. ' ' display. 
(table2 at: 'foo_9999') displayNl !
"  The Great Computer Language Shootout
   contributed by Isaac Gouy (improved by Paolo Bonzini)
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im heapsort.st -a 80000
"

!Array methodsFor: 'sorting'!

heapsort
   | j i ir l r |
   ir := self size.
   l := self size // 2 + 1.  
   
   [
      l > 1 
         ifTrue: [ r := self at: (l := l - 1)]
         ifFalse: [
            r := self at: ir.
            self at: ir put: (self at: 1).
            ir := ir - 1.
            ir = 1 ifTrue: [self at: 1 put: r. ^self] ]. 
                
      i := l.
      j := l * 2.
      [j <= ir] whileTrue: [
         (j < ir and: [(self at: j) < (self at: j + 1)]) 
            ifTrue: [j := j + 1].
          
         r < (self at: j)
            ifTrue: [self at: i put: (self at: j). i := j. j := j + i]
            ifFalse: [j := ir + 1].
      ].
      self at: i put: r.   
   ] repeat ! !


Object subclass: #RandomNumber
instanceVariableNames: 'seed scale'
classVariableNames: 'Increment Multiplier Modulus FModulus'
poolDictionaries: ''
category: nil !

!RandomNumber class methodsFor: 'initialize'!

initialize
   Increment := 29573.
   Multiplier := 3877.
   Modulus := 139968.
   FModulus := 139968.0d ! !
   
!RandomNumber class methodsFor: 'instance creation'!

to: anInteger
   ^self basicNew to: anInteger ! !
   
!RandomNumber methodsFor: 'accessing'!

next
   seed := seed * Multiplier + Increment \\ Modulus.
   ^(seed * scale) asFloatD / FModulus ! !
     
!RandomNumber methodsFor: 'private'!

to: anInteger
   seed := 42.
   scale := anInteger ! !
   
   
!Float methodsFor: 'printing'!

printStringRoundedTo: anInteger
   | s |
   s := (0.5d * (10 raisedToInteger: anInteger negated) + self) printString.
   ^s copyFrom: 1 to: (s indexOf: $.) + anInteger ! !  
   
   
| n data randomNumber |
n := Smalltalk arguments first asInteger.
data := Array new: n.
randomNumber := RandomNumber initialize; to: 1.
1 to: n do: [:i| data at: i put: randomNumber next]. 
data heapsort.
(data last printStringRoundedTo: 10) displayNl !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!

Transcript show: 'hello world'; nl !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!

Transcript show: 'hello world'; nl !



Object subclass: #Tests   instanceVariableNames: ''   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

!Tests class methodsFor: 'platform'!arg   ^Smalltalk arguments first asInteger! !!Tests class methodsFor: 'platform'!stdin   ^FileStream stdin! !

!Tests class methodsFor: 'platform'!stdinSpecial   ^self stdin bufferSize: 4096! ! !Tests class methodsFor: 'platform'!stdout   ^FileStream stdout! !

!Tests class methodsFor: 'platform'!stdoutSpecial
   ^self stdout bufferSize: 4096! ! 

!Stream methodsFor: 'platform'!print: number digits: decimalPlaces
   | n s |
   n := 0.5d0 * (10 raisedToInteger: decimalPlaces negated).
   s := ((number sign < 0) ifTrue: [number - n] ifFalse: [number + n]) printString.   self nextPutAll: (s copyFrom: 1 to: (s indexOf: $.) + decimalPlaces)! !

!Stream methodsFor: 'platform'!print: number paddedTo: width
   | s |
   s := number printString.   self nextPutAll: (String new: (width - s size) withAll: $ ), s! !!Integer methodsFor: 'platform'!asFloatD   ^self asFloat! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

Array subclass: #DoubleArray!
DoubleArray shape: #double!

!DoubleArray methodsFor: 'benchmarking'!
multiplyAtAv
   ^(self multiplyAv) multiplyAtv! !

!DoubleArray methodsFor: 'benchmarking'!
multiplyAtv
   | n atv sum |
   n := self size.
   atv := DoubleArray new: n.
   1 to: n do: [:i|
      sum := 0.0d.
      1 to: n do: [:j|
         sum := sum + ((j matrixA: i) * (self at: j)) ].
      atv at: i put: sum].
   ^atv! !

!DoubleArray methodsFor: 'benchmarking'!
multiplyAv
   | n av sum |
   n := self size.
   av := DoubleArray new: n.
   1 to: n do: [:i|
      sum := 0.0d.
      1 to: n do: [:j|
         sum := sum + ((i matrixA: j) * (self at: j)) ].
      av at: i put: sum].
   ^av! !


!SmallInteger methodsFor: 'benchmarking'!
matrixA: anInteger
   ^1.0d0 / ((self + anInteger - 2) * (self + anInteger - 1) /2  + self)! !


!Tests class methodsFor: 'benchmarking'!
spectralnorm: n
   | u v vBv vv |
   u := DoubleArray new: n withAll: 1.0d0.
   10 timesRepeat:
      [v := u multiplyAtAv.
       u := v multiplyAtAv].
   vBv := 0.0d0.
   vv := 0.0d0.
   1 to: n do:
      [:i |
       vBv := vBv + ((u at: i) * (v at: i)).
       vv := vv + ((v at: i) * (v at: i))].
   ^(vBv / vv) sqrt! !


!Tests class methodsFor: 'benchmark scripts'!
spectralnorm
   self stdout print: (self spectralnorm: self arg) digits: 9; nl.
   ^''! !

Tests spectralnorm!
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!


!SequenceableCollection methodsFor: 'computer language shootout'!
substringFrequencies: aLength using: aDictionary
   1 to: self size - aLength + 1 do:
      [:i | | fragment assoc |
      fragment := self copyFrom: i to: i + aLength - 1.

      (assoc := aDictionary associationAt: fragment ifAbsent: []) isNil 
         ifTrue: [aDictionary at: fragment put: 1]
         ifFalse: [assoc value: assoc value + 1] ].
   ^aDictionary ! !


!Tests class methodsFor: 'benchmarking'!
readFasta: sequenceName from: input
   | prefix newline buffer description line char |
   prefix := '>',sequenceName.
   newline := Character lf.

   "* find start of particular fasta sequence *"
   [(input atEnd) or: [
         (input peek = $>) 
            ifTrue: [((line := input upTo: newline) 
               indexOfSubCollection: prefix startingAt: 1) = 1]
            ifFalse: [input skipTo: newline. false]]
      ] whileFalse.

   "* line-by-line read - it would be a lot faster to block read *"
   description := line.
   buffer := ReadWriteStream on: (String new: 1028).
   [(input atEnd) or: [(char := input peek) = $>]] whileFalse: [
      (char = $;) 
         ifTrue: [input upTo: newline] 
         ifFalse: [buffer nextPutAll: (input upTo: newline)]
      ].
   ^Association key: description value: buffer contents ! !

!Tests class methodsFor: 'benchmarking'!
knucleotideFrom: input to: output
   | sequence newline writeFrequencies writeCount |

   sequence := (self readFasta: 'THREE' from: input) value asUppercase.
   newline := Character lf.

   writeFrequencies :=
      [:k | | frequencies count |
      frequencies := SortedCollection sortBlock: [:a :b|
         (a value = b value) ifTrue: [b key < a key] ifFalse: [b value < a value]].

      count := 0.0.
      (sequence substringFrequencies: k using: Dictionary new)
         associationsDo: [:each|
            frequencies add: each. count := count + each value].

      frequencies do: [:each | | percentage |
         percentage := (each value / count) * 100.0.
         output 
            nextPutAll: each key; space;
            print: percentage digits: 3; nl]].

   writeCount := [:nucleotideFragment | | frequencies count |
      frequencies := sequence substringFrequencies: nucleotideFragment size
         using: Dictionary new.
      count := frequencies at: nucleotideFragment ifAbsent: [0].
      output print: count; tab; nextPutAll: nucleotideFragment; nl].

   writeFrequencies value: 1. output nl.
   writeFrequencies value: 2. output nl.

   writeCount value: 'GGT'.
   writeCount value: 'GGTA'.
   writeCount value: 'GGTATT'.
   writeCount value: 'GGTATTTTAATT'.
   writeCount value: 'GGTATTTTAATTTATAGT'.! !
!Tests class methodsFor: 'benchmark scripts'!
knucleotide   self knucleotideFrom: self stdinSpecial to: self stdout.   ^''! !

Tests knucleotide!
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
 
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im lists.st -a 16
"

| size n list1 list2 list3 count |
size := 10000.
n := Smalltalk arguments first asInteger.

n timesRepeat: [
   list1 := OrderedCollection new: size.
   1 to: size do: [:each| list1 addLast: each].
   list2 := list1 copy.

   list3 := OrderedCollection new: size.
   [list2 notEmpty] whileTrue: [list3 addLast: list2 removeFirst]. 
   [list3 notEmpty] whileTrue: [list2 addLast: list3 removeLast].

   list1 := list1 reverse. 
   count := (list1 first = size and: [list1 = list2]) 
      ifTrue: [list1 size] ifFalse: [-1].
   ].
   
count displayNl !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

!Tests class methodsFor: 'benchmarking'!mandelbrot2: extent to: output   | limit2 m bits zr zi cr ci i tr stepr stepi |   limit2 := 4.0d0.
   m := 50.
   stepr := 2.0d0 / extent.
   stepi := 2.0d0 / extent.

   0 to: extent - 1 do: [ :y |
       bits := 0.
       ci := stepi * y asFloat - 1.0d0.
       0 to: extent - 1 do: [ :x |
           cr := stepr * x asFloat - 1.5d0.
           zr := cr. zi := ci.

           bits := bits bitShift: 1.
           i := 1.  
           [
               tr := (zr*zr) - (zi*zi) + cr.
               zi := 2.0d0 * zr * zi + ci.
               zr := tr.
               (zr*zr) + (zi*zi) < limit2 and: [ (i := i + 1) < m ]
           ] whileTrue.

           i = m ifTrue: [ bits := bits + 1 ].
           (x bitAnd: 7) == 7 ifTrue: [
               output nextPutByte: bits.
               bits := 0.
           ]
       ]. 
       (extent bitAnd: 7) == 0 ifFalse: [
           bits := bits bitShift: 8 - (extent bitAnd: 7).
           output nextPutByte: bits.
       ]
   ]! !

!Tests class methodsFor: 'benchmark scripts'!mandelbrot2   | n output |    n := self arg.    (output := self stdout)      nextPutAll: 'P4'; nl; print: n; space; print: n; nl.   self mandelbrot2: n to: output.   ^''! !

Tests mandelbrot2!
"  The Great Computer Language Shootout
   contributed by Paolo Bonzini
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im matrix.st -a 300
"

!Array class methodsFor: 'instance creation'!

newMatrix: rows columns: cols
   | count mx row |
   count := 1.
   mx := self new: rows.
   1 to: rows do: [ :i |
      row := mx at: i put: (Array new: cols).
      1 to: cols do: [ :j |
         row at: j put: count.
         count := count + 1
      ].
   ].
   ^mx! !
    
!Array methodsFor: 'testing'!

atXY: coord
   ^(self at: coord x) at: coord y!
    
mmult: m2
   | rows cols terms val mx row myRow |
   rows := self size.
   terms := m2 size.
   cols := m2 first size.
   mx := Array new: rows.
   1 to: rows do: [ :i |
      row := mx at: i put: (Array new: cols).
      myRow := self at: i.
      1 to: cols do: [ :j |
         val := 0.
         1 to: terms do: [ :k |
            val := val + ((myRow at: k) * ((m2 at: k) at: j) bitAnd: 16r3FFF_FFFF) ].
         row at: j put: val.
      ].
   ].
   ^mx! !
    
    
| m1 m2 mm size n |
n := Smalltalk arguments isEmpty
   ifTrue: [ 1 ]
   ifFalse: [ 1 max: Smalltalk arguments first asInteger ].
   
size := 30.
m1 := Array newMatrix: size columns: size.
m2 := Array newMatrix: size columns: size.
n timesRepeat: [ mm := m1 mmult: m2 ].

('%1 %2 %3 %4' bindWith: (mm atXY: 1@1)
   with: (mm atXY: 3@4) with: (mm atXY: 4@3)
   with: (mm atXY: 5@5)) displayNl!
    
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

Object subclass: #Consumer   instanceVariableNames: 'semaphore msg'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

Consumer subclass: #ProducerConsumer   instanceVariableNames: 'consumer'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

!Consumer methodsFor: 'accessing'!msg    semaphore wait.    ^msg! !!Consumer methodsFor: 'accessing'!msg: data    msg := data.    semaphore signal! !!Consumer methodsFor: 'accessing'!semaphore: aSemaphore    semaphore := aSemaphore! !!Consumer class methodsFor: 'instance creation'!new    | var |    var := self basicNew.    var semaphore: Semaphore new.    ^var! !!ProducerConsumer methodsFor: 'accessing'!consumer: aProcess    consumer := aProcess! !!ProducerConsumer methodsFor: 'accessing'!fork    [ self run ] fork! !!ProducerConsumer methodsFor: 'accessing'!run    [ consumer msg: self msg + 1 ] repeat! !!ProducerConsumer class methodsFor: 'instance creation'!fork: consumer    | proc |    proc := self new.    proc consumer: consumer.    proc fork.    ^proc! !!Tests class methodsFor: 'benchmarking'!message: n   | tail head sum |   head := tail := Consumer new.   500 timesRepeat: [head := ProducerConsumer fork: head].   sum := 0.   n timesRepeat:      [head msg: 0.      sum := sum + tail msg].   ^sum ! !!Tests class methodsFor: 'benchmark scripts'!message   self stdout print: (self message: self arg); nl.   ^''! !

Tests message!
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im methcall.st -a 1000000
"

Object subclass: #Toggle
instanceVariableNames: 'state'
classVariableNames: ''
poolDictionaries: ''
category: nil !

!Toggle class methodsFor: 'instance creation'!

new: aBoolean
   ^self basicNew initialize: aBoolean ! !
   
!Toggle methodsFor: 'private'!

initialize: aBoolean
   state := aBoolean ! !
   
!Toggle methodsFor: 'accessing'!

activate
   state := state not !
   
state
   ^state ! !
   
   
Toggle subclass: #NToggle
instanceVariableNames: 'trigger count'
classVariableNames: ''
poolDictionaries: ''
category: nil !

!NToggle class methodsFor: 'instance creation'!

new: aBoolean withTrigger: anInteger
   ^(super new: aBoolean) withTrigger: anInteger ! !
   
!NToggle methodsFor: 'private'!

withTrigger: anInteger
   trigger := anInteger.
   count := 0 ! !
   
!NToggle methodsFor: 'accessing'!

activate
   "Toggle and answer the receiver"
   (count := count + 1) >= trigger ifTrue: [
      state := state not. 
      count := 0
   ] ! !
   
   
| n toggle ntoggle value |
n := Smalltalk arguments first asInteger.
toggle := Toggle new: true.
n timesRepeat: [value := toggle activate state]. 
value displayNl.

ntoggle := NToggle new: true withTrigger: 3.
n timesRepeat: [value := ntoggle activate state].
value displayNl !
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im moments.st < input.txt 

"

!Float methodsFor: 'printing'!

printStringRoundedTo: anInteger
   | n s |
   n := 0.5d * (10 raisedToInteger: anInteger negated).
   s := ((self sign < 0) ifTrue: [self - n] ifFalse: [self + n]) printString.
   ^s copyFrom: 1 to: (s indexOf: $.) + anInteger ! !  
   
   
| stream numbers sum n mean dev adev dev2 
  variance skew kurtosis sdev mid median |  
  
stream := FileStream stdin bufferSize: 4096.
numbers := (stream splitAt: Character nl) collect: [:each| each asNumber].

n := numbers size.
sum := numbers inject: 0.0 into: [:i :each| i + each].
dev := adev := variance := skew := kurtosis := 0.0d.         
mean := sum / n. 

numbers do: [:each|
   dev := each - mean.   
   adev := adev + (dev abs).   
   variance := variance + (dev2 := dev * dev). 
   skew := skew + (dev2 * dev).
   kurtosis := kurtosis + (dev2 * dev2).            

].

adev := adev / n.   
variance := variance / (n - 1).     
sdev := variance sqrt.

variance ~= 0 
   ifTrue: 
     [skew := skew / (n * variance * sdev).  
      kurtosis := kurtosis / (n * variance * variance) - 3.0d.
     ]. 
     
numbers := numbers asSortedCollection: [:a :b| a < b].
mid := n // 2.      
median := (n \\ 2) ~= 0
   ifTrue: [numbers at: mid]
   ifFalse: [((numbers at: mid) + (numbers at: mid + 1)) / 2.0d].
   
Transcript
   nextPutAll: 'n:                  '; 
   nextPutAll: n displayString; nl;     
   
   nextPutAll: 'median:             '; 
   nextPutAll: (median printStringRoundedTo: 6) displayString; nl;  
   
   nextPutAll: 'mean:               ';  
   nextPutAll: (mean printStringRoundedTo: 6) displayString; nl; 
   
   nextPutAll: 'average_deviation:  ';   
   nextPutAll: (adev printStringRoundedTo: 6) displayString; nl; 
            
   nextPutAll: 'standard_deviation: ';  
   nextPutAll: (sdev printStringRoundedTo: 6) displayString; nl; 
     
   nextPutAll: 'variance:           ';   
   nextPutAll: (variance printStringRoundedTo: 6) displayString; nl; 
         
   nextPutAll: 'skew:               ';       
   nextPutAll: (skew printStringRoundedTo: 6) displayString; nl;
    
   nextPutAll: 'kurtosis:           '; 
   nextPutAll: (kurtosis printStringRoundedTo: 6) displayString; nl !
      
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!

Object subclass: #Body   instanceVariableNames: 'x y z vx vy vz mass'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

Object subclass: #NBodySystem   instanceVariableNames: 'bodies'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!Body methodsFor: 'accessing'!mass   ^mass! !!Body methodsFor: 'accessing'!x   ^x! !!Body methodsFor: 'accessing'!x: d1 y: d2 z: d3 vx: d4 vy: d5 vz: d6 mass: d7   x := d1.   y := d2.    z := d3.    vx := d4.   vy := d5.   vz := d6.   mass := d7! !!Body methodsFor: 'accessing'!y   ^y! !!Body methodsFor: 'accessing'!z   ^z! !!Body methodsFor: 'nbody'!addMomentumTo: anArray   anArray at: 1 put: (anArray at: 1) + (vx * mass).   anArray at: 2 put: (anArray at: 2) + (vy * mass).   anArray at: 3 put: (anArray at: 3) + (vz * mass).   ^anArray! !!Body methodsFor: 'nbody'!and: aBody velocityAfter: dt           | dx dy dz distance mag |   dx := x - aBody x.   dy := y - aBody y.   dz := z - aBody z.      distance := ((dx*dx) + (dy*dy) + (dz*dz)) sqrt.   mag := dt / (distance * distance * distance).   self decreaseVelocity: dx y: dy z: dz m: aBody mass * mag.      aBody increaseVelocity: dx y: dy z: dz m: mass * mag! !!Body methodsFor: 'nbody'!decreaseVelocity: dx y: dy z: dz m: m   vx := vx - (dx * m).   vy := vy - (dy * m).   vz := vz - (dz * m)! !!Body methodsFor: 'nbody'!increaseVelocity: dx y: dy z: dz m: m   vx := vx + (dx * m).   vy := vy + (dy * m).   vz := vz + (dz * m)! !!Body methodsFor: 'nbody'!kineticEnergy   ^0.5d0 * mass * ((vx * vx) + (vy * vy) + (vz * vz))! !!Body methodsFor: 'nbody'!offsetMomentum: anArray    | m |   m := self class solarMass.   vx := (anArray at: 1) negated / m.   vy := (anArray at: 2) negated / m.   vz := (anArray at: 3) negated / m! !!Body methodsFor: 'nbody'!positionAfter: dt   x := x + (dt * vx).   y := y + (dt * vy).   z := z + (dt * vz)! !!Body methodsFor: 'nbody'!potentialEnergy: aBody   | dx dy dz distance |   dx := x - aBody x.   dy := y - aBody y.   dz := z - aBody z.   distance := ((dx*dx) + (dy*dy) + (dz*dz)) sqrt.   ^mass * aBody mass / distance! !!Body class methodsFor: 'constants'!daysPerYear   ^365.24d0! !!Body class methodsFor: 'constants'!jupiter   ^self new      x: 4.84143144246472090d0      y: -1.16032004402742839d0      z: -1.03622044471123109d-1      vx: 1.66007664274403694d-3 * self daysPerYear      vy: 7.69901118419740425d-3 * self daysPerYear      vz: -6.90460016972063023d-5 * self daysPerYear      mass: 9.54791938424326609d-4 * self solarMass! !!Body class methodsFor: 'constants'!neptune   ^self new      x: 1.53796971148509165d1      y: -2.59193146099879641d1      z: 1.79258772950371181d-1      vx: 2.68067772490389322d-3 * self daysPerYear      vy: 1.62824170038242295d-3 * self daysPerYear      vz: -9.51592254519715870d-5 * self daysPerYear      mass: 5.15138902046611451d-5 * self solarMass! !!Body class methodsFor: 'constants'!pi   ^3.141592653589793d0! !!Body class methodsFor: 'constants'!saturn   ^self new      x: 8.34336671824457987d0      y: 4.12479856412430479d0      z: -4.03523417114321381d-1      vx: -2.76742510726862411d-3 * self daysPerYear      vy: 4.99852801234917238d-3 * self daysPerYear      vz: 2.30417297573763929d-5 * self daysPerYear      mass: 2.85885980666130812d-4 * self solarMass! !!Body class methodsFor: 'constants'!solarMass   ^4.0d0 * self pi * self pi! !!Body class methodsFor: 'constants'!sun   ^self new      x: 0.0d0      y: 0.0d0      z: 0.0d0      vx: 0.0d0      vy: 0.0d0      vz: 0.0d0      mass: self solarMass! !!Body class methodsFor: 'constants'!uranus   ^self new      x: 1.28943695621391310d1      y: -1.51111514016986312d1      z: -2.23307578892655734d-1      vx: 2.96460137564761618d-3 * self daysPerYear      vy: 2.37847173959480950d-3 * self daysPerYear      vz: -2.96589568540237556d-5 * self daysPerYear      mass: 4.36624404335156298d-5 * self solarMass! !!NBodySystem methodsFor: 'nbody'!after: dt   1 to: bodies size do: [:i|      i+1 to: bodies size do: [:j|                                     (bodies at: i) and: (bodies at: j) velocityAfter: dt].   ].      bodies do: [:each| each positionAfter: dt]! !!NBodySystem methodsFor: 'nbody'!energy   | e |   e := 0.0d0.   1 to: bodies size do: [:i|             e := e + (bodies at: i) kineticEnergy.      i+1 to: bodies size do: [:j|          e := e - ((bodies at: i) potentialEnergy: (bodies at: j))].   ].   ^e! !!NBodySystem methodsFor: 'initialize-release'!initialize   bodies := OrderedCollection new      add: Body sun; add: Body jupiter; add: Body saturn;      add: Body uranus; add: Body neptune; yourself.   bodies first offsetMomentum:      (bodies inject: (Array with: 0.0d0 with: 0.0d0 with: 0.0d0)         into: [:m :each | each addMomentumTo: m])! !!Tests class methodsFor: 'benchmark scripts'!nbody   | bodies |   bodies := NBodySystem new initialize.   self stdout print: bodies energy digits: 9; nl.   self arg timesRepeat: [bodies after: 0.01d0].   self stdout print: bodies energy digits: 9; nl.   ^''! !

Tests nbody!
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im nestedloop.st -a 16 
"

| n count |
n := (Smalltalk arguments at: 1) asInteger.

count := 0.
n timesRepeat: [
   n timesRepeat: [
      n timesRepeat: [
         n timesRepeat: [
            n timesRepeat: [
               n timesRepeat: [count := count + 1] ] ] ] ] ].
               
count displayNl !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!!Tests class methodsFor: 'benchmark scripts'!nsieve   | n |   n := self arg.   (n < 2) ifTrue: [n := 2].   self primeBenchmarkFor: n to: self stdout using: Array.   ^''! !!Tests class methodsFor: 'benchmarking'!nsieve: n using: arrayClass    | count isPrime |   count := 0.   isPrime := arrayClass new: n withAll: true.   2 to: n do:      [:i |       (isPrime at: i) ifTrue:          [i + i to: n by: i do:            [:k | isPrime at: k put: false].         count := count + 1]].   ^count! !!Tests class methodsFor: 'benchmarking'!primeBenchmarkFor: v to: output using: arrayClass   v to: v - 2 by: -1 do:      [:n| | m |      m := (2 raisedTo: n) * 10000.      output         nextPutAll: 'Primes up to '; 
         print: m paddedTo: 8;
         print: (self nsieve: m using: arrayClass) paddedTo: 9; nl
      ]! !

Tests nsieve!
"* The Computer Language Benchmarks Game    http://shootout.alioth.debian.org/    contributed by Isaac Gouy    modified by Eliot Miranda    then by Nicolas Cellier *"!!Tests class methodsFor: 'benchmark scripts'!nsieve2   | n |   n := self arg.   (n < 2) ifTrue: [n := 2].   self primeBenchmark2For: n to: self stdout using: Array.   ^''! !!Tests class methodsFor: 'benchmarking'!nsieve2: n using: arrayClass    | count isPrime k |   count := 0.   isPrime := arrayClass new: n withAll: true.   2 to: n do:      [:i |       (isPrime at: i) ifTrue:          [k := i.         [(k := k + i) <= n] whileTrue: [isPrime at: k put: false].         count := count + 1]].   ^count! !!Tests class methodsFor: 'benchmarking'!primeBenchmark2For: v to: output using: arrayClass   v to: v - 2 by: -1 do:      [:n| | m |      m := (2 raisedTo: n) * 10000.      output         nextPutAll: 'Primes up to ';         print: m paddedTo: 8;         print: (self nsieve2: m using: arrayClass) paddedTo: 9; nl      ]! !

Tests nsieve2!
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

ArrayedCollection variableByteSubclass: #BitArray   instanceVariableNames: ''   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

!BitArray methodsFor: 'accessing'!at: index    | wordIndex bitMask |    wordIndex := ((index - 1) bitShift: -3) + 1.    bitMask := 1 bitShift: (index - 1 bitAnd: 7).    ^((self basicAt: wordIndex) bitAnd: bitMask) > 0! !
!BitArray methodsFor: 'accessing'!at: index put: bit    | wordIndex bitMask word |    wordIndex := ((index - 1) bitShift: -3) + 1.    bitMask := 1 bitShift: (index - 1 bitAnd: 7).    word := self basicAt: wordIndex.    word := word bitOr: bitMask.    bit ifFalse: [word := word - bitMask].    self basicAt: wordIndex put: word.    ^bit! !!BitArray methodsFor: 'accessing'!atAllPut: anObject    "Put anObject at every one of the receivers indices."   | value |   value := anObject ifTrue: [255] ifFalse: [0].   1 to: self basicSize do: [:index | self basicAt: index put: value]! !!BitArray class methodsFor: 'instance creation'!new: size    ^super new: (size + 7 bitShift: -3)! !!Tests class methodsFor: 'benchmark scripts'!nsievebits   | n |   n := self arg.   (n < 2) ifTrue: [n := 2].   self primeBenchmarkFor: n to: self stdout using: BitArray.   ^''! !!Tests class methodsFor: 'benchmarking'!nsieve: n using: arrayClass    | count isPrime |   count := 0.   isPrime := arrayClass new: n withAll: true.   2 to: n do:      [:i |       (isPrime at: i) ifTrue:          [i + i to: n by: i do:            [:k | isPrime at: k put: false].         count := count + 1]].   ^count! !!Tests class methodsFor: 'benchmarking'!primeBenchmarkFor: v to: output using: arrayClass   v to: v - 2 by: -1 do:      [:n| | m |      m := (2 raisedTo: n) * 10000.      output         nextPutAll: 'Primes up to '; 
         print: m paddedTo: 8;
         print: (self nsieve: m using: arrayClass) paddedTo: 9; nl
      ]! !

Tests nsievebits!
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im objinst.st -a 1000000 
"

Object subclass: #Toggle
instanceVariableNames: 'state'
classVariableNames: ''
poolDictionaries: ''
category: nil !

!Toggle class methodsFor: 'instance creation'!

new: aBoolean
   ^self basicNew initialize: aBoolean ! !
   
!Toggle methodsFor: 'private'!

initialize: aBoolean
   state := aBoolean ! !
   
!Toggle methodsFor: 'accessing'!

activate
   state := state not !
   
state
   ^state ! !
   
   
Toggle subclass: #NToggle
instanceVariableNames: 'trigger count'
classVariableNames: ''
poolDictionaries: ''
category: nil !

!NToggle class methodsFor: 'instance creation'!

new: aBoolean withTrigger: anInteger
   ^(super new: aBoolean) withTrigger: anInteger ! !
   
!NToggle methodsFor: 'private'!

withTrigger: anInteger
   trigger := anInteger.
   count := 0 ! !
   
!NToggle methodsFor: 'accessing'!

activate
   "Toggle and answer the receiver"
   (count := count + 1) >= trigger ifTrue: [
      state := state not. 
      count := 0
   ] ! !
   
| n toggle ntoggle |
n := Smalltalk arguments first asInteger.

toggle := Toggle new: true.
5 timesRepeat: [toggle activate state displayNl].
n timesRepeat: [toggle := Toggle new: true].
Transcript nl.

ntoggle := NToggle new: true withTrigger: 3.
8 timesRepeat: [ntoggle activate state displayNl]. 
n timesRepeat: [ntoggle := NToggle new: true withTrigger: 3] ! 
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

!Tests class methodsFor: 'benchmarking'!partialsums: n to: output   | a1 a2 a3 a4 a5 a6 a7 a8 a9 twothirds alt |   a1 := a2 := a3 := a4 := a5 := a6 := a7 := a8 := a9 := 0.0d0.   twothirds := 2.0d0/3.0d0.   alt := -1.0d0.   1.0d0 to: n do: [:k| | k2 k3 sk ck |      k2 := k*k.      k3 := k2*k.      sk := k sin.      ck := k cos.      alt := -1.0d0 * alt.      a1 := a1 + (twothirds raisedTo: k - 1.0d0).      a2 := a2 + (k raisedTo: -0.5d0).      a3 := a3 + (1.0d0/(k*(k+1.0d0))).      a4 := a4 + (1.0d0/(k3*sk*sk)).      a5 := a5 + (1.0d0/(k3*ck*ck)).      a6 := a6 + (1.0d0/k).      a7 := a7 + (1.0d0/k2).      a8 := a8 + (alt/k).      a9 := a9 + (alt/(2.0d0*k - 1.0d0))].

   self print: a1 withName: '(2/3)^k' to: output.
   self print: a2 withName: 'k^-0.5' to: output.
   self print: a3 withName: '1/k(k+1)' to: output.
   self print: a4 withName: 'Flint Hills' to: output.
   self print: a5 withName: 'Cookson Hills' to: output.
   self print: a6 withName: 'Harmonic' to: output.
   self print: a7 withName: 'Riemann Zeta' to: output.
   self print: a8 withName: 'Alternating Harmonic' to: output.
   self print: a9 withName: 'Gregory' to: output.   ^''! !

!Tests class methodsFor: 'benchmarking'!
print: number withName: name to: output
   output print: number digits: 9; tab; nextPutAll: name; nl! !

!Tests class methodsFor: 'benchmark scripts'!partialsums   self partialsums: self arg asFloatD to: self stdout.   ^''! !

Tests partialsums!
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Object subclass: #PiDigitSpigot   instanceVariableNames: 'z x inverse'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

Object subclass: #Transformation   instanceVariableNames: 'q r s t k'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!PiDigitSpigot methodsFor: 'private'!consume: aTransformation   ^z * aTransformation! !!PiDigitSpigot methodsFor: 'private'!digit   ^(z extract: 3) floor! !!PiDigitSpigot methodsFor: 'private'!isSafe: aDigit   ^aDigit = (z extract: 4) floor! !!PiDigitSpigot methodsFor: 'private'!produce: anInteger   inverse q: 10 r: -10 * anInteger s: 0 t: 1.   ^inverse * z! !!PiDigitSpigot methodsFor: 'accessing'!next   | y |   ^(self isSafe: (y := self digit))      ifTrue: [z := self produce: y. y]      ifFalse: [z := self consume: x next. self next]! !!PiDigitSpigot methodsFor: 'initialize-release'!initialize   z := Transformation unity.   x := Transformation new.   inverse := Transformation new.! !!PiDigitSpigot class methodsFor: 'instance creation'!new   ^super new initialize! !!Tests class methodsFor: 'benchmarking'!pidigitsTo: v width: width to: output   | n i pidigits |   n := v.   i := 0.   pidigits := PiDigitSpigot new.   [n > 0] whileTrue:      [n < width         ifTrue:            [n timesRepeat: [output nextPut: (Character digitValue: pidigits next)].            n to: width do: [:each | output space].            i := i + n]         ifFalse:            [width timesRepeat: [output nextPut: (Character digitValue: pidigits next)].            i := i + width].
      output tab; nextPut: $:; print: i; nl.
      n := n - width]! !!Tests class methodsFor: 'benchmark scripts'!pidigits   self pidigitsTo: self arg width: 10 to: self stdout.   ^''! !!Transformation methodsFor: 'accessing'!* aTransformation   ^self species       q: q * aTransformation q      r: q * aTransformation r + (r * aTransformation t)      s: s * aTransformation q + (t * aTransformation s)      t: s * aTransformation r + (t * aTransformation t)! !!Transformation methodsFor: 'accessing'!extract: anInteger   ^(q * anInteger + r) // (s * anInteger + t)! !!Transformation methodsFor: 'accessing'!next   k := k +1.   q := k.   r := 4 * k + 2.   s := 0.   t := 2 * k + 1.! !!Transformation methodsFor: 'accessing'!q   ^q! !!Transformation methodsFor: 'accessing'!q: anInteger1 r: anInteger2 s: anInteger3 t: anInteger4   q := anInteger1.   r := anInteger2.   s := anInteger3.   t := anInteger4.   k := 0.! !!Transformation methodsFor: 'accessing'!r   ^r! !!Transformation methodsFor: 'accessing'!s   ^s! !!Transformation methodsFor: 'accessing'!t   ^t! !!Transformation methodsFor: 'initialize-release'!initialize   q := 0.   r := 0.   s := 0.   t := 0.   k := 0.! !!Transformation class methodsFor: 'instance creation'!new   ^super new initialize! !!Transformation class methodsFor: 'instance creation'!q: anInteger1 r: anInteger2 s: anInteger3 t: anInteger4   ^(super new) q: anInteger1 r: anInteger2 s: anInteger3 t: anInteger4! !!Transformation class methodsFor: 'instance creation'!unity   ^self q: 1 r: 0 s: 0 t: 1! !

Tests pidigits!
"  The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy

   To run: gst -QI /usr/local/share/smalltalk/gst.im process.st -a 10
"


Object subclass: #LinkedProcess
instanceVariableNames: 'message next sum'
classVariableNames: ''
poolDictionaries: ''
category: nil !

!LinkedProcess class methodsFor: 'instance creation'!

with: aLinkedProcess
   ^self new initializeWith: aLinkedProcess ! !
   
   
!LinkedProcess methodsFor: 'initialize-release'!   
   
initializeWith: aLinkedProcess
   next := aLinkedProcess.
   message := SharedQueue new.
   sum := 0 ! !
   
!LinkedProcess methodsFor: 'accessing'!        

put: aValue
   message nextPut: aValue !

take
   ^message next + 1 !

sum
   ^sum ! !
   
!LinkedProcess methodsFor: 'run'!    

runUntil: anInteger then: aSemaphore
   [ 
      next==nil "the last process checks if we're finished"
         ifTrue: [
            sum := sum + self take.
            (sum < anInteger) ifFalse: [aSemaphore signal] ] 

         ifFalse: [
            next put: self take]. 

      Processor yield "give other processes a chance to run"
   ] repeat ! !


| n join last p |
n := Smalltalk arguments first asInteger.
join := Semaphore new.

n timesRepeat: [
   p := LinkedProcess with: p.
   last isNil ifTrue: [last := p].
   [p runUntil: n then: join] fork.
].
p put: 0.

join wait.
last sum displayNl!
"  The Great Computer Language Shootout
   contributed by Paolo Bonzini
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im prodcons.st -a 100000 
"

| mutex empty full data consumed produced n join |
n := Smalltalk arguments isEmpty
   ifTrue: [ 10000 ]
   ifFalse: [ 1 max: Smalltalk arguments first asInteger ].
   
mutex := Semaphore forMutualExclusion.
empty := Semaphore new.
full := Semaphore new.
consumed := produced := 0.

join := Semaphore new.

empty signal.

[
   | i |
   i := 0.
   [
      full wait.
      mutex wait.
      i := data.
      mutex signal.
      empty signal.
      consumed := consumed + 1.
      i = n
   ] whileFalse.

   join signal.
] fork.

[
   1 to: n do: [ :i |
      empty wait.
      mutex wait.
      data := i.
      mutex signal.
      full signal.
      produced := produced + 1.
   ].

   join signal.
] fork.

join wait.
join wait.

('%1 %2' bindWith: produced with: consumed) displayNl !
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im random.st -a 900000
"

Object subclass: #RandomNumber
instanceVariableNames: 'seed scale'
classVariableNames: 'Increment Multiplier Modulus FModulus'
poolDictionaries: ''
category: nil !

!RandomNumber class methodsFor: 'initialize'!

initialize
   Increment := 29573.
   Multiplier := 3877.
   Modulus := 139968.
   FModulus := 139968.0d.     
! !

!RandomNumber class methodsFor: 'instance creation'!

to: anInteger
   ^self basicNew to: anInteger ! !
   
!RandomNumber methodsFor: 'accessing'!

next
   seed := seed * Multiplier + Increment \\ Modulus.
   ^(seed * scale) asFloatD / FModulus ! !
     
!RandomNumber methodsFor: 'private'!

to: anInteger
   seed := 42.
   scale := anInteger ! !
   
   
!Float methodsFor: 'printing'!

printStringRoundedTo: anInteger
   | n s |
   n := 0.5d * (10 raisedToInteger: anInteger negated).
   s := ((self sign < 0) ifTrue: [self - n] ifFalse: [self + n]) printString.
   ^s copyFrom: 1 to: (s indexOf: $.) + anInteger ! !  
   
   
| n random x |
n := Smalltalk arguments first asInteger.

random := RandomNumber initialize; to: 100.
n timesRepeat: [x := random next].
(x printStringRoundedTo: 9) displayNl !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

!Float methodsFor: 'benchmarking'!fib   ^self < 2.0 ifTrue: [1.0] ifFalse: [(self - 2.0) fib + (self - 1.0) fib]! !!Float methodsFor: 'benchmarking'!tak: y z: z   ^y < self       ifTrue: [((self - 1.0) tak: y z: z) tak:        ((y - 1.0) tak: z z: self) z: ((z - 1.0) tak: self z: y)]      ifFalse: [z]! !!SmallInteger methodsFor: 'benchmarking'!ack: aSmallInteger   ^self == 0      ifTrue: [aSmallInteger + 1]      ifFalse: [         aSmallInteger == 0            ifTrue: [self - 1 ack:  1]            ifFalse: [self - 1 ack: (self ack: aSmallInteger - 1)] ]! !!SmallInteger methodsFor: 'benchmarking'!fib   ^self < 2 ifTrue: [1] ifFalse: [(self - 2) fib + (self - 1) fib]! !!SmallInteger methodsFor: 'benchmarking'!tak: y z: z   ^y < self       ifTrue: [((self - 1) tak: y z: z) tak:        ((y - 1) tak: z z: self) z: ((z - 1) tak: self z: y)]      ifFalse: [z]! !!Tests class methodsFor: 'benchmarking'!recursive: nArg to: output   | n |   n := nArg.   output       nextPutAll: 'Ack(3,', n printString, '): '; print: (3 ack: n); nl;      nextPutAll: 'Fib('; print: 27.0+n digits: 1; nextPutAll: '): ';          print: (27.0d0+n) fib digits: 1; nl.   n := n - 1.   output       nextPutAll: 'Tak(', (3*n) printString, ',',         (2*n) printString, ',', n printString, '): ',           (3*n tak: 2*n z: n) printString; nl;      nextPutAll: 'Fib(3): '; print: 3 fib; nl;      nextPutAll: 'Tak(3.0,2.0,1.0): ';           print: (3.0d0 tak: 2.0d0 z: 1.0d0) digits: 1; nl! !!Tests class methodsFor: 'benchmark scripts'!recursive   self recursive: self arg to: self stdout.   ^''! !

Tests recursive!
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Paolo Bonzini *"


!Tests class methodsFor: 'benchmarking'!matchPatterns
   ^#(   'agggtaaa|tttaccct'
         '[cgt]gggtaaa|tttaccc[acg]'
         'a[act]ggtaaa|tttacc[agt]t'
         'ag[act]gtaaa|tttac[agt]ct'
         'agg[act]taaa|ttta[agt]cct'
         'aggg[acg]aaa|ttt[cgt]ccct'
         'agggt[cgt]aa|tt[acg]accct'
         'agggta[cgt]a|t[acg]taccct'
         'agggtaa[cgt]|[acg]ttaccct'
   )! !

!Tests class methodsFor: 'benchmarking'!substitutionPatterns
   ^#(   #('B' '(c|g|t)')
         #('D' '(a|g|t)')
         #('H' '(a|c|t)')
         #('K' '(g|t)')
         #('M' '(a|c)')
         #('N' '(a|c|g|t)')
         #('R' '(a|g)')
         #('S' '(c|g)')
         #('V' '(a|c|g)')
         #('W' '(a|t)')
         #('Y' '(c|t)'))! !


!Tests class methodsFor: 'benchmarking'!regexDNA: sequence to: output
   | s size1 size2 |
   size1 := sequence size.

   "* remove FASTA sequence descriptions and new-lines *"
   s := sequence copyReplacingAllRegex: '>.*\n|\n' with: ''.
   size2 := s size.

   self matchPatterns do: [:each| 
      output 
         nextPutAll: each; space; 
         print: (s occurrencesOfRegex: each); nl
      ]. 

   self substitutionPatterns do: [:each| 
      s := s copyReplacingAllRegex: each first with: each last].

   output
      nl;
      print: size1; nl; 
      print: size2; nl; 
      print: s size; nl! !!Tests class methodsFor: 'benchmark scripts'!regexdna
   self regexDNA: self stdinSpecial contents to: self stdout.
   ^'' ! !

Tests regexdna!
"*  The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Eliot Miranda and Isaac Gouy *"!


!Tests class methodsFor: 'benchmarking'!
reverseComplement: sequence named: sequenceName to: output
   | complement newline lineLength n |
   (sequenceName isNil) ifTrue: [^self].

   complement := String new: 128 withAll: $*.

   'ABCDGHKMNRSTVWY' with: 
   'TVGHCDMKNYSABWR'
      do: [:a :b|
         complement at: a asInteger put: b.
         complement at: a asLowercase asInteger put: b].

   newline := Character lf.
   lineLength := 60.
   n := sequence size.

   output nextPutAll: sequenceName; nextPut: newline.

   [n > 0] whileTrue: [ 
         1 to: ((n < lineLength) ifTrue: [n] ifFalse: [lineLength]) do:
            [:i | output nextPut: 
               (complement at: (sequence at: n - i + 1) asInteger)].
         output nextPut: newline.
         n := n - lineLength. 
      ] ! !


!Tests class methodsFor: 'benchmarking'!
readFasta: sequenceName from: input
   | prefix newline buffer description line char |
   prefix := '>',sequenceName.
   newline := Character lf.

   "* find start of particular fasta sequence *"
   [(input atEnd) or: [
         (input peek = $>) 
            ifTrue: [((line := input upTo: newline) 
               indexOfSubCollection: prefix startingAt: 1) = 1]
            ifFalse: [input skipTo: newline. false]]
      ] whileFalse.

   "* line-by-line read - it would be a lot faster to block read *"
   description := line.
   buffer := ReadWriteStream on: (String new: 1028).
   [(input atEnd) or: [(char := input peek) = $>]] whileFalse: [
      (char = $;) 
         ifTrue: [input upTo: newline] 
         ifFalse: [buffer nextPutAll: (input upTo: newline)]
      ].
   ^Association key: description value: buffer contents ! !


!Tests class methodsFor: 'benchmark scripts'!
revcomp
   | input output |
   input := self stdin.
   output := self stdout.

   #('ONE' 'TWO' 'THREE') do:
      [:sequenceName|   | fasta |
         fasta := self readFasta: sequenceName from: input.
         self reverseComplement: fasta value named: fasta key to: output.
      ].

   output flush. 
   ^'' ! !


Tests revcomp!
"  The Great Computer Language Shootout
   contributed by Paolo Bonzini 
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im reversefile < input.txt 
"

| s last out ptr |
s := (FileStream stdin bufferSize: 4096) contents.
last := s size.
out := String new: s size.
ptr := 1.

s size - 1 to: 1 by: -1 do: [ :i |
   (s at: i) == ##(Character nl) ifTrue: [
      out
         replaceFrom: ptr
         to: ptr + (last - i - 1)
         with: s
         startingAt: i + 1.

      ptr := ptr + last - i.
      last := i.
   ]
].

out
   replaceFrom: ptr
   to: out size
   with: s
   startingAt: 1.
   
stdout nextPutAll: out !
"  The Great Computer Language Shootout
   http://shootout.alioth.debian.org/ 
   
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im reversefile < input.txt 
"

((FileStream stdin bufferSize: 4096) splitAt: Character nl)
   reverseDo: [ :each | stdout nextPutAll: each; nl ]!
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im sieve.st -a 900
"

| n start stop isPrime count |
n := Smalltalk arguments first asInteger.

start := 2. stop := 8192. 
isPrime := Array new: stop.

n timesRepeat: [
   count := 0.
   start to: stop do: [:i| isPrime at: i put: true].

   start to: stop do: [:i|
      (isPrime at: i) ifTrue: [ 
         i+i to: stop by: i do: [:j| isPrime at: j put: false].
         count := count + 1.
      ].
   ].
].

Transcript show: 'Count: ', count printString; nl !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

Array subclass: #DoubleArray!
DoubleArray shape: #double!

!DoubleArray methodsFor: 'benchmarking'!
multiplyAtAv
   ^(self multiplyAv) multiplyAtv! !

!DoubleArray methodsFor: 'benchmarking'!
multiplyAtv
   | n atv sum |
   n := self size.
   atv := DoubleArray new: n.
   1 to: n do: [:i|
      sum := 0.0d.
      1 to: n do: [:j|
         sum := sum + ((j matrixA: i) * (self at: j)) ].
      atv at: i put: sum].
   ^atv! !

!DoubleArray methodsFor: 'benchmarking'!
multiplyAv
   | n av sum |
   n := self size.
   av := DoubleArray new: n.
   1 to: n do: [:i|
      sum := 0.0d.
      1 to: n do: [:j|
         sum := sum + ((i matrixA: j) * (self at: j)) ].
      av at: i put: sum].
   ^av! !


!SmallInteger methodsFor: 'benchmarking'!
matrixA: anInteger
   ^1.0d0 / ((self + anInteger - 2) * (self + anInteger - 1) /2  + self)! !


!Tests class methodsFor: 'benchmarking'!
spectralnorm: n
   | u v vBv vv |
   u := DoubleArray new: n withAll: 1.0d0.
   10 timesRepeat:
      [v := u multiplyAtAv.
       u := v multiplyAtAv].
   vBv := 0.0d0.
   vv := 0.0d0.
   1 to: n do:
      [:i |
       vBv := vBv + ((u at: i) * (v at: i)).
       vv := vv + ((v at: i) * (v at: i))].
   ^(vBv / vv) sqrt! !


!Tests class methodsFor: 'benchmark scripts'!
spectralnorm
   self stdout print: (self spectralnorm: self arg) digits: 9; nl.
   ^''! !

Tests spectralnorm!
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

!Array methodsFor: 'benchmarking'!
multiplyAtAv
   ^(self multiplyAv) multiplyAtv! !

!Array methodsFor: 'benchmarking'!
multiplyAtv
   | n atv sum |
   n := self size.
   atv := Array new: n.
   1 to: n do: [:i|
      sum := 0.0d.
      1 to: n do: [:j|
         sum := sum + ((j matrixA: i) * (self at: j)) ].
      atv at: i put: sum].
   ^atv! !

!Array methodsFor: 'benchmarking'!
multiplyAv
   | n av sum |
   n := self size.
   av := Array new: n.
   1 to: n do: [:i|
      sum := 0.0d.
      1 to: n do: [:j|
         sum := sum + ((i matrixA: j) * (self at: j)) ].
      av at: i put: sum].
   ^av! !


!SmallInteger methodsFor: 'benchmarking'!
matrixA: anInteger
   ^1.0d0 / ((self + anInteger - 2) * (self + anInteger - 1) /2  + self)! !


!Tests class methodsFor: 'benchmarking'!
spectralnorm: n
   | u v vBv vv |
   u := Array new: n withAll: 1.0d0.
   10 timesRepeat:
      [v := u multiplyAtAv.
       u := v multiplyAtAv].
   vBv := 0.0d0.
   vv := 0.0d0.
   1 to: n do:
      [:i |
       vBv := vBv + ((u at: i) * (v at: i)).
       vv := vv + ((v at: i) * (v at: i))].
   ^(vBv / vv) sqrt! !


!Tests class methodsFor: 'benchmark scripts'!
spectralnorm2
   self stdout print: (self spectralnorm: self arg) digits: 9; nl.
   ^''! !

Tests spectralnorm2!
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im spellcheck.st < input.txt
"

| dict stream newWord |
dict := Set new: 4096.
stream := (File name: 'Usr.Dict.Words') readStream.
[stream atEnd] whileFalse: [dict add: stream nextLine].
stream close.

stream := FileStream stdin bufferSize: 4096.
[stream atEnd] whileFalse: [
   (dict includes: (newWord := stream nextLine)) 
      ifFalse: [Transcript show: newWord; nl] ] !
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im strcat.st -a 40000
"

| n stream hello |
n := Smalltalk arguments first asInteger.

stream := WriteStream on: String new.
hello := 'hello', Character nl asString. 
n timesRepeat: [stream nextPutAll: hello].
stream position displayNl !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!
!Tests class methodsFor: 'benchmark scripts'!sumcol2   | s sum |   s := self stdinSpecial.   sum := 0.   [s atEnd] whileFalse: [      sum := sum + (s upTo: Character lf) asNumber].
   self stdout print: sum; nl.   ^''! !


Tests sumcol2!
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!
!Tests class methodsFor: 'benchmark scripts'!sumcol3   | s sum |
   s := self stdinSpecial.
   sum := 0.
   [s atEnd] whileFalse: [
      sum := sum + s nextLine asInteger].
   self stdout print: sum; nl.
   ^''! !


Tests sumcol3!
"  The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy 

   To run: gst -QI /usr/share/gnu-smalltalk/gst.im takfp.st -a 7
"

!Float methodsFor: 'shootout'!

takfp: aFloatY z: aFloatZ
   ^aFloatY < self 
      ifTrue: [ 
         ((self - 1.0) takfp: aFloatY z: aFloatZ)      
            takfp: ((aFloatY - 1.0) takfp: aFloatZ z: self) 
            z: ((aFloatZ - 1.0) takfp: self z: aFloatY)
         ]
      ifFalse: [aFloatZ] ! !


| n |
n := Smalltalk arguments first asInteger.

(((n * 3.0) takfp: (n * 2.0) z: (n * 1.0)) asScaledDecimal: 1) displayNl !




"
  vim: ts=4 ft=st
"
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    adapted from a program by Paolo Bonzini 
    contributed by Isaac Gouy *"!

Object subclass: #Thread   instanceVariableNames: 'name nextThread token semaphore done'   classVariableNames: ''   poolDictionaries: ''   category: 'BenchmarksGame' !

!Thread methodsFor: 'accessing'!
name: anInteger   name := anInteger ! !

!Thread methodsFor: 'accessing'!
nextThread: aThread   nextThread := aThread ! !

!Thread methodsFor: 'accessing'!tokenNotDone   semaphore wait.   ^token > 0 ! !

!Thread methodsFor: 'accessing'!semaphore: aSemaphore   semaphore := aSemaphore ! !

!Thread methodsFor: 'accessing'!done: aSemaphore   done := aSemaphore ! !

!Thread methodsFor: 'accessing'!
fork   [ self run ] fork ! !

!Thread methodsFor: 'accessing'!
run 
   [ self tokenNotDone ] whileTrue: [ nextThread takeToken: token - 1 ].
   Tests stdout print: name; nl.
   done signal ! !

!Thread methodsFor: 'accessing'!takeToken: x   token := x.   semaphore signal ! !


!Thread class methodsFor: 'instance creation'!new
   ^self basicNew semaphore: Semaphore new ! !

!Thread class methodsFor: 'instance creation'!named: anInteger next: aThread done: aSemaphore   ^self new name: anInteger; nextThread: aThread; done: aSemaphore; fork ! !


!Tests class methodsFor: 'benchmarking'!threadRing: aSemaphore   | first last |
   503 to: 1 by: -1 do: [:i| 
      first := Thread named: i next: first done: aSemaphore.
      last isNil ifTrue: [ last := first ].
   ].
   last nextThread: first.   ^first ! !

!Tests class methodsFor: 'benchmarking'!threadring
   | done |   (self threadRing: (done := Semaphore new)) takeToken: self arg.
   done wait.   ^''! !



Tests threadring!
"  The Great Computer Language Shootout
   contributed by Isaac Gouy
   
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im wc.st < input.txt
"

| newline space tab nl nw nc insideWord stream ch |

newline  := Character nl.
space := Character space.
tab := Character tab.

nl := nw := nc := 0.
insideWord := false.
stream := FileStream stdin bufferSize: 4096.

[(ch := stream next) notNil] whileTrue: [
   nc := nc + 1.
   ch = newline ifTrue: [nl := nl + 1].
   (ch = space or: [ch = newline or: [ch = tab]])
      ifTrue: [insideWord := false]
      ifFalse: [
         insideWord ifFalse: [
            insideWord := true. 
            nw := nw + 1
         ].
      ].
].      

Transcript 
   show: nl displayString; space;
   show: nw displayString; space;
   show: nc displayString; nl !
   
"  The Great Computer Language Shootout
   contributed by Isaac Gouy & Paolo Bonzini
    
   To run: gst -QI /usr/share/gnu-smalltalk/gst.im wordfreq.st < input.txt
"

!Bag methodsFor: 'extracting items'!

sortedByValueAndKey
   | assocs |
   assocs := (SortedCollection new: contents size) sortBlock: [:a :b| 
   a value = b value ifTrue: [a key > b key] ifFalse: [a value > b value] ].

   contents keysAndValuesDo: [:key :value| assocs add: key -> value].
   ^assocs ! !
   
   
| stream wordCounts |
stream := FileStream stdin bufferSize: 4096.
wordCounts := Bag new.

[stdin atEnd] whileFalse: [
   (stream nextLine collect: [:each|
      each isLetter ifTrue: [each asLowercase] ifFalse: [$ ]])
         subStrings do: [:word| wordCounts add: word]
].

wordCounts sortedByValueAndKey do: [:each| | number |
   number := each value printString.
   (7 - number size) timesRepeat: [stdout nextPut: $ ]. 
   stdout nextPutAll: number; nextPutAll: ' '; nextPutAll: each key; nl.
] !
