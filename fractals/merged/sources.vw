"*  The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy *"

!Shootout.Tests class methodsFor: 'benchmarking'!

ackermann
   | n |
   n := CEnvironment argv first asNumber.
   ^'Ack(3,', n printString, '): ', (3 ackermann: n) printString withNl ! !


!Core.SmallInteger methodsFor: 'computer language shootout'!

ackermann: anInteger
   ^self = 0
      ifTrue: [anInteger + 1]
      ifFalse: [
         anInteger = 0
            ifTrue: [self - 1 ackermann:  1]
            ifFalse: [self - 1 ackermann: (self ackermann: anInteger - 1)] ] ! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!


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
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Carlo Teixeira *"!

Object subclass: #Tests
   instanceVariableNames: ''
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

Tests class
   instanceVariableNames: ''!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!Tests class methodsFor: 'benchmarking-scripts'!

chameneosredux2
   Mall runBenchMark: self arg on: self stdout.
   ^''! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


Object subclass: #Pair
   instanceVariableNames: 'partner me sema '
   classVariableNames: ''
   poolDictionaries: ''
   category: '(none)'!

Pair class
   instanceVariableNames: ''!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!Pair class methodsFor: 'instance creation'!

new
   "Answer a newly created and initialized instance."
   ^super new initialize.!

with: me 
   "Answer a newly created and initialized instance."
self halt.
   ^super new initialize me: me! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


Pair comment:
''!

!Pair methodsFor: 'accessing'!

me
   ^me!

me: anObject
   me := anObject!

partner
   ^partner!

partner: anObject
   partner := anObject! !

!Pair methodsFor: 'initialize-release'!

initialize
   "Initialize a newly created instance. This method must answer the receiver."

   partner := nil.
   me := nil.
   sema := Semaphore new.
   ^self!

release
partner:=nil.!

signal
   sema signal!

wait
   sema wait! !

Object subclass: #Mall
   instanceVariableNames: 'guard maxRendezvous open process queue cache pairCache '
   classVariableNames: 'Units '
   poolDictionaries: ''
   category: 'chameleon'!

Mall class
   instanceVariableNames: ''!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!Mall class methodsFor: 'printing'!

generateReportFor: creatures printOn: stream 
   | sum |
   sum := creatures inject: 0 into: [:accum :each | accum + each creaturesMet].
   creatures do: 
         [:aCreature | 
         aCreature creaturesMet printOn: stream.
         stream
            space;
            nextPutAll: (self units at: aCreature selfMet + 1);
            nl].
   stream space.
   sum printString 
      do: [:el | stream nextPutAll: (self units at: el digitValue + 1)]
      separatedBy: [stream space].
   ^stream!

generateReportForColours: colours printOn: stream 
   stream space.
   colours do: [:colour | colour printOn: stream] separatedBy: [stream space].
   ^stream! !

!Mall class methodsFor: 'initialize-release'!

createAllowing: maxRendezvous 
   "Private"

   ^self basicNew initialize maxRendezvous: maxRendezvous!

createCreaturesWith: aCollectionOfColours 
   "Private"

   | aName |
   aName := 0.
   ^aCollectionOfColours collect: 
         [:aColour | 
         aName := aName + 1.
         Creature withName: aName colour: aColour]!

initialize
   "self initialize"

   Units := #('zero' 'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine')!

new
   ^self shouldNotImplement!

openMallWith: aCollectionOfColours forNumberOfMeets: aNumber 
   | mall creatures guard |
   mall := self createAllowing: aNumber.
   mall run.
   creatures := self createCreaturesWith: aCollectionOfColours.
   guard := Semaphore new.
   self 
      openMall: mall
      forCreatures: creatures
      usingGuard: guard.
   self 
      waitForClosingOfMall: mall
      withCreatures: creatures
      usingGuard: guard.
   ^creatures! !

!Mall class methodsFor: 'private'!

openMall: aMall forCreatures: creatures usingGuard: sema 
   | processes |
   processes := creatures 
            collect: [:aCreature | 
               [aCreature visitMall: aMall.
               sema signal] newProcess].
   processes do: 
         [:proc | 
         proc priority: Processor userBackgroundPriority.
         proc resume]!

waitForClosingOfMall: aMall withCreatures: creatures usingGuard: guard 
   creatures size timesRepeat: [guard wait].
   aMall close! !

!Mall class methodsFor: 'accessing'!

units
   ^Units! !

!Mall class methodsFor: 'public'!

runBenchMark: number on: anOutputStream 
   "self runBenchMark: 60000 on: Transcript."

   | firstTestColours secondTestColours blue red yellow creatures |
   blue := ChameneosColour blue.
   red := ChameneosColour red.
   yellow := ChameneosColour yellow.
   firstTestColours := Array 
            with: blue
            with: red
            with: yellow.
   secondTestColours := (OrderedCollection new)
            add: blue;
            add: red;
            add: yellow;
            add: red;
            add: yellow;
            add: blue;
            add: red;
            add: yellow;
            add: red;
            add: blue;
            yourself.
   (ChameneosColour generateReportOfColoursOn: anOutputStream) nl.
   (self generateReportForColours: firstTestColours printOn: anOutputStream) 
      nl.
   creatures := Mall openMallWith: firstTestColours forNumberOfMeets: number.
   (self generateReportFor: creatures printOn: anOutputStream)
      nl;
      nl.
   (self generateReportForColours: secondTestColours printOn: anOutputStream) 
      nl.
   creatures := Mall openMallWith: secondTestColours forNumberOfMeets: number.
   (self generateReportFor: creatures printOn: anOutputStream)
      nl;
      nl! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


Mall comment:
''!

!Mall methodsFor: 'accessing'!

maxRendezvous: max 
   maxRendezvous := max! !

!Mall methodsFor: 'private'!

obtainPair
   ^cache removeFirst!

processVisitors
   [open] whileTrue: 
         [1 to: maxRendezvous
            do: 
               [:x | 
               | first second |
               first := queue next.
               second := queue next.
               self setPartnersOn: first and: second.
               first signal.
               second signal].
         [queue isEmpty] whileFalse: [queue next signal]].
   process terminate.
   process := nil!

releasePair: pair 
   pair release.
   cache addFirst: pair!

setPartnersOn: first and: second
   first partner: second me.
   second partner: first me.
!

shutdown
   [queue isEmpty] whileFalse: [queue next signal].
   process terminate.
   process := nil! !

!Mall methodsFor: 'initialize-release'!

initialize
   guard := Semaphore forMutualExclusion.
   queue := SharedQueue new.
   cache := OrderedCollection new.
   1 to: 10 do: [:x | cache add: Pair new]!

run
   open := true.
   process ifNil: 
         [process := [self processVisitors] newProcess.
         process priority: Processor userBackgroundPriority -1 ].
   process resume! !

!Mall methodsFor: 'controlling'!

close
   open := false!

visitWith: aChameneos 
   | pair partner |
   pair := self obtainPair.
   pair me: aChameneos.
   queue nextPut: pair.
   pair wait.
   partner := pair partner.
   self releasePair: pair.
   ^partner! !

Mall initialize!

Object subclass: #Creature
   instanceVariableNames: 'creatureName colour selfMet creaturesMet '
   classVariableNames: ''
   poolDictionaries: ''
   category: 'chameleon'!

Creature class
   instanceVariableNames: ''!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!Creature class methodsFor: 'initialize-release'!

withName: aName colour: aColour 
   ^(Creature new initialize)
      name: aName;
      colour: aColour! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!Creature methodsFor: 'accessing'!

colour
   ^colour!

colour: anObject 
   colour := anObject!

creaturesMet
   ^creaturesMet!

creaturesMet: anObject 
   creaturesMet := anObject!

name
   ^creatureName!

name: anObject 
   creatureName := anObject!

selfMet
   ^selfMet!

selfMet: anObject 
   ^selfMet := anObject! !

!Creature methodsFor: 'initialize-release'!

initialize
   selfMet := 0.
   creaturesMet := 0! !

!Creature methodsFor: 'controlling'!

visitMall: mall 
   
   [| partner |
   partner := mall visitWith: self.
   partner ifNotNil: 
         [colour := colour complementaryColourFor: partner colour.
         self == partner ifTrue: [selfMet := selfMet + 1].
         creaturesMet := creaturesMet + 1].
   partner isNil] 
         whileFalse! !

Object subclass: #ChameneosColour
   instanceVariableNames: 'color '
   classVariableNames: 'Blue Red Yellow '
   poolDictionaries: ''
   category: 'chameleon'!

ChameneosColour class
   instanceVariableNames: ''!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!ChameneosColour class methodsFor: 'accessing'!

blue
   ^Blue!

blue: anObject
   Blue := anObject!

red
   ^Red!

red: anObject
   Red := anObject!

yellow
   ^Yellow!

yellow: anObject
   Yellow := anObject! !

!ChameneosColour class methodsFor: 'initialize-release'!

createBlue
   "comment stating purpose of message"

   ^super new color: #blue!

createRed
   "comment stating purpose of message"

   ^super new color: #red!

createYellow
   "comment stating purpose of message"

   ^super new color: #yellow!

initialize
   "self initialize"

   Red := self createRed.
   Blue := self createBlue.
   Yellow := self createYellow! !

!ChameneosColour class methodsFor: 'printing'!

generateReportOfColoursOn: readOut 
   | colours |
   colours := Array 
            with: Blue
            with: Red
            with: Yellow.
   colours do: 
         [:aColour | 
         colours do: 
               [:anotherColour | 
               aColour printOn: readOut.
               readOut nextPutAll: ' + '.
               anotherColour printOn: readOut.
               readOut nextPutAll: ' -> '.
               (aColour complementaryColourFor: anotherColour) printOn: readOut.
               readOut nl]].
   ^readOut! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!ChameneosColour methodsFor: 'as yet unclassified'!

complementaryColourFor: aChameneosColour 
   "determine the complementary colour defined as..."

   self == aChameneosColour ifTrue: [^self].
   self isBlue 
      ifTrue: 
         [aChameneosColour isRed 
            ifTrue: [^self class yellow]
            ifFalse: [^self class red]].
   self isRed 
      ifTrue: 
         [aChameneosColour isBlue 
            ifTrue: [^self class yellow]
            ifFalse: [^self class blue]].
   aChameneosColour isBlue 
      ifTrue: [^self class red]
      ifFalse: [^self class blue]! !

!ChameneosColour methodsFor: 'testing'!

hasSameColorAs: aChameneos 
   ^self color == aChameneos color!

isBlue
   ^self == self class blue!

isRed
   ^self == self class red!

isYellow
   ^self == self class yellow! !

!ChameneosColour methodsFor: 'accessing'!

color
   ^color!

color: aColor 
   color := aColor! !

!ChameneosColour methodsFor: 'printing'!

printOn: aStream 
   aStream nextPutAll: self color! !

ChameneosColour initialize!
"* The Computer Language Benchmarks Game
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
"* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Paolo Bonzini 
   modified by Isaac Gouy *"!


Object subclass: #PermGeneratorRedux
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



!PermGeneratorRedux methodsFor: 'initialize-release'!
initialize: size
   perm := (1 to: size) asArray.
   timesRotated := Array new: size withAll: 0.
   atEnd := false.
   permCount := 0! !



!PermGeneratorRedux methodsFor: 'initialize-release'!
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



!PermGeneratorRedux methodsFor: 'benchmarks'!
maxPfannkuchenTo: output
   | max permutation checksum permCount flipsCount |
   max := 0.
   permCount := 0.
   checksum := 0.
   [self atEnd] whileFalse:
      [permutation := self next.
      permCount := permCount + 1.
      (permCount = 1048576) ifTrue: [permCount := 0].
      flipsCount := permutation pfannkuchen.
      checksum := permCount odd ifTrue: [checksum+flipsCount] ifFalse: [checksum-flipsCount].
      max := max max: flipsCount].
   output print: checksum; nl.
   ^max! !



!PermGeneratorRedux methodsFor: 'accessing'!
atEnd
   ^atEnd! !



!PermGeneratorRedux methodsFor: 'accessing'!
next
   | result |
   result := perm copy.
   self makeNext.
   ^result! !



!PermGeneratorRedux class methodsFor: 'instance creation'!
new: size
   ^self new
      initialize: size;
      yourself! !



!Tests class methodsFor: 'benchmarking'!
fannkuchRedux: n to: output
   ^(PermGeneratorRedux new: n) maxPfannkuchenTo: output! !



!Tests class methodsFor: 'benchmark scripts'!
fannkuchredux
   | n f |
   n := self arg.
   f := self fannkuchRedux: n to: self stdout.
   self stdout
      nextPutAll: 'Pfannkuchen(', n printString, ') = ';
      print: f; nl.
   ^''! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Object subclass: #RandomNumber

ReadStream subclass: #RepeatStream

RepeatStream subclass: #RandomStream


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

fasta


"  The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy"!

!Shootout.Tests class methodsFor: 'benchmarking'!

harmonic
   | n partialSum |
   n := CEnvironment argv first asNumber.
   partialSum := 0.0d.
   1 to: n do: [:i| partialSum := partialSum + (1.0d/i)].
   ^((partialSum asFixedPoint: 9) printString copyWithout: $s) withNl ! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!

!Tests class methodsFor: 'benchmark scripts'!
hello
   self stdout nextPutAll: 'hello world'; nl.



Object subclass: #Tests


!Tests class methodsFor: 'platform'!

!Tests class methodsFor: 'platform'!


!Tests class methodsFor: 'platform'!
      (ExternalConnection ioAccessor: (UnixDiskFileAccessor new handle: 0))! !




!Tests class methodsFor: 'platform'!
      (ExternalConnection ioAccessor: (UnixDiskFileAccessor new handle: 1))! !


!Stream methodsFor: 'platform'!


!Stream methodsFor: 'platform'!
      ((number asFixedPoint: decimalPlaces) printString copyWithout: $s)! !


!Stream methodsFor: 'platform'!


!Integer methodsFor: 'platform'!

"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Andres Valloud *"!


!SequenceableCollection methodsFor: 'computer language shootout'!
substringFrequencies3a: aLength using: aDictionary

   | buffer |
   buffer := String new: aLength.
   1 to: self size - aLength + 1 do:
      [:i |
         | answer |
         buffer replaceFrom: 1 to: aLength with: self startingAt: i.
         answer := aDictionary
            at: buffer
            putValueOf: [:sum | sum + 1]
            ifAbsentPutValueOf: 1.
         answer = 1 ifTrue: [buffer := String new: aLength].
      ].
   ^aDictionary! !


!Dictionary methodsFor: 'computer language shootout'!
at: key putValueOf: putBlock ifAbsentPutValueOf: absentBlock
   "* Set the value at key to be the value of evaluating putBlock
    with the existing value. If key is not found, create a new
    entry for key and set is value to the evaluation of
    absentBlock. Answer the result of evaluating either block. *"

   | index element anObject |
   key == nil ifTrue:
      [^self
         subscriptBoundsErrorFor: #at:putValueOf:ifAbsentPutValueOf:
         index: key
         value: absentBlock value].
   index := self findKeyOrNil: key.
   element := self basicAt: index.
   element == nil
      ifTrue: [self atNewIndex: index put:
         (self createKey: key value: (anObject := absentBlock value))]
      ifFalse: [element value: (anObject := putBlock value: element value)].
   ^anObject ! !


!Tests class methodsFor: 'benchmarking'!
readFasta: sequenceName from: input
   | prefix newline buffer description line char |
   prefix := '>',sequenceName.
   newline := Character cr.

   "* find start of particular fasta sequence *"
   [(input atEnd) or: [
         (input peek = $>)
            ifTrue: [((line := input upTo: newline)
               indexOfSubCollection: prefix startingAt: 1) = 1]
            ifFalse: [input skipThrough: newline. false]]
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
knucleotide3bFrom: input to: output
   "Same as 3a, but presize the frequency dictionary better"

   | sequence writeFrequencies writeCount |

   sequence := (self readFasta: 'THREE' from: input) value asUppercase.

   writeFrequencies :=
      [:k | | frequencies count |
      frequencies := SortedCollection sortBlock: [:a :b|
      (a value = b value) ifTrue: [b key < a key] ifFalse: [b value < a value]].

   count := 0.0.
   (sequence substringFrequencies3a: k using: (Dictionary new: ((4 raisedToInteger: k) min: sequence size) * 3 // 2))
      associationsDo: [:each|
         frequencies add: each. count := count + each value].

   frequencies do: [:each | | percentage |
      percentage := (each value / count) * 100.0.
      output
         nextPutAll: each key; space;
         print: percentage digits: 3; nl]].

   writeCount := [:nucleotideFragment | | frequencies count |
      frequencies := sequence substringFrequencies3a: nucleotideFragment size
         using: (Dictionary new: ((4 raisedToInteger: nucleotideFragment size) min: sequence size) * 3 // 2).
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
knucleotide

   self knucleotide3bFrom: self stdinSpecial to: self stdout.
   ^''! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Andres Valloud *"!


!SequenceableCollection methodsFor: 'computer language shootout'!
substringFrequencies3a: aLength using: aDictionary

   | buffer |
   buffer := String new: aLength.
   1 to: self size - aLength + 1 do:
      [:i |
         | answer |
         buffer replaceFrom: 1 to: aLength with: self startingAt: i.
         answer := aDictionary
            at: buffer
            putValueOf: [:sum | sum + 1]
            ifAbsentPutValueOf: 1.
         answer = 1 ifTrue: [buffer := String new: aLength].
      ].
   ^aDictionary! !


!Dictionary methodsFor: 'computer language shootout'!
at: key putValueOf: putBlock ifAbsentPutValueOf: absentBlock
   "* Set the value at key to be the value of evaluating putBlock
    with the existing value. If key is not found, create a new
    entry for key and set is value to the evaluation of
    absentBlock. Answer the result of evaluating either block. *"

   | index element anObject |
   key == nil ifTrue:
      [^self
         subscriptBoundsErrorFor: #at:putValueOf:ifAbsentPutValueOf:
         index: key
         value: absentBlock value].
   index := self findKeyOrNil: key.
   element := self basicAt: index.
   element == nil
      ifTrue: [self atNewIndex: index put:
         (self createKey: key value: (anObject := absentBlock value))]
      ifFalse: [element value: (anObject := putBlock value: element value)].
   ^anObject ! !


!Tests class methodsFor: 'benchmarking'!
readFasta: sequenceName from: input
   | prefix newline buffer description line char |
   prefix := '>',sequenceName.
   newline := Character cr.

   "* find start of particular fasta sequence *"
   [(input atEnd) or: [
         (input peek = $>)
            ifTrue: [((line := input upTo: newline)
               indexOfSubCollection: prefix startingAt: 1) = 1]
            ifFalse: [input skipThrough: newline. false]]
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
knucleotide4From: input to: output
   "Same as 3a, but presize the frequency dictionary better"

   | sequence writeFrequencies writeCount maxDictionarySize |

   ObjectMemory currentMemoryPolicy
      memoryUpperBound: (640 bitShift: 20);
      growthRegimeUpperBound: (512 bitShift: 20);
      freeMemoryUpperBound: (64 bitShift: 20).

   sequence := (self readFasta: 'THREE' from: input) value asUppercase.

   maxDictionarySize :=  sequence size min: 80000000.

   writeFrequencies :=
      [:k | | frequencies count |
      frequencies := SortedCollection sortBlock: [:a :b|
      (a value = b value) ifTrue: [b key < a key] ifFalse: [b value < a value]].

   count := 0.0.
   (sequence substringFrequencies3a: k using: (Dictionary new: ((4 raisedToInteger: k) * 3 / 2 min: maxDictionarySize)))
      associationsDo: [:each|
         frequencies add: each. count := count + each value].

   frequencies do: [:each | | percentage |
      percentage := (each value / count) * 100.0.
      output
         nextPutAll: each key; space;
         print: percentage digits: 3; nl]].

   writeCount := [:nucleotideFragment | | frequencies count |
      frequencies := sequence substringFrequencies3a: nucleotideFragment size
         using: (Dictionary new: ((4 raisedToInteger: nucleotideFragment size) min: sequence size) * 3 // 2).
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
knucleotide4

   self knucleotide4From: self stdinSpecial to: self stdout.
   ^''! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

!Tests class methodsFor: 'benchmarking'!
   m := 50.

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
               output nextPut: bits.
               bits := 0.
           ]
       ]. 
       (extent bitAnd: 7) == 0 ifFalse: [
           bits := bits bitShift: 8 - (extent bitAnd: 7).
           output nextPut: bits.
       ]
   ]! !


      binary.
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

Object subclass: #Consumer

Consumer subclass: #ProducerConsumer

!Consumer methodsFor: 'accessing'!
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!

Object subclass: #Body

Object subclass: #NBodySystem
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!
         print: m paddedTo: 8;
         print: (self nsieve: m using: arrayClass) paddedTo: 9; nl
      ]! !
"* The Computer Language Benchmarks Game
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!





         print: m paddedTo: 8;
         print: (self nsieve: m using: arrayClass) paddedTo: 9; nl
      ]! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

!Tests class methodsFor: 'benchmarking'!

   self print: a1 withName: '(2/3)^k' to: output.
   self print: a2 withName: 'k^-0.5' to: output.
   self print: a3 withName: '1/k(k+1)' to: output.
   self print: a4 withName: 'Flint Hills' to: output.
   self print: a5 withName: 'Cookson Hills' to: output.
   self print: a6 withName: 'Harmonic' to: output.
   self print: a7 withName: 'Riemann Zeta' to: output.
   self print: a8 withName: 'Alternating Harmonic' to: output.
   self print: a9 withName: 'Gregory' to: output.

!Tests class methodsFor: 'benchmarking'!
print: number withName: name to: output
   output print: number digits: 9; tab; nextPutAll: name; nl! !


"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Object subclass: #PiDigitSpigot

Object subclass: #Transformation


"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

Stream subclass: #PiDigitSpigot
    instanceVariableNames: 'numer accum denom k'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Shootout'!
    ^false!
    | digit |
    [ self step. (digit := self extract) isNil ] whileTrue.
    self eliminate: digit.
    ^digit! !
    numer := denom := 1.
    k := accum := 0.!
    | tmp |
    numer > accum ifTrue: [ ^nil ].
    tmp := numer + numer + numer + accum.
    ^tmp \\ denom + numer < denom ifTrue: [ tmp // denom ] ifFalse: [ nil ]!
    accum := accum - (denom * digit).
    accum := accum * 10.
    numer := numer * 10!
    | y2 |
    k := k + 1.
    y2 := k * 2 + 1.
    accum := (numer + numer + accum) * y2.
    numer := numer * k.
    denom := denom * y2.! !
   | n i pidigits |
   n := v.
   i := 0.
   pidigits := PiDigitSpigot new.
   [n > 0] whileTrue:
      [n < width
         ifTrue:
            [n timesRepeat: [output nextPut: (Character digitValue: pidigits next)].
            n to: width do: [:each | output space].
            i := i + n]
         ifFalse:
            [width timesRepeat: [output nextPut: (Character digitValue: pidigits next)].
            i := i + width].

      output tab; nextPut: $:; print: i; nl.

      n := n - width]! !
   self pidigits3To: self arg width: 10 to: self stdout.
   ^''! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini 
    modified by Andres Valloud *"!

Stream subclass: #PiDigitSpigot
    instanceVariableNames: 'numer accum denom k'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Shootout'!

!PiDigitSpigot methodsFor: 'stream'!
atEnd
    ^false!

next
    | digit |
    [ self step. (digit := self extract) isNil ] whileTrue.
    self eliminate: digit.
    ^digit! !

!PiDigitSpigot methodsFor: 'private'!
initialize
    numer := denom := 1.
    k := accum := 0.!

extract
    | tmp |
    numer > accum ifTrue: [^nil].
    tmp := numer + numer + numer + accum.
    tmp \\ denom >= (denom - numer) ifTrue: [^nil].
    ^tmp // denom!

eliminate: digit
    accum := accum - (denom * digit).
    accum := accum * 10.
    numer := numer * 10!

step
    | y2 |
    k := k + 1.
    y2 := k * 2 + 1.
    accum := (numer + numer + accum) * y2.
    numer := numer * k.
    denom := denom * y2.! !


!PiDigitSpigot class methodsFor: 'instance creation'!
new
   ^super basicNew initialize! !


!Tests class methodsFor: 'benchmarking'!
pidigitsTo: v width: width to: output
   | n i pidigits |
   n := v.
   i := 0.
   pidigits := PiDigitSpigot new.
   [n > 0] whileTrue:
      [n < width
         ifTrue:
            [n timesRepeat: [output nextPut: (Character digitValue: pidigits next)].
            n to: width do: [:each | output space].
            i := i + n]
         ifFalse:
            [width timesRepeat: [output nextPut: (Character digitValue: pidigits next)].
            i := i + width].

      output tab; nextPut: $:; print: i; nl.

      n := n - width]! !


!Tests class methodsFor: 'benchmark scripts'!
pidigits4
   self pidigitsTo: self arg width: 10 to: self stdout.
   ^''! !
"  The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy"!

!Shootout.Tests class methodsFor: 'benchmarking'!

random
   | n random answer |
   n := CEnvironment argv first asNumber.
   random := RandomNumber to: 100.
   n timesRepeat: [answer := random next].
   ^(answer asStringWith: 9) withNl ! !


Smalltalk.Shootout defineClass: #RandomNumber
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: 'seed scale '
	classInstanceVariableNames: ''
	imports: ''
	category: 'Shootout'!

Shootout.RandomNumber defineSharedVariable: #Modulus
	private: false
	constant: false
	category: 'computer language shootout'
	initializer: '139968'!

#{Shootout.RandomNumber.Modulus} initialize!

Shootout.RandomNumber defineSharedVariable: #FModulus
	private: false
	constant: false
	category: 'computer language shootout'
	initializer: '139968.0d'!

#{Shootout.RandomNumber.FModulus} initialize!

Shootout.RandomNumber defineSharedVariable: #Multiplier
	private: false
	constant: false
	category: 'computer language shootout'
	initializer: '3877'!

#{Shootout.RandomNumber.Multiplier} initialize!

Shootout.RandomNumber defineSharedVariable: #Increment
	private: false
	constant: false
	category: 'computer language shootout'
	initializer: '29573'!

#{Shootout.RandomNumber.Increment} initialize!


!Shootout.RandomNumber class methodsFor: 'instance creation'!

to: anInteger
   ^self basicNew to: anInteger ! !


!Shootout.RandomNumber methodsFor: 'accessing'!

next
	seed := (seed * Multiplier + Increment) \\ Modulus.
	^(seed * scale) asDouble / FModulus ! !

!Shootout.RandomNumber methodsFor: 'private'!

to: anInteger
   seed := 42.
   scale := anInteger ! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

!Double methodsFor: 'benchmarking'!
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Eliot Miranda *"!


!Tests class methodsFor: 'benchmarking'!
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

!Tests class methodsFor: 'benchmarking'!
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


!Tests class methodsFor: 'benchmarking'!
   | s size1 size2 translation |
   size1 := sequence size.

   "* remove FASTA sequence descriptions and new-lines *"
   s := sequence copyWithRegex: '>[^\r]*\r|\r' matchesReplacedWith: ''.
   size2 := s size.

   "* regex match *"
   self matchPatterns do: [:each| 
      output 
         nextPutAll: each; space; 
         print: (s occurrencesOfRegex: each); nl
      ]. 

   "* regex substitution *"
   translation := Dictionary new.
   self substitutionPatterns do: [:each| 
      translation at: each first put: each last].

   s := s copyWithRegex: '[', 
         (translation keys asArray fold: [:a :b| a, b]), ']'
      matchesTranslatedUsing: [:l| translation at: l].

   output
      nl;
      print: size1; nl; 
      print: size2; nl; 
      print: s size; nl! !
   self regexDNA: self stdinSpecial contents to: self stdout.
   ^'' ! !
"* The Computer Language Benchmarks Game
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
   newline := Character cr.

   "* find start of particular fasta sequence *"
   [(input atEnd) or: [
         (input peek = $>) 
            ifTrue: [((line := input upTo: newline) 
               indexOfSubCollection: prefix startingAt: 1) = 1]
            ifFalse: [input skipThrough: newline. false]]
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
   input := self stdinSpecial.
   output := self stdoutSpecial.

   #('ONE' 'TWO' 'THREE') do:
      [:sequenceName|   | fasta |
         fasta := self readFasta: sequenceName from: input.
         self reverseComplement: fasta value named: fasta key to: output.
      ].

   output flush. 
   ^'' ! !
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
      sum := 0.0d0.
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
      sum := 0.0d0.
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
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!




"  The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy"!

!Shootout.Tests class methodsFor: 'benchmarking'!

takfp
   | n |
   n := CEnvironment argv first asNumber.
   ^(((n * 3.0) takfp: (n * 2.0) z: (n * 1.0)) asStringWith: 1) withNl ! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    adapted from a program by Paolo Bonzini
    contributed by Isaac Gouy 
    modified by Carlo Teixeira *"!

!Tests class methodsFor: 'benchmarking'!

threadring2
   | done |
   (self threadRing: (done := Semaphore new)) takeToken: self arg.
   done wait.
   ^''!

threadRing: aSemaphore
   | first last |
   503 to: 1 by: -1 do: [:i|
      first := Thread named: i next: first done: aSemaphore.
      last isNil ifTrue: [ last:=first. ].
   ].
   last nextThread: first.
   ^first! !


Object subclass: #Thread
   instanceVariableNames: 'name nextThread token semaphore done '
   classVariableNames: ''
   poolDictionaries: ''
   category: 'BenchmarksGame'!

Thread class
   instanceVariableNames: ''!


!Thread class methodsFor: 'instance creation'!

named: anInteger next: aThread done: aSemaphore
   ^self new name: anInteger; nextThread: aThread; done: aSemaphore; fork !

new
   ^self basicNew semaphore: Semaphore new ! !


!Thread methodsFor: 'accessing'!

done: aSemaphore
   done := aSemaphore !

fork
   [ self run ] forkAt: Processor userBackgroundPriority.!

name: anInteger
   name := anInteger !

nextThread: aThread
   nextThread := aThread !

run
   [semaphore wait.
   0==token] whileFalse: [nextThread takeToken: token - 1].
   name printOn: Tests stdout.
   Tests stdout cr.
   done signal!

semaphore: aSemaphore
   semaphore := aSemaphore !

takeToken: x
   token := x.
   semaphore signal ! !