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

Object subclass: #TreeNode   instanceVariableNames: 'left right item'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!Tests class methodsFor: 'benchmarking'!binarytrees: n to: output   | minDepth maxDepth stretchDepth check longLivedTree iterations |   minDepth := 4.   maxDepth := minDepth + 2 max: n.   stretchDepth := maxDepth + 1.   check := (TreeNode bottomUpTree: 0 depth: stretchDepth) itemCheck.   output      nextPutAll: 'stretch tree of depth '; print: stretchDepth; tab;      nextPutAll: ' check: '; print: check; nl.   longLivedTree := TreeNode bottomUpTree: 0 depth: maxDepth.   minDepth to: maxDepth by: 2 do: [:depth|      iterations := 1 bitShift: maxDepth - depth + minDepth.      check := 0.      1 to: iterations do: [:i|         check := check + (TreeNode bottomUpTree: i depth: depth) itemCheck.         check := check + (TreeNode bottomUpTree: -1*i depth: depth) itemCheck         ].      output         print:  (2*iterations); tab;         nextPutAll: ' trees of depth '; print: depth; tab;         nextPutAll: ' check: '; print: check; nl      ].   output      nextPutAll: 'long lived tree of depth '; print: maxDepth; tab;      nextPutAll: ' check: '; print: longLivedTree itemCheck; nl! !!Tests class methodsFor: 'benchmark scripts'!binarytrees   self binarytrees: self arg to: self stdout.   ^''! !!TreeNode methodsFor: 'initialize-release'!left: leftChild right: rightChild item: anItem   left := leftChild.   right := rightChild.   item := anItem! !!TreeNode methodsFor: 'accessing'!itemCheck   ^left isNil       ifTrue: [item] ifFalse: [item + (left itemCheck - right itemCheck)]! !!TreeNode class methodsFor: 'instance creation'!bottomUpTree: anItem depth: anInteger   ^(anInteger > 0)       ifTrue: [         self             left: (self bottomUpTree: 2*anItem - 1 depth: anInteger - 1)             right: (self bottomUpTree: 2*anItem depth: anInteger - 1)              item: anItem         ]      ifFalse: [self left: nil right: nil item: anItem]! !!TreeNode class methodsFor: 'instance creation'!left: leftChild right: rightChild item: anItem         ^(super new) left: leftChild right: rightChild item: anItem! !
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

Object subclass: #RandomNumber   instanceVariableNames: 'seed scale'   classVariableNames: 'FModulus Increment Modulus Multiplier'   poolDictionaries: ''   category: 'Shootout'!

ReadStream subclass: #RepeatStream   instanceVariableNames: 'repeatPtr repeatLimit'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

RepeatStream subclass: #RandomStream   instanceVariableNames: 'random percentages'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!RandomNumber methodsFor: 'private'!to: anInteger   seed := 42.   scale := anInteger! !!RandomNumber methodsFor: 'accessing'!next   seed := (seed * Multiplier + Increment) \\ Modulus.   ^(seed * scale) / FModulus! !!RandomNumber class methodsFor: 'class initialization'!initialize   FModulus := 139968.0d0.   Increment := 29573.   Modulus := 139968.   Multiplier := 3877.! !!RandomNumber class methodsFor: 'initialize-release'!to: anInteger   ^self basicNew to: anInteger! !!RepeatStream methodsFor: 'accessing'!next   position >= readLimit ifTrue: [ self position: 0 ].   repeatPtr := repeatPtr + 1.   ^collection at: (position := position + 1)! !!RepeatStream methodsFor: 'testing'!atEnd   ^repeatPtr >= repeatLimit! !!RepeatStream methodsFor: 'initialize-release'!to: anInteger   repeatPtr := 0.   repeatLimit := anInteger! !!RandomStream methodsFor: 'accessing'!next   | r |   r := random next.   repeatPtr := repeatPtr + 1.   1 to: percentages size do: [:i|      (r < (percentages at: i)) ifTrue: [^collection at: i]]! !!RandomStream methodsFor: 'accessing'!random: aRandomNumber"* Share the random number generator so we can get the expected results. *"   random := aRandomNumber! !!RandomStream methodsFor: 'initialize-release'!on: aCollection   | size cp |   repeatPtr := 0.   random := RandomNumber to: 1.0d0.   size := aCollection size.   percentages := Array new: size.   collection := Array new: size.   cp := 0.0d0.   1 to: size do: [:i|      collection at: i put: (aCollection at: i) first.      percentages at: i put: (cp := cp + (aCollection at: i) last).   ]! !!RepeatStream class methodsFor: 'instance creation'!to: anInteger on: aCollection   ^(super on: aCollection) to: anInteger! !


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
fasta   self fasta: self arg to: self stdoutSpecial.   ^''! !

RandomNumber initialize!
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
   self stdout nextPutAll: 'hello world'; nl.   ^''! !



Object subclass: #Tests   instanceVariableNames: ''   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!


!Tests class methodsFor: 'platform'!arg   ^CEnvironment commandLine last asNumber! !

!Tests class methodsFor: 'platform'!stdin   ^Stdin! !


!Tests class methodsFor: 'platform'!stdinSpecial   ^ExternalReadStream on:
      (ExternalConnection ioAccessor: (UnixDiskFileAccessor new handle: 0))! !

!Tests class methodsFor: 'platform'!stdout   ^Stdout! !


!Tests class methodsFor: 'platform'!stdoutSpecial   ^ExternalWriteStream on:
      (ExternalConnection ioAccessor: (UnixDiskFileAccessor new handle: 1))! !


!Stream methodsFor: 'platform'!nl   self nextPut: Character lf! !


!Stream methodsFor: 'platform'!print: number digits: decimalPlaces   self nextPutAll: 
      ((number asFixedPoint: decimalPlaces) printString copyWithout: $s)! !


!Stream methodsFor: 'platform'!print: number paddedTo: width   number printOn: self paddedWith: $  to: width base: 10! !


!Integer methodsFor: 'platform'!asFloatD   ^self asDouble! !

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
               output nextPut: bits.
               bits := 0.
           ]
       ]. 
       (extent bitAnd: 7) == 0 ifFalse: [
           bits := bits bitShift: 8 - (extent bitAnd: 7).
           output nextPut: bits.
       ]
   ]! !

!Tests class methodsFor: 'benchmark scripts'!mandelbrot2   | n output |   n := self arg.   (output := self stdout)      nextPutAll: 'P4'; nl; print: n; space; print: n; nl;
      binary.   self mandelbrot2: n to: output.   ^''! !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

Object subclass: #Consumer   instanceVariableNames: 'semaphore msg'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

Consumer subclass: #ProducerConsumer   instanceVariableNames: 'consumer'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

!Consumer methodsFor: 'accessing'!msg    semaphore wait.    ^msg! !!Consumer methodsFor: 'accessing'!msg: data    msg := data.    semaphore signal! !!Consumer methodsFor: 'accessing'!semaphore: aSemaphore    semaphore := aSemaphore! !!Consumer class methodsFor: 'instance creation'!new    | var |    var := self basicNew.    var semaphore: Semaphore new.    ^var! !!ProducerConsumer methodsFor: 'accessing'!consumer: aProcess    consumer := aProcess! !!ProducerConsumer methodsFor: 'accessing'!fork    [ self run ] fork! !!ProducerConsumer methodsFor: 'accessing'!run    [ consumer msg: self msg + 1 ] repeat! !!ProducerConsumer class methodsFor: 'instance creation'!fork: consumer    | proc |    proc := self new.    proc consumer: consumer.    proc fork.    ^proc! !!Tests class methodsFor: 'benchmarking'!message: n   | tail head sum |   head := tail := Consumer new.   500 timesRepeat: [head := ProducerConsumer fork: head].   sum := 0.   n timesRepeat:      [head msg: 0.      sum := sum + tail msg].   ^sum ! !!Tests class methodsFor: 'benchmark scripts'!message   self stdout print: (self message: self arg); nl.   ^''! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy *"!

Object subclass: #Body   instanceVariableNames: 'x y z vx vy vz mass'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

Object subclass: #NBodySystem   instanceVariableNames: 'bodies'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!Body methodsFor: 'accessing'!mass   ^mass! !!Body methodsFor: 'accessing'!x   ^x! !!Body methodsFor: 'accessing'!x: d1 y: d2 z: d3 vx: d4 vy: d5 vz: d6 mass: d7   x := d1.   y := d2.    z := d3.    vx := d4.   vy := d5.   vz := d6.   mass := d7! !!Body methodsFor: 'accessing'!y   ^y! !!Body methodsFor: 'accessing'!z   ^z! !!Body methodsFor: 'nbody'!addMomentumTo: anArray   anArray at: 1 put: (anArray at: 1) + (vx * mass).   anArray at: 2 put: (anArray at: 2) + (vy * mass).   anArray at: 3 put: (anArray at: 3) + (vz * mass).   ^anArray! !!Body methodsFor: 'nbody'!and: aBody velocityAfter: dt           | dx dy dz distance mag |   dx := x - aBody x.   dy := y - aBody y.   dz := z - aBody z.      distance := ((dx*dx) + (dy*dy) + (dz*dz)) sqrt.   mag := dt / (distance * distance * distance).   self decreaseVelocity: dx y: dy z: dz m: aBody mass * mag.      aBody increaseVelocity: dx y: dy z: dz m: mass * mag! !!Body methodsFor: 'nbody'!decreaseVelocity: dx y: dy z: dz m: m   vx := vx - (dx * m).   vy := vy - (dy * m).   vz := vz - (dz * m)! !!Body methodsFor: 'nbody'!increaseVelocity: dx y: dy z: dz m: m   vx := vx + (dx * m).   vy := vy + (dy * m).   vz := vz + (dz * m)! !!Body methodsFor: 'nbody'!kineticEnergy   ^0.5d0 * mass * ((vx * vx) + (vy * vy) + (vz * vz))! !!Body methodsFor: 'nbody'!offsetMomentum: anArray    | m |   m := self class solarMass.   vx := (anArray at: 1) negated / m.   vy := (anArray at: 2) negated / m.   vz := (anArray at: 3) negated / m! !!Body methodsFor: 'nbody'!positionAfter: dt   x := x + (dt * vx).   y := y + (dt * vy).   z := z + (dt * vz)! !!Body methodsFor: 'nbody'!potentialEnergy: aBody   | dx dy dz distance |   dx := x - aBody x.   dy := y - aBody y.   dz := z - aBody z.   distance := ((dx*dx) + (dy*dy) + (dz*dz)) sqrt.   ^mass * aBody mass / distance! !!Body class methodsFor: 'constants'!daysPerYear   ^365.24d0! !!Body class methodsFor: 'constants'!jupiter   ^self new      x: 4.84143144246472090d0      y: -1.16032004402742839d0      z: -1.03622044471123109d-1      vx: 1.66007664274403694d-3 * self daysPerYear      vy: 7.69901118419740425d-3 * self daysPerYear      vz: -6.90460016972063023d-5 * self daysPerYear      mass: 9.54791938424326609d-4 * self solarMass! !!Body class methodsFor: 'constants'!neptune   ^self new      x: 1.53796971148509165d1      y: -2.59193146099879641d1      z: 1.79258772950371181d-1      vx: 2.68067772490389322d-3 * self daysPerYear      vy: 1.62824170038242295d-3 * self daysPerYear      vz: -9.51592254519715870d-5 * self daysPerYear      mass: 5.15138902046611451d-5 * self solarMass! !!Body class methodsFor: 'constants'!pi   ^3.141592653589793d0! !!Body class methodsFor: 'constants'!saturn   ^self new      x: 8.34336671824457987d0      y: 4.12479856412430479d0      z: -4.03523417114321381d-1      vx: -2.76742510726862411d-3 * self daysPerYear      vy: 4.99852801234917238d-3 * self daysPerYear      vz: 2.30417297573763929d-5 * self daysPerYear      mass: 2.85885980666130812d-4 * self solarMass! !!Body class methodsFor: 'constants'!solarMass   ^4.0d0 * self pi * self pi! !!Body class methodsFor: 'constants'!sun   ^self new      x: 0.0d0      y: 0.0d0      z: 0.0d0      vx: 0.0d0      vy: 0.0d0      vz: 0.0d0      mass: self solarMass! !!Body class methodsFor: 'constants'!uranus   ^self new      x: 1.28943695621391310d1      y: -1.51111514016986312d1      z: -2.23307578892655734d-1      vx: 2.96460137564761618d-3 * self daysPerYear      vy: 2.37847173959480950d-3 * self daysPerYear      vz: -2.96589568540237556d-5 * self daysPerYear      mass: 4.36624404335156298d-5 * self solarMass! !!NBodySystem methodsFor: 'nbody'!after: dt   1 to: bodies size do: [:i|      i+1 to: bodies size do: [:j|                                     (bodies at: i) and: (bodies at: j) velocityAfter: dt].   ].      bodies do: [:each| each positionAfter: dt]! !!NBodySystem methodsFor: 'nbody'!energy   | e |   e := 0.0d0.   1 to: bodies size do: [:i|             e := e + (bodies at: i) kineticEnergy.      i+1 to: bodies size do: [:j|          e := e - ((bodies at: i) potentialEnergy: (bodies at: j))].   ].   ^e! !!NBodySystem methodsFor: 'initialize-release'!initialize   bodies := OrderedCollection new      add: Body sun; add: Body jupiter; add: Body saturn;      add: Body uranus; add: Body neptune; yourself.   bodies first offsetMomentum:      (bodies inject: (Array with: 0.0d0 with: 0.0d0 with: 0.0d0)         into: [:m :each | each addMomentumTo: m])! !!Tests class methodsFor: 'benchmark scripts'!nbody   | bodies |   bodies := NBodySystem new initialize.   self stdout print: bodies energy digits: 9; nl.   self arg timesRepeat: [bodies after: 0.01d0].   self stdout print: bodies energy digits: 9; nl.   ^''! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!!Tests class methodsFor: 'benchmark scripts'!nsieve   | n |   n := self arg.   (n < 2) ifTrue: [n := 2].   self primeBenchmarkFor: n to: self stdout using: Array.   ^''! !!Tests class methodsFor: 'benchmarking'!nsieve: n using: arrayClass    | count isPrime |   count := 0.   isPrime := arrayClass new: n withAll: true.   2 to: n do:      [:i |       (isPrime at: i) ifTrue:          [i + i to: n by: i do:            [:k | isPrime at: k put: false].         count := count + 1]].   ^count! !!Tests class methodsFor: 'benchmarking'!primeBenchmarkFor: v to: output using: arrayClass   v to: v - 2 by: -1 do:      [:n| | m |      m := (2 raisedTo: n) * 10000.      output         nextPutAll: 'Primes up to '; 
         print: m paddedTo: 8;
         print: (self nsieve: m using: arrayClass) paddedTo: 9; nl
      ]! !
"* The Computer Language Benchmarks Game    http://shootout.alioth.debian.org/    contributed by Isaac Gouy    modified by Eliot Miranda    then by Nicolas Cellier *"!!Tests class methodsFor: 'benchmark scripts'!nsieve2   | n |   n := self arg.   (n < 2) ifTrue: [n := 2].   self primeBenchmark2For: n to: self stdout using: Array.   ^''! !!Tests class methodsFor: 'benchmarking'!nsieve2: n using: arrayClass    | count isPrime k |   count := 0.   isPrime := arrayClass new: n withAll: true.   2 to: n do:      [:i |       (isPrime at: i) ifTrue:          [k := i.         [(k := k + i) <= n] whileTrue: [isPrime at: k put: false].         count := count + 1]].   ^count! !!Tests class methodsFor: 'benchmarking'!primeBenchmark2For: v to: output using: arrayClass   v to: v - 2 by: -1 do:      [:n| | m |      m := (2 raisedTo: n) * 10000.      output         nextPutAll: 'Primes up to ';         print: m paddedTo: 8;         print: (self nsieve2: m using: arrayClass) paddedTo: 9; nl      ]! !
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

ArrayedCollection variableByteSubclass: #BitArray   instanceVariableNames: ''   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

!BitArray methodsFor: 'accessing'!at: index    | wordIndex bitMask |    wordIndex := ((index - 1) bitShift: -3) + 1.    bitMask := 1 bitShift: (index - 1 bitAnd: 7).    ^((self basicAt: wordIndex) bitAnd: bitMask) > 0! !
!BitArray methodsFor: 'accessing'!at: index put: bit    | wordIndex bitMask word |    wordIndex := ((index - 1) bitShift: -3) + 1.    bitMask := 1 bitShift: (index - 1 bitAnd: 7).    word := self basicAt: wordIndex.    word := word bitOr: bitMask.    bit ifFalse: [word := word - bitMask].    self basicAt: wordIndex put: word.    ^bit! !!BitArray methodsFor: 'accessing'!atAllPut: anObject    "Put anObject at every one of the receivers indices."   | value |   value := anObject ifTrue: [255] ifFalse: [0].   1 to: self basicSize do: [:index | self basicAt: index put: value]! !!BitArray class methodsFor: 'instance creation'!new: size    ^super new: (size + 7 bitShift: -3)! !!Tests class methodsFor: 'benchmark scripts'!nsievebits   | n |   n := self arg.   (n < 2) ifTrue: [n := 2].   self primeBenchmarkFor: n to: self stdout using: BitArray.   ^''! !!Tests class methodsFor: 'benchmarking'!nsieve: n using: arrayClass    | count isPrime |   count := 0.   isPrime := arrayClass new: n withAll: true.   2 to: n do:      [:i |       (isPrime at: i) ifTrue:          [i + i to: n by: i do:            [:k | isPrime at: k put: false].         count := count + 1]].   ^count! !!Tests class methodsFor: 'benchmarking'!primeBenchmarkFor: v to: output using: arrayClass   v to: v - 2 by: -1 do:      [:n| | m |      m := (2 raisedTo: n) * 10000.      output         nextPutAll: 'Primes up to '; 
         print: m paddedTo: 8;
         print: (self nsieve: m using: arrayClass) paddedTo: 9; nl
      ]! !
"* The Computer Language Benchmarks Game
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
"* The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Object subclass: #PiDigitSpigot   instanceVariableNames: 'z x inverse'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!

Object subclass: #Transformation   instanceVariableNames: 'q r s t k'   classVariableNames: ''   poolDictionaries: ''   category: 'Shootout'!!PiDigitSpigot methodsFor: 'private'!consume: aTransformation   ^z * aTransformation! !!PiDigitSpigot methodsFor: 'private'!digit   ^(z extract: 3) floor! !!PiDigitSpigot methodsFor: 'private'!isSafe: aDigit   ^aDigit = (z extract: 4) floor! !!PiDigitSpigot methodsFor: 'private'!produce: anInteger   inverse q: 10 r: -10 * anInteger s: 0 t: 1.   ^inverse * z! !!PiDigitSpigot methodsFor: 'accessing'!next   | y |   ^(self isSafe: (y := self digit))      ifTrue: [z := self produce: y. y]      ifFalse: [z := self consume: x next. self next]! !!PiDigitSpigot methodsFor: 'initialize-release'!initialize   z := Transformation unity.   x := Transformation new.   inverse := Transformation new.! !!PiDigitSpigot class methodsFor: 'instance creation'!new   ^super new initialize! !!Tests class methodsFor: 'benchmarking'!pidigitsTo: v width: width to: output   | n i pidigits |   n := v.   i := 0.   pidigits := PiDigitSpigot new.   [n > 0] whileTrue:      [n < width         ifTrue:            [n timesRepeat: [output nextPut: (Character digitValue: pidigits next)].            n to: width do: [:each | output space].            i := i + n]         ifFalse:            [width timesRepeat: [output nextPut: (Character digitValue: pidigits next)].            i := i + width].
      output tab; nextPut: $:; print: i; nl.
      n := n - width]! !!Tests class methodsFor: 'benchmark scripts'!pidigits   self pidigitsTo: self arg width: 10 to: self stdout.   ^''! !!Transformation methodsFor: 'accessing'!* aTransformation   ^self species       q: q * aTransformation q      r: q * aTransformation r + (r * aTransformation t)      s: s * aTransformation q + (t * aTransformation s)      t: s * aTransformation r + (t * aTransformation t)! !!Transformation methodsFor: 'accessing'!extract: anInteger   ^(q * anInteger + r) // (s * anInteger + t)! !!Transformation methodsFor: 'accessing'!next   k := k +1.   q := k.   r := 4 * k + 2.   s := 0.   t := 2 * k + 1.! !!Transformation methodsFor: 'accessing'!q   ^q! !!Transformation methodsFor: 'accessing'!q: anInteger1 r: anInteger2 s: anInteger3 t: anInteger4   q := anInteger1.   r := anInteger2.   s := anInteger3.   t := anInteger4.   k := 0.! !!Transformation methodsFor: 'accessing'!r   ^r! !!Transformation methodsFor: 'accessing'!s   ^s! !!Transformation methodsFor: 'accessing'!t   ^t! !!Transformation methodsFor: 'initialize-release'!initialize   q := 0.   r := 0.   s := 0.   t := 0.   k := 0.! !!Transformation class methodsFor: 'instance creation'!new   ^super new initialize! !!Transformation class methodsFor: 'instance creation'!q: anInteger1 r: anInteger2 s: anInteger3 t: anInteger4   ^(super new) q: anInteger1 r: anInteger2 s: anInteger3 t: anInteger4! !!Transformation class methodsFor: 'instance creation'!unity   ^self q: 1 r: 0 s: 0 t: 1! !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Paolo Bonzini *"!

Stream subclass: #PiDigitSpigot
    instanceVariableNames: 'numer accum denom k'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Shootout'!!PiDigitSpigot methodsFor: 'stream'!atEnd
    ^false!next
    | digit |
    [ self step. (digit := self extract) isNil ] whileTrue.
    self eliminate: digit.
    ^digit! !!PiDigitSpigot methodsFor: 'private'!initialize
    numer := denom := 1.
    k := accum := 0.!extract
    | tmp |
    numer > accum ifTrue: [ ^nil ].
    tmp := numer + numer + numer + accum.
    ^tmp \\ denom + numer < denom ifTrue: [ tmp // denom ] ifFalse: [ nil ]!eliminate: digit
    accum := accum - (denom * digit).
    accum := accum * 10.
    numer := numer * 10!step
    | y2 |
    k := k + 1.
    y2 := k * 2 + 1.
    accum := (numer + numer + accum) * y2.
    numer := numer * k.
    denom := denom * y2.! !!PiDigitSpigot class methodsFor: 'instance creation'!new   ^super basicNew initialize! !!Tests class methodsFor: 'benchmarking'!pidigits3To: v width: width to: output
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

      n := n - width]! !!Tests class methodsFor: 'benchmark scripts'!pidigits3
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

!Double methodsFor: 'benchmarking'!fib   ^self < 2.0d0 ifTrue: [1.0d0] ifFalse: [(self - 2.0d0) fib + (self - 1.0d0) fib]! !!Double methodsFor: 'benchmarking'!tak: y z: z   ^y < self       ifTrue: [((self - 1.0d0) tak: y z: z) tak:        ((y - 1.0d0) tak: z z: self) z: ((z - 1.0d0) tak: self z: y)]      ifFalse: [z]! !!SmallInteger methodsFor: 'benchmarking'!ack: aSmallInteger   ^self == 0      ifTrue: [aSmallInteger + 1]      ifFalse: [         aSmallInteger == 0            ifTrue: [self - 1 ack:  1]            ifFalse: [self - 1 ack: (self ack: aSmallInteger - 1)] ]! !!SmallInteger methodsFor: 'benchmarking'!fib   ^self < 2 ifTrue: [1] ifFalse: [(self - 2) fib + (self - 1) fib]! !!SmallInteger methodsFor: 'benchmarking'!tak: y z: z   ^y < self       ifTrue: [((self - 1) tak: y z: z) tak:        ((y - 1) tak: z z: self) z: ((z - 1) tak: self z: y)]      ifFalse: [z]! !!Tests class methodsFor: 'benchmarking'!recursive: nArg to: output   | n |   n := nArg.   output       nextPutAll: 'Ack(3,', n printString, '): '; print: (3 ack: n); nl;      nextPutAll: 'Fib('; print: 27.0+n digits: 1; nextPutAll: '): ';          print: (27.0d0+n) fib digits: 1; nl.   n := n - 1.   output       nextPutAll: 'Tak(', (3*n) printString, ',',         (2*n) printString, ',', n printString, '): ',           (3*n tak: 2*n z: n) printString; nl;      nextPutAll: 'Fib(3): '; print: 3 fib; nl;      nextPutAll: 'Tak(3.0,2.0,1.0): ';           print: (3.0d0 tak: 2.0d0 z: 1.0d0) digits: 1; nl! !!Tests class methodsFor: 'benchmark scripts'!recursive   self recursive: self arg to: self stdout.   ^''! !
"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Eliot Miranda *"!


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
      print: s size; nl! !!Tests class methodsFor: 'benchmark scripts'!regexdna
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
!Tests class methodsFor: 'benchmark scripts'!sumcol2   | input sum |   input := self stdinSpecial.   sum := 0.   [input atEnd] whileFalse: [      sum := sum + (input upTo: Character cr) asNumber].
   self stdout print: sum; nl.   ^''! !


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
