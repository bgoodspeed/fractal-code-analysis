%%% $Id: ackermann.oz,v 1.1 2004-05-23 05:36:23 bfulgham Exp $
%%% http://dada.perl.it/shootout/
functor
import System Application
define
fun {Ack M N}
    if M==0 then N + 1
    elseif N == 0 then {Ack M-1 1}
    else {Ack M-1 {Ack M N-1}}
    end
end
in 
    local A in
        [A] = {Application.getArgs plain}
        {System.printInfo "Ack(3,"}
        {System.printInfo A}
        {System.printInfo "): "}
        {System.printInfo {Ack 3 {String.toInt A}}}
	{System.printInfo "\n"}
    end
    {Application.exit 0}
end
%%% $Id: ary.oz,v 1.1 2004-05-23 05:43:44 bfulgham Exp $
%%% http://dada.perl.it/shootout/

%%% Code contributed by Andrew McDowell

functor
import
  System
  Application
define
 local Args N A1 A2 in

   {Application.getCmdArgs plain Args}
   if {List.length Args} \= 1 then 
     N = 1
   else
     {String.toInt Args.1 N}
   end

   {NewArray 0 N 0 A1} 
   {NewArray 0 N 0 A2} 

   {For 0 (N - 1) 1 
     proc {$ I} {Put A1 I (I + 1)} end }

   {For 0 999 1 
      proc {$ I} 
        {For (N - 1) 0 ~1 
          proc {$ I} {Put A2 I ({Array.get A2 I} + {Get A1 I})} end}
      end}


   {System.showInfo {Get A2 0}#" "#{Get A2 (N - 1)}}
   {Application.exit 0}
 end
end
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application System

define
   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

   Min_ = 4
   Max_ = {Max Min_+2 N}
   StretchDepth = Max_ + 1
   LongLivedTree 

   fun {NewTree I D}
      if D == 0 then
         tree(I nil nil)
      else 
         tree(I {NewTree 2*I-1 D-1} {NewTree 2*I D-1})
      end
   end

   fun {ItemCheck T}
      if T == nil then 0 
      else tree(I L R) = T in I + {ItemCheck L} - {ItemCheck R} end
   end

   proc {ShowItemCheck S D T}
      {System.showInfo S # D # "\t check: " # {ItemCheck T}}
   end

   proc {ShowCheck I D Check}
      {System.showInfo 2*I # "\t trees of depth " # D # "\t check: " # Check}
   end

in      
   {ShowItemCheck "stretch tree of depth " StretchDepth {NewTree 0 StretchDepth}}
   LongLivedTree = {NewTree 0 Max_}

   for D in Min_; D=<Max_; D+2 do 
      N = {Pow 2 Max_-D+Min_}
      Check = {NewCell 0}
   in
      for I in 1..N do
         Check := @Check + {ItemCheck {NewTree I D}} + {ItemCheck {NewTree ~I D}}
      end
      {ShowCheck N D @Check}
   end

   {ShowItemCheck "long lived tree of depth " Max_ LongLivedTree}
   {Application.exit 0}   
end
% The Computer Language Shootout                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application System 

define
   fun {NewMeetingPlace}
      ColourList
      ColourPort = {Port.new ColourList}
      OtherList 
      OtherPort = {Port.new OtherList}     
      CountDown = {NewCell N}

      proc {Meetings C1|C2|C O1|O2|O}
         if @CountDown > 0 then
            O1 = C2
            O2 = C1
            CountDown := @CountDown - 1
         else
            O1 = faded
            O2 = faded
         end
         {Meetings C O}
      end

      fun {OtherColour Me}
         Other
      in 
         {Port.send ColourPort Me}
         {Port.send OtherPort Other} 
         Other 
      end

   in
      thread {Meetings ColourList OtherList} end
      place(otherCreaturesColour: OtherColour)
   end

   MeetingPlace = {NewMeetingPlace}


   class Creature
      attr creaturesMet:0 colour

      meth init(Colour) colour := Colour end 
      meth creaturesMet($) @creaturesMet end    

      meth be(Done Marker)
         thread {self meet} Done=Marker end
      end

      meth meet()
         Other = {MeetingPlace.otherCreaturesColour @colour}
      in
         if Other \= faded then
            creaturesMet := @creaturesMet + 1
            colour := {self complement(Other $)}
            {self meet}
         end
      end

      meth complement(Other $)
         if @colour == Other then 
            Other
         else
            case @colour
            of blue then if Other == red then yellow else red end
            [] red then if Other == blue then yellow else blue end
            [] yellow then if Other == blue then red else blue end
            end
         end
      end
   end   % Creature

 
   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}
 
   Colours = [blue red yellow blue]
   Creatures = {Map Colours fun{$ C} {New Creature init(C)} end}

   proc {WaitForMeetingsToEnd Creatures Marker}
      if Creatures \= nil then
         C|Cs = Creatures
         Done
      in
         {C be(Done Marker)}         
         {WaitForMeetingsToEnd Cs Done}
      end
      {Wait Marker}
   end


in  
   {WaitForMeetingsToEnd Creatures unit}

   {System.show
      {FoldL 
         Creatures 
         fun{$ Count C} {C creaturesMet($)} + Count end 
         0} }

   {Application.exit 0}   
end
% The Computer Language Shootout                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application System 

define
   fun {NewMeetingPlace}
      ColourList
      ColourPort = {Port.new ColourList}
      OtherList 
      OtherPort = {Port.new OtherList}     
      CountDown = {NewCell N}

      proc {Meetings C1|C2|C O1|O2|O}
         if @CountDown > 0 then
            O1 = C2
            O2 = C1
            CountDown := @CountDown - 1
            {Meetings C O}
         else
            Done = true   % exit without cleaning up
         end
      end

      fun {OtherColour Me}
         Other
      in 
         {Port.send ColourPort Me}
         {Port.send OtherPort Other} 
         Other 
      end

   in
      thread {Meetings ColourList OtherList} end
      place(otherCreaturesColour: OtherColour)
   end



   class Creature
      attr creaturesMet:0 colour

      meth init(Colour) 
         colour := Colour
         thread {self meetCreatures} end 
      end 
      
      meth meetCreatures()
         Other = {MeetingPlace.otherCreaturesColour @colour}
      in
         % We might be Done before Other has a value
         if Other \= faded then  
            creaturesMet := @creaturesMet + 1
            colour := {self complement(Other $)}
            {self meetCreatures}
         end
      end

      meth creaturesMet($) @creaturesMet end    

      meth complement(Other $)
         if @colour == Other then 
            Other
         else
            case @colour
            of blue then if Other == red then yellow else red end
            [] red then if Other == blue then yellow else blue end
            [] yellow then if Other == blue then red else blue end
            end
         end
      end
   end   % Creature


 
   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}
 
   MeetingPlace = {NewMeetingPlace}
   Colours = [blue red yellow blue]
   Creatures = {Map Colours fun{$ C} {New Creature init(C)} end}
   Done
in  
   {Wait Done}

   {System.show
      {FoldL 
         Creatures 
         fun{$ Count C} {C creaturesMet($)} + Count end 
         0} }

   {Application.exit 0}   % exit without cleaning up
end
% The Computer Language Benchmarks Game                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import System Application

define
   fun {RendezvousServer Done N}
      CreatureList
      CreaturePort = {Port.new CreatureList}
      OtherList 
      OtherPort = {Port.new OtherList}     
      CountDown = {NewCell N}

      proc {Meetings C1|C2|C O1|O2|O}
         if @CountDown > 0 then
            O1 = C2
            O2 = C1
            CountDown := @CountDown - 1
            {Meetings C O}
         else
            Done = true   % exit without cleaning up
         end
      end

      fun {Notify Me}
         Other
      in 
         {Port.send CreaturePort Me}
         {Port.send OtherPort Other} 
         Other 
      end

   in
      thread {Meetings CreatureList OtherList} end
      rendezvous(notify: Notify)
   end



   class Creature
      attr name colour selfMet:0 creaturesMet:0 rendezvous

      meth init(Name Colour Rendezvous) 
         name := Name
         colour := Colour
         rendezvous := Rendezvous

         if Name \= 0 then 
            thread {self meetOthers} end 
         end
      end 
      
      meth meetOthers()
         Name Colour      
      in
         Name # Colour = {@rendezvous.notify @name # @colour}
         colour := {self complementColour(Colour $)}
         if Name == @name then selfMet := @selfMet + 1 end
         creaturesMet := @creaturesMet + 1
         {self meetOthers}
      end

      meth colour($) @colour end   

      meth selfMet($) @selfMet end    

      meth creaturesMet($) @creaturesMet end    

      meth complementColour(OtherColour $)
         if @colour == OtherColour then 
            OtherColour
         else
            case @colour
            of blue then if OtherColour == red then yellow else red end
            [] red then if OtherColour == blue then yellow else blue end
            [] yellow then if OtherColour == blue then red else blue end
            end
         end
      end
   end   % Creature



   fun {StartRendezvousCreaturesWith ColourList}
      Done
      Rendezvous = {RendezvousServer Done N}

      C = {List.mapInd ColourList 
         fun{$ I C} {New Creature init(I C Rendezvous)} end }
   in
      {Wait Done}
      C
   end


   proc {CheckCreatureColourChanges}
      ColourList = [blue red yellow]
      Creatures = {Map ColourList fun{$ C} {New Creature init(0 C nil)} end }
   in
      {ForAll Creatures 
         proc {$ C} {
            ForAll ColourList 
               proc {$ X} { 
                  System.showInfo 
                     {C colour($)} # " + " # X # " -> " #
                        {C complementColour(X $)}
               } end
         } end
      }
      {System.showInfo ""}
   end


   proc {ReportRendezvouses ColourList}

      Numbers = {Tuple.toArray {List.toTuple '#' 
         {Map 
            [zero one two three four five six seven eight nine] 
            fun {$ A} {AtomToString A} end 
         } } } 

      fun {Spellout K}
         {Flatten {Map {IntToString K} 
            fun {$ C} [" " Numbers.({StringToInt [C]} + 1)] end } }
      end 

      Sum = {NewCell 0}
      RendezvousCreatures
   in
      {System.showInfo
         {Flatten {Map ColourList fun {$ C} [" " {AtomToString C}] end } } }

      RendezvousCreatures = {StartRendezvousCreaturesWith ColourList}

      {ForAll RendezvousCreatures
         proc {$ C} 
            Meetings = {C creaturesMet($)} 
         in            
            Sum := @Sum + Meetings 
            {System.showInfo {IntToString Meetings} # {Spellout {C selfMet($)} } }
         end
      }

      {System.showInfo {Spellout @Sum}}
      {System.showInfo ""}
   end

 
   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}
in  
   {CheckCreatureColourChanges}
   {ReportRendezvouses [blue red yellow]}
   {ReportRendezvouses [blue red yellow red yellow blue red yellow red blue]}

   {Application.exit 0}   % exit without cleaning up
end
%%% $Id: echo.oz,v 1.4 2005-05-13 16:24:17 igouy-guest Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x echo.oz -o echo.oz.exe
%%     echo.oz.exe 100000 

functor
import
   System
   Application
   Open
   OS

define
   Data = "Hello there sailor\n"

proc {ServerThread Sock SPort Bytes}
   Sock = {New Open.socket server(port:SPort)}
   {ServerLoop Sock 0 Bytes}
   {Sock shutDown(how: [receive send])}{Sock close}
end

proc {ServerLoop Sock Sum Bytes}
   local Message NewSum DR DW CR ST in

      %% low-level Selects seem ~6% faster total
      {Sock getDesc(DR DW)}{OS.readSelect DR}
      {OS.read DR 1024 Message nil CR}      
      %% {Sock read(list: Message)} %% normal read
      
      if Message == nil then %% connection has been closed
         Bytes = Sum
      else
         NewSum = {Length Message} + Sum

         {OS.writeSelect DW}{OS.write DW Message ST}
         %% {Sock write(vs: Message)} %% normal write
      
         {ServerLoop Sock NewSum Bytes}
      end
   end
end


proc {ClientThread SPort N}
   local Sock in 
      Sock = {New Open.socket client(port:SPort)}
      {ClientLoop Sock N}
      {Sock shutDown(how: [receive send])}{Sock close}
   end
end

proc {ClientLoop Sock N}
   local Message DR DW CR ST in
      if N > 0 then

         {Sock getDesc(DR DW)}
         {OS.writeSelect DW}{OS.write DW Data ST}     
             {OS.readSelect DR}{OS.read DR 1024 Message nil CR}
     
         %% {Sock write(vs: Data)}     %% normal write
             %% {Sock read(list: Message)} %% normal read

         if Message == Data then {ClientLoop Sock N-1} end
      end
   end
end

in
   local Args A1 A2 A3 Socket SPort Bytes ArgList Pid in
      Args = {Application.getArgs plain}

      if {Length Args} == 1 then
         %% We are the server process      

         A3|nil = Args

         thread {ServerThread Socket SPort Bytes} end

         %% Prepare to fork an OS process for the client 
         %%    automatically close cmd.exe
         %%    start echo.oz.exe
         %%    pass a flag indicating the client process
         %%    pass the open server port SPort 
         %%    pass the number of times the client should send the data

         ArgList = ["/C" "echo.oz" "client" {IntToString SPort} A3]
         Pid = {New Open.pipe init(cmd: "/bin/sh" args: ArgList)}

         %% Synchronize with server thread completion and indirectly with
         %% the client process. Wait here until the dataflow variable Bytes 
         %% has been given a value in the server thread. That happens after
         %% the client process closes the socket connection, when the client
         %% process has sent all it's data and received all the replies.
 
         {System.showInfo 'server processed '#{IntToString Bytes}#' bytes'} 

      elseif {Length Args} == 3 then 
         %% We are the client process

         %% Take the flag, server port, times-to-repeat from the args
         %% and use the main thread for the client
         A1|A2|A3|nil = Args
         if A1 == "client" then
            {ClientThread {StringToInt A2} {StringToInt A3}}
         end  
      end
   end
   {Application.exit 0}
end
%%% $Id: except.oz,v 1.1 2004-05-23 06:34:14 bfulgham Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x except.oz -o except.oz.exe
%%     except.oz.exe 2000

functor
import System Application

define

   LoCount
   HiCount
   
   proc {Blowup N}
      if N mod 2 == 0 then raise loException() end
      else raise hiException() end end
   end

   proc {LoFun N}
      try {Blowup N} catch
     loException then {Assign LoCount {Access LoCount}+1}
      end
   end

   proc {HiFun N}
      try {LoFun N} catch
     hiException then {Assign HiCount {Access HiCount}+1}
      end
   end

   proc {SomeFun N}
      if N > 0 then
     try {HiFun N} catch
        loException then {System.showInfo 'loException should not get here'}
        [] hiException then {System.showInfo 'hiException should not get here'}
     end
     {SomeFun N-1}
      end
   end
in
   local Args N in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}

      LoCount = {NewCell 0}
      HiCount = {NewCell 0}
      {SomeFun N}

      {System.printInfo "Exceptions: HI="}
      {System.printInfo {Access HiCount}}
      {System.printInfo " / LO="}
      {System.showInfo {Access LoCount}}
   end
   {Application.exit 0}
end
% The Computer Language Benchmarks Game                               
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy
% based on Andrei Formiga's functional Scala program

functor
import Application System

define
   fun {Flips A}
      Last = A.1
   in
      if Last == 1 then 0
      else
         for I in 1..Last div 2 do
            From = Last + 1 - I
            Swap = A.I       
         in
            A.I := A.From
            A.From := Swap
         end

         {Flips A} + 1
      end
   end


   proc {ShowPermutation A}
      for I in 1..{Array.high A} do {System.printInfo A.I} end
      {System.showInfo ""}
   end

   proc {FlipPermutation A}
      Count = {Flips {Array.clone A}}
   in       
      if Count > @MaxFlipsCount then MaxFlipsCount := Count end
      if @Check < 30 then {ShowPermutation A} Check := @Check + 1 end
   end


   proc {RotateLeft ?A N}
      Swap = A.1
   in
      for I in 1..N-1 do A.I := A.(I+1) end
      A.N := Swap
   end

   proc {Permutations A N J}
      if J < N then 
         if N == 1 then 
            {FlipPermutation A} 
         else
            {Permutations A N-1 0}
            {RotateLeft A N}
            {Permutations A N J+1}
         end
      end
   end


   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

   fun {Numbers N} 
      A = {NewArray 1 N 0}
   in
      for I in 1..N do A.I := I end
      A
   end

   MaxFlipsCount = {NewCell 0}
   Check = {NewCell 0}

in    
   {Permutations {Numbers N} N 0}
   {System.showInfo "Pfannkuchen(" # N # ") = " # @MaxFlipsCount}
   {Application.exit 0}   
end

% The Computer Language Benchmarks Game  
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application Open

define
   % lists convenient for declaring data

   RawALU = 
      "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" #
      "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" #
      "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" #
      "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" #
      "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" #
      "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" #
      "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

   RawIUB = [
      "a"#0.27 "c"#0.12 "g"#0.12 "t"#0.27
      "B"#0.02 "D"#0.02 "H"#0.02 "K"#0.02
      "M"#0.02 "N"#0.02 "R"#0.02 "S"#0.02
      "V"#0.02 "W"#0.02 "Y"#0.02 ]

   RawHomoSapiens = [
      "a"# 0.3029549426680
      "c"# 0.1979883004921
      "g"# 0.1975473066391
      "t"# 0.3015094502008 ]


   % arrays faster for indexed selection

   fun {MakeStringArray L} 
      A = {NewArray 1 {Length L} nil}
   in
     {List.forAllInd L 
        proc{$ I C} A.I := [C] end}
     A
   end

   fun {MakeCumulative L}
      N = {Length L}
      A = {NewArray 1 N 0}
      Sum = {NewCell 0.0}

      proc {Acc I Code#Percent}
         A.I := Code # (Percent + @Sum)
         Sum := @Sum + Percent
      end
   in 
      {List.forAllInd L Acc}
      A
   end

   ALU = {MakeStringArray {VirtualString.toString RawALU} }
   IUB = {MakeCumulative RawIUB}
   HomoSapiens = {MakeCumulative RawHomoSapiens}


   % random selection

   fun {PseudoRandomFunction Max}
      IM = 139968 IMF = 139968.0
      IA = 3877
      IC = 29573
      Seed = {NewCell 42} 
   in
      fun {$} 
         Seed := (@Seed * IA + IC) mod IM
         Max * {IntToFloat @Seed} / IMF
      end
   end

   RandomNumber = {PseudoRandomFunction 1.0}


   fun {SelectRandom A}      
      fun {Select R A I N}   % simple sequential search
         Code#Percent = A.I
      in 
         if R =< Percent then Code else {Select R A I+1 N} end
      end      
   in 
      {Select {RandomNumber} A {Array.low A} {Array.high A}}
   end   


   % based on Paul Hsieh's C program
   proc {MakeRandomFasta Id Desc A N F}
      fun {Line I L}
         if I > 0 then {Line I-1 L#{SelectRandom A} } else L end
      end
   in
      {F  write(vs: ">" # Id # " " # Desc # "\n")}

      for I in N; I > 0; I-LineLength do
         M = if I < LineLength then I else LineLength end
      in
         {F  write(vs: {Line M ""} # "\n")}
      end
   end


   % repeat selection

   proc {MakeRepeatFasta Id Desc A N F}
      Start = {NewCell 1}
      Last = {Array.high A}

      fun {Line I N L}
         if I =< N then {Line I+1 N L#A.I } else L end
      end
   in
      {F write(vs: ">" # Id # " " # Desc # "\n")}

      for I in N; I > 0; I-LineLength do
         M = if I < LineLength then I else LineLength end
         K = @Start + M - 1
         Stop = if K > Last then K-Last else K end
         L
      in
         if K > Last then
            L = {Line @Start Last nil}
            Start := 1
         else
            L = nil          
         end
         {F write(vs: {Line @Start Stop L} # "\n") }
         Start := Stop + 1 
      end
   end


   LineLength = 60

   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}
   StdOut = {New Open.file init(name:stdout)}

in   
   {MakeRepeatFasta "ONE" "Homo sapiens alu" ALU N*2 StdOut}
   {MakeRandomFasta "TWO" "IUB ambiguity codes" IUB N*3 StdOut} 
   {MakeRandomFasta "THREE" "Homo sapiens frequency" HomoSapiens N*5 StdOut} 
   {Application.exit 0}   
end
%%% $Id: fibo.oz,v 1.4 2005-04-25 19:01:38 igouy-guest Exp $
%%% http://dada.perl.it/shootout/
functor
import System Application
define
fun {Fib N}
	if N < 2 then 1 else {Fib N-2} + {Fib N-1} end
end
A
in 
    [A] = {Application.getArgs plain}
    {System.show {Fib {String.toInt A}}}
    {Application.exit 0}
end
%%% $Id: harmonic.oz,v 1.3 2005-03-06 05:21:23 bfulgham Exp $
%%%
%%% http://shootout.alioth.debian.org/
%%% Contributed by Brent Fulgham
%%% using String conversion routines proposed by Juergen Stuber
%%%   and Jorge Marques Pelizzoni
functor
import System Application
define
   fun {FloatAbs X}
      if X >= 0.0 then X else ~X end
   end

   fun {FloatSquare X}
      X * X
   end

   fun {FloatPower X E}
      if E==0 then 1.0
      elseif E<0 then raise negativeExponent(E) end
      else
	 if E mod 2 == 1 then X else 1.0 end
	 * {FloatSquare {FloatPower X E div 2}}
      end
   end

   fun {MakePadding Ch L}
      if L > 0 then
	 Padding = {MakeList L}
      in
	 for V in Padding do V = Ch end
	 Padding
      else
	 nil
      end
   end

   fun {PadLeft Ch String L}
      PL = L - {List.length String}
   in
      {MakePadding Ch PL} # String
   end

   fun {FloatToVS F Prec}
      fun {FractionToString Frac Prec}
	 if Prec =< 0 then ""
	 elseif Prec > 9 then raise excessivePrecision(Prec) end
	 else
	    Shifted = {FloatPower 10.0 Prec} * Frac
	    Digits = {FloatToInt {Round Shifted}}
	 in
	    {PadLeft &0 {IntToString Digits} Prec}
	 end
      end

      I = {FloatToInt {if F >= 0.0 then Floor else Ceil end F}}
      Frac = {FloatAbs F - {IntToFloat I}}
   in
      {IntToString I} # "." # {FractionToString Frac Prec}
   end

   fun {FloatToString F Prec}
      {VirtualString.toString {FloatToVS F Prec}}
   end
   
   fun {Sum_Harmonic Goal Curr Accum}
      if Curr>Goal then
	 Accum
      else
	 {Sum_Harmonic Goal (Curr + 1) (Accum + 1.0 / {Int.toFloat Curr})}
      end
   end
in
   local A in
      [A] = {Application.getArgs plain}
      {System.printInfo {FloatToString {Sum_Harmonic {String.toInt A} 1 0.0} 9 }}
      {System.printInfo "\n"}
   end
   {Application.exit 0}
end
%%% $Id: hash.oz,v 1.2 2004-07-04 07:09:25 bfulgham Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x hash.oz -o hash.oz.exe
%%     hash.oz.exe 2000

functor
import System Application

define

   fun {IntToHexString I Hex}
      if I =< 0 then Hex else
     local M D in 
        D = I div 16
        M = I mod 16
        if M < 10 then 
           {IntToHexString D M+&0|Hex}
        else
           {IntToHexString D (M-10)+&a|Hex}
        end
     end
      end
   end

   proc {InsertHexKeys H N}
      for I in 0..N do 
    {Dictionary.put H {String.toAtom {IntToHexString I nil}} I}
      end
   end

   proc {CountLookups H I S C}
      if I >= 0 then 
     if {Dictionary.member H {String.toAtom {IntToString I}}} then
        {CountLookups H I-1 S+1 C}
     else 
        {CountLookups H I-1 S C}
     end
      else C = S end
   end

in 
   local Args N H Count in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}

      {NewDictionary H}
      {InsertHexKeys H N}
      {CountLookups H N+1 0 Count}

      {System.showInfo Count}
   end
   {Application.exit 0}
end
%%% $Id: hash2.oz,v 1.1 2004-05-23 06:21:23 bfulgham Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x hash2.oz -o hash2.oz.exe
%%     hash2.oz.exe 150

functor
import System Application

define

proc {InitHash H}
   for I in 0..9999 do 
      {Dictionary.put H
         {String.toAtom {List.append "foo_" {IntToString I}}} I} 
   end
end

proc {AddValues L H}
   local Key Value Tail V in
      if L \= nil then
         (Key#Value|Tail) = L
         {Dictionary.condGet H Key nil V}
         if V == nil then
            {Dictionary.put H Key Value}
         else
            {Dictionary.put H Key Value+V}
         end
         {AddValues Tail H}
      end
   end
end

in 
   local Args N H1 H2 in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}

      {NewDictionary H1}
      {NewDictionary H2}

      {InitHash H1}
      for I in 1..N do 
         {AddValues {Dictionary.entries H1} H2} end

      {System.printInfo {Dictionary.get H1 'foo_1'}}
      {System.printInfo ' '}
      {System.printInfo {Dictionary.get H1 'foo_9999'}}
      {System.printInfo ' '}
      {System.printInfo {Dictionary.get H2 'foo_1'}}
      {System.printInfo ' '}
      {System.printInfo {Dictionary.get H2 'foo_9999'}}
      {System.printInfo "\n"}
   end
   {Application.exit 0}
end
%%% $Id: heapsort.oz,v 1.3 2005-05-11 16:29:48 igouy-guest Exp $
%%% http://dada.perl.it/shootout/
%%%
%%% contributed by Isaac Gouy 
%%% Using string conversion routines suggested by Juergen Stuber
%%%   and Jorge Marques Pelizzoni

%%  Transliterated from the Mercury solution
%%
%%  Usage: start from command line with
%%     ozc -x heapsort.oz -o heapsort.oz.exe
%%     heapsort.oz.exe 1000

functor
import System Application

define
   IM = 139968
   IA = 3877
   IC = 29573
   Seed = 42
   
   fun {Random_heap H I S0}
      local R S in
     if I =< {Array.high H} then
        {Gen_random R S0 S}
        {Random_heap {Up_heap H I R} I+1 S}
     else
        H
     end
      end
   end

   fun {Up_heap H N Y}
      local HalfN X in
     HalfN = N div 2
     X = {Get H HalfN}
     if 0 < N andthen X < Y then
        {Put H N X}
        {Up_heap H HalfN Y}
     else
        {Put H N Y}
        H
     end
      end
   end

   fun {Heapsort H N}
      if N == 0 then H
      else {Heapsort {Remove_greatest H N} N-1} end
   end

   fun {Remove_greatest H N}
      local X Y in
     X = {Get H 0}
     Y = {Get H N}
     {Put H N X}
     {Down_heap H 0 N-1 Y}
      end
   end

   fun {Down_heap H I N X}
      local L R J Y in
     L = I + I + 1
     R = L + 1
     if N < L then
        {Put H I X}
        H
     else
        J = if R < N andthen {Get H R} > {Get H L}
        then  R else L end
        Y = {Get H J}
        if X > Y then
           {Put H I X}
           H
        else
           {Put H I Y}
           {Down_heap H J N X}
        end
     end
      end
   end

   proc {Gen_random R S0 S}
      S = (S0 * IA + IC) mod IM
      R = {IntToFloat S} / {IntToFloat IM}
   end

   fun {FloatAbs X}
      if X >= 0.0 then X else ~X end
   end

   fun {FloatSquare X}
      X * X
   end

   fun {FloatPower X E}
      if E==0 then 1.0
      elseif E<0 then raise negativeExponent(E) end
      else
	 if E mod 2 == 1 then X else 1.0 end
	 * {FloatSquare {FloatPower X E div 2}}
      end
   end

   fun {MakePadding Ch L}
      if L > 0 then
	 Padding = {MakeList L}
      in
	 for V in Padding do V = Ch end
	 Padding
      else
	 nil
      end
   end

   fun {PadLeft Ch String L}
      PL = L - {List.length String}
   in
      {MakePadding Ch PL} # String
   end

   fun {FloatToVS F Prec}
      fun {FractionToString Frac Prec}
	 if Prec =< 0 then ""
	 elseif Prec > 9 then raise excessivePrecision(Prec) end
	 else
	    Shifted = {FloatPower 10.0 Prec} * Frac
	    Digits = {FloatToInt {Round Shifted}}
	 in
	    {PadLeft &0 {IntToString Digits} Prec}
	 end
      end

      I = {FloatToInt {if F >= 0.0 then Floor else Ceil end F}}
      Frac = {FloatAbs F - {IntToFloat I}}
   in
      {IntToString I} # "." # {FractionToString Frac Prec}
   end

   fun {FloatToString F Prec}
      {VirtualString.toString {FloatToVS F Prec}}
   end   
in
   local Args N RandomHeap SortedHeap in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}
      RandomHeap = {Random_heap {NewArray 0 N-1 0.0} 0 Seed}
      SortedHeap = {Heapsort RandomHeap N-1}
      {System.showInfo {FloatToString {Get SortedHeap N-1} 9}}
   end
   {Application.exit 0}
end
%%% $Id: hello.oz,v 1.1 2004-05-23 07:03:09 bfulgham Exp $
%%% http://dada.perl.it/shootout/
functor
import System Application
define
    {System.printInfo "hello world\n"}
    {Application.exit 0}
end
%% The Computer Language Benchmarks Game                              
%% http://shootout.alioth.debian.org/  
%% contributed by Isaac Gouy

functor
export 
   FloatToString

define

   fun {NSign X}
      if X<0.0 then "-" else "" end
   end

   fun {NDigits X0 Z I S0 Exp}
      if X0<Z then
         S0 # Exp
      else  
         local X S in 
            X = (X0-I)*10.0
            if Exp == ~1 then S = S0#"." else S = S0 end
            {NDigits X Z*10.0 {Floor X} S#{IntToString {FloatToInt I}} Exp-1}
         end
      end
   end

   fun {NZeros S Exp N Point}
      if Exp<~N then
         S
      else
         if Exp==~1 andthen Point then
            {NZeros S#"." Exp N false}
         else
            {NZeros S#"0" Exp-1 N Point}
         end
      end
   end

   fun {FloatToString F N}
      local 
         Z = {Pow 10.0 ~{IntToFloat N}}
         X = 0.5*Z + {Abs F}
         T = {NDigits X Z {Floor X} {NSign F} 0}
      in
         {NZeros T.1 T.2 N true}              
      end
   end

end
% The Computer Language Benchmarks Game                           
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import 
   Application System Open
   S at 'Include/oz/shootout.ozf'

define
   Sequence

   proc {FindSequence F Id}
      case {F getS($)} 
      of false then skip
      [] &>|T then 
         if {List.isPrefix Id T} then skip else {FindSequence F Id} end
      else {FindSequence F Id} end
   end

   fun {ReadSequence F S}
      case {F getS($)} 
      of false then S
      [] &>|_ then S 
      [] &;|_ then {ReadSequence F S}
      [] Line then {ReadSequence F S#{Map Line Char.toUpper}} end
   end

   fun {GenerateFrequencies Length}
      D = {NewDictionary}

      proc {KFrequency Offset J}
         N = {ByteString.length Sequence} - J + 1
      in 
         for I in Offset; I<N; I+J do
            Slice = {ByteString.slice Sequence I I+J} 
            K = {String.toAtom {ByteString.toString Slice}}
            V = {Dictionary.condGet D K nil}
         in
            if V == nil then {Dictionary.put D K {NewCell 1}}
            else V := @V + 1 end
         end
      end
   in
      for Offset in 0; Offset<Length; Offset+1 do 
         {KFrequency Offset Length} 
      end
      D
   end

   proc {WriteFrequencies J}
      D = {GenerateFrequencies J}
      F = fun {$ Ak#Av Bk#Bv} if @Av==@Bv then Ak>Bk else @Av>@Bv end end
      L = {Sort {Dictionary.entries D} F}
      N = {IntToFloat {ByteString.length Sequence} - J + 1}
   in
      for K#V in L do 
         Percentage = {IntToFloat @V} / N * 100.0
      in
         {System.showInfo K # " " # {S.floatToString Percentage 3}}          
      end
      {System.showInfo ""} 
   end

   proc {WriteCount Fragment}
      D = {GenerateFrequencies {Length Fragment}}
      K = {String.toAtom Fragment}
      Count = {Dictionary.condGet D K {NewCell 0}}
   in
      {System.showInfo @Count # "\t" # Fragment}
   end

   class TextFile from Open.file Open.text end
   StdIn = {New TextFile init(name:stdin)}

in    
   {FindSequence StdIn "THREE"}
   Sequence = {VirtualString.toByteString {ReadSequence StdIn nil}}

   {WriteFrequencies 1}
   {WriteFrequencies 2}

   {WriteCount "GGT"}
   {WriteCount "GGTA"}
   {WriteCount "GGTATT"}
   {WriteCount "GGTATTTTAATT"}
   {WriteCount "GGTATTTTAATTTATAGT"}

   {Application.exit 0}   
end
% The Computer Language Benchmarks Game                            
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import 
   Application System Open
   S at 'Include/oz/shootout.ozf'

define
   Sequence

   proc {FindSequence F Id}
      case {F getS($)} 
      of false then skip
      [] &>|T then 
         if {List.isPrefix Id T} then skip else {FindSequence F Id} end
      else {FindSequence F Id} end
   end

   fun {ReadSequence F S}
      case {F getS($)} 
      of false then S
      [] &>|_ then S 
      [] &;|_ then {ReadSequence F S}
      [] Line then {ReadSequence F S#{Map Line Char.toUpper}} end
   end

   fun {GenerateFrequencies Length}
      D = {NewDictionary}

      proc {KFrequency Offset J}
         N = {ByteString.length Sequence} - J + 1
      in 
         for I in Offset; I<N; I+J do
            Slice = {ByteString.slice Sequence I I+J} 
            K = {String.toAtom {ByteString.toString Slice}}
            V NewV
         in
            {Dictionary.condExchange D K 0 V NewV}
            NewV = V + 1
         end
      end
   in
      for Offset in 0; Offset<Length; Offset+1 do 
         {KFrequency Offset Length} 
      end
      D
   end

   proc {WriteFrequencies J}
      D = {GenerateFrequencies J}
      F = fun {$ Ak#Av Bk#Bv} if Av==Bv then Ak>Bk else Av>Bv end end
      L = {Sort {Dictionary.entries D} F}
      N = {IntToFloat {ByteString.length Sequence} - J + 1}
   in
      for K#V in L do 
         Percentage = {IntToFloat V} / N * 100.0
      in
         {System.showInfo K # " " # {S.floatToString Percentage 3}}          
      end
      {System.showInfo ""} 
   end

   proc {WriteCount Fragment}
      D = {GenerateFrequencies {Length Fragment}}
      K = {String.toAtom Fragment}
      Count = {Dictionary.condGet D K 0}
   in
      {System.showInfo Count # "\t" # Fragment}
   end

   class TextFile from Open.file Open.text end
   StdIn = {New TextFile init(name:stdin)}

in    
   {FindSequence StdIn "THREE"}
   Sequence = {VirtualString.toByteString {ReadSequence StdIn nil}}

   {WriteFrequencies 1}
   {WriteFrequencies 2}

   {WriteCount "GGT"}
   {WriteCount "GGTA"}
   {WriteCount "GGTATT"}
   {WriteCount "GGTATTTTAATT"}
   {WriteCount "GGTATTTTAATTTATAGT"}

   {Application.exit 0}   
end
%%  Always 10000's times faster to append to the head of
%%  the list (and then Reverse) rather than traversing
%%  the entire list in each append before adding to tail.
%%
%%  http://shootout.alioth.debian.org/
%%
%%  contributed by Isaac Gouy
%%
%%  Usage: start from command line with
%%     ozc -x lists.oz -o lists.oz.exe
%%     lists.oz.exe 16

functor
import System Application

define
   L1 L2 L3
   
   fun {MoveLeftRight Li2 Li3}
      %% list|item requires list to be traversed to end
      %% before appending item. item|list always faster.
      %% {Reverse item|list} result same as list|item
      local H T in 
	 if Li2 == nil then Li2|[{Reverse Li3}]
	 else
	    H|T = Li2               %% take from left
	    {MoveLeftRight T H|Li3} %% append to left, reverse later
	 end
      end
   end

   proc {AssignL2L3 L}
      %% Factor out the assignment to allow simple reuse
      {Assign L2 {Nth L 1}}
      {Assign L3 {List.last L}}
   end
   
in
   local Args N Size in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}
      Size = 10000
      {NewCell nil L1}{NewCell nil L2}{NewCell nil L3}    
      for I in 1..N do
	    %% Create L1
	 {Assign L1 {List.number 1 Size 1} }
	    %% Copy L1 to L2
	 {Assign L2 {List.take {Access L1} Size} }
	 {Assign L3 nil}

	    %% Move items from L2 to L3
	 {AssignL2L3 {MoveLeftRight {Access L2}{Access L3} }}
	    %% Move items from L3 to L2
	 {AssignL2L3 {Reverse {MoveLeftRight {Reverse {Access L3}}{Access L2}} }}
	    %% Reverse L1
	 {Assign L1 {Reverse {Access L1}}}
         if {Not {Nth {Access L1} 1} == Size andthen {Access L1} == {Access L2}} then
            {System.showInfo 'L1 != L2'}
         end
      end
      {System.showInfo {Length {Access L1}}}
   {Application.exit 0}
   end
end
% The Computer Language Benchmarks Game                        
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application Open

define
   proc {Mandelbrot F Side}
      IXMax = Side - 1
      IYMax = IXMax
      XMax = {IntToFloat Side}
      YMax = XMax
      M = 50
      Limit2 = 4.0


      proc {XLoop IX Y B N}
         X = {IntToFloat IX}
         Cr = 2.0*X / XMax - 1.5
         Ci = 2.0*Y / YMax - 1.0
         Bits
         Bitnum
    
         fun {Include J Zr Zi Tr Ti}
            if J<M andthen Tr + Ti =< Limit2 then 
               I = 2.0 * Zr * Zi + Ci
               R = Tr - Ti + Cr
            in
               {Include J+1 R I R*R I*I}
            else 
               Tr + Ti =< Limit2
            end
         end

      in
         if IX =< IXMax then
            Bits = if {Include 0 0.0 0.0 0.0 0.0} then B*2+1 else B*2 end
            Bitnum = N+1 

            if Bitnum == 8 then 
               {F putC(Bits)}
               {XLoop IX+1 Y 0 0}
            elseif IX == IXMax then
               {F putC( Bits * {Pow 2 (8 - (Side mod 8))} )}
               {XLoop IX+1 Y 0 0}
            else
               {XLoop IX+1 Y Bits Bitnum}
            end
         end
      end


      proc {YLoop IY}
         if IY =< IYMax then
            {XLoop 0 {IntToFloat IY} 0 0} 
            {YLoop IY+1}
         end
      end

   in
      {F putS("P4")}
      {F putS({IntToString Side} # " " # {IntToString Side})}
      {YLoop 0}
   end


   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

   class TextFile from Open.file Open.text end
   StdOut = {New TextFile init(name:stdout)}

in      
   {Mandelbrot StdOut N}
   {Application.exit 0}   
end
%%% $Id: matrix.oz,v 1.1 2004-05-23 07:14:27 bfulgham Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x matrix.oz -o matrix.oz.exe
%%     matrix.oz.exe 300

functor
import System Application

define

   proc {MakeMatrix Rows Cols M}
      local Count in
     {NewArray 0 Rows-1 0 M}
     Count ={NewCell 0}
     for I in 0..Rows-1 do
        local R in
           {NewArray 0 Cols-1 0 R}
           {Put M I R}
           for J in 0..Cols-1 do
          {Assign Count {Access Count}+1}
          {Put R J {Access Count}}
           end
        end
     end
      end
   end


   proc {MMult M1 M2 MM}
      local S1 N1 Prod S2 N2 in
     S1 = {Array.low M1}
     N1 = {Array.high M1}
     S2 = {Array.low {Get M1 S1}}
     N2 = {Array.high {Get M1 S1}}
     Prod = {NewCell 0}
     {NewArray S1 N1 0 MM}
     for I in S1..N1 do
        local R in
           {NewArray S1 N1 0 R}
           {Put MM I R}
           for J in S1..N1 do
          {Assign Prod 0}
          for K in S2..N2 do
             {Assign Prod {Get {Get M1 I} K}*
              {Get {Get M2 K} J}+{Access Prod}}
          end
          {Put R J {Access Prod}}
           end
        end
     end
      end
   end

   proc {RepeatMMult N M1 M2 MM}
      local T in
     if N > 1 then
        {MMult M1 M2 T}
        {RepeatMMult N-1 M1 M2 MM}
     else {MMult M1 M2 T} MM = T end
      end
   end
   
in 
   local Args Repeat N M1 M2 MM in
      [Args] = {Application.getArgs plain}
      Repeat = {String.toInt Args}
      N = 30
      {MakeMatrix N N M1}
      {MakeMatrix N N M2}
      {RepeatMMult Repeat M1 M2 MM}
      
      {System.printInfo {Get {Get MM 0} 0}}{System.printInfo ' '}
      {System.printInfo {Get {Get MM 2} 3}}{System.printInfo ' '} % get col 3 out of row 2
      {System.printInfo {Get {Get MM 3} 2}}{System.printInfo ' '} % get col 2 out of row 3
      {System.showInfo {Get {Get MM 4} 4} }     
   end
   {Application.exit 0}
end
% The Computer Language Shootout                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application System 

define
   fun {NewIncrementer Next}
      MessageList
      MessagePort = {Port.new MessageList}

      proc {Loop J|Js S}
         if Next \= nil then 
            {Next.take J}
            {Loop Js 0}

         else
            Sum = S+J
         in
            if Sum < Final then 
               {Loop Js Sum}  
            else
               {System.show Sum}
               {Application.exit 0}   % exit without cleaning up
            end
         end         
      end

      proc {Take J}
         {Port.send MessagePort J+1} 
      end
   in
      thread {Loop MessageList 0} end
      incrementer(take: Take)
   end


   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

   NThreads = 500
   Final = NThreads * N
 
   fun {ThreadChain N NextThread} 
      if N > 0 then
         {ThreadChain N-1 {NewIncrementer NextThread}}
      else 
         NextThread
      end
   end

   FirstThread = {ThreadChain NThreads nil}

in  
   for I in 1..N do {FirstThread.take 0} end
end
%%% $Id: methcall.oz,v 1.2 2004-06-26 10:37:12 ekarttun-guest Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x methcall.oz -o methcall.oz.exe
%%     methcall.oz.exe 1000000

functor
import System Application

define

class Toggle
   attr state: true
   meth state(V)
     V = @state
   end
   meth activate
      state <- {Not @state}
   end
   meth init(State)
      state <- State
   end
end

class NthToggle from Toggle
   attr trigger:0 count:0
   meth activate
      count <- @count + 1
      if @count >= @trigger then
     state <- {Not @state}
     count <- 0
      end
   end
   meth init(State Trigger)
      Toggle,init(State)
      trigger <- Trigger
      count <- 0
   end
end

fun {MethodSends N T}
   local V in
      if N==0 then {T state(V)} V
      else
     {T activate}{T state(V)}
     {MethodSends N-1 T}
      end
   end
end

in 
   local Args N in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}
      {System.show {MethodSends N {New Toggle init(true)}}}
      {System.show {MethodSends N {New NthToggle init(true 3)}}}   
   end
   {Application.exit 0}
end
% ----------------------------------------------------------------------
% The Great Computer Language Shootout                              
% http://shootout.alioth.debian.org/                                
%                                                                   
% Floating point conversion routines courtesy Juergen Stuber and Jorge
% Marques Pelizzoni [previously used in other Mozart/Oz Shooutout
% submissions].
%
% Contributed by Anthony Borla
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(file text)

define

% ------------- %

  class TextFile_
    from Open.file Open.text
  end

% ------------- %

  fun {LoadData FILE}
    case {FILE getS($)} of false then
      nil
    elseof DATUM then
      {Int.toFloat {String.toInt DATUM}}|{LoadData FILE}
    end
  end

% ------------- %

  local
    N MEAN MEDIAN AVGDEV STDDEV VARIANCE SKEW KURTOSIS

  in

    proc {ShowStatistics}
      {System.showInfo "n:                  " # {FloatToInt N}}
      {System.showInfo "median:             " # {FloatToString MEDIAN 6}}
      {System.showInfo "mean:               " # {FloatToString MEAN 6}}
      {System.showInfo "average_deviation:  " # {FloatToString AVGDEV 6}}
      {System.showInfo "standard_deviation: " # {FloatToString STDDEV 6}}
      {System.showInfo "variance:           " # {FloatToString VARIANCE 6}}
      {System.showInfo "skew:               " # {FloatToString SKEW 6}}
      {System.showInfo "kurtosis:           " # {FloatToString KURTOSIS 6}}
    end

    % ------------- %

    proc {ComputeStatistics DATATBL}

      !N = {IntToFloat {Length DATATBL}}
      !MEAN = {FoldL DATATBL fun {$ X Y} Y + X end 0.0} / N

      % ------------- %

      proc {ComputeSpread}
        DEV_ = {NewCell 0.0} AVGDEV_ = {NewCell 0.0} VARIANCE_ = {NewCell 0.0}
        SKEW_ = {NewCell 0.0} KURTOSIS_ = {NewCell 0.0}
      in
        for I in DATATBL do
          DEV_ := I - MEAN
          AVGDEV_ := @AVGDEV_ + {Number.abs @DEV_}
          VARIANCE_ := @VARIANCE_ + {Number.pow @DEV_ 2.0}
          SKEW_ := @SKEW_ + {Number.pow @DEV_ 3.0}
          KURTOSIS_ := @KURTOSIS_ + {Number.pow @DEV_ 4.0}
        end

        AVGDEV_ := @AVGDEV_ / N
        VARIANCE_ := @VARIANCE_ / (N - 1.0)
        STDDEV = {Float.sqrt @VARIANCE_}

        if @VARIANCE_ > 0.0 then
          SKEW_ := @SKEW_ / (N * @VARIANCE_ * STDDEV)
          KURTOSIS_ := @KURTOSIS_ / (N * {Number.pow @VARIANCE_ 2.0}) - 3.0
        end

        AVGDEV = @AVGDEV_ VARIANCE = @VARIANCE_ SKEW = @SKEW_ KURTOSIS = @KURTOSIS_
      end

      % ------------- %

      proc {ComputeMedian Median}
        Ni = {FloatToInt N}  Mid = (Ni div 2)  K = (Mid + 1)
      in
        Median = if Ni mod 2 \= 0 then
          {Nth DATATBL Mid}
        else
          ({Nth DATATBL K} + {Nth DATATBL Mid}) / 2.0
        end
      end

      % ------------- %

    in 
      {ComputeMedian MEDIAN}
      {ComputeSpread}
    end

  end

% ------------- %

  %% Floating Point Conversion Routines

  fun {FloatAbs X}
    if X >= 0.0 then X else ~X end
  end

  fun {FloatSquare X}
    X * X
  end

  fun {FloatPower X E}
    if E==0 then 1.0
    elseif E<0 then raise negativeExponent(E) end
    else
      if E mod 2 == 1 then X else 1.0 end
      * {FloatSquare {FloatPower X E div 2}}
    end
  end

  fun {MakePadding Ch L}
    if L > 0 then
      Padding = {MakeList L}
    in
      for V in Padding do V = Ch end
        Padding
      else
        nil
    end
  end

  fun {PadLeft Ch String L}
    PL = L - {List.length String}
  in
    {MakePadding Ch PL} # String
  end

  fun {FloatToVS F Prec}
    fun {FractionToString Frac Prec}
      if Prec =< 0 then ""
      elseif Prec > 10 then raise excessivePrecision(Prec) end
      else
        Shifted = {FloatPower 10.0 Prec} * Frac
        Digits = {FloatToInt {Round Shifted}}
        in {PadLeft &0 {IntToString Digits} Prec}
      end
    end

    I = {FloatToInt {if F >= 0.0 then Floor else Ceil end F}}
    Frac = {FloatAbs F - {IntToFloat I}}
  in
    {IntToString I} # "." # {FractionToString Frac Prec}
  end

  fun {FloatToString F Prec}
    Result = {NewCell {VirtualString.toString {FloatToVS F Prec}}}
    P = fun {$ C} if C == &~ then &- else C end end
  in
    if F < 0.0 andthen {Nth @Result 1} \= &~ then
      Result := &~|@Result 
    end
    {Map @Result P}
  end

% ------------- %

in
  {ComputeStatistics {Sort {LoadData {New TextFile_ init(name:stdin)}} fun {$ X Y} X < Y end}}
  {ShowStatistics}
  {Application.exit 0}
end

% The Computer Language Benchmarks Game 
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import
   System Application 
   S at 'Include/oz/shootout.ozf'   

define
   Pi = 3.141592653589793
   SolarMass = 4.0 * Pi * Pi
   DaysPerYear = 365.24


   class Body
      attr x y z vx vy vz mass

      meth init(X Y Z Vx Vy Vz Mass)
         x := X
         y := Y
         z := Z
         vx := Vx 
         vy := Vy
         vz := Vz
         mass := Mass 
      end

      meth x($) @x end
      meth y($) @y end
      meth z($) @z end
      meth mass($) @mass end

      meth kineticEnergy($) 0.5 * @mass * (@vx*@vx + @vy*@vy + @vz*@vz) end

      meth potentialEnergy(B $)
         Dx = @x - {B x($)} 
         Dy = @y - {B y($)} 
         Dz = @z - {B z($)} 
         Distance = {Sqrt (Dx*Dx + Dy*Dy + Dz*Dz)}
      in 
         (@mass * {B mass($)}) / Distance
      end     

      meth increaseVelocity(Dx Dy Dz M)  
         vx := @vx + Dx*M
         vy := @vy + Dy*M
         vz := @vz + Dz*M
      end    

      meth decreaseVelocity(Dx Dy Dz M)  
         vx := @vx - Dx*M
         vy := @vy - Dy*M
         vz := @vz - Dz*M
      end    
  
      meth addMomentumTo(?A)
         A.1 := A.1 + @vx*@mass
         A.2 := A.2 + @vy*@mass
         A.3 := A.3 + @vz*@mass
      end

      meth offsetMomentum(A)
         vx := ~ A.1 / SolarMass
         vy := ~ A.2 / SolarMass
         vz := ~ A.3 / SolarMass
      end

      meth updatePositionAfter(Dt)
         x := @x + Dt*@vx
         y := @y + Dt*@vy
         z := @z + Dt*@vz
      end

      meth updateVelocitiesAfter(Dt ?B)
         Dx = @x - {B x($)} 
         Dy = @y - {B y($)} 
         Dz = @z - {B z($)} 
         Distance = {Sqrt (Dx*Dx + Dy*Dy + Dz*Dz)}
         Magnitude = Dt / (Distance * Distance * Distance)
      in 
         {self decreaseVelocity(Dx Dy Dz ({B mass($)} * Magnitude))}
         {B increaseVelocity(Dx Dy Dz (@mass * Magnitude))}
      end

   end   % Body


   Sun = {New Body init(0.0 0.0 0.0 0.0 0.0 0.0 SolarMass)}

   Jupiter = {New Body init(
      4.84143144246472090e00 
      ~1.16032004402742839e00 
      ~1.03622044471123109e~01
      1.66007664274403694e~03 * DaysPerYear
      7.69901118419740425e~03 * DaysPerYear
      ~6.90460016972063023e~05 * DaysPerYear
      9.54791938424326609e~04 * SolarMass 
      )}

   Saturn = {New Body init(
      8.34336671824457987e00
      4.12479856412430479e00
      ~4.03523417114321381e~01
      ~2.76742510726862411e~03 * DaysPerYear
      4.99852801234917238e~03 * DaysPerYear
      2.30417297573763929e~05 * DaysPerYear
      2.85885980666130812e~04 * SolarMass
      )}

   Uranus = {New Body init(
      1.28943695621391310e01
      ~1.51111514016986312e01
      ~2.23307578892655734e~01
      2.96460137564761618e~03 * DaysPerYear
      2.37847173959480950e~03 * DaysPerYear
      ~2.96589568540237556e~05 * DaysPerYear
      4.36624404335156298e~05 * SolarMass
      )}

   Neptune = {New Body init(
      1.53796971148509165e01
      ~2.59193146099879641e01
      1.79258772950371181e~01
      2.68067772490389322e~03 * DaysPerYear
      1.62824170038242295e~03 * DaysPerYear
      ~9.51592254519715870e~05 * DaysPerYear
      5.15138902046611451e~05 * SolarMass
      )}


   class NBodySystem
      attr bodies

      meth init()
         bodies := {Tuple.toArray Sun # Jupiter # Saturn # Uranus # Neptune} 
         A = {Tuple.toArray 0.0 # 0.0 # 0.0} 
         N = {Array.high @bodies} 
      in
         for I in 1..N do {@bodies.I addMomentumTo(A)} end
         {@bodies.1 offsetMomentum(A)}     
      end

      meth after(Dt)
         N = {Array.high @bodies} 
      in 
         for I in 1..N do 
            for J in I+1..N do 
               {@bodies.I updateVelocitiesAfter(Dt @bodies.J)}
            end
         end
         for I in 1..N do {@bodies.I updatePositionAfter(Dt)} end
      end

      meth energy($)
         E = {NewCell 0.0} 
         N = {Array.high @bodies} 
      in
         for I in 1..N do
            E := @E + {@bodies.I kineticEnergy($)}
            for J in I+1..N do
               E := @E - {@bodies.I potentialEnergy(@bodies.J $)}
            end
         end
         @E
      end

   end   % NBodySystem


   NBody = {New NBodySystem init}

   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

in   
   {System.showInfo {S.floatToString {NBody energy($)} 9}}
   for I in 1..N do {NBody after(0.01)} end
   {System.showInfo {S.floatToString {NBody energy($)} 9}}
   {Application.exit 0}   
end
%%% $Id: nestedloop.oz,v 1.2 2004-07-04 07:09:26 bfulgham Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% Isaac Gouy suggested the use of a cell

functor
import System Application
define

    local P X A B C D E F N in
        [P] = {Application.getArgs plain}
        N = {String.toInt P}
        X = {NewCell 0}
        for A in 1..N do
            for B in 1..N do
                for C in 1..N do
                    for D in 1..N do
                        for E in 1..N do
                            for F in 1..N do
                                {Assign X {Access X}+1}
                            end
                        end
                    end
                end
            end
        end
        {System.show {Access X}}
    end
    {Application.exit 0}
end
% The Computer Language Benchmarks Game                             
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import System Application 

define
   fun {NSieve N ?IsPrime} 
      Count = {NewCell 0} 
   in
      for I in 2..N do IsPrime.I := true end

      for I in 2..N do 
         if IsPrime.I then
            Count := @Count + 1
            for K in I+I; K=<N; K+I do IsPrime.K := false end              
         end
      end
      @Count
   end


   proc {Line N A}
      S = "Primes up to " N1 = N*10000 in
         {System.showInfo S # {Pad N1 8} # " " # {Pad {NSieve N1 A} 8} }
   end

   fun {Pad I W} 
      S = {NewCell {IntToString I}}
      L = W - {Length @S}
   in
      for I in 1..L do S := {Append " " @S} end
      @S
   end

   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

   BooleanArray = {NewArray 0 ({Pow 2 N  }*10000)+1 true}

in   
   {Line {Pow 2 N  } BooleanArray}
   {Line {Pow 2 N-1} BooleanArray}
   {Line {Pow 2 N-2} BooleanArray}
   {Application.exit 0}   
end

% The Computer Language Shootout
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import System Application 

define
   fun {NSieve N ?IsPrime} 
      Count = {NewCell 0} 
   in
      for I in 2..N do {BitArray.set IsPrime I} end

      for I in 2..N do 
         if {BitArray.test IsPrime I} then
            Count := @Count + 1
            for K in I+I; K=<N; K+I do {BitArray.clear IsPrime K} end              
         end
      end
      @Count   % {BitArray.card IsPrime} would work when N == {BitArray.high IsPrime}
   end


   proc {Line N A}
      S = "Primes up to " N1 = N*10000 in
         {System.showInfo S # {Pad N1 8} # " " # {Pad {NSieve N1 A} 8} }
   end

   fun {Pad I W} 
      S = {NewCell {IntToString I}}
      L = W - {Length @S}
   in
      for I in 1..L do S := {Append " " @S} end
      @S
   end

   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

   BooleanArray = {BitArray.new 0 ({Pow 2 N  }*10000)+1}

in   
   {Line {Pow 2 N  } BooleanArray}
   {Line {Pow 2 N-1} BooleanArray}
   {Line {Pow 2 N-2} BooleanArray}
   {Application.exit 0}   
end
%%% $Id: objinst.oz,v 1.3 2004-06-26 10:37:12 ekarttun-guest Exp $
%%% http://dada.perl.it/shootout/
%%% 
%%% contributed by Isaac Gouy

%% Uses local variables within the Object Instantiation
%% loops. It might be quicker to create a Cell outside 
%% of the loop and use {Assign T {New Toggle init(true)}}
%%
%%  Usage: start from command line with
%%     ozc -x objinst.oz -o objinst.oz.exe
%%     objinst.oz.exe 1000000

functor
import System Application

define

class Toggle
   attr state: true
   meth state(V)
     V = @state
   end
   meth activate
      state <- {Not @state}
   end
   meth init(State)
      state <- State
   end
end

class NthToggle from Toggle
   attr trigger:0 count:0
   meth activate
      count <- @count + 1
      if @count >= @trigger then
     state <- {Not @state}
     count <- 0
      end
   end
   meth init(State Trigger)
      Toggle,init(State)
      trigger <- Trigger
      count <- 0
   end
end

in 
   local Args N T1 T2 in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}

      T1 = {New Toggle init(true)}
      for I in 1..5 do
         local V in {T1 activate}{T1 state(V)}{System.show V} end
      end
      {System.showInfo ""}
      for I in 1..N do
         local T in T = {New Toggle init(true)} end
      end
   
      T2 = {New NthToggle init(true 3)}
      for I in 1..8 do
         local V in {T2 activate}{T2 state(V)}{System.show V} end
      end
   
      for I in 1..N do
         local T in T = {New NthToggle init(true 3)} end
      end
   end
   {Application.exit 0}
end
% The Computer Language Shootout                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import
   System Application
   S at '../../Include/oz/shootout.ozf'  

define
   TwoThirds = 2.0/3.0
   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}

   fun {Sums I A1 A2 A3 A4 A5 A6 A7 A8 A9 Alt}
      if I =< N then
         K K2 K3 SK CK 
      in
         K = {IntToFloat I}
         K2 = K * K
         K3 = K2 * K
         SK = {Sin K}
         CK = {Cos K}

         {Sums 
            I+1 	 
            A1 + {Pow TwoThirds K-1.0}
            A2 + 1.0/{Sqrt K}
            A3 + 1.0/(K*(K+1.0))
            A4 + 1.0/(K3*(SK*SK))
            A5 + 1.0/(K3*(CK*CK))	 
            A6 + 1.0/K
            A7 + 1.0/K2
            A8 + Alt/K
            A9 + Alt/(2.0*K - 1.0)
            ~Alt }
      else
         A1 # A2 # A3 # A4 # A5 # A6 # A7 # A8 # A9
      end
   end

   A = {Sums 1 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 }
in   
   {System.showInfo {S.floatToString A.1 9}#"\t(2/3)^k"}
   {System.showInfo {S.floatToString A.2 9}#"\tk^-0.5"}
   {System.showInfo {S.floatToString A.3 9}#"\t1/k(k+1)"}
   {System.showInfo {S.floatToString A.4 9}#"\tFlint Hills"}
   {System.showInfo {S.floatToString A.5 9}#"\tCookson Hills"}
   {System.showInfo {S.floatToString A.6 9}#"\tHarmonic"}
   {System.showInfo {S.floatToString A.7 9}#"\tRiemann Zeta"}
   {System.showInfo {S.floatToString A.8 9}#"\tAlternating Harmonic"}
   {System.showInfo {S.floatToString A.9 9}#"\tGregory"}
   {Application.exit 0}   
end
% The Computer Language Benchmarks Game  
% http://shootout.alioth.debian.org/
%                                                                   
% Contributed by YANG Shouxun


functor
import
   System(showInfo printInfo) Application(exit getArgs)

define
   fun {Next Z}
      case Z of [Q R S T] then (3*Q + R) div (3*S + T)
      else raise invalidArg(Z) end
      end
   end

   fun {Safe Z N}
      case Z of [Q R S T] then N == ((4*Q + R) div (4*S + T))
      else raise invalidArg(Z) end
      end
   end

   fun {Comp Z1 Z2}
      case Z1#Z2
      of [Q1 R1 S1 T1]#[Q2 R2 S2 T2] then
	 [(Q1*Q2+R1*S2) (Q1*R2+R1*T2) (S1*Q2+T1*S2) (S1*R2+T1*T2)]
      else raise invalidArg(Z1#Z2) end
      end
   end

   fun {Prod Z N} {Comp [10 ~10*N 0 1] Z} end

   fun {Cons Z K} {Comp Z [K 4*K+2 0 2*K+1]} end

   proc {PrintNSpace N}
      proc {Aux _} {System.printInfo " "} end
   in {For 1 N 1 Aux} end

   proc {Digit K Z N Row Col}
      if N > 0 then Y in
	 Y = {Next Z}
	 if {Safe Z Y} then
	    if Col == 10 then
	       {System.printInfo "\t:"#(10+Row)#"\n"#Y}
	       {Digit K {Prod Z Y} N-1 10+Row 1}
	    else
	       {System.printInfo Y}
	       {Digit K {Prod Z Y} N-1 Row Col+1}
	    end
	 else
	    {Digit K+1 {Cons Z K} N Row Col}
	 end
      else
	 {PrintNSpace 10-Col}
	 {System.showInfo "\t:"#(Row+Col)}
      end
   end

   proc {Digits N} {Digit 1 [1 0 0 1] N 0 0} end

in
   {Digits {String.toInt {Application.getArgs plain}.1}}
   {Application.exit 0}
end
%%% $Id: prodcons.oz,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
%%% http://dada.perl.it/shootout/
%%%
%%% contributed by Isaac Gouy

%%  Section 11.5 of the Oz Tutorial provides these
%%  implementations of Event and UnitBufferM and states:
%%  in Oz, it is very rare to write programs in the 
%%  (traditional) monitor style shown above. In general
%%  it is very awkward.
%%
%%  There's an extensive treatment of Oz concurrency in 
%%  the book 'Concepts, Techniques, and Models of Computer 
%%  Programming' - find it online with google.
%%
%%  Usage: start from command line with
%%     ozc -x prodcons.oz -o prodcons.oz.exe
%%     prodcons.oz.exe 100000

functor
import System Application

define

Produced
Consumed

class Event from BaseObject 
   prop locking
   attr f r
   meth init 
      X in f <- X r <- X
   end 
   meth put(I)
      X in lock @r=I|X r<-X end 
   end 
   meth get(?I)
      X in lock @f=I|X f<-X end {Wait I}
   end
   meth wait 
      {self get(_)}
   end 
   meth notify 
      {self put(unit)}
   end    
end


class UnitBufferM 
   attr item empty psignal csignal
   prop locking
   meth init 
      empty <- true 
      psignal <- {New Event init}
      csignal <- {New Event init}
   end 
   meth put(I)
      X in 
      lock 
         if @empty then 
            item <- I
            empty <- false 
            X = yes
            {@csignal notify}
         else X = no end 
      end 
      if X == no then 
         {@psignal wait}
         {self put(I)}
      end 
   end 
   meth get(I)
      X in 
      lock 
         if {Not @empty} then 
            I = @item
            empty <- true 
            {@psignal notify}
            X = yes
         else X = no end 
      end 
      if X == no then 
         {@csignal wait}
         {self get(I)}
      end 
   end 
end


proc {Producer N I B}
   if N > 0 then
      {B put(I)}
      %% {System.showInfo 'Produced '#I} %% just to check synchronization
      {Producer N-1 I+1 B}
   else Produced = {NewCell I} end
end 


proc {Consumer N I B}
   if N > 0 then
      {B get(I)}
      %% {System.showInfo 'Consumed '#I} %% just to check synchronization
      {Consumer N-1 I+1 B}
   else Consumed = {NewCell I} end
end 


in
   local Args N UB in
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}

      UB = {New UnitBufferM init}
      thread {Producer N 0 UB} end
      thread {Consumer N 0 UB} end

         %% Oz is a dataflow language.
         %% The main thread will wait until logic variables  
         %% Produced and Consumed have been given values
      {System.showInfo {Access Produced}#' '#{Access Consumed}}
   end
   {Application.exit 0}
end


%%% $Id: random.oz,v 1.3 2005-03-06 05:21:24 bfulgham Exp $
%%% http://shootout.alioth.debian.org/
%%% Using string conversion routines suggested by Juergen Stuber
%%%   and Jorge Marques Pelizzoni
functor
import System Application Property
define
   fun {RandLoop N SEED R MAX}
      case N 
      of 0 then R
      else 
	 local IA IC IM NEWSEED NEWRAND in
	    IM = 139968
	    IA =   3877
	    IC =  29573
	    NEWSEED = (SEED * IA + IC) mod IM
	    NEWRAND = MAX * {Int.toFloat NEWSEED}/{Int.toFloat IM}
	    {RandLoop N-1 NEWSEED NEWRAND MAX}
	 end
      end
   end
   fun {FloatAbs X}
      if X >= 0.0 then X else ~X end
   end

   fun {FloatSquare X}
      X * X
   end

   fun {FloatPower X E}
      if E==0 then 1.0
      elseif E<0 then raise negativeExponent(E) end
      else
	 if E mod 2 == 1 then X else 1.0 end
	 * {FloatSquare {FloatPower X E div 2}}
      end
   end

   fun {MakePadding Ch L}
      if L > 0 then
	 Padding = {MakeList L}
      in
	 for V in Padding do V = Ch end
	 Padding
      else
	 nil
      end
   end

   fun {PadLeft Ch String L}
      PL = L - {List.length String}
   in
      {MakePadding Ch PL} # String
   end

   fun {FloatToVS F Prec}
      fun {FractionToString Frac Prec}
	 if Prec =< 0 then ""
	 elseif Prec > 9 then raise excessivePrecision(Prec) end
	 else
	    Shifted = {FloatPower 10.0 Prec} * Frac
	    Digits = {FloatToInt {Round Shifted}}
	 in
	    {PadLeft &0 {IntToString Digits} Prec}
	 end
      end

      I = {FloatToInt {if F >= 0.0 then Floor else Ceil end F}}
      Frac = {FloatAbs F - {IntToFloat I}}
   in
      {IntToString I} # "." # {FractionToString Frac Prec}
   end

   fun {FloatToString F Prec}
      {VirtualString.toString {FloatToVS F Prec}}
   end
in 
    local A NUM I in
       [A] = {Application.getArgs plain}
       NUM = {String.toInt A}
       {Property.put 'print.width' 12}
       {System.printInfo {FloatToString {RandLoop NUM 42 0 100.0} 9}}
       {System.printInfo "\n"}
    end
    {Application.exit 0}
end
% ----------------------------------------------------------------------
% The Computer Language Shootout                              
% http://shootout.alioth.debian.org/
% Contributed by Anthony Borla
% modified by Isaac Gouy
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit getArgs)
   S at '../../Include/oz/shootout.ozf'

define



  fun {Ack X Y}
    if X == 0 then Y + 1
    elseif Y == 0 then {Ack (X - 1) 1}
    else {Ack (X - 1) {Ack X (Y - 1)}}
    end
  end



  fun {Fib N}
    if N < 2 then 1
    else {Fib (N - 2)} + {Fib (N - 1)}
    end
  end



  fun {FibFlt N}
    if N < 2.0 then 1.0
    else {FibFlt (N - 2.0)} + {FibFlt (N - 1.0)}
    end
  end



  fun {Tak X Y Z}
    if Y < X then {Tak {Tak (X - 1) Y Z} {Tak (Y - 1) Z X} {Tak (Z - 1) X Y}}
    else Z
    end
  end



  fun {TakFlt X Y Z}
    if Y < X then {TakFlt {TakFlt (X - 1.0) Y Z} {TakFlt (Y - 1.0) Z X} {TakFlt (Z - 1.0) X Y}}
    else Z
    end
  end



  fun {CmdlNArg Nth Default}
    N Nt in
    try
      Nt = {String.toInt {Application.getArgs plain}.Nth}
      N = if Nt < Default then Default else Nt end
    catch error(...) then
      N = Default
    end
    N
  end


  N NFlt NAdj


in
  N = {CmdlNArg 1 1} NFlt = {Int.toFloat N} NAdj = N - 1

  {System.showInfo "Ack(3," # N # "): " # {Ack 3 N}}
  {System.showInfo "Fib(" # (27.0 + NFlt) # "): " # {S.floatToString {FibFlt (27.0 + NFlt)} 1}}

  {System.showInfo "Tak(" # NAdj * 3 # "," # NAdj * 2 # "," # NAdj # "): " # {Tak (NAdj * 3) (NAdj * 2) NAdj}}

  {System.showInfo "Fib(3): " # {Fib 3}}
  {System.showInfo "Tak(3.0,2.0,1.0): " # {S.floatToString {TakFlt 3.0 2.0 1.0} 1}}

  {Application.exit 0}
end

% The Computer Language Shootout                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import 
   Application System
   S at '../../Include/oz/shootout.ozf'

define
   fun {Fib One Two}
      F = fun {$ N} 
         if N < Two then One 
         else {F N-One} + {F N-Two} end 
      end
   in F end

   fun {Tak One}
      F = fun {$ X Y Z}
         if Y < X then {F {F X-One Y Z} {F Y-One Z X} {F Z-One X Y}}
         else Z end
      end
   in F end

   FibI = {Fib 1   2  }
   FibF = {Fib 1.0 2.0}
   TakI = {Tak 1  }
   TakF = {Tak 1.0}

   fun {AckI M N}
      if M == 0 then N + 1 
      elseif N == 0 then {AckI M-1 1} 
      else {AckI M-1 {AckI M N-1}} end 
   end

   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}
   M = N-1
   A = 27.0 + {IntToFloat N}

   Prefix1 = "Fib(" # {S.floatToString A 1} # "): "
   Prefix2 = "Tak(" # 3*M # "," # 2*M # "," # M # "): "
in
   {System.showInfo "Ack(3," # N # "): " # {AckI 3 N}}
   {System.showInfo Prefix1 # {S.floatToString {FibF A} 1} }
   {System.showInfo Prefix2 # {TakI 3*M 2*M 1*M}}
   {System.showInfo "Fib(3): " # {FibI 3}}
   {System.showInfo "Tak(3.0,2.0,1.0): " # {S.floatToString {TakF 3.0 2.0 1.0} 1} }
   {Application.exit 0}   
end
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game  
% http://shootout.alioth.debian.org/
%
% Contributed by Anthony Borla [with thanks to Kevin Glynn]
% Further modified by YANG Shouxun
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(file text)
  Regex at 'x-oz://contrib/regex'

define

% ------------- %

  VARIANTS = [
    "agggtaaa|tttaccct" "[cgt]gggtaaa|tttaccc[acg]"
    "a[act]ggtaaa|tttacc[agt]t" "ag[act]gtaaa|tttac[agt]ct"
    "agg[act]taaa|ttta[agt]cct" "aggg[acg]aaa|ttt[cgt]ccct"
    "agggt[cgt]aa|tt[acg]accct" "agggta[cgt]a|t[acg]taccct"
    "agggtaa[cgt]|[acg]ttaccct"]

  IUBS = ["B"#"(c|g|t)" "D"#"(a|g|t)" "H"#"(a|c|t)" "K"#"(g|t)"
	  "M"#"(a|c)" "N"#"(a|c|g|t)" "R"#"(a|g)" "S"#"(c|g)"
	  "V"#"(a|c|g)" "W"#"(a|t)" "Y"#"(c|t)"]

  LF = "\n"

% ------------- %

  class TextFile_
    from Open.file Open.text
  end

% ------------- %

  Initial_Length Code_Length SEQ = {NewCell nil}

% ------------- %

in
  % Load file as a list and record its length
  SEQ := {{New TextFile_ init(name:stdin)} read(list:$ size:all)}
  Initial_Length = {Length @SEQ}

  % Remove newline and segment divider line occurrences
  SEQ := {Regex.replace @SEQ {Regex.make "(>.*\n)|(\n)"} fun {$ X Y} "" end}
  Code_Length = {ByteString.length @SEQ}

  % Perform regexp counts
  for Item in VARIANTS do
    {System.showInfo Item # " " # {Length {Regex.allMatches {Regex.compile Item [icase extended]} @SEQ}}}
  end

  % Perform replacements
  for Key#S in IUBS do
     SEQ := {Regex.replace @SEQ {Regex.compile Key [icase]} fun {$ X Y} S end}
  end

  % Print statistics
  {System.showInfo LF # Initial_Length # LF # Code_Length # LF # {ByteString.length @SEQ}}

  {Application.exit 0}
end
% The Computer Language Benchmarks Game  
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application Open

define
   fun {NextHeader F}
      case {F getS($)} 
      of false then false
      [] &>|_ = Header then Header
      else {NextHeader F} end
   end

   fun {ReversedLines F L}
      case {F getS($)} 
      of false then L # false
      [] &>|_ = Header then L # Header
      [] &;|_ then {ReversedLines F L}
      [] Line then {ReversedLines F Line|L} end
   end


   local
      fun {IubCodeComplements}
         Code = "ABCDGHKMNRSTVWYabcdghkmnrstvwy"
         Comp = "TVGHCDMKNYSABWRTVGHCDMKNYSABWR"
         A = {NewArray 1 &z &*}
      in
         {ForAll
            {List.zip Code Comp fun{$ K V} K#V end}
               proc{$ K#V} A.K := V end}
         A
      end

      IUB = {IubCodeComplements}

   in
      proc {WriteReverseComplement FOut FirstLine|RemainingLines}
         ShortestLength = {Length FirstLine}

         fun {ReverseComplement L C} IUB.C|L end

         fun {FastaReverseComplement I L C} 
            if I == ShortestLength then &\n|IUB.C|L 
            else IUB.C|L end 
         end

      in 
         {FOut write(vs: {FoldL FirstLine ReverseComplement nil}) }

         for Line in RemainingLines do
            {FOut write(vs: {List.foldLInd Line FastaReverseComplement nil})}
         end
         {FOut write(vs: "\n")}
      end
   end


   proc {ReadReverseComplementWrite F Header FOut}
      if Header \= false then 
         Lines # NextHeader = {ReversedLines F nil}
      in
         {FOut write(vs: Header # "\n")}
         {WriteReverseComplement FOut Lines}
         {ReadReverseComplementWrite F NextHeader FOut}
      end
   end

   class TextFile from Open.file Open.text end
   StdIn = {New TextFile init(name:stdin)}
   StdOut = {New Open.file init(name:stdout)}
in
   {ReadReverseComplementWrite StdIn {NextHeader StdIn} StdOut}
   {Application.exit 0}   
end
% ----------------------------------------------------------------------
% The Great Computer Language Shootout                              
% http://shootout.alioth.debian.org/                                
%                                                                   
% Contributed by Anthony Borla
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(text file)

define

% ------------- %

  class TextFile_
    from Open.file Open.text
  end

  proc {ReverseFile FILE}
    case {FILE getS($)} of false then
      skip
    elseof LINE then
      {ReverseFile FILE}
      {System.showInfo LINE}
    end
  end

% ------------- %

in
  {ReverseFile {New TextFile_ init(name:stdin)}}
  {Application.exit 0}
end

% ----------------------------------------------------------------------
% The Great Computer Language Shootout                              
% http://shootout.alioth.debian.org/                                
%                                                                   
% Attempt at a reasonably memory-efficient implementation which should:
%                                                                   
% * Handle 'large' files [~ >2MB] faster than existing version
% * Handle arbitrarily large files [~ >10MB] files without crashing
%   [unlike existing version which, since it uses stack unwinding to perform
%   its task, is very sensitive to the input file size]
%                                                                   
% Use made of code from 'Concepts, Techniques and Models of Computer
% Programming' [CTM] by P. van Roy, S. Haridi.
% 
% Contributed by Anthony Borla
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(text file)

define

% ------------- %

  class TextFile_
    from Open.file Open.text
  end

% ------------- %

  %% General Purpose Stateful Stack [CTM implementation]

  fun {NewStack}
    C = {NewCell nil}

    proc {Push X} S in S = @C C := X|S end

    fun {Pop} S1 in
      S1 = @C
      case S1 of X|S then
        C := S
        X
      end
    end

    fun {IsEmpty} S in S = @C S == nil end
  in
    ops(push:Push pop:Pop isEmpty:IsEmpty)
  end

% ------------- %

  local
    LF = &\012 Lines = {NewStack} Spill = {NewCell nil}
  in
    proc {ProcessBuffer Buffer} Ys Yr in
      {String.token Buffer LF Ys Yr}
      {SplitAndPrint Yr}
      Spill := Ys
    end

    proc {FlushBuffer}
      {DumpSpill}      
    end

    proc {DumpLines}
      if {Lines.isEmpty} then skip
      else {System.showInfo {Lines.pop}} {DumpLines} 
      end
    end

    proc {DumpSpill}
      if @Spill \= nil then {System.showInfo @Spill} Spill := nil end
    end

    proc {SplitAndPrint Xs}
      case Xs of nil then skip
      else Ys Yr in
        {String.token Xs LF Ys Yr}

        %% Should really handle blank / LF-only lines here ...
        %% case Ys#Yr of nil#nil then ...
        %% [] nil#_ then ...
        %% [] _#nil then ...

        case Yr of nil then
          if {List.last Xs} \= LF then
            Spill := {List.append Ys @Spill}
          else
            {Lines.push Ys}
          end
          {DumpSpill}
          {DumpLines} 
        else
          {Lines.push Ys}          
          {SplitAndPrint Yr}
        end
      end 
    end

  end

% ------------- %

  proc {ReverseFile FILE BufferSize}

    proc {ReadBuffer RemainingBytes} BytesRead Buffer StartPos ToRead in
      if RemainingBytes < 1 then
        {FlushBuffer}
      else
        ToRead = if RemainingBytes < BufferSize then RemainingBytes else BufferSize end
        {FILE seek(whence:current offset:~ToRead)}
        {FILE tell(offset:StartPos)}
        {FILE read(list:Buffer size:ToRead len:BytesRead)}
        {FILE seek(whence:set offset:StartPos)}
        {ProcessBuffer Buffer}
        {ReadBuffer (RemainingBytes - BytesRead)}
      end
    end

    RemainingBytes
  in
    {FILE seek(whence:'end' offset:0)}
    {FILE tell(offset:RemainingBytes)}
    {ReadBuffer RemainingBytes}
  end

% ------------- %

  READSIZE = 4096

% ------------- %

in
  {ReverseFile {New TextFile_ init(name:stdin flags:[read text])} READSIZE}
  {Application.exit 0}
end

%%% $Id: sieve.oz,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
%%% http://dada.perl.it/shootout/

%%% 
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x sieve.oz -o sieve.oz.exe
%%     sieve.oz.exe 900

functor
import System Application

define Args N Flags Start Stop in

    [Args] = {Application.getArgs plain}
    N = {String.toInt Args}

    Start = 2
    Stop = 8192

    Flags = {BitArray.new Start Stop}
    for I in Start..Stop do {BitArray.set Flags I} end

    for I in 1..N do
           for J in Start..Stop do
            if {BitArray.test Flags J} then
                for K in J+J..Stop;J do {BitArray.clear Flags K} end 
            end
        end
    end

   {System.showInfo "Count: "#{BitArray.card Flags}}

   {Application.exit 0}
end
% The Computer Language Benchmarks Game                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import
   System Application 
   S at 'Include/oz/shootout.ozf'  

define
   [Arg] = {Application.getArgs plain}
   N = {String.toInt Arg}
   M = N-1

   % return element i,j of infinite matrix A
   fun {A I J} 1.0 / {IntToFloat (I+J)*(I+J+1) div 2 + I+1} end

   % multiply vector v by matrix A
   proc {MultiplyAv V ?Av}
      for I in 0..M do
         Av.I := 0.0
         for J in 0..M do 
            Av.I := Av.I + {A I J}*V.J
         end
      end
   end

   % multiply vector v by matrix A transposed
   proc {MultiplyAtv V ?Atv}
      for I in 0..M do
         Atv.I := 0.0 
         for J in 0..M do 
            Atv.I := Atv.I + {A J I}*V.J
         end
      end
   end

   % multiply vector v by matrix A and then by matrix A transposed
   proc {MultiplyAtAv V ?AtAv}
      U = {NewArray 0 M 0.0} 
   in 
      {MultiplyAv V U}
      {MultiplyAtv U AtAv}
   end

   U = {NewArray 0 M 1.0}
   V = {NewArray 0 M 0.0}

   Vbv = {NewCell 0.0}
   Vv = {NewCell 0.0}

in   
   % 20 steps of the power method
   for I in 1..10 do 
      {MultiplyAtAv U V}
      {MultiplyAtAv V U}
   end

   for I in 0..M do 
      Vbv := @Vbv + U.I*V.I 
      Vv := @Vv + V.I*V.I 
   end

   {System.showInfo {S.floatToString {Sqrt @Vbv/@Vv} 9}}
   {Application.exit 0}   
end
% ----------------------------------------------------------------------
% The Great Computer Language Shootout                              
% http://shootout.alioth.debian.org/                                
%                                                                   
% Contributed by Anthony Borla
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(text file)

define

% ------------- %

  class TextFile_
    from Open.file Open.text
  end

% ------------- %

  proc {LoadDictionary FILE DICTIONARY}
    case {FILE getS($)} of false then
      skip
    elseof WORD then
      {Dictionary.put DICTIONARY {String.toAtom WORD} true}
      {LoadDictionary FILE DICTIONARY}
    end
  end

% ------------- %

  proc {CheckAgainstDictionary FILE DICTIONARY}
    case {FILE getS($)} of false then
      skip
    elseof WORD then
      if {Not {Dictionary.member DICTIONARY {String.toAtom WORD}}} then
        {System.showInfo WORD}
      end
      {CheckAgainstDictionary FILE DICTIONARY}
    end
  end

% ------------- %

  DICTIONARY = {NewDictionary}

% ------------- %

in
  {LoadDictionary {New TextFile_ init(name:'Usr.Dict.Words')} DICTIONARY}
  {CheckAgainstDictionary {New TextFile_ init(name:stdin)} DICTIONARY}
  {Application.exit 0}
end

%%% $Id: strcat.oz,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
%%% http://dada.perl.it/shootout/
%%%
%%% contributed by Isaac Gouy

%%  Usage: start from command line with
%%     ozc -x strcat.oz -o strcat.oz.exe
%%     strcat.oz.exe 40000

functor
import System Application

define

fun {RepeatAppend N S}
   if N == 0 then S else
      {RepeatAppend N-1 {Append "hello\n" S}} end
end

in
   local Args N in 
      [Args] = {Application.getArgs plain}
      N = {String.toInt Args}

      {System.showInfo {Length {RepeatAppend N nil}} }
   end
   {Application.exit 0}
end
% The Computer Language Shootout                              
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application System Open

define
   fun {Sum F S}
      L = {F getS($)} 
   in 
      if L == false then S else {Sum F S+{String.toInt L}} end
   end

   class TextFile from Open.file Open.text end
   StdIn = {New TextFile init(name:stdin)}
 
in    
   {System.showInfo {Sum StdIn 0} }
   {Application.exit 0}   
end
%%% $Id: takfp.oz,v 1.1 2005-03-07 05:49:47 bfulgham Exp $
%%% http://shootout.alioth.debian.org/
functor
import System Application
define
   fun {Tak X Y Z}
      if (Y >= X) then
	 Z
      else
	 {Tak
	  {Tak (X - 1.0) Y Z}
	  {Tak (Y - 1.0) Z X}
	  {Tak (Z - 1.0) X Y}}
      end
   end
   A N
in 
   [A] = {Application.getArgs plain}
   N = {Int.toFloat {String.toInt A}}
   {System.show {Tak (N*3.0) (N*2.0) (N*1.0)}}
   {Application.exit 0}
end
% The Computer Language Benchmarks Game                             
% http://shootout.alioth.debian.org/    
% contributed by Isaac Gouy

functor
import Application System 

define
   fun {NewThread Name Next}
      MessageList
      MessagePort = {Port.new MessageList}

      proc {Loop Token|Tokens}     % wait for list-head list-tail pattern to
         if Token > 0 then         % match and bind Token
            {Next.take Token-1}
            {Loop Tokens}
         else
            {System.show Name}
            {Application.exit 0}   % exit without cleaning up
         end         
      end

      proc {Take Token}
         {Port.send MessagePort Token} 
      end
   in
      thread {Loop MessageList} end % spawn a thread 
      newthread(take: Take)         % let function Take be used elsewhere
   end


   fun {NewRing NumberOfThreads}

      fun {MakeRing N NextThread} 
         if N > 0 then
            {MakeRing N-1 {NewThread N NextThread}}
         else 
            NextThread
         end
      end

      FirstThread
   in
      % The unbound logic variable FirstThread is passed into recursive 
      % function MakeRing and the value returned by that function is then
      % bound to variable FirstThread, closing the thread ring.

      FirstThread = {MakeRing NumberOfThreads FirstThread}
   end


   [Arg] = {Application.getArgs plain}

in  
   {{NewRing 503}.take {String.toInt Arg}}
end
% ----------------------------------------------------------------------
% The Great Computer Language Shootout                              
% http://shootout.alioth.debian.org/                                
%                                                                   
% Contributed by Anthony Borla
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(text file)

define

% ------------- %

  SPACE = &\040  NEWLINE_LENGTH = 1

% ------------- %

  class TextFile_
    from Open.file Open.text
  end

% ------------- %

  fun {CountLinesWordsChars FILE}

    fun {CountLinesWordsChars_ LINES WORDS CHARS}
      case {FILE getS($)} of false then
        [LINES WORDS CHARS]
      elseof LINE then
        {CountLinesWordsChars_ (LINES + 1) (WORDS + {CountWords LINE}) (CHARS + {CountChars LINE NEWLINE_LENGTH})}
      end
    end

    % ------------- %

    fun {CountWords LINE}
      {Length {String.tokens {CompressWhiteSpace {LeftTrim LINE}} SPACE}}
    end

    % ------------- %

    fun {CountChars LINE PADDING}
      {Length LINE} + PADDING
    end

  in
    {CountLinesWordsChars_ 0 0 0}
  end

% ------------- %

  fun {LeftTrim S}
    {List.dropWhile S Char.isSpace}
  end

  fun {CompressWhiteSpace S}
    {Compress S false Char.isSpace}
  end

  fun {Compress S Flag P}
    case S of nil then nil
    elseof H|T then Pt in
      if (Pt = {P H}) andthen Flag then
        {Compress T true P}
      else
        H|{Compress T Pt P}
      end
    end
  end

% ------------- %

  LINES WORDS CHARS

% ------------- %

in
  [LINES WORDS CHARS] = {CountLinesWordsChars {New TextFile_ init(name:stdin)}}
  {System.showInfo LINES # " " # WORDS # " " # CHARS}
  {Application.exit 0}
end

% ----------------------------------------------------------------------
% The Great Computer Language Shootout                              
% http://shootout.alioth.debian.org/                                
%
% An attempt at a faster [approx. 50% better] implementation via:
%
% * Utilising 4K read buffer [rather than reading line-by-line]
% * Computing results in a single traversal of the input data [rather
%   than multiply traversing, or otherwise manipulating, input data]
%
% Contributed by Anthony Borla
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(text file)

define

% ------------- %

  LF = &\012 READSIZE = 4096

% ------------- %

  class TextFile_
    from Open.file Open.text
  end

% ------------- %

  fun {CountLinesWordsChars FILE BufferSize}

    Lines = {NewCell 0} Words = {NewCell 0} Chars = {NewCell 0} RemainingBytes

    % ------------- %

    proc {ReadBuffer RemainingBytes}
      BytesRead Buffer ToRead
    in
      if RemainingBytes < 1 then
        skip
      else
        ToRead = if RemainingBytes < BufferSize then RemainingBytes else BufferSize end
        {FILE read(list:Buffer size:ToRead len:BytesRead)}
        {ProcessBuffer Buffer}
        {ReadBuffer (RemainingBytes - BytesRead)}
      end
    end

    % ------------- %

    local

      CountingWordStatus = {NewCell false} FirstCall = {NewCell true}

      % ------------- %

      proc {CheckBuffer_ X Xr CountingWord}
        Chars := @Chars + 1
        if X == LF then Lines := @Lines + 1 end

        if CountingWord then
          if {Char.isSpace X} then
            {ProcessBuffer_ Xr false}
          else
            {ProcessBuffer_ Xr CountingWord}
          end
        else
          if {Not {Char.isSpace X}} then
            Words := @Words + 1
            {ProcessBuffer_ Xr true}
          else
            {ProcessBuffer_ Xr CountingWord}
          end
        end
      end

      % ------------- %

      proc {ProcessBuffer_ Buffer CountingWord}
        case Buffer of nil then
          CountingWordStatus := CountingWord
        elseof X|Xr then
          {CheckBuffer_ X Xr CountingWord}
        end
      end

      % ------------- %

    in

      proc {ProcessBuffer Buffer}
        X|Xr = Buffer
      in
        if @FirstCall then CountingWord in
          FirstCall := false
          if (CountingWord = {Not {Char.isSpace X}}) then Words := @Words + 1 end
          {CheckBuffer_ X Xr CountingWord}
        else
          {CheckBuffer_ X Xr @CountingWordStatus}
        end
      end

    end

    % ------------- %

  in
    {FILE seek(whence:'end' offset:0)}
    {FILE tell(offset:RemainingBytes)}
    {FILE seek(whence:set offset:0)}
    {ReadBuffer RemainingBytes}

    [@Lines @Words @Chars]
  end

% ------------- %

  LINES WORDS CHARS

% ------------- %

in
  [LINES WORDS CHARS] =
    {CountLinesWordsChars
      {New TextFile_ init(name:stdin flags:[read text])} READSIZE}

  {System.showInfo LINES # " " # WORDS # " " # CHARS}

  {Application.exit 0}
end

% ----------------------------------------------------------------------
% The Great Computer Language Shootout                              
% http://shootout.alioth.debian.org/                                
%                                                                   
% Word frequency benchmark implemented in a functional style, and using
% only native list processing facilities. An alternate version using
% regexp is included within comment markers interspersed throughout the
% code. List processing version appears to be faster and more efficient,
% in general at least 50% faster. 
% 
% Contributed by Anthony Borla
% ----------------------------------------------------------------------

functor

import
  System(showInfo) Application(exit) Open(file text)

  %
  %  Regex at 'x-oz://contrib/regex'
  %

define
  class TextFile_
    from Open.file Open.text
  end

% ------------- %

  LF = &\012 SPACE = &\040

% ------------- %

  fun {MakeWordFreqTable FILE}

    proc {AddToTable E} Key in
      if E \= nil then
        Key = {String.toAtom E}
        {Dictionary.put Table Key ({Dictionary.condGet Table Key 0} + 1)}
      end
    end

    % ------------- %

    %
    % local
    %   CFT = {MakeCFT Char.isSpace nil Char.toLower}
    % in
    %   fun {CompressLowercaseAndSplit S}
    %     {String.tokens {CFT S false} SPACE}
    %   end
    % end
    %
    % RX = {Regex.make "[^A-Za-z]"}
    %

    local
      Fp = fun {$ E} {Not {Char.isAlpha E}} end
      CFT = {MakeCFT Char.isSpace Fp Char.toLower}
    in
      fun {CompressFilterLowercaseAndSplit S}
        {String.tokens {CFT S false} SPACE}
      end
    end

    % ------------- %

    proc {LoadTable}
      case {FILE getS($)} of false then
        skip
      elseof !LF|_ then
        skip
      elseof LINE then

        %
        % {ForAll
        %    {CompressLowercaseAndSplit
        %      {ByteString.toString {Regex.replace
        %         LINE
        %         RX
        %         fun {$ X Y} " " end}}}
        %    AddToTable}
        %

        {ForAll
          {CompressFilterLowercaseAndSplit LINE}
          AddToTable}

        {LoadTable}
      end
    end

    % ------------- %

    Table = {NewDictionary}
  in
    {LoadTable}
    Table
  end

% ------------- %

  fun {MakeCFT Cp Fp Tp}
    Compressable = if Cp \= nil then Cp else fun {$ E} E end end
    Filterable = if Fp \= nil then Fp else fun {$ _} false end end
    Transform = if Tp \= nil then Tp else fun {$ E} E end end

    fun {CFT S Flag}
      case S of nil then nil
      elseof H|T then Pt in
        if (Pt = {Compressable H}) then
          if Flag then
            {CFT T true}
          else
            if {Filterable H} then SPACE|{CFT T Pt} else H|{CFT T Pt} end
          end
        else
          if {Filterable H} then
            SPACE|{CFT T Pt}
          else
            {Transform H}|{CFT T Pt}
          end
        end
      end
    end
  in
    CFT
  end

  fun {PadLeft S Padlen C} {List.append {MakePad S Padlen C} S} end

  fun {MakePad S Padlen C}
    L Reqlen = {List.length S} - Padlen
  in
    if Reqlen < 0 then
      L = {List.make {Number.abs Reqlen}}
      for I in L do I = C end
    else
      L = nil
    end
    L
  end

% ------------- %

  Sorter ShowEntry

% ------------- %

in
  Sorter = fun {$ X#Xt Y#Yt} if Xt == Yt then X > Y else Xt > Yt end end
  ShowEntry = proc {$ K#V} {System.showInfo {PadLeft {Int.toString V} 7 SPACE} # " " # K} end

  {ForAll
    {List.sort {Dictionary.entries {MakeWordFreqTable {New TextFile_ init(name:stdin)}}} Sorter}
    ShowEntry}

  {Application.exit 0}
end

