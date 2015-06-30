(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)


MODULE ackermann;
IMPORT Out, ProgramArgs, TextRider;

VAR n: LONGINT;

PROCEDURE Ack (m, n: LONGINT): LONGINT;
BEGIN
   IF m=0 THEN RETURN n+1; END;
   IF n=0 THEN RETURN Ack(m-1, 1); END;
   RETURN Ack(m-1, Ack(m, n-1));
END Ack;


(* We'll put this in a library module Shootout *)
PROCEDURE Argi* (): LONGINT;
VAR
   value: LONGINT;
   r: TextRider.Reader; 
BEGIN
   r := TextRider.ConnectReader(ProgramArgs.args);
   IF (r # NIL) & (ProgramArgs.args.ArgNumber() > 0) THEN
      r.ReadLn;
      r.ReadLInt(value);
      IF r.res # TextRider.done THEN
         value := 1;
      END;
   ELSE
      value := 1;
   END;
   RETURN value;
END Argi;


BEGIN
   n := Argi();
   Out.String("Ack(3," ); Out.Int(n,0); Out.String("): ");
   Out.Int( Ack(3,n), 0); Out.Ln;
END ackermann.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org
   
   contributed by Isaac Gouy (Oberon-2 novice)
*) 

MODULE ary;
IMPORT Shootout, Out;

VAR n, m, i, j: LONGINT;
    x, y: POINTER TO ARRAY OF LONGINT;
    
BEGIN
   n := Shootout.Argi();
   m := n-1;
   
   NEW(x, n);
   NEW(y, n);
   
   FOR i := 0 TO m DO
      x[i] := i+1;
   END;
   
   FOR j := 1 TO 1000 DO
      FOR i := 0 TO m DO
         INC(y[i], x[i]);
      END;
   END;
   
   Out.Int(y[0],1); Out.String(" "); Out.Int(y[m],1); Out.Ln;
END ary.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org
   
   Unoptimised reference implementation

   contributed by Isaac Gouy (Oberon-2 novice)   
*)


MODULE binarytrees;
IMPORT Shootout, Out;

CONST
   minDepth = 4;

TYPE
   TreeNode = POINTER TO TreeNodeDesc;
   TreeNodeDesc = RECORD
      left, right: TreeNode;
      item: LONGINT;
   END;

VAR
   n, maxDepth, stretchDepth, depth, iterations, check, i: LONGINT;
   stretchTree, longLivedTree, tempTree: TreeNode;


PROCEDURE NewTreeNode(item: LONGINT): TreeNode;
VAR t: TreeNode;
BEGIN
   NEW(t); t.item := item;
   RETURN t;
END NewTreeNode;


PROCEDURE NewTree(left, right: TreeNode; item: LONGINT): TreeNode;
VAR t: TreeNode;
BEGIN
   NEW(t); t.item := item; t.left := left; t.right := right;
   RETURN t;
END NewTree;


PROCEDURE BottomUpTree(item, depth: LONGINT): TreeNode;
BEGIN
   IF depth > 0 THEN
      RETURN NewTree(
          BottomUpTree(2*item-1,depth-1)
         ,BottomUpTree(2*item,depth-1)
         ,item
      );
   ELSE
      RETURN NewTreeNode(item);
   END;
END BottomUpTree;


PROCEDURE (t:TreeNode) ItemCheck(): LONGINT;
BEGIN
   IF t.left = NIL THEN RETURN t.item;
   ELSE RETURN t.item + t.left.ItemCheck() - t.right.ItemCheck();
   END;
END ItemCheck;


PROCEDURE ShowCheck(i,depth,check: LONGINT);
BEGIN
   Out.Int(i,1); Out.Char(9X); Out.String(" trees of depth ");
   Out.Int(depth,1); Out.Char(9X); Out.String(" check: ");
   Out.Int(check,1);  Out.Ln;
END ShowCheck;


PROCEDURE ShowItemCheck(depth: LONGINT; t: TreeNode; s: ARRAY OF CHAR);
BEGIN
   Out.String(s); Out.Int(depth,1); Out.Char(9X);  Out.String(" check: "); 
   Out.Int(t.ItemCheck(),1);  Out.Ln;
END ShowItemCheck;


BEGIN
   n := Shootout.Argi();

   IF minDepth+2 > n THEN maxDepth := minDepth+2; ELSE maxDepth := n; END;
   stretchDepth := maxDepth + 1;

   stretchTree := BottomUpTree(0,stretchDepth);
   ShowItemCheck(stretchDepth, stretchTree, "stretch tree of depth ");   
   stretchTree := NIL;

   longLivedTree := BottomUpTree(0,maxDepth);

   FOR depth:=minDepth TO maxDepth BY 2 DO
      iterations := ASH(1, maxDepth-depth+minDepth);
      check := 0;
      FOR i:=1 TO iterations DO
         tempTree := BottomUpTree(i,depth);
         INC(check, tempTree.ItemCheck());
         tempTree := NIL;   
         
         tempTree := BottomUpTree(-i,depth);
         INC(check, tempTree.ItemCheck());
         tempTree := NIL;               
      END;
      ShowCheck(iterations*2,depth,check);
   END;

   ShowItemCheck(maxDepth, longLivedTree, "long lived tree of depth ");
END binarytrees.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org
   
   contributed by Isaac Gouy (Oberon-2 novice)  
   
   Unoptimised ***procedural*** implementation 
   
   Note: Oberon-2 does provide type-extension and
   type-bound procedures (virtual methods).
   
   We might package the multiple implementations of
   corresponding procedures in separate named modules. 
   For example: 
      EmptyState.Next() and FullState.Next()
   
   Here the procedures are just prefixed so we can keep 
   everything in a single source file. 
   So, for example:
      ES_Next() and FS_Next()
*)


MODULE dispatch;
IMPORT Shootout, Out;

CONST
   ES_Tag = 1;
   FS_Tag = 2;
   SS_Tag = 3;

   UE_Tag = 4;
   UF_Tag = 5;
   PU_Tag = 6;
   PS_Tag = 7;

TYPE
   IB_State = POINTER TO IB_StateDesc;
   IB_StateDesc = RECORD
      tag : INTEGER;
   END;

   IB_Bottle = POINTER TO IB_BottleDesc;
   IB_BottleDesc = RECORD
      state : IB_State;
      id : INTEGER;
   END;

VAR
   IB_Empty ,IB_Full ,IB_Sealed : IB_State;

   IB_UnpressurizedEmpty ,IB_UnpressurizedFull
      ,IB_PressurizedUnsealed ,IB_PressurizedSealed : IB_State;

   b1, b2, b3, b4, b5, b6, b7, b8, b9, b0 : IB_Bottle;
   p1, p2, p3, p4, p5, p6, p7, p8, p9, p0 : IB_Bottle;

   n, i, check : LONGINT;



(* IBottle Module *)

PROCEDURE IB_SetState (b: IB_Bottle; s: IB_State);
BEGIN b.state := s; END IB_SetState;


PROCEDURE IB_NewBottle (id: INTEGER): IB_Bottle;
VAR
   b : IB_Bottle;
BEGIN
   NEW(b); b.id := id; b.state := IB_Empty;
   RETURN b;
END IB_NewBottle;


PROCEDURE IB_NewPressurizedBottle (id: INTEGER): IB_Bottle;
VAR
   b : IB_Bottle;
BEGIN
   NEW(b); b.id := id; b.state := IB_UnpressurizedEmpty;
   RETURN b;
END IB_NewPressurizedBottle;


PROCEDURE IB_NewState (VAR s: IB_State; tag: INTEGER);
BEGIN NEW(s); s.tag := tag; END IB_NewState;



(* EmptyState Module *)

PROCEDURE ES_Next (b: IB_Bottle);
BEGIN
   IB_SetState(b, IB_Full);
END ES_Next;



(* FullState Module *)

PROCEDURE FS_Next (b: IB_Bottle);
BEGIN
   IB_SetState(b, IB_Sealed);
END FS_Next;



(* SealedState Module *)

PROCEDURE SS_Next (b: IB_Bottle);
BEGIN
   IB_SetState(b, IB_Empty);
END SS_Next;



(* Bottle Module *)

PROCEDURE B_DispatchNext (b: IB_Bottle);
BEGIN
   CASE b.state.tag OF
        ES_Tag : ES_Next(b);
      | FS_Tag : FS_Next(b);
      | SS_Tag : SS_Next(b);                
   END;   
END B_DispatchNext;


PROCEDURE B_Empty (b: IB_Bottle);
BEGIN B_DispatchNext(b); END B_Empty;


PROCEDURE B_Fill (b: IB_Bottle);
BEGIN B_DispatchNext(b); END B_Fill;


PROCEDURE B_Seal (b: IB_Bottle);
BEGIN B_DispatchNext(b); END B_Seal;


PROCEDURE B_Cycle (b: IB_Bottle);
BEGIN B_Fill(b); B_Seal(b); B_Empty(b); END B_Cycle;


PROCEDURE B_Check0 (b: IB_Bottle; c: LONGINT): LONGINT;
BEGIN RETURN b.state.tag + b.id + c; END B_Check0;


PROCEDURE B_New (id: INTEGER): IB_Bottle;
BEGIN RETURN IB_NewBottle(id); END B_New;


PROCEDURE B_Check (a1,a2,a3,a4,a5: IB_Bottle; i: LONGINT): LONGINT;
VAR
   c : LONGINT;
BEGIN
   B_Cycle(a1); B_Cycle(a2); B_Cycle(a3); B_Cycle(a4); B_Cycle(a5);

   c := i MOD 2;

   RETURN B_Check0(a1,c) + B_Check0(a2,c)
      + B_Check0(a3,c) + B_Check0(a4,c) + B_Check0(a5,c);
END B_Check;



(* UnpressurizedEmptyState Module *)

PROCEDURE UE_Next (b: IB_Bottle);
BEGIN
   IB_SetState(b, IB_UnpressurizedFull);
END UE_Next;



(* UnpressurizedFullState Module *)

PROCEDURE UF_Next (b: IB_Bottle);
BEGIN
   IB_SetState(b, IB_PressurizedUnsealed);
END UF_Next;



(* PressurizedUnsealedState Module *)

PROCEDURE PU_Next (b: IB_Bottle);
BEGIN
   IB_SetState(b, IB_PressurizedSealed);
END PU_Next;



(* PressurizedSealedState Module *)

PROCEDURE PS_Next (b: IB_Bottle);
BEGIN
   IB_SetState(b, IB_UnpressurizedEmpty);
END PS_Next;



(* PressurizedBottle Module *)

PROCEDURE PB_DispatchNext (b: IB_Bottle);
BEGIN
   CASE b.state.tag OF
        UE_Tag : UE_Next(b);
      | UF_Tag : UF_Next(b);
      | PU_Tag : PU_Next(b);
      | PS_Tag : PS_Next(b);                  
   END;
END PB_DispatchNext;


PROCEDURE PB_Empty (b: IB_Bottle);
BEGIN PB_DispatchNext(b); END PB_Empty;


PROCEDURE PB_Fill (b: IB_Bottle);
BEGIN PB_DispatchNext(b); END PB_Fill;


PROCEDURE PB_Seal (b: IB_Bottle);
BEGIN PB_DispatchNext(b); END PB_Seal;


PROCEDURE PB_Pressurize (b: IB_Bottle);
BEGIN PB_DispatchNext(b); END PB_Pressurize;


PROCEDURE PB_Cycle (b: IB_Bottle);
BEGIN PB_Fill(b); PB_Pressurize(b); PB_Seal(b); PB_Empty(b); END PB_Cycle;


PROCEDURE PB_New (id: INTEGER): IB_Bottle;
BEGIN RETURN IB_NewPressurizedBottle(id); END PB_New;


PROCEDURE PB_Check (a1,a2,a3,a4,a5: IB_Bottle; i: LONGINT): LONGINT;
VAR
   c : LONGINT;
BEGIN
   PB_Cycle(a1); PB_Cycle(a2); PB_Cycle(a3); PB_Cycle(a4); PB_Cycle(a5);

   c := i MOD 2;

   RETURN B_Check0(a1,c) + B_Check0(a2,c)
      + B_Check0(a3,c) + B_Check0(a4,c) + B_Check0(a5,c);
END PB_Check;




(* FlyweightState Module *)

BEGIN
   IB_NewState(IB_Empty, ES_Tag);
   IB_NewState(IB_Full, FS_Tag);
   IB_NewState(IB_Sealed, SS_Tag);

   IB_NewState(IB_UnpressurizedEmpty, UE_Tag);
   IB_NewState(IB_UnpressurizedFull, UF_Tag);
   IB_NewState(IB_PressurizedUnsealed, PU_Tag);
   IB_NewState(IB_PressurizedSealed, PS_Tag);


   n := Shootout.Argi();

   b1 := B_New(1); b2 := B_New(2);
   b3 := B_New(3); b4 := B_New(4);
   b5 := B_New(5); b6 := B_New(6);
   b7 := B_New(7); b8 := B_New(8);
   b9 := B_New(9); b0 := B_New(0);

   p1 := PB_New(1); p2 := PB_New(2);
   p3 := PB_New(3); p4 := PB_New(4);
   p5 := PB_New(5); p6 := PB_New(6);
   p7 := PB_New(7); p8 := PB_New(8);
   p9 := PB_New(9); p0 := PB_New(0);

   check := 0;
   FOR i := 1 TO n DO
      INC(check, B_Check(b1,b2,b3,b4,b5,i));
      INC(check, B_Check(b6,b7,b8,b9,b0,i));

      INC(check, PB_Check(p1,p2,p3,p4,p5,i));
      DEC(check, PB_Check(p6,p7,p8,p9,p0,i));
   END;

   Out.Int(check,1); Out.Ln;
END dispatch.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org
   
   contributed by Isaac Gouy (Oberon-2 novice)
   
   Unoptimised implementation
*)


MODULE dispatch;
IMPORT Shootout, Out;

TYPE
   BottleState = POINTER TO BottleStateDesc;
   BottleStateDesc = RECORD
      tag : INTEGER;
   END;

   EmptyState = POINTER TO EmptyStateDesc;
   EmptyStateDesc = RECORD (BottleStateDesc) END;

   FullState = POINTER TO FullStateDesc;
   FullStateDesc = RECORD (BottleStateDesc) END;

   SealedState = POINTER TO SealedStateDesc;
   SealedStateDesc = RECORD (BottleStateDesc) END;


   PressurizedBottleStateDesc = RECORD (BottleStateDesc) END;

   UnpressurizedEmptyState = POINTER TO UnpressurizedEmptyStateDesc;
   UnpressurizedEmptyStateDesc = RECORD (PressurizedBottleStateDesc) END;

   UnpressurizedFullState = POINTER TO UnpressurizedFullStateDesc;
   UnpressurizedFullStateDesc = RECORD (PressurizedBottleStateDesc) END;

   PressurizedUnsealedState = POINTER TO PressurizedUnsealedStateDesc;
   PressurizedUnsealedStateDesc = RECORD (PressurizedBottleStateDesc) END;

   PressurizedSealedState = POINTER TO PressurizedSealedStateDesc;
   PressurizedSealedStateDesc = RECORD (PressurizedBottleStateDesc) END;


   Bottle = POINTER TO BottleDesc;
   BottleDesc = RECORD
      state : BottleState;
      id : INTEGER;
   END;

   PressurizedBottle = POINTER TO PressurizedBottleDesc;
   PressurizedBottleDesc = RECORD (BottleDesc) END;


VAR
   Empty : EmptyState;
   Full : FullState;
   Sealed : SealedState;

   UnpressurizedEmpty : UnpressurizedEmptyState;
   UnpressurizedFull : UnpressurizedFullState;
   PressurizedUnsealed : PressurizedUnsealedState;
   PressurizedSealed : PressurizedSealedState;

   b1, b2, b3, b4, b5, b6, b7, b8, b9, b0 : Bottle;
   p1, p2, p3, p4, p5, p6, p7, p8, p9, p0 : PressurizedBottle;

   n, i, check : LONGINT;



PROCEDURE (s: BottleState) Tag (tag: INTEGER);
BEGIN s.tag := tag; END Tag;

PROCEDURE (b: Bottle) SetState (s: BottleState);
BEGIN b.state := s; END SetState;



PROCEDURE (s: BottleState) Next (b: Bottle);
BEGIN  END Next;

PROCEDURE (s: EmptyState) Next (b: Bottle);
BEGIN b.SetState(Full); END Next;

PROCEDURE (s: FullState) Next (b: Bottle);
BEGIN b.SetState(Sealed); END Next;

PROCEDURE (s: SealedState) Next (b: Bottle);
BEGIN b.SetState(Empty); END Next;



PROCEDURE (s: UnpressurizedEmptyState) Next (b: Bottle);
BEGIN b.SetState(UnpressurizedFull); END Next;

PROCEDURE (s: UnpressurizedFullState) Next (b: Bottle);
BEGIN b.SetState(PressurizedUnsealed); END Next;

PROCEDURE (s: PressurizedUnsealedState) Next (b: Bottle);
BEGIN b.SetState(PressurizedSealed); END Next;

PROCEDURE (s: PressurizedSealedState) Next (b: Bottle);
BEGIN b.SetState(UnpressurizedEmpty); END Next;



PROCEDURE (b: Bottle) Id (id: INTEGER);
BEGIN b.id := id; b.state := Empty; END Id;

PROCEDURE (b: Bottle) Empty ();
BEGIN b.state.Next(b); END Empty;

PROCEDURE (b: Bottle) Fill ();
BEGIN b.state.Next(b); END Fill;

PROCEDURE (b: Bottle) Seal ();
BEGIN b.state.Next(b); END Seal;

PROCEDURE (b: Bottle) Cycle ();
BEGIN b.Fill(); b.Seal(); b.Empty(); END Cycle;

PROCEDURE (b: Bottle) Check0 (c: LONGINT): LONGINT;
BEGIN RETURN b.state.tag + b.id + c; END Check0;

PROCEDURE Check (a1,a2,a3,a4,a5: Bottle; i: LONGINT): LONGINT;
VAR
   c : LONGINT;
BEGIN
   a1.Cycle(); a2.Cycle(); a3.Cycle(); a4.Cycle(); a5.Cycle();

   c := i MOD 2;
   RETURN a1.Check0(c) + a2.Check0(c) + a3.Check0(c) + a4.Check0(c) + a5.Check0(c);
END Check;



PROCEDURE (b: PressurizedBottle) Id (id: INTEGER);
BEGIN b.id := id; b.state := UnpressurizedEmpty; END Id;

PROCEDURE (b: PressurizedBottle) Pressurize ();
BEGIN b.state.Next(b); END Pressurize;

PROCEDURE (b: PressurizedBottle) Cycle ();
BEGIN b.Fill(); b.Pressurize(); b.Seal(); b.Empty(); END Cycle;



BEGIN
   NEW(Empty); Empty.Tag(1);
   NEW(Full); Full.Tag(2);
   NEW(Sealed); Sealed.Tag(3);

   NEW(UnpressurizedEmpty); UnpressurizedEmpty.Tag(4);
   NEW(UnpressurizedFull); UnpressurizedFull.Tag(5);
   NEW(PressurizedUnsealed); PressurizedUnsealed.Tag(6);
   NEW(PressurizedSealed); PressurizedSealed.Tag(7);

   n := Shootout.Argi();

   NEW(b1); b1.Id(1); NEW(b2); b2.Id(2); NEW(b3); b3.Id(3); NEW(b4); b4.Id(4);
   NEW(b5); b5.Id(5); NEW(b6); b6.Id(6); NEW(b7); b7.Id(7); NEW(b8); b8.Id(8);
   NEW(b9); b9.Id(9); NEW(b0); b0.Id(0);

   NEW(p1); p1.Id(1); NEW(p2); p2.Id(2); NEW(p3); p3.Id(3); NEW(p4); p4.Id(4);
   NEW(p5); p5.Id(5); NEW(p6); p6.Id(6); NEW(p7); p7.Id(7); NEW(p8); p8.Id(8);
   NEW(p9); p9.Id(9); NEW(p0); p0.Id(0);

   check := 0;
   FOR i := 1 TO n DO
      INC(check, Check(b1,b2,b3,b4,b5,i));
      INC(check, Check(b6,b7,b8,b9,b0,i));

      INC(check, Check(p1,p2,p3,p4,p5,i));
      DEC(check, Check(p6,p7,p8,p9,p0,i));
   END;

   Out.Int(check,1); Out.Ln;
END dispatch.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE except;
IMPORT Shootout, Exception, Out;
 
TYPE   
   LoException = POINTER TO LoExceptionDesc;
   LoExceptionDesc = RECORD (Exception.UncheckedDesc) END; 
   
   HiException = POINTER TO HiExceptionDesc;
   HiExceptionDesc = RECORD (Exception.UncheckedDesc) END;  
                
VAR
   n, i, loCount, hiCount: LONGINT;
   lo: LoException; 
   hi: HiException;   
             
PROCEDURE Blowup(n: LONGINT);             
BEGIN
   IF ODD(n) THEN RAISE(hi); ELSE RAISE(lo); END;
END Blowup;              
             
PROCEDURE LoProc(n: LONGINT);             
BEGIN
   TRY Blowup(n); CATCH LoException: INC(loCount); END;
END LoProc; 

PROCEDURE HiProc(n: LONGINT);             
BEGIN
   TRY LoProc(n); CATCH HiException: INC(hiCount); END;
END HiProc;     

PROCEDURE SomeProc(n: LONGINT);             
BEGIN
   TRY HiProc(n); 
   CATCH Exception.Unchecked: 
      Out.String("We shouldn't get here!"); Out.Ln;
   END;
END SomeProc;       
                          
BEGIN    
   n := Shootout.Argi(); NEW(lo); NEW(hi);
   FOR i := 1 TO n DO SomeProc(i); END;
   Out.String("Exceptions: HI="); Out.Int(hiCount, 0);
   Out.String(" / LO="); Out.Int(loCount, 0); Out.Ln;
END except. 
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)

   To run:   fannkuch 9 
*)


MODULE fannkuch;
IMPORT Shootout, Out;

VAR n, f: LONGINT;


PROCEDURE F(n: LONGINT): LONGINT;
VAR
   perm, perm1, count, maxPerm: POINTER TO ARRAY OF LONGINT;
   check, m, r, i, k, temp, maxFlipsCount, flipsCount, perm0: LONGINT;
BEGIN
   NEW(perm, n); NEW(perm1, n); NEW(count, n); NEW(maxPerm, n);

   check := 0;
   maxFlipsCount := 0;
   m := n - 1;
   FOR i := 0 TO m DO perm1[i] := i; END;

   r := n;
   LOOP
      (* write-out the first 30 permutations *)
      IF (check < 30) THEN
         FOR i := 0 TO m DO Out.Int( perm1[i]+1, 1); END;
         Out.Ln;
         INC(check);
      END;

      WHILE r # 1 DO count[r-1] := r; DEC(r); END;
      IF ~((perm1[0]=0) OR (perm1[m]=m)) THEN
         FOR i := 0 TO m DO perm[i] := perm1[i]; END;
         flipsCount := 0;
         LOOP
            k := perm[0];	
            IF k = 0 THEN EXIT; END;
            FOR i := 0 TO ASH(k+1,-1) - 1 DO
               temp := perm[i]; perm[i] := perm[k-i]; perm[k-i] := temp;
            END;
            INC(flipsCount);	    	
         END;	
         IF flipsCount > maxFlipsCount THEN	
            maxFlipsCount := flipsCount;
            FOR i := 0 TO m DO maxPerm[i] := perm1[i]; END;
         END;
      END;
      LOOP
         IF r = n THEN RETURN maxFlipsCount; END;
         perm0 := perm1[0];
         i := 0;
         WHILE i < r DO
            k := i + 1;
            perm1[i] := perm1[k];
            i := k;
         END;
         perm1[r] := perm0;
	
         DEC(count[r]);	
         IF count[r] > 0 THEN EXIT; END;
         INC(r);
      END;
   END;
END F;


BEGIN
   n := Shootout.Argi();
   f := F(n);
   Out.String("Pfannkuchen("); Out.Int(n,1); Out.String(") = ");
   Out.Int( f, 1); Out.Ln;
END fannkuch.

(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)


MODULE fasta;
IMPORT Shootout, Random, Strings, Out;

CONST
   LineLength = 60;

TYPE
   Frequency = RECORD
      c: CHAR;
      p: LONGREAL;
   END;

   Freqency4 = ARRAY 4 OF Frequency;
   Freqency15 = ARRAY 15 OF Frequency;

VAR
   n: LONGINT;
   homosapiens: Freqency4;
   iub: Freqency15;
   alu: ARRAY 300 OF CHAR;


PROCEDURE SelectRandom(VAR a: ARRAY OF Frequency): CHAR;
VAR
   i: LONGINT;
   r: LONGREAL;
BEGIN
   r := Random.Next(1.0);
   FOR i := 0 TO LEN(a)-1 DO
      IF r < a[i].p THEN RETURN a[i].c; END;
   END;
   RETURN a[LEN(a)-1].c;
END SelectRandom;


PROCEDURE MakeRandomFasta(
   id,desc: ARRAY OF CHAR; VAR a: ARRAY OF Frequency; n: LONGINT);
VAR
   i, m: LONGINT;
BEGIN
   Out.String(">"); Out.String(id); Out.Char(20X); Out.String(desc); Out.Ln;

   WHILE n > 0 DO
      IF n < LineLength THEN m := n; ELSE m := LineLength; END;
      FOR i := 1 TO m DO Out.Char( SelectRandom(a) ); END; Out.Ln;
      DEC(n,LineLength);
   END;
END MakeRandomFasta;


PROCEDURE MakeRepeatFasta(
   id,desc: ARRAY OF CHAR; VAR s: ARRAY OF CHAR; n: LONGINT);
VAR
   j, k, kn, m: LONGINT;
BEGIN
   Out.String(">"); Out.String(id); Out.Char(20X); Out.String(desc); Out.Ln;

   k := 0; kn := Strings.Length(s)-1;
   WHILE n > 0 DO
      IF n < LineLength THEN m := n; ELSE m := LineLength; END;
      j := 1;
      WHILE j <= m DO
         IF k > kn THEN k := 0; END;
         Out.Char(s[k]);
         INC(j); INC(k);
      END;
      Out.Ln;	
      DEC(n,LineLength);
   END;
END MakeRepeatFasta;


PROCEDURE MakeALU(VAR s: ARRAY OF CHAR);
BEGIN
   Strings.Append( "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" ,s);   
   Strings.Append( "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" ,s);
   Strings.Append( "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" ,s);
   Strings.Append( "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" ,s);
   Strings.Append( "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" ,s);
   Strings.Append( "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" ,s);   
   Strings.Append( "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA" ,s);             
END MakeALU;


PROCEDURE MakeCumulative(VAR a: ARRAY OF Frequency);
VAR
   i: LONGINT;
   cp: LONGREAL;
BEGIN
   cp := 0.0;
   FOR i := 0 TO LEN(a) - 1 DO
      cp := cp + a[i].p;
      a[i].p := cp;
   END;
END MakeCumulative;


PROCEDURE MakeIUB(VAR a: Freqency15);
BEGIN
   a[0].c := "a";   a[0].p := 0.27;
   a[1].c := "c";   a[1].p := 0.12;
   a[2].c := "g";   a[2].p := 0.12;
   a[3].c := "t";   a[3].p := 0.27;

   a[4].c := "B";   a[4].p := 0.02;
   a[5].c := "D";   a[5].p := 0.02;
   a[6].c := "H";   a[6].p := 0.02;
   a[7].c := "K";   a[7].p := 0.02;
   a[8].c := "M";   a[8].p := 0.02;
   a[9].c := "N";   a[9].p := 0.02;
  a[10].c := "R";  a[10].p := 0.02;
  a[11].c := "S";  a[11].p := 0.02;
  a[12].c := "V";  a[12].p := 0.02;
  a[13].c := "W";  a[13].p := 0.02;
  a[14].c := "Y";  a[14].p := 0.02;

   MakeCumulative(a);
END MakeIUB;


PROCEDURE MakeHomoSapiens(VAR a: Freqency4);
BEGIN
   a[0].c := "a";   a[0].p := 0.3029549426680;
   a[1].c := "c";   a[1].p := 0.1979883004921;
   a[2].c := "g";   a[2].p := 0.1975473066391;
   a[3].c := "t";   a[3].p := 0.3015094502008;

   MakeCumulative(a);
END MakeHomoSapiens;


BEGIN
   MakeALU(alu);
   MakeIUB(iub);
   MakeHomoSapiens(homosapiens);

   n := Shootout.Argi();
   MakeRepeatFasta('ONE', 'Homo sapiens alu', alu, n*2);
   MakeRandomFasta('TWO', 'IUB ambiguity codes', iub, n*3);
   MakeRandomFasta('THREE', 'Homo sapiens frequency', homosapiens, n*5);
END fasta.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)


MODULE fibo;
IMPORT Shootout, Out;


PROCEDURE Fibo (n: LONGINT): LONGINT;
BEGIN
   IF n<2 THEN
      RETURN 1;
   ELSE
      RETURN Fibo(n-2) + Fibo(n-1);
   END;
END Fibo;


BEGIN
   Out.Int( Fibo( Shootout.Argi() ),0); Out.Ln;
END fibo.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org 

   contributed by Isaac Gouy (Oberon-2 novice)

   To run:   harmonic 1000000
*)

MODULE harmonic;
IMPORT Shootout, Out;

VAR
   i, n: LONGINT;
   partialSum, d: LONGREAL;

BEGIN
   n := Shootout.Argi();

   WHILE i < n DO
      d := d + 1.0D+00;
      partialSum := partialSum + (1.0D+00/d);
      INC(i);
   END;

   Out.LongRealFix(partialSum,0,9); Out.Ln;
END harmonic.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   SimpleHash is a line-by-line transliteration
   of the C implementation.

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE hash;
IMPORT Shootout, SimpleHash, Strings, IntStr, Out, S:=SYSTEM;

CONST
   maxLength = 11; 

VAR
   buffer: ARRAY maxLength OF CHAR;
   n, i, count: LONGINT;
   table: SimpleHash.Hashtable;
   item: SimpleHash.Item;
   key: SimpleHash.ItemKey;

PROCEDURE HexToItemKey(n: LONGINT): SimpleHash.ItemKey;
VAR
   i, m, digits: LONGINT;
   key: SimpleHash.ItemKey;
BEGIN
   i := maxLength - 1;
   WHILE n > 0 DO
      m := S.VAL(LONGINT, S.VAL(SET, n) * {0..3}); 
      CASE m OF
         | 0..9 : buffer[i] := CHR(ORD("0") + m);
      ELSE
                  buffer[i] := CHR(ORD("A") - 10 + m);   
      END;      
      n := ASH(n,-4); DEC(i);                  
   END;
   
   INC(i); digits := maxLength - i; 
   NEW(key, digits);
   S.MOVE(S.ADR(buffer[i]), S.VAL(LONGINT,key), digits);
   RETURN key;
END HexToItemKey;


PROCEDURE IntToItemKey(n: LONGINT): SimpleHash.ItemKey;
VAR
   digits: LONGINT;
   key: SimpleHash.ItemKey;
BEGIN
   IntStr.IntToStr(n,buffer);
   digits := Strings.Length(buffer);
   NEW(key, digits);
   S.MOVE(S.ADR(buffer[0]), S.VAL(LONGINT,key), digits); 
   RETURN key;
END IntToItemKey;


BEGIN
   n := Shootout.Argi();
   table := SimpleHash.New(n);

   FOR i := 1 TO n DO
      key := HexToItemKey(i);
      item := SimpleHash.AddKey(table, key);
      item.value := i; 	
   END;

   count := 0;
   FOR i := n TO 1 BY -1 DO
      key := IntToItemKey(i);
      IF SimpleHash.Find(table, key) # NIL THEN INC(count); END;
   END;
   Out.Int(count, 1); Out.Ln; 
END hash.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   SimpleHash is a line-by-line transliteration
   of the C implementation.

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE hash2;
IMPORT Shootout, SimpleHash, Strings, IntStr, Out, S:=SYSTEM;

CONST
   maxLength = 11;

VAR
   buffer: ARRAY maxLength OF CHAR;
   n, i: LONGINT;
   table1, table2: SimpleHash.Hashtable;
   item1, item2: SimpleHash.Item;
   key, key1, key9999: SimpleHash.ItemKey;   


PROCEDURE IntToItemKey(n: LONGINT): SimpleHash.ItemKey;
VAR
   digits: LONGINT;
   key: SimpleHash.ItemKey;
BEGIN
   IntStr.IntToStr(n,buffer);
   digits := Strings.Length(buffer);
   NEW(key, digits);
   S.MOVE(S.ADR(buffer[0]), S.VAL(LONGINT,key), digits); 
   RETURN key;
END IntToItemKey;


BEGIN
   n := Shootout.Argi();
   table1 := SimpleHash.New(10000);
   table2 := SimpleHash.New(10000);

   FOR i := 0 TO 9999 DO
      key := IntToItemKey(i);
      item1 := SimpleHash.AddKey(table1, key);
      item1.value := i;
   END;

   FOR i := 0 TO n-1 DO
      item1 := SimpleHash.First(table1);
      WHILE item1 # NIL DO
         item2 := SimpleHash.AddKey(table2, item1.key);
	 INC(item2.value, item1.value);
	 item1 := SimpleHash.Next(table1);
      END;
   END;

   key1 := IntToItemKey(1);
   key9999 := IntToItemKey(9999);

   item1 := SimpleHash.Find(table1, key1);
   Out.Int(item1.value, 1); Out.Char(20X);
   item1 := SimpleHash.Find(table1, key9999);
   Out.Int(item1.value, 0); Out.Char(20X);
   item2 := SimpleHash.Find(table2, key1);
   Out.Int(item2.value, 0); Out.Char(20X);
   item2 := SimpleHash.Find(table2, key9999);
   Out.Int(item2.value, 0); Out.Ln;
END hash2.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*) 

MODULE heapsort;
IMPORT Shootout, Random, Out;

VAR
   n, i: LONGINT;
   x:  POINTER TO ARRAY OF LONGREAL;


PROCEDURE Heapsort(n: LONGINT; VAR ra: ARRAY OF LONGREAL);
VAR i, j, k, ir: LONGINT;
    rra: LONGREAL;
BEGIN
   ir := n;
   k := n DIV 2 + 1;
   LOOP
      IF k > 1 THEN
         DEC(k);
         rra := ra[k];
      ELSE
         rra := ra[ir];
         ra[ir] := ra[1];
         DEC(ir);
         IF ir = 1 THEN
            ra[1] := rra;
            EXIT;
         END;
      END;
      
      i := k;
      j := ASH(k,1);

      WHILE j<=ir DO
         IF (j < ir) & (ra[j] < ra[j+1]) THEN INC(j); END;
         IF rra < ra[j] THEN
            ra[i] := ra[j];
            i := j;
            INC(j,j);
         ELSE
            j := ir + 1;
         END;
      END;	
      ra[i] := rra;
   END;
END Heapsort;


BEGIN
   n := Shootout.Argi();
   
   NEW(x, n+1);
   x[0] := 0.0;
   FOR i := 1 TO n DO x[i] := Random.Next(1.0); END;
   Heapsort(n, x^);
   Out.LongRealFix(x[n],0,10); Out.Ln;
END heapsort.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice) 
*)

MODULE hello; IMPORT Out; BEGIN Out.String("hello world"); Out.Ln; END hello.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org
 
   contributed by Isaac Gouy (Oberon-2 novice)
   Line-by-line (more or less) translation of the C implemention.
*)


MODULE lists;
IMPORT Shootout, SYSTEM, Out;

CONST size = 10000;

TYPE
   List = POINTER TO ListRec;
   ListRec = RECORD
      val: LONGINT;
      next: List;
      prev: List;
   END;

VAR
   n, result: LONGINT;
   l1, l2, l3, item: List;


PROCEDURE Length (head: List): LONGINT;
BEGIN
   RETURN head.val;
END Length;


PROCEDURE Empty (head: List): BOOLEAN;
BEGIN
   RETURN Length(head) = 0;
END Empty;


PROCEDURE First (head: List): List;
BEGIN
   RETURN head.next;
END First;


PROCEDURE Last (head: List): List;
BEGIN
   RETURN head.prev;
END Last;


PROCEDURE PushTail (VAR head: List; item: List);
VAR tail: List;
BEGIN
   tail := head.prev;
   tail.next := item;
   item.next := head;
   head.prev := item;
   item.prev := tail;
   INC(head.val);
END PushTail;


PROCEDURE PopTail (VAR head: List): List;
VAR prev, tail: List;
BEGIN
   IF Empty(head) THEN RETURN NIL; END;
   tail := head.prev;
   prev := tail.prev;
   prev.next := head;
   head.prev := prev;
   DEC(head.val);
   RETURN tail;
END PopTail;


PROCEDURE PushHead (VAR head, item: List); (* never used *)
VAR next: List;
BEGIN
   next := head.next;
   head.next := item;
   next.prev := item;
   item.next := next;
   item.prev := head;
   INC(head.val);
END PushHead;


PROCEDURE PopHead (VAR head: List): List;
VAR next: List;
BEGIN
   IF Empty(head) THEN RETURN NIL; END;
   next := head.next;
   head.next := next.next;
   next.next.prev := head;
   DEC(head.val);
   RETURN next;
END PopHead;


PROCEDURE Equal (x, y: List): BOOLEAN;
VAR xp, yp: List;
BEGIN
   xp := x;
   yp := y;
   WHILE xp.next # x DO
      IF xp.val # yp.val THEN RETURN FALSE; END;
      xp := xp.next;
      yp := yp.next;
   END;
   IF xp.val # yp.val THEN RETURN FALSE; END;
   RETURN  yp.next = y;
END Equal;


PROCEDURE Print (msg: ARRAY OF CHAR; x: List); (* never used *)
VAR
   xp, first: List;
   i: LONGINT;
BEGIN
   first := x.next;
   i := 0;
   xp := x.next;;
   WHILE xp.next # first DO
      Out.String(msg);
      Out.String("i:"); Out.Int(i,3);
      Out.String("   v:"); Out.Int(xp.val,3);
      Out.String("   n:"); Out.Int(xp.next.val,3);
      Out.String("   p:"); Out.Int(xp.prev.val,3);
      Out.Ln;
      INC(i);
      xp := xp.next;
   END;
   Out.String("[last entry points to list head]"); Out.Ln;
   Out.String("[val of next of tail is: ");
   Out.Int(xp.next.val,3); Out.String("]"); Out.Ln; Out.Ln;
END Print;



PROCEDURE New (): List;
VAR l: List;
BEGIN
   NEW(l);
   l.next := l;
   l.prev := l;
   l.val := 0;
   RETURN l;
END New;


PROCEDURE Sequence (from, to: LONGINT): List;
VAR
   size, tmp, i, j: LONGINT;
   a: POINTER TO ARRAY OF ListRec;
BEGIN
   IF from > to THEN
      tmp := from; from := to; to := tmp;
   END;
   size := to - from + 1;

   NEW(a, size+1);
   DEC(from);
   j := 1;
   FOR i := 0 TO size-1 DO
      a[i].next := SYSTEM.VAL(List, SYSTEM.ADR( a[i+1] ));
      a[j].prev := SYSTEM.VAL(List, SYSTEM.ADR( a[j-1] ));
      a[i].val := from;
      INC(from);
      INC(j);
   END;
   a[0].prev := SYSTEM.VAL(List, SYSTEM.ADR( a[size] ));
   a[size].next := SYSTEM.VAL(List, SYSTEM.ADR( a[0] ));
   a[size].prev := SYSTEM.VAL(List, SYSTEM.ADR( a[size-1] ));
   a[size].val := from;
   a[0].val := size;
   RETURN SYSTEM.VAL(List, SYSTEM.ADR( a[0] ));
END Sequence;


PROCEDURE Copy (x: List): List;
VAR
   size, i, j: LONGINT;
   xp: List;
   a: POINTER TO ARRAY OF ListRec;
BEGIN
   size := Length(x);
   NEW(a, size+1);
   j := 1;
   xp := x;
   FOR i := 0 TO size-1 DO
      a[i].next := SYSTEM.VAL(List, SYSTEM.ADR( a[j] ));
      a[j].prev := SYSTEM.VAL(List, SYSTEM.ADR( a[i] ));
      a[i].val := xp.val;
      INC(j);
      xp := xp.next;
   END;
   a[0].prev := SYSTEM.VAL(List, SYSTEM.ADR( a[size] ));
   a[size].next := SYSTEM.VAL(List, SYSTEM.ADR( a[0] ));
   xp := Last(x);
   a[size].val := xp.val;
   RETURN SYSTEM.VAL(List, SYSTEM.ADR( a[0] ));
END Copy;


PROCEDURE Reverse (head: List);
VAR tmp, p: List;
BEGIN
   p := head;
   REPEAT
      tmp := p.next;
      p.next := p.prev;
      p.prev := tmp;
      p := tmp;
   UNTIL p = head;
END Reverse;


PROCEDURE TestLists (): LONGINT;
BEGIN
   l1 := Sequence(1,size); (* Print("L1 ", l1); *)
   l2 := Copy(l1);         (* Print("L2 ", l2); *)
   l3 := New();            (* Print("L3 ", l3); *)

   IF ~Equal(l2, l1) THEN
      Out.String("l2 and l1 are not equal"); Out.Ln;
      HALT(1);
   END;

   WHILE ~ Empty(l2) DO
      PushTail(l3, PopHead(l2));
   END;
                           (* Print("L2 ", l2);
			      Print("L3 ", l3); *)
   IF ~Empty(l2) THEN
      Out.String("l2 should be empty now"); Out.Ln;
      HALT(1);
   END;			
								
   WHILE ~ Empty(l3) DO
      PushTail(l2, PopTail(l3));
   END;
                           (* Print("L2 ", l2);
			      Print("L3 ", l3); *)
   IF ~Empty(l3) THEN
      Out.String("l3 should be empty now"); Out.Ln;
      HALT(1);
   END;	
   			
   Reverse(l1);            (* Print("L1 ", l1); *)

   item := First(l1);
   IF item.val # size THEN
      Out.String("L1 first value wrong, wanted "); Out.Int(size,1);
      Out.String(", got "); Out.Int(item.val,1); Out.Ln;
      HALT(1);
   END;

   item := Last(l1);
   IF item.val # 1 THEN
      Out.String("L1 last value wrong, wanted 1, got ");
      Out.Int(item.val,1); Out.Ln;
      HALT(1);
   END;

   item := First(l2);
   IF item.val # size THEN
      Out.String("L2 first value wrong, wanted "); Out.Int(size,1);
      Out.String(", got "); Out.Int(item.val,1); Out.Ln;
      HALT(1);
   END;

   item := Last(l2);
   IF item.val # 1 THEN
      Out.String("L2 last value wrong, wanted 1, got ");
      Out.Int(item.val,1); Out.Ln;
      HALT(1);
   END;

   IF Length(l1) # size THEN
      Out.String("L1 size wrong, wanted "); Out.Int(size,1);
      Out.String(", got "); Out.Int(Length(l1),1); Out.Ln;
      HALT(1);
   END;

   IF ~Equal(l1, l2) THEN
      Out.String("l1 and l2 are not equal"); Out.Ln;
      HALT(1);
   END;

   RETURN Length(l1);
END TestLists;


BEGIN
   n := Shootout.Argi();
   result := 0;
   WHILE n > 0 DO
      result := TestLists();
      DEC(n);
   END;
   Out.Int(result,1); Out.Ln;
END lists.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)

   To run:   Mandelbrot 200
*)


MODULE mandelbrot;
IMPORT Shootout, Out;

CONST
   m = 50;
   limit2 = 4.0;

VAR
   width, height, y, x, i, bits, bitnum: LONGINT;
   isOverLimit: BOOLEAN;
   Zr, Zi, Cr, Ci, Tr, Ti: LONGREAL;

BEGIN
   width := Shootout.Argi();
   height := width;

   Out.String("P4"); Out.Ln;
   Out.Int(width,0); Out.Char(20X); Out.Int(width,0); Out.Ln;

   FOR y := 0 TO height-1 DO
      FOR x := 0 TO width-1 DO
         Zr := 0.0; Zi := 0.0;
         Cr := 2.0 * x / width - 1.5;
         Ci := 2.0 * y / height - 1.0;

         i := 0;
         REPEAT
            Tr := Zr*Zr - Zi*Zi + Cr;
            Ti := 2*Zr*Zi + Ci;
            Zr := Tr; Zi := Ti;
            INC(i);
            isOverLimit := Zr*Zr + Zi*Zi > limit2;
         UNTIL isOverLimit OR (i = m);
	
         bits := ASH(bits,1);
         IF ~isOverLimit THEN INC(bits); END;
         INC(bitnum);

         IF x = width - 1 THEN
            bits := ASH(bits,8-bitnum); bitnum := 8;	
         END;

         IF bitnum = 8 THEN
            Out.Char(CHR(bits)); bits := 0; bitnum := 0;	
         END;		
      END;
   END;
END mandelbrot.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE matrix;
IMPORT Shootout, Out;

CONST
   size = 30;
   rowSize = size;
   rowLast = rowSize - 1;
   colSize = size;
   colLast = colSize - 1;

TYPE
   Matrix = ARRAY rowSize, colSize OF LONGINT;

VAR
   n: LONGINT;
   m1, m2, mm: Matrix;


PROCEDURE MakeMatrix(VAR m: Matrix);
VAR i, j, count: LONGINT;
BEGIN
   count := 1;
   FOR i := 0 TO rowLast DO
      FOR j := 0 TO colLast DO
         m[i,j] := count;
         INC(count);
      END;
   END;
END MakeMatrix;


PROCEDURE MatrixMultiply(leftm, rightm: Matrix; VAR m: Matrix);
VAR i, j, k, sumOfProduct: LONGINT;
BEGIN
   FOR i := 0 TO rowLast DO
      FOR j := 0 TO colLast DO
         sumOfProduct := 0;
	 FOR k := 0 TO colLast DO
            INC(sumOfProduct, leftm[i,k] * rightm[k,j]);
         END;
	 m[i,j] := sumOfProduct;
      END;
   END;
END MatrixMultiply;


BEGIN
   n := Shootout.Argi();

   MakeMatrix(m1);
   MakeMatrix(m2);

   WHILE n > 0 DO
      DEC(n);
      MatrixMultiply(m1, m2, mm);
   END;

   Out.Int(mm[0,0],0); Out.Char(20X); Out.Int(mm[2,3],0); Out.Char(20X);
   Out.Int(mm[3,2],0); Out.Char(20X); Out.Int(mm[4,4],0); Out.Ln;
END matrix.
(* The Computer Language Shootout
   http://shootout.alioth.debian.org
   contributed by Isaac Gouy (Oberon-2 novice)
*)


MODULE meteor;
IMPORT Shootout, Out;

CONST
   NW = 0; NE = 1; W = 2; E = 3; SW = 4; SE = 5;
   SIDES = 6;

   BOARD_COLS = 5;
   BOARD_ROWS = 10;
   BOARD_SIZE = BOARD_COLS * BOARD_ROWS;

   PIECES = 10;
   PIECE_SIZE = 5;
   PIECE_ROTATIONS = SIDES;
   PIECE_FLIPS = 2;
   PIECE_ORIENTATIONS = PIECE_ROTATIONS * PIECE_FLIPS;


TYPE
   Cell = POINTER TO CellDesc;
   CellDesc =
      RECORD
         marked : BOOLEAN; 
      END;

   PieceCell = POINTER TO PieceCellDesc;
   PieceCellDesc = 
      RECORD (CellDesc)
         next : ARRAY SIDES OF PieceCell; 
      END;

   PieceShape = ARRAY PIECE_SIZE OF PieceCell;
   PieceCache = ARRAY PIECE_ORIENTATIONS OF PieceShape;

   Piece = POINTER TO PieceDesc;
   PieceDesc = 
      RECORD 
         number : LONGINT;
         orientation : LONGINT;
         cache : PieceCache;
      END;

   BoardCell = POINTER TO BoardCellDesc;
   BoardCellDesc = 
      RECORD (CellDesc)
         next : ARRAY SIDES OF BoardCell; 
         number : LONGINT;
         piece: Piece;
      END;


   BoardPieceShape = ARRAY PIECE_SIZE OF BoardCell;
   BoardPiece = POINTER TO BoardPieceShape;
   BoardCache = ARRAY 
      PIECES, PIECE_ORIENTATIONS, PIECE_SIZE, BOARD_SIZE OF BoardPiece;

   Board =
      RECORD
         cells : ARRAY BOARD_SIZE OF BoardCell;
         cellsPieceWillFill : BoardPieceShape;
         cellCount : LONGINT; 
         cache : BoardCache;
      END;

   BoardPieces = ARRAY BOARD_SIZE OF LONGINT;


VAR
   countdown, n : LONGINT;
   board : Board;
   pieces : ARRAY PIECES OF Piece;
   unplaced : SET;
   first, last, current: BoardPieces;
   once : BOOLEAN;
   noFit : BoardPiece;


PROCEDURE (c: Cell) INIT* (); BEGIN c.marked := FALSE; END INIT;


PROCEDURE (c: Cell) Mark (); BEGIN c.marked := TRUE; END Mark;


PROCEDURE (c: Cell) Unmark (); BEGIN c.marked := FALSE; END Unmark;


PROCEDURE (c: BoardCell) INIT* (i: LONGINT);
BEGIN c.INIT^(); c.number := i; END INIT;


PROCEDURE (c: BoardCell) Empty (); BEGIN c.piece := NIL; END Empty;


PROCEDURE (c: BoardCell) IsEmpty () : BOOLEAN; 
BEGIN RETURN c.piece = NIL; END IsEmpty;


PROCEDURE (c: BoardCell) ContiguousEmptyCells () : LONGINT; 
VAR
   count, i : LONGINT;
   neighbour : BoardCell;
BEGIN 
   IF ~c.marked & c.IsEmpty() THEN
      c.Mark(); 
      count := 1;
      FOR i := 0 TO SIDES - 1 DO
         neighbour := c.next[i];
         IF (neighbour # NIL) & neighbour.IsEmpty() THEN
            INC(count, neighbour.ContiguousEmptyCells());
         END;
      END;
   ELSE
      count := 0;
   END;
   RETURN count; 
END ContiguousEmptyCells;


PROCEDURE (c: PieceCell) Flip (); 
VAR
   swap : PieceCell;
BEGIN
   swap := c.next[NE];
   c.next[NE] := c.next[NW];
   c.next[NW] := swap;

   swap := c.next[E];
   c.next[E] := c.next[W];
   c.next[W] := swap;

   swap := c.next[SE];
   c.next[SE] := c.next[SW];
   c.next[SW] := swap; 
END Flip;


PROCEDURE (c: PieceCell) Rotate (); 
VAR
   swap : PieceCell;
BEGIN 
   swap := c.next[E];
   c.next[E] := c.next[NE];
   c.next[NE] := c.next[NW];
   c.next[NW] := c.next[W];
   c.next[W] := c.next[SW];
   c.next[SW] := c.next[SE];
   c.next[SE] := swap; 
END Rotate;


PROCEDURE (p: Piece) Make0 (VAR a : PieceShape);
BEGIN
   a[0].next[E] := a[1];
   a[1].next[W] := a[0];
   a[1].next[E] := a[2];
   a[2].next[W] := a[1];
   a[2].next[E] := a[3];
   a[3].next[W] := a[2];
   a[3].next[SE] := a[4];
   a[4].next[NW] := a[3];
END Make0;


PROCEDURE (p: Piece) Make1 (VAR a : PieceShape);
BEGIN
   a[0].next[SE] := a[1];
   a[1].next[NW] := a[0];
   a[1].next[SW] := a[2];
   a[2].next[NE] := a[1];
   a[2].next[W] := a[3];
   a[3].next[E] := a[2];
   a[3].next[SW] := a[4];
   a[4].next[NE] := a[3];
END Make1;


PROCEDURE (p: Piece) Make2 (VAR a : PieceShape);
BEGIN
   a[0].next[W] := a[1];
   a[1].next[E] := a[0];
   a[1].next[SW] := a[2];
   a[2].next[NE] := a[1];
   a[2].next[SE] := a[3];
   a[3].next[NW] := a[2];
   a[3].next[SE] := a[4];
   a[4].next[NW] := a[3];
END Make2;


PROCEDURE (p: Piece) Make3 (VAR a : PieceShape);
BEGIN
   a[0].next[SW] := a[1];
   a[1].next[NE] := a[0];
   a[1].next[W] := a[2];
   a[2].next[E] := a[1];
   a[1].next[SW] := a[3];
   a[3].next[NE] := a[1];
   a[2].next[SE] := a[3];
   a[3].next[NW] := a[2];
   a[3].next[SE] := a[4];
   a[4].next[NW] := a[3];
END Make3;


PROCEDURE (p: Piece) Make4 (VAR a : PieceShape);
BEGIN
   a[0].next[SE] := a[1];
   a[1].next[NW] := a[0];
   a[1].next[SW] := a[2];
   a[2].next[NE] := a[1];
   a[1].next[E] := a[3];
   a[3].next[W] := a[1];
   a[3].next[SE] := a[4];
   a[4].next[NW] := a[3];
END Make4;


PROCEDURE (p: Piece) Make5 (VAR a : PieceShape);
BEGIN
   a[0].next[SW] := a[1];
   a[1].next[NE] := a[0];
   a[0].next[SE] := a[2];
   a[2].next[NW] := a[0];
   a[1].next[SE] := a[3];
   a[3].next[NW] := a[1];
   a[2].next[SW] := a[3];
   a[3].next[NE] := a[2];
   a[3].next[SW] := a[4];
   a[4].next[NE] := a[3];
END Make5;


PROCEDURE (p: Piece) Make6 (VAR a : PieceShape);
BEGIN
   a[0].next[SW] := a[1];
   a[1].next[NE] := a[0];
   a[2].next[SE] := a[1];
   a[1].next[NW] := a[2];
   a[1].next[SE] := a[3];
   a[3].next[NW] := a[1];
   a[3].next[SW] := a[4];
   a[4].next[NE] := a[3];
END Make6;


PROCEDURE (p: Piece) Make7 (VAR a : PieceShape);
BEGIN
   a[0].next[SE] := a[1];
   a[1].next[NW] := a[0];
   a[0].next[SW] := a[2];
   a[2].next[NE] := a[0];
   a[2].next[SW] := a[3];
   a[3].next[NE] := a[2];
   a[3].next[SE] := a[4];
   a[4].next[NW] := a[3];
END Make7;


PROCEDURE (p: Piece) Make8 (VAR a : PieceShape);
BEGIN
   a[0].next[E] := a[1];
   a[1].next[W] := a[0];
   a[1].next[E] := a[2];
   a[2].next[W] := a[1];
   a[2].next[NE] := a[3];
   a[3].next[SW] := a[2];
   a[3].next[E] := a[4];
   a[4].next[W] := a[3];
END Make8;


PROCEDURE (p: Piece) Make9 (VAR a : PieceShape);
BEGIN
   a[0].next[E] := a[1];
   a[1].next[W] := a[0];
   a[1].next[E] := a[2];
   a[2].next[W] := a[1];
   a[2].next[NE] := a[3];
   a[3].next[SW] := a[2];
   a[2].next[E] := a[4];
   a[4].next[W] := a[2];
   a[4].next[NW] := a[3];
   a[3].next[SE] := a[4];
END Make9;


PROCEDURE (p: Piece) INIT* (n: LONGINT);
VAR
   i, j, k : LONGINT;
BEGIN
   p.orientation := 0;
   p.number := n;

   FOR k := 0 TO PIECE_ORIENTATIONS - 1 DO
      FOR i := 0 TO PIECE_SIZE - 1 DO p.cache[k][i] := NEW(PieceCell); END; 

      CASE n OF
         0 : p.Make0( p.cache[k] );
      |  1 : p.Make1( p.cache[k] );
      |  2 : p.Make2( p.cache[k] );
      |  3 : p.Make3( p.cache[k] );
      |  4 : p.Make4( p.cache[k] );
      |  5 : p.Make5( p.cache[k] );
      |  6 : p.Make6( p.cache[k] );
      |  7 : p.Make7( p.cache[k] );
      |  8 : p.Make8( p.cache[k] );
      |  9 : p.Make9( p.cache[k] );
      END; 

      FOR i := 0 TO k - 1 DO
         IF (i MOD PIECE_ROTATIONS) = 0 THEN 
            FOR j := 0 TO PIECE_SIZE - 1 DO p.cache[k][j].Flip(); END;
         ELSE 
            FOR j := 0 TO PIECE_SIZE - 1 DO p.cache[k][j].Rotate(); END;
         END;
      END;
   END; 
END INIT;


PROCEDURE (p: Piece) Unmark ();
VAR i : LONGINT;
BEGIN 
   FOR i := 0 TO PIECE_SIZE - 1 DO p.cache[p.orientation][i].Unmark(); END; 
END Unmark;


PROCEDURE (p: Piece) NextOrientation () : Piece;
BEGIN 
   p.orientation := (p.orientation + 1) MOD PIECE_ORIENTATIONS;
   RETURN p;
END NextOrientation;


PROCEDURE (p: Piece) cells (i : LONGINT) : PieceCell; 
BEGIN RETURN p.cache[p.orientation][i]; END cells;


PROCEDURE (VAR b: Board) Initialize ();
VAR
   i, row, m : LONGINT;
   c : BoardCell;
   isFirst, isLast : BOOLEAN;
BEGIN
   b.cellCount := 0;
   FOR i := 0 TO BOARD_SIZE - 1 DO b.cells[i] := NEW(BoardCell,i); END;
   m := (BOARD_SIZE DIV BOARD_COLS) - 1;

   FOR i := 0 TO BOARD_SIZE - 1 DO
      row := i DIV BOARD_COLS;
      isFirst := i MOD BOARD_COLS = 0; 
      isLast := (i+1) MOD BOARD_COLS = 0;
      c := b.cells[i];

      IF ODD(row) THEN
         IF ~isLast THEN
            c.next[NE] := b.cells[i - (BOARD_COLS - 1)];
         END;
         c.next[NW] := b.cells[i - BOARD_COLS];

         IF row # m THEN 
            IF ~isLast THEN 
               c.next[SE] := b.cells[i + BOARD_COLS + 1];
            END;
            c.next[SW] := b.cells[i + BOARD_COLS];
         END; 
      ELSE
         IF row # 0 THEN
            IF ~isFirst THEN
               c.next[NW] := b.cells[i - (BOARD_COLS + 1)];
            END;
            c.next[NE] := b.cells[i - BOARD_COLS];
         END;

         IF row # m THEN
            IF ~isFirst THEN
               c.next[SW] := b.cells[i + (BOARD_COLS - 1)];
            END;
            c.next[SE] := b.cells[i + BOARD_COLS];
         END;  
      END;
      IF ~isFirst THEN c.next[W] := b.cells[i - 1]; END;
      IF ~isLast THEN c.next[E] := b.cells[i + 1]; END;
   END;  
END Initialize;


PROCEDURE (VAR b: Board) Unmark ();
VAR i : LONGINT;
BEGIN FOR i := 0 TO BOARD_SIZE - 1 DO b.cells[i].Unmark(); END; END Unmark;


PROCEDURE (VAR b: Board) FirstEmptyCellIndex () : LONGINT;
VAR i : LONGINT;
BEGIN 
   FOR i := 0 TO BOARD_SIZE - 1 DO 
      IF b.cells[i].IsEmpty() THEN RETURN i; END;
   END; 
   RETURN -1;
END FirstEmptyCellIndex;


PROCEDURE (VAR b: Board) Remove (p: Piece);
VAR i : LONGINT;
BEGIN 
   FOR i := 0 TO BOARD_SIZE - 1 DO 
      IF b.cells[i].piece = p THEN b.cells[i].piece := NIL; END;
   END; 
END Remove;


PROCEDURE (VAR b: Board) Find (p: PieceCell; c: BoardCell);
VAR i : LONGINT;
BEGIN 
   IF (p # NIL) & ~p.marked & (c # NIL) THEN
      b.cellsPieceWillFill[b.cellCount] := c;
      INC(b.cellCount);
      p.Mark();
      FOR i := 0 TO SIDES - 1 DO 
         b.Find(p.next[i],c.next[i]);
      END; 
   END;
END Find;


PROCEDURE (VAR b: Board) Add (
      pieceIndex: LONGINT; boardIndex: LONGINT; p: Piece) : BOOLEAN;
VAR 
   i : LONGINT;
   a : BoardPiece;
BEGIN 
   a := b.cache[p.number][p.orientation][pieceIndex][boardIndex];

   b.cellCount := 0;
   p.Unmark();

   IF a = NIL THEN
      b.Find(p.cells(pieceIndex), b.cells[boardIndex]);

      IF b.cellCount # PIECE_SIZE THEN
         b.cache[p.number][p.orientation][pieceIndex][boardIndex] := noFit;
         RETURN FALSE;
      END;

      NEW(a); 
      FOR i := 0 TO PIECE_SIZE - 1 DO a[i] := b.cellsPieceWillFill[i]; END;
      b.cache[p.number][p.orientation][pieceIndex][boardIndex] := a;
   ELSE
      IF a = noFit THEN RETURN FALSE; END;
   END;
   FOR i := 0 TO PIECE_SIZE - 1 DO 
      IF ~a[i].IsEmpty() THEN RETURN FALSE; END;
   END;

   FOR i := 0 TO PIECE_SIZE - 1 DO a[i].piece := p; END;
   RETURN TRUE;
END Add;


PROCEDURE Initialize ();
VAR 
   i : LONGINT;
BEGIN
   board.Initialize(); 
   FOR i := 0 TO LEN(pieces) - 1 DO pieces[i] := NEW(Piece,i); END;
   unplaced := {0..LEN(pieces)-1};
   once := TRUE;
   NEW(noFit);
END Initialize;


PROCEDURE UpdateFirstLast ();
VAR 
   i, n : LONGINT;
   lessFirst, moreFirst, lessLast, moreLast : BOOLEAN;

   PROCEDURE CopyTo(VAR a : BoardPieces);
   BEGIN FOR i := 0 TO BOARD_SIZE - 1 DO a[i] := current[i]; END; END CopyTo;
BEGIN 
   IF once THEN
      FOR i := 0 TO BOARD_SIZE - 1 DO
         n := board.cells[i].piece.number; first[i] := n; last[i] := n;
      END;
      once := FALSE;
   ELSE
      lessFirst := FALSE; moreFirst := FALSE;
      lessLast := FALSE; moreLast := FALSE;
      FOR i := 0 TO BOARD_SIZE - 1 DO
         n := board.cells[i].piece.number;

         IF ~moreFirst & ~lessFirst & (n < first[i]) THEN 
            lessFirst := TRUE; 
         ELSIF (n > first[i]) THEN
            moreFirst := TRUE;
         END;
         IF ~lessLast & ~moreLast & (n > last[i]) THEN 
            moreLast := TRUE;  
         ELSIF (n < last[i]) THEN
            lessLast := TRUE; 
         END;
         current[i] := n;
      END;
      IF lessFirst THEN CopyTo(first); END; 
      IF moreLast THEN CopyTo(last); END;  
   END;
END UpdateFirstLast;


PROCEDURE PrintSolutions ();
   PROCEDURE PrintBoard (a : BoardPieces);
   VAR
      indent : BOOLEAN;
      i, j : LONGINT;
   BEGIN
      indent := FALSE;
      i := 0;
      WHILE i < LEN(a) DO
         IF indent THEN Out.Char(' '); END;
         FOR j := 0 TO BOARD_COLS - 1 DO 
            Out.Char(CHR(a[i] + 48)); Out.Char(' '); 
            INC(i);
         END;
         Out.Ln;
         indent := ~indent;
      END; 
      Out.Ln;
   END PrintBoard;
BEGIN
   Out.Int(n,1); Out.String(" solutions found"); Out.Ln; Out.Ln;
   PrintBoard(first);
   PrintBoard(last);
END PrintSolutions;


PROCEDURE PuzzleSolved ();
BEGIN
   UpdateFirstLast();
   DEC(countdown);
END PuzzleSolved;


PROCEDURE ShouldPrune () : BOOLEAN;
VAR 
   i : LONGINT;
   forall : BOOLEAN;
BEGIN
   board.Unmark(); 
   FOR i := 0 TO BOARD_SIZE - 1 DO
      forall := (board.cells[i].ContiguousEmptyCells() MOD PIECE_SIZE) = 0;
      IF ~forall THEN RETURN ~forall; END;
   END;
   RETURN ~forall;
END ShouldPrune;


PROCEDURE FindSolutions ();
VAR
   emptyCellIndex, k, i, j : LONGINT;   
   piece : Piece;
BEGIN
   IF countdown > 0 THEN
      IF unplaced # {} THEN
         emptyCellIndex := board.FirstEmptyCellIndex();

         FOR k := 0 TO LEN(pieces) - 1 DO
            IF k IN unplaced THEN
               unplaced := unplaced - {k};

               FOR i := 0 TO PIECE_ORIENTATIONS - 1 DO
                  piece := pieces[k].NextOrientation();  
                  FOR j := 0 TO PIECE_SIZE - 1 DO
                     IF board.Add(j,emptyCellIndex,piece) THEN
                        IF ~ShouldPrune() THEN FindSolutions(); END;
                        board.Remove(piece); 
                     END;
                  END; 
               END; 
               unplaced := unplaced + {k};
            END;
         END;
      ELSE
         PuzzleSolved();
      END;
   END;
END FindSolutions;


BEGIN
   n := Shootout.Argi();
   countdown := n;
   Initialize();

   FindSolutions(); 
   PrintSolutions();  
END meteor.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)

See:
OBJECT-ORIENTED PROGRAMMING IN OBERON-2
http://www.cas.mcmaster.ca/~kahl/SE3E03/2002/Oberon2/Oberon2.OOP.pdf
*)

MODULE methcall;
IMPORT Shootout, Out;

TYPE
   ToggleDesc = RECORD
      state: BOOLEAN;
   END;

   Toggle = POINTER TO ToggleDesc;


   NthToggleDesc = RECORD (ToggleDesc)
      countMax, counter: LONGINT;
   END;

   NthToggle = POINTER TO NthToggleDesc;

VAR
   n, i: LONGINT;
   toggle: Toggle;
   ntoggle: NthToggle;
   v: BOOLEAN;


PROCEDURE NewToggle(state: BOOLEAN): Toggle;
VAR t: Toggle;
BEGIN
   NEW(t); t.state := state;
   RETURN t;
END NewToggle;

PROCEDURE (t:Toggle) Activate();
BEGIN
   t.state := ~t.state;
END Activate;

PROCEDURE (t:Toggle) Value(): BOOLEAN;
BEGIN
   RETURN t.state;
END Value;


PROCEDURE NewNthToggle(state: BOOLEAN; countMax: LONGINT): NthToggle;
VAR t: NthToggle;
BEGIN
   NEW(t);
   t.state := state;
   t.countMax := countMax;
   RETURN t;
END NewNthToggle;

PROCEDURE (t:NthToggle) Activate();
BEGIN
   INC(t.counter);
   IF t.counter >= t.countMax THEN
      t.state := ~t.state;
      t.counter := 0;
   END;
END Activate;


BEGIN
   n := Shootout.Argi();
   toggle := NewToggle(TRUE);
   FOR i := 1 TO n DO
      toggle.Activate();
      v := toggle.Value();
   END;
   IF v THEN Out.String("true");
        ELSE Out.String("false"); END;
   Out.Ln;

   ntoggle := NewNthToggle(TRUE,3);
   FOR i := 1 TO n DO
      ntoggle.Activate();
      v := ntoggle.Value();
   END;
   IF v THEN Out.String("true");
        ELSE Out.String("false"); END;
   Out.Ln;
END methcall.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)


MODULE moments;
IMPORT IO, IO:StdChannels, S:=SYSTEM, RealConv, Out, LRealMath;

CONST
   readSize = 4096;

TYPE
   FixedBuffer = ARRAY readSize OF CHAR;
   FixedBufferProc = PROCEDURE (VAR b: FixedBuffer; VAR pos: LONGINT);   
   Reals = POINTER TO ARRAY OF REAL;

VAR
   buffer: FixedBuffer;
   x: Reals;
   i, n, mid: LONGINT;
   sum, mean, dev, adev, dev2, var, skew, kurt, sdev, median: LONGREAL;


(* Separate special processing in procedure Process so we can use
   FillAndProcess in other Shootout tests *)
PROCEDURE FillAndProcess (VAR buffer: FixedBuffer; Process: FixedBufferProc);
VAR
   channel: IO.ByteChannel;
   partialFill: BOOLEAN;
   size, pos, count: LONGINT;
BEGIN
   channel := StdChannels.stdin;
   pos := 0;
   size := readSize;
   LOOP
      TRY
         count := channel.Read(buffer, pos, size * SIZE(CHAR)); 
         partialFill := count < size; 
         IF partialFill THEN size := pos + count; ELSE size := readSize; END;               
         pos := size;	

         Process(buffer,pos);

         IF partialFill THEN EXIT; END;  
         
         IF pos = size THEN 
            pos := 0;
            size := readSize;
         ELSE
            S.MOVE(S.ADR(buffer[pos]), S.ADR(buffer[0]), (size-pos)*SIZE(CHAR));
            pos := size - pos;
            size := readSize - pos;
         END;   
                      
      CATCH IO.Error:
         EXIT;
      END;               
   END;
END FillAndProcess;


PROCEDURE ReadReals(VAR buffer: FixedBuffer; VAR pos: LONGINT);
TYPE
   Chars = POINTER TO ARRAY 32 OF CHAR;
VAR
   i, j: LONGINT;
   chars: Chars;
   tmp: Reals;
BEGIN
   i := 0; j := 0; 
   WHILE j < pos DO	
      IF buffer[j] = 0AX THEN
            
         IF n = LEN(x^) THEN (* increase array size *)
            NEW(tmp, ASH(LEN(x^),1));
            S.MOVE(S.ADR(x[0]), S.ADR(tmp[0]), n*SIZE(REAL));            
            x := tmp; tmp := NIL;
         END;            
            
         buffer[j] := 0X;	       
         chars := S.VAL(Chars, S.ADR(buffer[i]));           
         x[n] := RealConv.ValueReal(chars^);                          
         sum := sum + x[n]; INC(n);         
         
         INC(j); i := j;	
      ELSE
         INC(j);
      END;                  
   END;         
      
   IF j > i THEN pos := i; END; (* indicate buffer position *)
END ReadReals; 


PROCEDURE QuickSelect(n: LONGINT; VAR a: ARRAY OF REAL): REAL; 
VAR
   lo, hi, median, mid, l, h: LONGINT;
   t: REAL;
   
PROCEDURE Swap(i,j: LONGINT); BEGIN t := a[i]; a[i] := a[j]; a[j] := t; END Swap;  
   
BEGIN
   lo := 0; hi := n-1; median := (lo + hi) DIV 2;
   LOOP
      IF hi <= lo THEN RETURN a[median]; END;
      IF hi = lo + 1 THEN 
         IF a[lo] > a[hi] THEN Swap(lo,hi); END;
         RETURN a[median];
      END;      
   
      mid := (lo + hi) DIV 2;
      IF a[mid] > a[hi] THEN Swap(mid,hi); END;
      IF a[lo] > a[hi] THEN Swap(lo,hi); END;  
      IF a[mid] > a[lo] THEN Swap(mid,lo); END;
      
      Swap(mid,lo+1);       
      
      l := lo + 1; h := hi;     
      LOOP  
         REPEAT INC(l); UNTIL a[lo] <= a[l];
         REPEAT DEC(h); UNTIL a[h] <= a[lo]; 
         IF h < l THEN EXIT; END; 
         Swap(l,h);         
      END;       
      
      Swap(lo,h);
      IF h <= median THEN lo := l; END;
      IF h >= median THEN hi := h - 1; END; 
   END;     
END QuickSelect; 


BEGIN
   n := 0; sum := 0.0; NEW(x,4096);
   FillAndProcess(buffer, ReadReals);  
   
   mean := sum / n;
   FOR i := 0 TO n-1 DO
      dev := x[i] - mean;
      adev := adev + ABS(dev);
      dev2 := dev * dev;
      var := var + dev2;
      skew := skew + dev2 * dev;
      kurt := kurt + dev2 * dev2;
   END;
   adev := adev / n;
   var := var / (n - 1);
   sdev := LRealMath.sqrt(var);
   IF var # 0.0 THEN
      skew := skew / (n * var * sdev);
      kurt := kurt / (n * var * var) - 3.0;
   END;

   median := QuickSelect(n, x^);   
   mid := (n DIV 2) - 1;
   IF ODD(mid) THEN median := (x[mid] + x[mid+1]) / 2; END;    
               
   Out.String("n:                  "); Out.Int(n,0); Out.Ln;
   Out.String("median:             "); Out.LongRealFix(median,0,6); Out.Ln;
   Out.String("mean:               "); Out.LongRealFix(mean,0,6); Out.Ln;
   Out.String("average_deviation:  "); Out.LongRealFix(adev,0,6); Out.Ln;
   Out.String("standard_deviation: "); Out.LongRealFix(sdev,0,6); Out.Ln;
   Out.String("variance:           "); Out.LongRealFix(var,0,6); Out.Ln;
   Out.String("skew:               "); Out.LongRealFix(skew,0,6); Out.Ln;
   Out.String("kurtosis:           "); Out.LongRealFix(kurt,0,6); Out.Ln; 
END moments.


(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)

   To run:   nbody 1000000
*)


MODULE nbody;
IMPORT Shootout, LRealMath, Out;

CONST
   PI = 3.141592653589793D+00;
   SOLAR_MASS = 4.0D+00 * PI * PI;
   DAYS_PER_YEAR = 365.24D+00;

TYPE
   Body = RECORD x, y, z, vx, vy, vz, mass: LONGREAL; END;

VAR
   i, n: LONGINT;
   sun, jupiter, saturn, uranus, neptune: Body;
   bodies: ARRAY 5 OF Body;

PROCEDURE Advance(VAR bodies: ARRAY OF Body; dt: LONGREAL);
VAR
   dx, dy, dz, distance, mag: LONGREAL;
   i, j, n: LONGINT;
BEGIN
   n := LEN(bodies) - 1;
   FOR i := 0 TO n DO
      FOR j := i+1 TO n DO
         dx := bodies[i].x - bodies[j].x;
         dy := bodies[i].y - bodies[j].y;
         dz := bodies[i].z - bodies[j].z;

         distance := LRealMath.sqrt(dx*dx + dy*dy + dz*dz);
         mag := dt / (distance * distance * distance);

         bodies[i].vx := bodies[i].vx - (dx * bodies[j].mass * mag);
         bodies[i].vy := bodies[i].vy - (dy * bodies[j].mass * mag);
         bodies[i].vz := bodies[i].vz - (dz * bodies[j].mass * mag);

         bodies[j].vx := bodies[j].vx + (dx * bodies[i].mass * mag);
         bodies[j].vy := bodies[j].vy + (dy * bodies[i].mass * mag);
         bodies[j].vz := bodies[j].vz + (dz * bodies[i].mass * mag);
      END;
   END;

   FOR i := 0 TO n DO
      bodies[i].x := bodies[i].x + (dt * bodies[i].vx);
      bodies[i].y := bodies[i].y + (dt * bodies[i].vy);
      bodies[i].z := bodies[i].z + (dt * bodies[i].vz);
   END;
END Advance;


PROCEDURE Energy(VAR bodies: ARRAY OF Body): LONGREAL;
VAR
   e, dx, dy, dz, distance: LONGREAL;
   i, j, n: LONGINT;
BEGIN
   e := 0.0D+00;
   n := LEN(bodies) - 1;
   FOR i := 0 TO n DO
      e := e + (0.5D+00 * bodies[i].mass *
         ( bodies[i].vx * bodies[i].vx
         + bodies[i].vy * bodies[i].vy
         + bodies[i].vz * bodies[i].vz ));
	 	 	
      FOR j := i+1 TO n DO
         dx := bodies[i].x - bodies[j].x;
         dy := bodies[i].y - bodies[j].y;
         dz := bodies[i].z - bodies[j].z;

         distance := LRealMath.sqrt(dx*dx + dy*dy + dz*dz);	
         e := e - (bodies[i].mass * bodies[j].mass / distance);	
      END;
   END;
   RETURN e;
END Energy;


PROCEDURE OffsetMomentum(VAR bodies: ARRAY OF Body);
VAR
   px, py, pz: LONGREAL;
   i: LONGINT;
BEGIN
   px := 0.0D+00; py := 0.0D+00; pz := 0.0D+00;
   FOR i := 0 TO LEN(bodies)-1 DO
      px := px + (bodies[i].vx * bodies[i].mass);
      py := py + (bodies[i].vy * bodies[i].mass);
      pz := pz + (bodies[i].vz * bodies[i].mass);
   END;
   bodies[0].vx := -px / SOLAR_MASS;
   bodies[0].vy := -py / SOLAR_MASS;
   bodies[0].vz := -pz / SOLAR_MASS;
END OffsetMomentum;


BEGIN
   n := Shootout.Argi();

   (* define planetary masses, initial positions, velocities *)

   jupiter.x := 4.84143144246472090D+00;
   jupiter.y := -1.16032004402742839D+00;
   jupiter.z := -1.03622044471123109D-01;
   jupiter.vx := 1.66007664274403694D-03 * DAYS_PER_YEAR;
   jupiter.vy := 7.69901118419740425D-03 * DAYS_PER_YEAR;
   jupiter.vz := -6.90460016972063023D-05 * DAYS_PER_YEAR;
   jupiter.mass := 9.54791938424326609D-04 * SOLAR_MASS;

   saturn.x := 8.34336671824457987D+00;
   saturn.y := 4.12479856412430479D+00;
   saturn.z := -4.03523417114321381D-01;
   saturn.vx := -2.76742510726862411D-03 * DAYS_PER_YEAR;
   saturn.vy := 4.99852801234917238D-03 * DAYS_PER_YEAR;
   saturn.vz := 2.30417297573763929D-05 * DAYS_PER_YEAR;
   saturn.mass := 2.85885980666130812D-04 * SOLAR_MASS;

   uranus.x := 1.28943695621391310D+01;
   uranus.y := -1.51111514016986312D+01;
   uranus.z := -2.23307578892655734D-01;
   uranus.vx := 2.96460137564761618D-03 * DAYS_PER_YEAR;
   uranus.vy := 2.37847173959480950D-03 * DAYS_PER_YEAR;
   uranus.vz := -2.96589568540237556D-05 * DAYS_PER_YEAR;
   uranus.mass := 4.36624404335156298D-05 * SOLAR_MASS;

   neptune.x := 1.53796971148509165D+01;
   neptune.y := -2.59193146099879641D+01;
   neptune.z := 1.79258772950371181D-01;
   neptune.vx := 2.68067772490389322D-03 * DAYS_PER_YEAR;
   neptune.vy := 1.62824170038242295D-03 * DAYS_PER_YEAR;
   neptune.vz := -9.51592254519715870D-05 * DAYS_PER_YEAR;
   neptune.mass := 5.15138902046611451D-05 * SOLAR_MASS;

   sun.x := 0.0; sun.y := 0.0; sun.z := 0.0;
   sun.vx := 0.0; sun.vy := 0.0; sun.vz := 0.0; sun.mass := SOLAR_MASS;

   bodies[0] := sun;
   bodies[1] := jupiter; bodies[2] := saturn;
   bodies[3] := uranus; bodies[4] := neptune;

   OffsetMomentum(bodies);

   Out.LongRealFix( Energy(bodies), 0,9); Out.Ln;
   FOR i := 1 TO n DO Advance(bodies,0.01D+00); END;
   Out.LongRealFix( Energy(bodies), 0,9); Out.Ln;
END nbody.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)


MODULE nestedloop;
IMPORT Shootout, Out;

VAR n, count, g, h, i, j, k, l: LONGINT;

BEGIN
   n := Shootout.Argi();
   count := 0;

   FOR g := 1 TO n DO
      FOR h := 1 TO n DO
         FOR i := 1 TO n DO
            FOR j := 1 TO n DO
               FOR k := 1 TO n DO
                  FOR l := 1 TO n DO
                     INC(count);
                  END;
               END;
            END;
         END;
      END;
   END;  	
		
   Out.Int(count,1); Out.Ln;
END nestedloop.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE nsieve;
IMPORT Shootout,Out;

TYPE
   FlagsType = ARRAY OF BOOLEAN;

VAR
   n, m: LONGINT;
   flags: POINTER TO FlagsType;


PROCEDURE NSieve(m: LONGINT; VAR isPrime: FlagsType): LONGINT;
VAR
   count, i, k: LONGINT;
BEGIN
   FOR i := 2 TO m DO isPrime[i] := TRUE; END;

   count := 0;
   FOR i := 2 TO m DO
      IF isPrime[i] THEN
	 INC(count);
         k := i+i;
         WHILE k <= m DO
            isPrime[k] := FALSE;
	    INC(k, i);
         END;
      END;
   END;
   RETURN count;
END NSieve;

BEGIN
   n := Shootout.Argi();
   IF n < 2 THEN n := 2; END;                    
    
   m := 10000 * ASH(1,n);
   NEW(flags, m+1);
   Out.String("Primes up to "); Out.Int(m,8); Out.String(" "); Out.Int(NSieve(m,flags^),8); Out.Ln;
    
   m := 10000 * ASH(1,n-1);
   Out.String("Primes up to "); Out.Int(m,8); Out.String(" "); Out.Int(NSieve(m,flags^),8); Out.Ln;
    
   m := 10000 * ASH(1,n-2);
   Out.String("Primes up to "); Out.Int(m,8); Out.String(" "); Out.Int(NSieve(m,flags^),8); Out.Ln;         
END nsieve.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)


See:
OBJECT-ORIENTED PROGRAMMING IN OBERON-2
http://www.cas.mcmaster.ca/~kahl/SE3E03/2002/Oberon2/Oberon2.OOP.pdf
*)

MODULE objinst;
IMPORT Shootout, Out;

TYPE
   ToggleDesc = RECORD
      state: BOOLEAN;
   END;

   Toggle = POINTER TO ToggleDesc;


   NthToggleDesc = RECORD (ToggleDesc)
      countMax, counter: LONGINT;
   END;

   NthToggle = POINTER TO NthToggleDesc;

VAR
   n, i: LONGINT;
   toggle: Toggle;
   ntoggle: NthToggle;


PROCEDURE NewToggle(state: BOOLEAN): Toggle;
VAR t: Toggle;
BEGIN
   NEW(t); t.state := state;
   RETURN t;
END NewToggle;

PROCEDURE (t:Toggle) Activate();
BEGIN
   t.state := ~t.state;
END Activate;

PROCEDURE (t:Toggle) Value(): BOOLEAN;
BEGIN
   RETURN t.state;
END Value;


PROCEDURE NewNthToggle(state: BOOLEAN; countMax: LONGINT): NthToggle;
VAR t: NthToggle;
BEGIN
   NEW(t);
   t.counter := 0;
   t.state := state;
   t.countMax := countMax;
   RETURN t;
END NewNthToggle;

PROCEDURE (t:NthToggle) Activate();
BEGIN
   INC(t.counter);
   IF t.counter >= t.countMax THEN
      t.state := ~t.state;
      t.counter := 0;
   END;
END Activate;


BEGIN
   n := Shootout.Argi();

   toggle := NewToggle(TRUE);
   FOR i := 1 TO 5 DO
      toggle.Activate();
      IF toggle.Value() THEN Out.String("true");
                        ELSE Out.String("false"); END;
      Out.Ln;
   END;

   FOR i := 1 TO n DO toggle := NewToggle(TRUE); END;

   Out.Ln;
   ntoggle := NewNthToggle(TRUE,3);
   FOR i := 1 TO 8 DO
      ntoggle.Activate();
      IF ntoggle.Value() THEN Out.String("true");
                         ELSE Out.String("false"); END;
      Out.Ln;
   END;
   FOR i := 1 TO n DO ntoggle := NewNthToggle(TRUE,3); END;
END objinst.
(* The Computer Language Shootout
   http://shootout.alioth.debian.org
   contributed by Isaac Gouy (Oberon-2 novice) *)

MODULE partialsums;
IMPORT Shootout, Out, LRealMath;

CONST
   twothirds = 2.0/3.0;
VAR
   k, n: LONGINT;
   a1, a2, a3, a4, a5, a6, a7, a8, a9, k2, k3, sk, ck, alt: LONGREAL;

PROCEDURE WriteLn(VAR a: LONGREAL; name: ARRAY OF CHAR);
BEGIN Out.LongRealFix(a,0,9); Out.Char(9X); Out.String(name); Out.Ln; END WriteLn;

BEGIN
   n := Shootout.Argi();
   alt := -1.0D+00;

   FOR k := 1 TO n DO 
      k2 := LRealMath.power(k,2.0D+00);
      k3 := k2*k;
      sk := LRealMath.sin(k);
      ck := LRealMath.cos(k);
      alt := -alt;

      a1 := a1 + LRealMath.power(twothirds,k-1.0D+00);
      a2 := a2 + LRealMath.power(k,-0.5D+00);
      a3 := a3 + 1.0D+00/(k*(k+1.0D+00));
      a4 := a4 + 1.0D+00/(k3*(sk*sk));
      a5 := a5 + 1.0D+00/(k3*(ck*ck));
      a6 := a6 + 1.0D+00/k;
      a7 := a7 + 1.0D+00/k2;
      a8 := a8 + alt/k;
      a9 := a9 + alt/(2.0D+00*k -1.0D+00);
   END;

   WriteLn(a1,"(2/3)^k");
   WriteLn(a2,"k^-0.5");
   WriteLn(a3,"1/k(k+1)");
   WriteLn(a4,"Flint Hills");
   WriteLn(a5,"Cookson Hills");
   WriteLn(a6,"Harmonic");
   WriteLn(a7,"Riemann Zeta");
   WriteLn(a8,"Alternating Harmonic"); 
   WriteLn(a9,"Gregory");
END partialsums.
(* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE primes;
IMPORT Shootout, Out, LRealMath;

VAR
   n, i: LONGINT;


PROCEDURE IsPrime(k: LONGINT): BOOLEAN;
VAR
   limit, i: LONGINT;
BEGIN
   IF k < 2 THEN RETURN FALSE;
   ELSIF k < 4 THEN RETURN TRUE;
   ELSIF ~ODD(k) THEN RETURN FALSE;
   ELSIF ((k+1) MOD 6 # 0) & ((k-1) MOD 6 # 0) THEN RETURN FALSE;
   ELSE
      limit := ENTIER(LRealMath.sqrt(k));
      FOR i := 5 TO limit BY 2 DO 
         IF k MOD i = 0 THEN RETURN FALSE; END;
      END;
   END;
   RETURN TRUE;
END IsPrime;


PROCEDURE Prime(n: LONGINT): LONGINT;
VAR
   count, k: LONGINT;
BEGIN
   count := 0; 
   k := 1;
   WHILE count < n DO
      IF IsPrime(k) THEN INC(count); END;
      INC(k);
   END;
   RETURN k-1;
END Prime;


BEGIN
   n := Shootout.Argi();

   Out.String("1st prime is "); Out.Int(Prime(1),1); Out.Ln;
   Out.String("2nd prime is "); Out.Int(Prime(2),1); Out.Ln;

   i := 10*n;
   WHILE i <= 50*n DO
      Out.Int(i,1); Out.String("th prime is "); Out.Int(Prime(i),1); Out.Ln;
      INC(i,10*n);
   END;
END primes.
(* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org
   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE primes;
IMPORT Shootout, Out, LRealMath;

VAR
   n, i: LONGINT;


PROCEDURE Prime(n: LONGINT): LONGINT;
VAR
   count, primeNumber, k, limit, i: LONGINT;
   isTrivial: BOOLEAN;
BEGIN
   IF n < 2 THEN
      IF n = 1 THEN count := 1; primeNumber := 2; END;
   ELSE
      count := 2; primeNumber := 3;
   END;

   k := 5;
   WHILE count < n DO
      IF ((k+1) MOD 6 = 0) OR ((k-1) MOD 6 = 0) THEN

         isTrivial := TRUE;
         limit := ENTIER(LRealMath.sqrt(k));

         i := 5;
         LOOP
            IF i > limit THEN EXIT END;
            IF k MOD i = 0 THEN isTrivial := FALSE; EXIT END;
            INC(i,2);
         END;

         IF isTrivial THEN INC(count); primeNumber := k; END;
      END;
      INC(k,2);
   END;

   RETURN primeNumber;
END Prime;


BEGIN
   n := Shootout.Argi();

   Out.String("1st prime is "); Out.Int(Prime(1),1); Out.Ln;
   Out.String("2nd prime is "); Out.Int(Prime(2),1); Out.Ln;

   i := 10*n;
   WHILE i <= 50*n DO
      Out.Int(i,1); Out.String("th prime is "); Out.Int(Prime(i),1); Out.Ln;
      INC(i,10*n);
   END;
END primes.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE random;
IMPORT Shootout, Out;

CONST
   im = 139968;
   ia = 3877;
   ic = 29573;

VAR
   n, last: LONGINT;
   result: LONGREAL;

PROCEDURE Next (max: LONGREAL): LONGREAL;
BEGIN
   last := (last*ia + ic) MOD im;
   RETURN max * last / im;
END Next;

BEGIN
   last := 42;
   n := Shootout.Argi();

   WHILE n > 0 DO
      DEC(n);
      result := Next(100.0);
   END;  	

   Out.LongRealFix(result,0,9); Out.Ln;
END random.
(* The Computer Language Shootout
   http://shootout.alioth.debian.org
   contributed by Isaac Gouy (Oberon-2 novice) *)

MODULE recursive;
IMPORT Shootout, Out;

VAR n: LONGINT;

PROCEDURE Ack (m, n: LONGINT): LONGINT;
BEGIN
   IF m=0 THEN RETURN n+1; END;
   IF n=0 THEN RETURN Ack(m-1, 1); END;
   RETURN Ack(m-1, Ack(m, n-1));
END Ack;

PROCEDURE Fib (n: LONGINT): LONGINT;
BEGIN
   IF n<2 THEN RETURN 1; ELSE RETURN Fib(n-2) + Fib(n-1); END;
END Fib;

PROCEDURE Tak (x,y,z: LONGINT) : LONGINT;
BEGIN
   IF y<x THEN RETURN Tak(Tak(x-1,y,z), Tak(y-1,z,x), Tak(z-1,x,y)); 
   ELSE RETURN z; END;	
END Tak;

PROCEDURE Fibr (n: LONGREAL): LONGREAL;
BEGIN
   IF n<2.0D+00 THEN RETURN 1.0D+00; 
   ELSE RETURN Fibr(n-2.0D+00) + Fibr(n-1.0D+00); END;
END Fibr;

PROCEDURE Takr (x,y,z: LONGREAL) : LONGREAL;
BEGIN
   IF y<x THEN RETURN 
      Takr( Takr(x-1.0D+00,y,z), Takr(y-1.0D+00,z,x), Takr(z-1.0D+00,x,y) ); 
   ELSE RETURN z; END;	
END Takr;

BEGIN
   n := Shootout.Argi();

   Out.String("Ack(3,"); Out.Int(n,0); Out.String("): "); 
   Out.Int(Ack(3,n),0); Out.Ln;

   Out.String("Fib("); Out.LongRealFix(27.0+n,0,1); Out.String("): "); 
   Out.LongRealFix(Fibr(27.0+n),0,1); Out.Ln;

   DEC(n); 
   Out.String("Tak("); Out.Int(n*3,0); Out.String(","); Out.Int(n*2,0);
   Out.String(","); Out.Int(n,0); Out.String("): "); Out.Int(Tak(n*3,n*2,n),0); Out.Ln;

   Out.String("Fib(3): "); Out.Int(Fib(3),0); Out.Ln;
   Out.String("Tak(3.0,2.0,1.0): "); Out.LongRealFix(Takr(3.0,2.0,1.0),0,1); Out.Ln;
END recursive.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org
   
   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE reversefile;
IMPORT StdChannels, Channel, Out, SYSTEM;

TYPE  
   Line = POINTER TO ARRAY 120 OF CHAR; 
   
VAR
   in: Channel.Reader;
   buffer: POINTER TO ARRAY OF CHAR;
   i, j: LONGINT;
   line: Line;
   
BEGIN
   in := StdChannels.stdin.NewReader();
   i := in.Available();   
   IF i > 0 THEN      
      NEW(buffer, i+1);
      in.ReadBytes(buffer^,0,i); buffer[i] := 0X; DEC(i);              
      IF buffer[i] = 0AX THEN DEC(i); END;
      j := i;
      WHILE j > 0 DO
         IF buffer^[i] = 0AX THEN
            line := SYSTEM.VAL(Line, SYSTEM.ADR(buffer[j]));
            Out.String(line^);
            buffer[j] := 0X;      
         END;
         j := i; DEC(i);
      END;
      line := SYSTEM.VAL(Line, SYSTEM.ADR(buffer[j]));
      Out.String(line^);
   END;   
END reversefile.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE sieve;
IMPORT Shootout, Out;

CONST
   start = 2;
   stop = 8192;
   size = stop+1;

VAR
   n, count, i, k: LONGINT;
   isPrimeNumber: ARRAY size OF BOOLEAN;

BEGIN
   n := Shootout.Argi();
   WHILE n > 0 DO
      DEC(n);
      count := 0;      
      FOR i := start TO stop DO isPrimeNumber[i] := TRUE; END;
      FOR i := start TO stop DO
         IF isPrimeNumber[i] THEN
            INC(count);
            k := i+i;
            WHILE k <= stop DO
               isPrimeNumber[k] := FALSE;
               INC(k, i);
            END;
         END;
      END;
   END;  	

   Out.String("Count: "); Out.Int(count,0); Out.Ln;
END sieve.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)

   To run:   spectralnorm 100 
*)


MODULE spectralnorm;
IMPORT Shootout, LRealMath, Out;

TYPE Vector = POINTER TO ARRAY OF LONGREAL;
VAR
   i, n: LONGINT;
   u, v: Vector;
   vBv, vv: LONGREAL;


(* return element i,j of infinite matrix A *)
PROCEDURE A(i,j: LONGINT): LONGREAL;
BEGIN RETURN 1.0 / ((i+j)*(i+j+1) /2 +i+1); END A;


(* multiply vector v by matrix A *)
PROCEDURE MultiplyAv(v, Av: Vector);
VAR
   i,j: LONGINT;
BEGIN
   FOR i := 0 TO n-1 DO
      Av[i] := 0.0;
      FOR j := 0 TO n-1 DO Av[i] := Av[i] + A(i,j) * v[j]; END;
   END;
END MultiplyAv;


(* multiply vector v by matrix A transposed *)
PROCEDURE MultiplyAtv(v, Atv: Vector);
VAR
   i,j: LONGINT;
BEGIN
   FOR i := 0 TO n-1 DO
      Atv[i] := 0.0;
      FOR j := 0 TO n-1 DO Atv[i] := Atv[i] + A(j,i) * v[j]; END;
   END;
END MultiplyAtv;


(* multiply vector v by matrix A and then by matrix A transposed *)
PROCEDURE MultiplyAtAv(v, AtAv: Vector);
VAR
   u: Vector;
BEGIN
   NEW(u, n);    
   MultiplyAv(v, u);
   MultiplyAtv(u, AtAv);
END MultiplyAtAv;


BEGIN
   n := Shootout.Argi();

   (* create unit vector *)
   NEW(u, n);
   FOR i := 0 TO n-1 DO u[i] := 1.0; END;   

   (* 20 steps of the power method *)
   NEW(v, n);
   FOR i := 0 TO n-1 DO v[i] := 0.0; END;

   FOR i := 1 TO 10 DO
      MultiplyAtAv(u, v);
      MultiplyAtAv(v, u);
   END;

   FOR i := 0 TO n-1 DO
      vBv := vBv + u[i]*v[i];
      vv := vv + v[i]*v[i];
   END;

   Out.LongRealFix( LRealMath.sqrt(vBv/vv), 0,9); Out.Ln;
END spectralnorm.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE spellcheck;
IMPORT IO, IO:StdChannels, S:=SYSTEM, SimpleHash, In, Out, Files, TextRider;

CONST
   maxWordSize = 32;
   readSize = 4096;   
   
TYPE
   FixedBuffer = ARRAY readSize OF CHAR;
   FixedBufferProc = PROCEDURE (VAR b: FixedBuffer; VAR pos: LONGINT); 
   WordString = ARRAY maxWordSize OF CHAR;   

VAR
   buffer: FixedBuffer;
   word: WordString;
   result: Files.Result;
   f: Files.File;
   dictionary: SimpleHash.Hashtable;
   key: SimpleHash.ItemKey;
   item: SimpleHash.Item;


(* Separate special processing in procedure Process so we can use
   FillAndProcess in other Shootout tests *)
PROCEDURE FillAndProcess (VAR buffer: FixedBuffer; Process: FixedBufferProc);
VAR
   channel: IO.ByteChannel;
   partialFill: BOOLEAN;
   size, pos, count: LONGINT;
BEGIN
   channel := StdChannels.stdin;
   pos := 0;
   size := readSize;
   LOOP
      TRY
         count := channel.Read(buffer, pos, size * SIZE(CHAR)); 
         partialFill := count < size; 
         IF partialFill THEN size := pos + count; ELSE size := readSize; END;               
         pos := size;	

         Process(buffer,pos);

         IF partialFill THEN EXIT; END;  
         
         IF pos = size THEN 
            pos := 0;
            size := readSize;
         ELSE
            S.MOVE(S.ADR(buffer[pos]), S.ADR(buffer[0]), (size-pos)*SIZE(CHAR));
            pos := size - pos;
            size := readSize - pos;
         END;   
                      
      CATCH IO.Error:
         EXIT;
      END;               
   END;
END FillAndProcess;


PROCEDURE CheckWords(VAR buffer: FixedBuffer; VAR pos: LONGINT);
TYPE
   WordStringPtr = POINTER TO WordString;
VAR
   i, j: LONGINT;
   bufferWord: WordStringPtr;
BEGIN
   i := 0; j := 0; NEW(key, maxWordSize);
   WHILE j < pos DO	
      IF buffer[j] = 0AX THEN
         buffer[j] := 0X;	
         bufferWord := S.VAL(WordStringPtr, S.ADR(buffer[i]));	         	    		
         COPY(bufferWord^, key^);
	
         IF SimpleHash.Find(dictionary, key) = NIL THEN
            Out.String(key^); Out.Ln;
         END;	
	 	
         INC(j); i := j;	
      ELSE
         INC(j);
      END;
   END;
      
   IF j > i THEN pos := i; END; (* indicate buffer position *)
END CheckWords;


BEGIN
   f := Files.Old("Usr.Dict.Words", {Files.read}, result);
   IF result # Files.done THEN
      Out.String("Usr.Dict.Words not opened"); Out.Ln; HALT(1);
   END;   

   dictionary := SimpleHash.New(40000);	
   In.SetReader( TextRider.ConnectReader(f) );
   LOOP	
      In.Line(word);
      IF ~In.Done() THEN EXIT; END;
      
      NEW(key, maxWordSize);
      COPY(word,key^);         
      item := SimpleHash.AddKey(dictionary, key);	
   END;     
   f.Close();

   FillAndProcess(buffer, CheckWords);      
END spellcheck.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)

   Do the same low-level stuff as the C implementations.
*)

MODULE strcat;
IMPORT Shootout, SYSTEM, Out, Strings;

TYPE
   CharArray = ARRAY 8 OF CHAR;   
   String = POINTER TO CharArray;

VAR
   buffer, tmp: POINTER TO ARRAY OF CHAR; 
   bufferEnd: String;
   stuff: CharArray;
   n, stufflen, start, end, buflen, newbuflen: LONGINT;


(* Strings.Length only returns INTEGER, we need LONGINT *)
PROCEDURE Length(VAR a: ARRAY OF CHAR): LONGINT;
VAR i: LONGINT;
BEGIN
   i := 0;
   WHILE (i < LEN(a)) & (a[i] # 0X) DO INC(i); END; 
   RETURN i;   
END Length; 


BEGIN
   n := Shootout.Argi();
   stuff := "hello"; stuff[5] := 0AX; stuff[6] := 0X;
   stufflen := Length(stuff);   
   
   buflen := 32; NEW(buffer, buflen); buffer[0] := 0X;
   start := SYSTEM.ADR(buffer[0]);
   end := start;
   WHILE n > 0 DO 
      IF (start + buflen) - end < stufflen + 1 THEN
         newbuflen := ASH(buflen,1);
         NEW(tmp, newbuflen);
         COPY(buffer^, tmp^);
         buffer := tmp; tmp := NIL;
         DEC(end, start);
         start := SYSTEM.ADR(buffer[0]);
         INC(end, start);
         buflen := newbuflen;
      END;   
         (* much faster to strcat to strend than to strbuf! *)
      bufferEnd := SYSTEM.VAL(String, end);        
      Strings.Append(stuff, bufferEnd^);      
      INC(end, stufflen);

      DEC(n);
   END;   
        
   Out.Int( Length(buffer^), 0); Out.Ln; 
END strcat.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)

MODULE sumcol;
IMPORT In, IntConv, Out;

CONST
   maxLineSize = 120;
VAR 
   line: ARRAY maxLineSize OF CHAR;
   sum: LONGINT;
BEGIN
   sum := 0;
   LOOP
      In.Line(line);           
      IF line[0] = 0X THEN EXIT; END;    
      INC(sum, IntConv.ValueInt(line)); 
   END;
   Out.Int(sum, 0); Out.Ln;
END sumcol.
(* $Id: takfp.ooc,v 1.1 2005-06-08 18:03:26 igouy-guest Exp $

   The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   Contributed by Brent Fulgham
 *)

MODULE takfp;
IMPORT Shootout, Out;

VAR N: LONGINT;

PROCEDURE Tak (x,y,z: REAL) : REAL;
BEGIN
	IF (y >= x) THEN RETURN z; END;
	RETURN Tak(Tak(x-1,y,z), Tak(y-1,z,x), Tak(z-1,x,y));
END Tak;

BEGIN
	N := Shootout.Argi();
	Out.LongRealFix( Tak( N * 3.0, N * 2.0, N * 1.0),0, 1); Out.Ln;
END takfp.

 
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Stewart Greenhill
*)


MODULE wc;
IMPORT IO, IO:StdChannels, Out;

VAR
    insideWord: BOOLEAN;
    nc, nl, nw: LONGINT;
    in : IO.ByteChannel;
    buffer : ARRAY 4096 OF CHAR;
    i, count : LONGINT;

BEGIN
    nc := 0; nl := 0; nw := 0;
    insideWord := FALSE;
    in := StdChannels.stdin;
    REPEAT
      count := in.Read(buffer, 0, LEN(buffer) * SIZE(CHAR));
      FOR i := 0 TO count-1 DO
         INC(nc);
         CASE buffer[i] OF
            | 0AX :
               INC(nl);
               insideWord := FALSE;
            | 9X, 20X :
               insideWord := FALSE;
         ELSE
            IF ~insideWord THEN
               insideWord := TRUE;
               INC(nw);
            END;
         END;
      END;
    UNTIL count <= 0;

    Out.Int(nl,1); Out.Char(20X); Out.Int(nw,0); Out.Char(20X);
    Out.Int(nc,0); Out.Ln;
END wc.
(* The Great Computer Language Shootout
   http://shootout.alioth.debian.org

   contributed by Isaac Gouy (Oberon-2 novice)
*)


MODULE wordfreq;
IMPORT IO, IO:StdChannels, S:=SYSTEM, SimpleHash, Out;
		
CONST
   readSize = 4096;		
		
TYPE
   FixedBuffer = ARRAY readSize OF CHAR;
   FixedBufferProc = PROCEDURE (VAR b: FixedBuffer; VAR pos: LONGINT);   		
		
VAR
   buffer: FixedBuffer; 
   wordCounts: SimpleHash.Hashtable;   

   i, size: LONGINT; 
   item: SimpleHash.Item;
   items: SimpleHash.Table;


(* Separate special processing in procedure Process so we can use
   FillAndProcess in other Shootout tests *)
PROCEDURE FillAndProcess (VAR buffer: FixedBuffer; Process: FixedBufferProc);
VAR
   channel: IO.ByteChannel;
   partialFill: BOOLEAN;
   size, pos, count: LONGINT;
BEGIN
   channel := StdChannels.stdin;
   pos := 0;
   size := readSize;
   LOOP
      TRY
         count := channel.Read(buffer, pos, size * SIZE(CHAR)); 
         partialFill := count < size; 
         IF partialFill THEN size := pos + count; ELSE size := readSize; END;               
         pos := size;	

         Process(buffer,pos);

         IF partialFill THEN EXIT; END;  
         
         IF pos = size THEN 
            pos := 0;
            size := readSize;
         ELSE
            S.MOVE(S.ADR(buffer[pos]), S.ADR(buffer[0]), (size-pos)*SIZE(CHAR));
            pos := size - pos;
            size := readSize - pos;
         END;   
                      
      CATCH IO.Error:
         EXIT;
      END;               
   END;
END FillAndProcess;


PROCEDURE CountWords(VAR buffer: FixedBuffer; VAR pos: LONGINT);
CONST
   maxWordSize = 32;
TYPE
   String = POINTER TO ARRAY maxWordSize OF CHAR;
VAR
   i, j: LONGINT;
   key: SimpleHash.ItemKey;
   word: String;
   c: CHAR;     
BEGIN
   i := 0; j := 0; NEW(key, maxWordSize);
   WHILE j < pos DO
      c := buffer[j];
      CASE c OF
         | "A".."Z" : buffer[j] := CHR(ORD(c)+32); INC(j);      
         | "a".."z" : INC(j);
      ELSE
         IF j > i THEN                        
            buffer[j] := 0X;	
            word := S.VAL(String, S.ADR(buffer[i]));		    		
            COPY(word^, key^);         
         
            item := SimpleHash.AddKey(wordCounts, key);
            INC(item.value, 1);         
                 
            IF item.value = 1 THEN NEW(key, maxWordSize); END;		
         END;
         INC(j); i := j;	                          
      END;   
   END;   
   IF j > i THEN pos := i; END; (* indicate buffer position *)
END CountWords;   
   

PROCEDURE QuicksortItems (n: LONGINT; VAR a: ARRAY OF SimpleHash.Item);

   PROCEDURE Swap(i, j: LONGINT);
   VAR t: SimpleHash.Item;
   BEGIN
      t := a[i]; a[i] := a[j]; a[j] := t;
   END Swap;

   PROCEDURE Sort(l, r: LONGINT);
   VAR
      i, j: LONGINT;
      pivot: SimpleHash.Item;
   BEGIN
      i := l;
      j := r;
      pivot := a[(l+r) DIV 2];
      REPEAT          
         WHILE (a[i].value > pivot.value) OR
            ((a[i].value = pivot.value) &
	      (a[i].key^ > pivot.key^)) DO INC(i); END;
         WHILE (pivot.value > a[j].value) OR
            ((pivot.value = a[j].value) &
	      (pivot.key^ > a[j].key^)) DO DEC(j); END;   
   	
         IF i <= j THEN Swap(i, j); INC(i); DEC(j); END;
      UNTIL i > j;
      IF l < j THEN Sort(l, j); END;
      IF r > i THEN Sort(i, r); END;
   END Sort;
   
BEGIN
   Sort(0, n-1);
END QuicksortItems;


BEGIN  
   wordCounts := SimpleHash.New(4000);
   FillAndProcess(buffer, CountWords);    
      
   size := SimpleHash.Count(wordCounts);
   NEW(items, size);
   item := SimpleHash.First(wordCounts);
   i := 0;
   WHILE item # NIL DO
      items[i] := item;
      item := SimpleHash.Next(wordCounts);
      INC(i);
   END;

   QuicksortItems(size, items^);

   FOR i := 0 TO size - 1 DO
      Out.Int(items[i].value,7); Out.Char(' ');
      Out.String(items[i].key^); Out.Ln;
   END;
END wordfreq.
