% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
   unix( argv([H|_]) ), number_atom(N,H),

   MIN_DEPTH is 4, set_limits(N, MIN_DEPTH, MAX_DEPTH, STRETCH_DEPTH),

   bottom_up_tree(0, STRETCH_DEPTH, ST),

   check_tree(ST, ITS),
   format('stretch tree of depth ~w\t check: ~w~n', [STRETCH_DEPTH, ITS]),

   bottom_up_tree(0, MAX_DEPTH, LLT),

   descend_trees(MIN_DEPTH, MIN_DEPTH, MAX_DEPTH),

   check_tree(LLT, ITL),
   format('long lived tree of depth ~w\t check: ~w~n', [MAX_DEPTH, ITL]).

% ------------------------------- %

set_limits(N, MinDepth, MaxDepth, StretchDepth) :-
   MinDepth1 is MinDepth + 2,
   (MinDepth1 > N -> MaxDepth is MinDepth1 ; MaxDepth is N),
   StretchDepth is MaxDepth + 1.

% ------------------------------- %

descend_trees(CurrentDepth, MinDepth, MaxDepth) :-
(
   CurrentDepth =< MaxDepth ->
    N is integer(2 ** (MaxDepth - CurrentDepth + MinDepth)), Iterations is 2 * N,
    sum_trees(N, CurrentDepth, 0, Sum),
    format('~w\t trees of depth ~w\t check: ~w~n', [Iterations, CurrentDepth, Sum]),
    NewDepth is CurrentDepth + 2, !, descend_trees(NewDepth, MinDepth, MaxDepth)
;
    true
).

% ------------- %

sum_trees(0, _, AccSum, AccSum) :- !.

sum_trees(N, CurrentDepth, AccSum, Sum) :-
   bottom_up_tree(N, CurrentDepth, TreeLeft),
   Nneg is -1 * N, bottom_up_tree(Nneg, CurrentDepth, TreeRight),
   check_tree(TreeLeft, ItemLeft), check_tree(TreeRight, ItemRight),
   AccSum1 is AccSum + ItemLeft + ItemRight,
   N1 is N - 1, !, sum_trees(N1, CurrentDepth, AccSum1, Sum).

% ------------------------------- %

make_tree(Item, Left, Right, tree(Item, Left, Right)).

% ------------- %

bottom_up_tree(Item, 0, tree(Item, nil, nil)) :- !.

bottom_up_tree(Item, Depth, Tree) :-
   ItemLeft is 2 * Item - 1, DepthLeft is Depth - 1, 
   bottom_up_tree(ItemLeft, DepthLeft, TreeLeft),
   ItemRight is 2 * Item, DepthRight is Depth - 1, 
   bottom_up_tree(ItemRight, DepthRight, TreeRight),
   make_tree(Item, TreeLeft, TreeRight, Tree).

% ------------- %

check_tree(tree(Item, nil, _), Item) :- !.
check_tree(tree(Item, _, nil), Item) :- !.

check_tree(tree(Item, Left, Right), ItemNew) :-
   check_tree(Left, ItemLeft),
   check_tree(Right, ItemRight),
   ItemNew is Item + ItemLeft - ItemRight.

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
%   yap -L fannkuch.plog -- 11
%
% Contributed by Anthony Borla
% Modified by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- use_module(library(lists)).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),

  init_fannkuch,

  f_permutations(N, MaxFlips),
  format('Pfannkuchen(~d) = ~d~n', [N, MaxFlips]).

% ------------------------------- %

init_fannkuch :- setvar(perm_N, 0), setvar(max_flips, 0).

% ------------------------------- %

f_permutations(N, MaxFlips) :-
  numlist(1, N, L),
  f_permutations_(L, N, 0),
  getvar(max_flips, MaxFlips).

% ------------- %

f_permutations_(L, N, I) :-
  (I < N ->
    (N =:= 1 ->
      !, processPerm(L)
    ;
      N1 is N - 1,
      f_permutations_(L, N1, 0),
      split_list(L, N, Lt, Ld),
      rotateLeft(Lt, LtRL), append(LtRL, Ld, La), Ii is I + 1,
      !, f_permutations_(La, N, Ii))
  ;
    !, true).

% ------------------------------- %

flips(L, Flips) :- flips_(L, 0, Flips).

flips_([1|_], Fla, Fla) :- !.

flips_([N|T], Fla, Flips) :-
	take_drop([N|T], N, Lt, Ld), append(Lt, Ld, La),
	Fla1 is Fla + 1, !, flips_(La, Fla1, Flips).

% ------------------------------- %

rotateLeft([H|T], RL) :- append(T, [H], RL).
rotateLeft([], []).

% ------------------------------- %

numlist(N, M, [N|Ls]) :- N < M, !, N1 is N + 1, numlist(N1, M, Ls).
numlist(M, M, [M]).

% ------------------------------- %

printPerm([L|Ls]) :- write(L), printPerm(Ls).
printPerm([]) :- nl.

% ------------------------------- %

processPerm(L) :-
  getvar(max_flips, MaxFlips), getvar(perm_N, PermN),
  flips(L, Flips),
  (Flips > MaxFlips ->
    setvar(max_flips, Flips)
  ;
    true),
  (PermN < 30 ->
    printPerm(L),
    PermN1 is PermN + 1,
    setvar(perm_N, PermN1)
  ;
    true).

% ------------------------------- %

split_list([L|Ls], N, [L|Hs], Ts) :- 
	N > 0, !, N1 is N - 1,
	split_list(Ls, N1, Hs, Ts). 

split_list(Ls, 0, [], Ls) :- !.

% ------------------------------- %

take_drop(L, N, Taken, Rest) :- take_drop_(L, N, 0, [], Taken, Rest).

%
% 'take' list returned in reverse order. If wanting it in order, use:
%
% take_drop_(L, N, N, Ta, Taken, L) :- !, reverse(Ta, Taken).
%

take_drop_(L, N, N, Ta, Ta, L) :- !.

take_drop_([H|T], N, Nc, Ta, Taken, Rest) :-
  Nc1 is Nc + 1, !, take_drop_(T, N, Nc1, [H|Ta], Taken, Rest).

% ------------------------------- %

getvar(Id, Value) :- nb_getval(Id, Value).
setvar(Id, Value) :- nb_setval(Id, Value).

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),

  init_fasta(ALU, IUB, HOMOSAPIENS, RAND0),

  N1 is N * 2,
  N2 is N * 3,
  N3 is N * 5,

  repeat_fasta('ONE', 'Homo sapiens alu', N1, ALU),

  make_cumulative(IUB, CVIUB),
  
  random_fasta('TWO', 'IUB ambiguity codes', N2, CVIUB, RAND0, RAND1),

  make_cumulative(HOMOSAPIENS, CVHOMOSAPIENS),

  random_fasta('THREE', 'Homo sapiens frequency', N3, CVHOMOSAPIENS, RAND1, RAND).

% ------------------------------- %

init_fasta(ALU, IUB, HOMOSAP, RAND) :-
  ALU = 'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA',
  IUB = [a:0.27, c:0.12, g:0.12, t:0.27, 
		'B':0.02, 'D':0.02, 'H':0.02, 'K':0.02, 'M':0.02, 
		'N':0.02, 'R':0.02, 'S':0.02, 'V':0.02, 'W':0.02, 'Y':0.02],
  HOMOSAP = [a:0.3029549426680, c:0.1979883004921, 
			g:0.1975473066391, t:0.3015094502008],
  init_gen_random(42, RAND).

% ------------------------------- %

repeat_fasta(Id, Desc, N, ALU) :-
  LineLength = 60, 
  atom_length(ALU, ALULength),
  write('>'), write(Id), tab(1), write(Desc), nl,
  repeat_fasta_(N, 0, LineLength, ALU, ALULength).

% ------------- %

repeat_fasta_(N, _, _, _, _) :- N =< 0, !.

repeat_fasta_(N, Q, L, ALU, ALULength) :-
  (N < L -> L1 = N ; L1 = L),
  (L1 + Q < ALULength ->
    sub_atom(ALU, Q, L1, Lineout), Q1 is L1 + Q,
    write(Lineout), nl
  ;
    Rest is ALULength - Q, sub_atom(ALU, Q, Rest, Prefix),
    atom_length(Prefix, PrefixLength), Q1 is L1 - PrefixLength,
    sub_atom(ALU, 0, Q1, Segment), 
	write(Prefix), write(Segment), nl),

  N1 is N - L1, !, repeat_fasta_(N1, Q1, L1, ALU, ALULength).

% ------------------------------- %

random_fasta(Id, Desc, N, CumTbl, RAND0, RAND) :-
  LineLength = 60,
  write('>'), write(Id), tab(1), write(Desc), nl,
  random_fasta_(N, LineLength, CumTbl, RAND0, RAND).

% ------------- %

random_fasta_(N, _, _, RAND, RAND) :- N =< 0, !.

random_fasta_(N, L, CumTbl, RAND0, RAND) :-
  (N < L -> L1 = N ; L1 = L),
  gen_line(L1, CumTbl, Codesout, RAND0, RAND1),
  atom_chars(Lineout, Codesout), write(Lineout), nl,
  N1 is N - L1, !, random_fasta_(N1, L1, CumTbl, RAND1, RAND).

% ------------- %

gen_line(0, _, [], RAND, RAND).
gen_line(N, CumTbl, K, RAND0, RAND) :-
  select_random(CumTbl, C, RAND0, RAND1), 
  char_code(C, C1), K = [C1|T1], N1 is N - 1, !,
  gen_line(N1, CumTbl, T1, RAND1, RAND).

% ------------------------------- %

make_cumulative(L, RL) :- make_cumulative_(L, RL, 0).

make_cumulative_([], [], _) :- !.
make_cumulative_([K:V|T], L, CV) :- 
	CV1 is CV + V, L = [K:CV1|T1], !, make_cumulative_(T, T1, CV1).

% ------------- %

select_random(L, RK, RAND0, RAND) :- 
	gen_random(1.0, R, RAND0, RAND), 
	select_random_(L, R, RK).

select_random_([], _, _) :- !.
select_random_([K:V|T], R, RK) :- 
	(R < V -> RK = K ; !, select_random_(T, R, RK)).

% ------------------------------- %

init_gen_random(Seed, [3877, 29573, 139968, Seed]).

% ------------- %

gen_random(UB, R, RAND0, RAND) :-
  RAND0 = [IA, IC, IM, LAST], 
  LAST1 is (LAST * IA + IC) mod IM,
  RAND = [IA, IC, IM, LAST1], 
  R is UB * LAST1 / IM.

% ------------------------------- %
% BUG FIX - sub_atom/5 errors out if Size = 0.

sub_atom(_,_,0,'') :- !.
sub_atom(A,Bef,Size,Aout) :- sub_atom(A,Bef,Size,_,Aout).

% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% contributed Isaac Gouy

:- write('hello world'), nl.
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% contributed by Anthony Borla
% modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(assoc)).

:- initialization(main).

main :-
   current_input(Cin),
   load_sequence(Cin, Seq),
   
   FragmentLengths = [1, 2],
   forall(member(E, FragmentLengths), (print_frequencies(Seq, E), nl)),

   Fragments = ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"],
   forall(member(E, Fragments), print_count(Seq, E)).

% ------------------------------- %

print_frequencies(Seq, KeyLen) :-
   generate_counts(Seq, KeyLen, CountTable),
   sum_counts_(CountTable, 0, SumCounts),
   make_freq_table_(CountTable, SumCounts, [], FTable),
   keysort(FTable, SFTable), reverse(SFTable, FreqTable),
   print_freq_table_(FreqTable).

% ------------- %

sum_counts_([_-C|T], Acc, Sum) :- Acc1 is Acc + C, !, sum_counts_(T, Acc1, Sum).
sum_counts_([], Acc, Acc).

% ------------- %

make_freq_table_([K-C|T], SumCounts, FTA, FreqTable) :-
   F is C / SumCounts * 100.0, append([F-K], FTA, FTA1),
   !, make_freq_table_(T, SumCounts, FTA1, FreqTable).
make_freq_table_([], _, FTA, FTA).

% ------------- %

print_freq_table_([F-K|T]) :-
   format('~w ~3f\n', [K, F]),
   !, print_freq_table_(T).
print_freq_table_([]).

% ------------------------------- %

print_count(Seq, Fragment) :-
   length(Fragment, FragLen), 
   generate_counts(Seq, FragLen, CountTable),
   atom_codes(FragKey, Fragment), 
   (
      select(FragKey-Count, CountTable, _)
   ;
      Count = 0
   ), !,
   format('~d\t~s\n', [Count, Fragment]).

% ------------- %

generate_counts(Seq, Length, CountTable) :-
   length(Seq, SeqLen), Last is SeqLen - Length + 1,
   make_count_table(Length, Last, Seq, CountTable).

% ------------------------------- %

make_count_table(Length, Last, Seq, CountTable) :-
   empty_assoc(A),
   mct_i_loop_(0, Length, Last, Seq, A, ACT),
   assoc_to_list(ACT, CountTable).

% ------------- %

mct_i_loop_(I, Length, Last, Seq, CTA, CountTable) :-
   I < Length, !,
   mct_j_loop_(Last, Length, Seq, CTA, CTA1),
   I1 is I + 1, !, 
   Seq = [_|Ss], Last1 is Last - 1,
   mct_i_loop_(I1, Length, Last1, Ss, CTA1, CountTable).
mct_i_loop_(Length, Length, _, _, CTA, CTA).


% ------------- %

mct_j_loop_(Last, Length, Seq, CTA, CountTable) :-
   Last > 0, !,
   sub_list_(Seq, Length, KeyString, Rest), atom_codes(Key, KeyString),
   (
      get_assoc(Key, CTA, Value) ->
      V1 is Value + 1, put_assoc(Key, CTA, V1, CTA1)
   ;
      put_assoc(Key, CTA, 1, CTA1)
   ),
   !, Last1 is Last - Length,
   mct_j_loop_(Last1, Length, Rest, CTA1, CountTable).
mct_j_loop_(Last, _, _, CTA, CTA) :- Last =< 0, !.

% ------------------------------- %

load_sequence(S, Seq) :- load_sequence_(S, fail, "", Seq).

% ------------- %

load_sequence_(S, Loading, Seq, RetSeq) :-
   catch(read_line_to_codes(S, L), _, fail), is_list(L), !,
   (
      Loading ->
      process_sequence(L, S, Seq, RetSeq)
   ;
      ignore_sequence(L, S, Seq, RetSeq)
   ).
load_sequence_(S, _, Seq, Seq).

% ------------- %

ignore_sequence([62,84,72,82,69,69|_], S, Seq, RetSeq) :- !, 
   load_sequence_(S, true, Seq, RetSeq).
ignore_sequence(_, S, Seq, RetSeq) :- !, 
   load_sequence_(S, fail, Seq, RetSeq).

process_sequence([62|_], _, Seq, Seq) :- !.
process_sequence([59|_], S, Seq, RetSeq) :- !, 
   load_sequence_(S, true, Seq, RetSeq).

process_sequence(L, S, Seq, RetSeq) :-
   to_upper(L, UL), 
   append(Seq, UL, NewSeq),
   !, load_sequence_(S, true, NewSeq, RetSeq).

% ------------------------------- %

to_upper(L, U) :- to_upper_(L, [], U).

% ------------- %

to_upper_([], UA, U) :- reverse(UA, U), !.

to_upper_([C|T], UA, U) :-
   is_lower(C), C1 is C - 32,
   !, to_upper_(T, [C1|UA], U).

to_upper_([C|T], UA, U) :-
   !, to_upper_(T, [C|UA], U).

% ------------- %

is_lower(C) :- C >= 97, C =< 122.

% ------------------------------- %

forall(Gen, Proc) :- findall(_,(Gen, Proc), _).

% ------------- %

sub_list_([S|Seq], L, [S|Ks], Rs) :- L > 0, !, 
   L1 is L - 1,
   sub_list_(Seq, L1, Ks, Rs).
sub_list_(Rs, 0, [], Rs).

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/

% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(Height,H), Width = Height,

  format('P4~N~d ~d~N',[Height, Width]),
  pointsY(Height, Width, 0, 0, 0, 0, 0).

% ------------------------------- %

pointsY(Height, Width, Y, X, 
	OUTFLAG0, 
	BYTEOUT0, 
	BITN0) :-
	Y1 is Y + 1, Height >= Y1, !, 
	pointsX(Height, Width, Y, 0, 
		OUTFLAG0, OUTFLAG, 
		BYTEOUT0, BYTEOUT,
		BITN0, BITN),
	pointsY(Height, Width, Y1, X,
		OUTFLAG,
		BYTEOUT,
		BITN).

pointsY(_, _, _, _, _, _, _) :- !.

% ------------- %

pointsX(Height, Width, Y, X, 
	OUTFLAG0, OUTFLAG, 
	BYTEOUT0, BYTEOUT, 
	BITN0, BITN) :-
  
	X1 is X + 1, Width >= X1, !,

	(mandel(Height, Width, Y, X, 50) -> LimitAdj = 0 ; LimitAdj = 1),

	BITN1 is BITN0 + 1, 
	(BITN1 == 8 -> OUTFLAG1 = 1 ; OUTFLAG1 = OUTFLAG0),

	BYTEOUT1 is BYTEOUT0 * 2 + LimitAdj, 
	(
		(Width == X1, BITN1 \== 8) -> 
		(BYTEOUT2 is BYTEOUT1 * integer(2 ** (8 - Width mod 8)), OUTFLAG2 = 1) 
	; 
		(BYTEOUT2 = BYTEOUT1, OUTFLAG2 = OUTFLAG1)
	),

	output(OUTFLAG2, OUTFLAG3, BYTEOUT2, BYTEOUT3, BITN1, BITN2),

	pointsX(Height, Width, Y, X1, 
		OUTFLAG3, OUTFLAG, 
		BYTEOUT3, BYTEOUT, 
		BITN2, BITN).

pointsX(_, _, _, _, OUTFLAG, OUTFLAG, BYTEOUT, BYTEOUT, BITN, BITN) :- !.

% ------------- %

mandel(Height, Width, Y, X, Repetitions) :-
	Cr is (2.0 * X / Width - 1.5), Ci is (2.0 * Y / Height - 1.0),
	mandel_(Cr, Ci, 0.0, 0.0, Repetitions, 0).

mandel_(_, _, Zr, Zi, Repetitions, Repetitions) :- !, 
	Limit is Zr * Zr + Zi * Zi, Limit > 4.0.

mandel_(Cr, Ci, Zr, Zi, Repetitions, N) :-
	Zr1 is Zr * Zr - Zi * Zi + Cr, 
	Zi1 is 2.0 * Zr * Zi + Ci, 
	Limit is Zr1 * Zr1 + Zi1 * Zi1,
	Limit =< 4.0, N1 is N + 1, !, 
	mandel_(Cr, Ci, Zr1, Zi1, Repetitions, N1).

mandel_(_, _, _, _, _, _) :- !.

% ------------- %

output(OUTFLAG0, OUTFLAG, BYTEOUT0, BYTEOUT, BITN0, BITN) :-
(	
	OUTFLAG0 =:= 1 -> 
	(
		put_byte(BYTEOUT0), 
		BITN = 0,
		BYTEOUT = 0,
		OUTFLAG = 0
	)
; 
	(
		BYTEOUT = BYTEOUT0,
		BITN = BITN0,
		OUTFLAG = OUTFLAG0
	)
).

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
   unix( argv([H|_]) ), number_atom(N,H),

   make_bodies(Bodies0),

   energy(Bodies0, EnergyStart),

   loop_advance(N, 0.01, Bodies0, Bodies),

   energy(Bodies, EnergyAfter),

   format('~9f~N~9f~N', [EnergyStart, EnergyAfter]).

% ------------------------------- %

energy(Bodies, Energy) :- energy_(Bodies, 0.0, Energy).

energy_([ _:B | Bs], Energy0, Energy) :-
    !, B = [_X, _Y, _Z, VX, VY, VZ, Mass],
    Energy1 is Energy0 + 0.5 * Mass * (VX * VX + VY * VY + VZ * VZ),   
   energy_diff_(B, Bs, Energy1, Energy2),
   energy_(Bs, Energy2, Energy).
   
energy_([], Energy, Energy).

energy_diff_(Planet, [_:B | Bs], Energy0, Energy) :-
   Planet = [X, Y, Z, _VX, _VY, _VZ, Mass],
   B = [XT, YT, ZT, _VXT, _VYT, _VZT, MassT],
   DX is X - XT, DY is Y - YT, DZ is Z - ZT,
   DISTANCE is sqrt(DX * DX + DY * DY + DZ * DZ),
   Energy1 is Energy0 - (Mass * MassT) / DISTANCE,
   energy_diff_(Planet, Bs, Energy1, Energy).

energy_diff_(_, [], Energy, Energy).

% ------------------------------- %

loop_advance(N, Dt, Bodies0, Bodies) :-
   N > 0, !,
   advance(Dt, Bodies0, Bodies1),
   N1 is N - 1,
   loop_advance(N1, Dt, Bodies1, Bodies).

loop_advance(_, _, Bodies, Bodies).

advance(Dt, Bodies0, Bodies) :-
   Bodies0 = [B0 | B0s], !,
   advance_(Dt, B0, B1, B0s, B1s),
   advance(Dt, B1s, Bs),
   B1 = E:[X, Y, Z, VX, VY, VZ, Mass],
   X1 is X + Dt * VX,
   Y1 is Y + Dt * VY,
   Z1 is Z + Dt * VZ,
   B = E:[X1, Y1, Z1, VX, VY, VZ, Mass],
   Bodies = [ B | Bs].
   
advance(_, Bodies, Bodies).

advance_(Dt, Planet0, Planet, Bodies0, Bodies) :-
   Bodies0 = [B0 | B0s], !,
   Planet0 = E:[X, Y, Z, VX, VY, VZ, Mass],
   B0 = ET:[XT, YT, ZT, VXT, VYT, VZT, MassT],
   
   DX is X - XT, DY is Y - YT, DZ is Z - ZT,
   DISTANCE is sqrt(DX * DX + DY * DY + DZ * DZ),
   Mag is Dt / (DISTANCE * DISTANCE * DISTANCE),

   VX1 is VX - DX * MassT * Mag, 
   VY1 is VY - DY * MassT * Mag, 
   VZ1 is VZ - DZ * MassT * Mag,
   VXT1 is VXT + DX * Mass * Mag, 
   VYT1 is VYT + DY * Mass * Mag, 
   VZT1 is VZT + DZ * Mass * Mag,
   
   Planet3 = E:[X, Y, Z, VX1, VY1, VZ1, Mass],
   advance_(Dt, Planet3, Planet, B0s, Bs),
   
   B = ET:[XT, YT, ZT, VXT1, VYT1, VZT1, MassT],
   Bodies = [B | Bs].

advance_(_, P, P, Bs, Bs).

% ------------------------------- %

make_bodies(Bodies) :-
   SOLAR_MASS = 3.9478417604357432000e+01,
   Bodies0 =
   [
      jupiter:[4.84143144246472090e+00, -1.16032004402742839e+00, 
         -1.03622044471123109e-01, 6.06326392995832020e-01, 
         2.811986844916260200e+00, -2.5218361659887636e-02, 
         3.7693674870389486e-02],
      saturn:[8.34336671824457987e+00, 4.12479856412430479e+00, 
         -4.03523417114321381e-01, -1.010774346178792400e+00, 
         1.825662371230411900e+00, 8.415761376584154e-03, 
         1.1286326131968767e-02],
      uranus:[1.28943695621391310e+01, -1.51111514016986312e+01, 
         -2.23307578892655734e-01, 1.082791006441535600e+00, 
         8.68713018169607890e-01, -1.0832637401363636e-02, 
         1.723724057059711e-03],
      neptune:[1.53796971148509165e+01, -2.59193146099879641e+01, 
         1.79258772950371181e-01, 9.79090732243897980e-01, 
         5.94698998647676060e-01, -3.4755955504078104e-02, 
         2.033686869924631e-03]
   ],
 
   Sun0 = sun:[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SOLAR_MASS],
   offset_momentum(Sun0, Sun, Bodies0, SOLAR_MASS),
   Bodies = [Sun | Bodies0].

% ------------- %

offset_momentum(Sun0, Sun, Bodies, SOLAR_MASS) :-
   offset_momentum_(Bodies, [0.0, 0.0, 0.0], [PX, PY, PZ]),
   Sun0 = E:[X, Y, Z, VX, VY, VZ, Mass], 
   VX1 is -(PX / SOLAR_MASS), 
   VY1 is -(PY / SOLAR_MASS), 
   VZ1 is -(PZ / SOLAR_MASS),
   Sun = E:[X, Y, Z, VX1, VY1, VZ1, Mass].

offset_momentum_([_:E|Bs], Pt0, Pt) :-
   E = [_X, _Y, _Z, VX, VY, VZ, Mass],
   Pt0 = [PX, PY, PZ], 
   PX1 is PX + VX * Mass, 
   PY1 is PY + VY * Mass, 
   PZ1 is PZ + VZ * Mass,
   offset_momentum_(Bs, [PX1, PY1, PZ1], Pt).

offset_momentum_([], Pt, Pt).

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% This is a slightly-modified version of the exising nsieve implementation
% differing only in the mechanism used to mimic array creation and
% access. This version [when compared to existing version]:
%
% * Makes only modest demands of the global stack, so should execute using
%   default values, at least up to a load of N = 9. However, its heap
%   memory demands make it prone to thrashing [existing version is more
%   stable as long as a sufficiently large stack size is specified]
%
% * Execution times are on par at up to N = 6, then diverge quite
%   dramatically [e.g. at N = 8 this version is roughly twice as fast as
%   existing version]
%
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
   unix( argv([H|_]) ), number_atom(N,H),

   N1 is 10000 << N,
   N2 is 10000 << (N - 1),
   N3 is 10000 << (N - 2),

   calcAndshowSieve(N1),
   calcAndshowSieve(N2),
   calcAndshowSieve(N3).

% ------------------------------- %

calcAndshowSieve(N) :-
   make_array(N, 1),
   nsieve(2, N, 0, R),
   format('Primes up to~t~w~21|~t~w~30|~n', [N, R]).

% ------------------------------- %

nsieve(ASize, ASize, R, R) :- !.
nsieve(N, ASize, A, R) :- 
   (
      is_slot(N) -> 
      clear_sieve(N, N, ASize), A1 is A + 1 
   ; 
      A1 is A
   ),
   N1 is N + 1, !, 
   nsieve(N1, ASize, A1, R).

% ------------- %

clear_sieve(N, M, ASize) :-
   N1 is N + M, clear_sieve_(N1, M, ASize).

% ------------- %

clear_sieve_(N, _, ASize) :- ASize < N, !.

clear_sieve_(N, M, ASize) :-
   clear_slot(N),
   N1 is N + M, !, clear_sieve_(N1, M, ASize).

% ------------------------------- %

make_array(N, V) :- fill_array(N, V).

% ------------- %

fill_array(0, _) :- !.
fill_array(N, V) :- bb_put(N, V), N1 is N - 1, !, fill_array(N1, V).

% ------------- %

set_slot(N) :- bb_put(N, 1).
clear_slot(N) :- bb_put(N, 0).
is_slot(N) :- bb_get(N, 1).

% ------------------------------- %

% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% This is a modified version of the orignal 'nsieve.swiprolog'
% submission. Whilst that particular implementation made quite heavy
% demands of the global stack [owing to the creation of a very large
% array], the current version:
%
% * Requires an array approximately 1/32 the size since each array slot
%   stores 32 encoded values [as opposed to a single value]
%
% * As expected, utilises bit twiddling for encoding / decoding values
%
% In short, while memory use is curbed, runtime suffers [a trading of
% speed for a saving in space as they say]. At a value of N = 9 runtime
% *should* be within the timeout period, but probably not by much
%
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),

  N1 is 10000 << N,
  N2 is 10000 << (N - 1),
  N3 is 10000 << (N - 2),

  calcAndshowSieve(N1),
  calcAndshowSieve(N2),
  calcAndshowSieve(N3).

% ------------------------------- %

calcAndshowSieve(N) :-
   make_array(ns, N, 0xffffffff, Array),
   nsieve(2, Array, N, 0, R),
   format('Primes up to~t~w~21|~t~w~30|~n', [N, R]).

% ------------------------------- %

nsieve(ASize, _, ASize, R, R) :- !.

nsieve(N, Array, ASize, A, R) :-
   (
      is_arg(N, Array) -> 
      clear_sieve(N, N, Array, ASize), A1 is A + 1 
   ; 
      A1 is A
   ),
   N1 is N + 1, !, 
   nsieve(N1, Array, ASize, A1, R).

% ------------- %

clear_sieve(N, M, Array, ASize) :-
   N1 is N + M, clear_sieve_(N1, M, Array, ASize).

% ------------- %

clear_sieve_(N, _, _, ASize) :- ASize < N, !.

clear_sieve_(N, M, Array, ASize) :-
   clear_arg(N, Array),
   N1 is N + M, !, clear_sieve_(N1, M, Array, ASize).

% ------------------------------- %

array_slots(N, Slots) :- Slots is ((N + 15) >> 4) + 1.

% ------------- %

make_array(Name, N, V, Array) :-
   array_slots(N, Slots),
   functor(Array, Name, Slots),
   fill_array(Slots, V, Array).

% ------------- %

fill_array(0, _, _) :- !.

fill_array(N, V, Array) :-
   setarg(N, Array, V), N1 is N - 1, !,
   fill_array(N1, V, Array).

% ------------- %

clear_arg(N, Array) :-
   Idx is (N >> 4) + 1, Value is (1 << (N /\ 15)), 
   arg(Idx, Array, OldValue),
   Complement is \ Value,
   NewValue is OldValue /\ Complement,
   setarg(Idx, Array, NewValue).

is_arg(N, Array) :-
   Idx is (N >> 4) + 1, Value is 1 << (N /\ 15), 
   arg(Idx, Array, OldValue),
   CurrentValue is OldValue /\ Value,
   CurrentValue =\= 0.

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Based on D language implementation by David Fladebo
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),
  compute_sums(N, SUMS), 
  print_sums(SUMS).

% ------------------------------- %

compute_sums(N, SUMS) :-
  SUMS0 = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
  compute_sums_(1.0, N, 1.0, SUMS0, SUMS).

% ------------- %

compute_sums_(D, N, _, SUMS, SUMS) :- D > N, !.

compute_sums_(D, N, ALT, SUMS0, SUMS) :-
  SUMS0 = [A1, A2, A3, A4, A5, A6, A7, A8, A9],

  D2 is D * D, D3 is D2 * D, DS is sin(D), DC is cos(D),

  A1N is A1 + (2 / 3.0) ** (D - 1.0),
  A2N is A2 + 1 / sqrt(D),
  A3N is A3 + 1 / (D * (D + 1)),
  A4N is A4 + 1 / (D3 * DS * DS),
  A5N is A5 + 1 / (D3 * DC * DC),
  A6N is A6 + 1 / D,
  A7N is A7 + 1 / (D2),
  A8N is A8 + ALT / D,
  A9N is A9 + ALT / (2 * D - 1),

  SUMS1 = [A1N, A2N, A3N, A4N, A5N, A6N, A7N, A8N, A9N],
  DN is D + 1.0, ALTN is -ALT, !, 
  compute_sums_(DN, N, ALTN, SUMS1, SUMS).

% ------------------------------- %

print_sums(SUMS) :-
  SUMS = [A1, A2, A3, A4, A5, A6, A7, A8, A9],

  format('~9f\t~w\n', [A1, '(2/3)^k']),
  format('~9f\t~w\n', [A2, 'k^-0.5']),
  format('~9f\t~w\n', [A3, '1/k(k+1)']),
  format('~9f\t~w\n', [A4, 'Flint Hills']),
  format('~9f\t~w\n', [A5, 'Cookson Hills']),
  format('~9f\t~w\n', [A6, 'Harmonic']),
  format('~9f\t~w\n', [A7, 'Riemann Zeta']),
  format('~9f\t~w\n', [A8, 'Alternating Harmonic']),
  format('~9f\t~w\n', [A9, 'Gregory']).

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),
  pidigits(N).

% ------------------------------- %

pidigits(N) :- pidigits_(1, [1, 0, 0, 1], N, 0, 0).

% ------------- %

pidigits_(K, Z, N, Row, Col) :-
  (N > 0 ->
    next(Z, Y), safe(Z, Y, IsSafe),
    (IsSafe ->
      prod(Z, Y, RL), N1 is N - 1,
      (Col =:= 10 ->
        Cf is 1, Rf is 10 + Row, format('\t:~w\n~w', [Rf, Y])
      ;
        Cf is 1 + Col, Rf is Row, format('~w', [Y])),
      !, pidigits_(K, RL, N1, Rf, Cf)
    ;
      cons(Z, K, RL), K1 is K + 1,
      !, pidigits_(K1, RL, N, Row, Col))
  ;
    NS is 10 - Col, tab(NS), RC is Row + Col, format('\t:~w\n', [RC])).

% ------------- %

next([Q, R, S, T], RV) :- RV is (3 * Q + R) // (3 * S + T).

safe([Q, R, S, T], N, RV) :-
  V is ((4 * Q + R) // (4 * S + T)), (V =:= N -> RV = true ; RV = fail).

comp([Q1, R1, S1, T1], [Q2, R2, S2, T2], [QO, RO, SO, TO]) :-
  QO is Q1 * Q2 + R1 * S2, RO is Q1 * R2 + R1 * T2,
  SO is S1 * Q2 + T1 * S2, TO is S1 * R2 + T1 * T2.

prod(Z, N, RL) :- A2 is -10 * N, comp([10, A2, 0, 1], Z, RL).

cons(Z, K, RL) :- A2 is 4 * K + 2, A4 is 2 * K + 1, comp(Z, [K, A2, 0, A4], RL).

% ------------------------------- %
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% SWI-Prolog contributed by Anthony Borla, Christoph Bauer
% converted to YAP by Isaac Gouy


:- initialization(main).

ack(0, N, Val) :- Val is N + 1, !.
ack(M, 0, Val) :- M1 is M - 1, ack(M1, 1, Val), !.
ack(M, N, Val) :-
  M1 is M - 1, N1 is N - 1,
  ack(M, N1, Val1), ack(M1, Val1, Val).


fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, Val) :-
       N > 1,
       N1 is N-1,
       N2 is N1-1,
       fib(N2, Val2),
       fib(N1, Val1),
       Val is Val1 + Val2.

fib_float(1.0, 1.0) :- !.
fib_float(0.0, 1.0) :- !.
fib_float(N, Val) :-
       N > 1,
       N1 is N-1,
       N2 is N1-1,
       fib_float(N2, Val2),
       fib_float(N1, Val1),
       Val is Val1 + Val2.


tak(X, Y, Z, R) :-
   Y < X,
   X1 is X-1,
   Y1 is Y-1,
   Z1 is Z-1,
   tak(X1, Y, Z, A),
   tak(Y1, Z, X, B),
   tak(Z1, X, Y, C),
   tak(A, B, C, R), !.

tak(_, _, Z, Z).


main :-
  unix( argv([H|_]) ), number_atom(A,H),
  B is A-1,
  C is 27.0 + A,
  write('Ack(3,'), write(A), write('): '),
  ack(3, A, Val),!,
  write(Val), nl,
  write('Fib('), write(C), write('): '), fib_float(C,V), format('~1f', V), nl, !,
  X is 3*B,
  Y is 2*B,
  Z is B,
  write('Tak('), write(X), write(','), write(Y), write(','), write(Z), write('): '),
  tak(X,Y,Z,R),
  write(R), nl,
  write('Fib(3): '), fib(3,V1), write(V1), nl,
  write('Tak(3.0,2.0,1.0): '),
  tak(3.0,2.0,1.0,FR),
  format('~1f', FR), nl.

% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by anon
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
   unix( argv([H|_]) ), number_atom(N,H),
   approximate(N, R),
   format("~9f~n", [R]).

% ------------------------------- %

approximate(N, R) :-
   make_array(app_u, N, 1.0, U), make_array(app_v, N, 0.0, V),

   approx_(10, N, U, V),
   
   vbv_loop(N, U, V, VbV), vv_loop(N, V, V, Vv),

   Vi is VbV / Vv, R is sqrt(Vi).
  
approx_(I, N, U, V) :-
   I > 0, 
    mulAtAv(N, U, V),
    mulAtAv(N, V, U),
   I1 is I - 1, approx_(I1, N, U, V). 
approx_(0, _, _, _).

% ------------- %

vbv_loop(N, U, V, VbV) :- vbv_loop_(N, U, V, 0.0, VbV).

vbv_loop_(0, _, _, VAcc, VAcc) :- !.

vbv_loop_(N, U, V, VAcc, VbV) :-
   arg(N, U, UValue), arg(N, V, VValue),
   VAcc1 is VAcc + UValue * VValue,
   N1 is N - 1, !, vbv_loop_(N1, U, V, VAcc1, VbV).

% ------------- %

vv_loop(N, U, V, Vv) :- vv_loop_(N, U, V, 0.0, Vv).

vv_loop_(0, _, _, VAcc, VAcc) :- !.

vv_loop_(N, U, V, VAcc, Vv) :-
   arg(N, V, VValue),
   VAcc1 is VAcc + VValue * VValue,
   N1 is N - 1, !, vv_loop_(N1, U, V, VAcc1, Vv).

% ------------------------------- %

a(I, J, AResult) :-
   Ia is I - 1.0, Ja is J - 1.0,
   AResult is 1.0 / ((Ia + Ja) * (Ia + Ja + 1.0) / 2.0 + Ia + 1.0).

% ------------------------------- %

mulAv(N, V, Av) :-  mulAv_(N, N, N, V, Av).

% ------------- %

mulAv_(0, _, _, _, _) :- !.

mulAv_(I, J, N, V, Av) :-
   setarg(I, Av, 0.0),
   mulAvJ_(I, J, N, V, Av),
   I1 is I - 1, !, mulAv_(I1, J, N, V, Av).

mulAvJ_(_, 0, _, _, _) :- !.

mulAvJ_(I, J, N, V, Av) :-
   arg(I, Av, AvValue), arg(J, V, VValue), a(I, J, AResult),
   AvNew is AvValue + AResult * VValue,
   setarg(I, Av, AvNew),
   J1 is J - 1, !, mulAvJ_(I, J1, N, V, Av).

% ------------------------------- %

mulAtV(N, V, Atv) :-  mulAtV_(N, N, N, V, Atv).

% ------------- %

mulAtV_(0, _, _, _, _) :- !.

mulAtV_(I, J, N, V, Atv) :-
   setarg(I, Atv, 0.0),
   mulAtVJ_(I, J, N, V, Atv),
   I1 is I - 1, !, mulAtV_(I1, J, N, V, Atv).

mulAtVJ_(_, 0, _, _, _) :- !.

mulAtVJ_(I, J, N, V, Atv) :-
   arg(I, Atv, AtvValue), arg(J, V, VValue), a(J, I, AResult),
   AtvNew is AtvValue + AResult * VValue,
   setarg(I, Atv, AtvNew),
   J1 is J - 1, !, mulAtVJ_(I, J1, N, V, Atv).

% ------------------------------- %

mulAtAv(N, V, AtAv) :-
   make_array(mul_u, N, 0.0, U),
   mulAv(N, V, U), mulAtV(N, U, AtAv).

% ------------------------------- %

make_array(Name, N, V, Array) :- functor(Array, Name, N), fill_array(N, V, Array).

% ------------- %

fill_array(0, _, _) :- !.

fill_array(N, V, Array) :-
   setarg(N, Array, V), N1 is N - 1, !,
   fill_array(N1, V, Array).

% ------------------------------- %
% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- use_module(library(readutil)).

:- initialization(main).

main :-
	current_input(Cin),
	current_output(Cout),

	sum_file(Cin, 0, N),
	
	write(Cout, N), nl(Cout).

% ------------------------------- %

sum_file(S, A, N) :- catch(read_integer(S, I), _, fail), A1 is A + I, !, sum_file(S, A1, N).
sum_file(_, A, A).

% ------------------------------- %

read_integer(S, N) :- read_line_to_codes(S, L), catch(number_codes(N, L), _, fail).
read_integer(_, 0).

