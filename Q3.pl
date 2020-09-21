:- use_module(library(csv)).
%read file functions
readPlainText(X, A) :-
  open(X, read, Stream),
  readWords(Stream,_, A),
  close(Stream).

readWords(InStream, _ , [W|B]) :-
  \+ at_end_of_stream(InStream),
  readWord(InStream, W),
  readWords(InStream, _, B).

readWords(_, _, []):-!.

readWord(InStream, W) :-
  get_code(InStream, Char),
  checkCharAndReadRest(Char, Chars, InStream),
  atom_codes(W, Chars).

checkCharAndReadRest(10, [], _) :- !.

checkCharAndReadRest(-1, [], _) :- !.

checkCharAndReadRest(end_of_file, [], _) :- !.

checkCharAndReadRest(Char, [Char | Chars] , InStream) :-
  get_code(InStream, NextChar),
  checkCharAndReadRest(NextChar, Chars, InStream).

% turn input into list
inputList([A|B], [L|C]):-
	%split A from a string into a list.
	split_string(A, ",", "", L),
	inputList(B, C) .

inputList(_, []):-!.

% split_string("a.b.c.d", ".", "", L).
% coop_e_3x3.csv   coop_s_3x3.csv
% coop_e_10x10.csv coop_s_10x10.csv
% findStableMatch("coop_e_3x3.csv", "coop_s_3x3.csv").

findStableMatch(EPF, SPF):-
	%read file and store it in string.
	readPlainText(EPF, EPL),
	readPlainText(SPF, SPL),
	%turn all elements inside the list into list so that we can work with the elements inside.
	inputList(EPL, EPL2),
	inputList(SPL, SPL2),
	stableMatching(EPL2, SPL2, M),
	length_1(SPL2, N),
	(N = 3 -> csv_write_file("matches_prolog_3x3.csv", M); csv_write_file("matches_prolog_10x10.csv", M)).

length_1([], 0):- !.
length_1([_|T], H) :- length_1(T, T1), H is T1 + 1.

%create match
match(E, S, [S|E]):- !.

%create student has no offer yet
student(S, [S|_]):-!.
%create employer with no offer yet
employer(E, [E|_]):-!.

%checkOffer
checkoffer(O,N):-
	%if N is 0, there are no offer; if N = 1, there has been an offer.
	listlength(O, N).

%create student list
studentL(_, []):-!.
studentL([A|B], [S|T]):-
	%get the name of the first student
	head(A, SN),
	student(SN, S),
	studentL(B, T).

%create employer list
employerL([], []):-!.
employerL([A|B], [E|T]):-
	%get the name of the first student
	head(A, EN),
	employer(EN, E),
	employerL(B, T).

head([A|_], A):-!.
tail([_|B], B):-!.

listlength([], 0 ).
listlength([_|Xs] , L ):- 
    listlength(Xs,N), 
    L is N+1. 


% EPL is the employer preference list, SPLs is student preference list.
% SPL = [["Olivia", "Thales", "Canada Post", "Cisco"], ["Jackson", "Thales", "Canada Post", "Cisco"], ["Sophia", "Cisco", "Thales", "Canada Post"]]
% EPL = [["Thales", "Olivia", "Jackson", "Sophia"], ["Canada Post", "Sophia", "Jackson", "Olivia"], ["Cisco", "Olivia", "Sophia", "Jackson"]] 

stableMatching(EPL, SPL, EOL):-
	% SL = [["Olivia"|_4646], ["Jackson"|_4658], ["Sophia"|_4670]]
	%studentL(SPL, SL),
	% EL = [["Thales"|_9300], ["Canada Post"|_9312], ["Cisco"|_9324]]
	employerL(EPL, EL),
	%get the employers to offer to students and return a employer offer list. EOL is a list that contain the final pair.
	checkoffer2(EL,EPL,SPL, EOL).

%check offer of employer and start offering to students.
%[A|B] is [["Thales"|_9300], ["Canada Post"|_9312], ["Cisco"|_9324]]; Ex: A is ["Thales"|_9300]
checkoffer2([], _, _, []):- !.

checkoffer2([A|B],EPL, SPL, [OF|OR]):-
	% tail of A is to see if employer A has offered anyone yet
	tail(A, O), checkoffer(O, N),
	%get the employer name (A1) and make the offer. Ex: OF = ["Thales", "Olivia"]
	head(A, A1), offer(N, EPL, SPL, A1, O, OF),
	%repeat the process for other employers Ex: Canada Post and Cisco will make offer for students.
	checkoffer2(B, EPL,SPL, OR).

%offer: case where the employer X already made and offer --> do not do anything.
offer(N, _ , _ , EN ,O, [EN|O]):- N = 1, !.

%offer: case where we have not found the name of the employer in the first list then check the next list.	
offer(N, [A|B], SPL, C, _, OA):-
	%if no offer from the employer X yet
	N = 0,
	%if the name of the employer not match then we look into the next list.
	head(A, EN), EN \= C,
	%then try again
	offer(N, B, SPL, C, _, OA).

% offer: case where we find the name of the employer
% N indicate whether A has made an offer to a student or not.
% C is the name of the employer.
% [A|B] is employer preference list.
% ***Warning***: Not done yet, need to fix problem that if 2 employers offer the same students. Only done the case where there are no collision.
% Whenever employer offer, have to check too see if any of the other employers has offer that student yet.

%need to put in SPL after [A|_] later.
offer(N, [A|_], _, C, _, OA):-
	% if the employer has not offer for anyone yet then look at the first person in the list and offer that student a job
	N = 0,
	%now we want to check the first name in A (the employer name) is the same as the employer that we are looking at.
	head(A, EN), EN = C,
	%get employer preference list and get the first student name. Ex: EP = ["Olivia", "Jackson", "Sophia"] --> FS = "Olivia"
	tail(A, EP), head(EP, FS),
	%checking point: check other employers, to see if any of them has made an offer to the same student or not.
	%match employer C with student FS to get list OA.
	match(C, FS, OA).
	






	

























































































