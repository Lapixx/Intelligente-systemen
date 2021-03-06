
:- source.

:- style_check(all).

:- yap_flag(unknown,error).

:- yap_flag(open_expands_filename,false).

% redefines stuff in prolog module.

:- module(swi, []).

:- ensure_loaded(library(atts)).

:- use_module(library(charsio),[write_to_chars/2,read_from_chars/2]).

:- use_module(library(lists),[append/2,
			      append/3,
			      delete/3,
			      member/2,
			      memberchk/2,
			      min_list/2,
			      nth/3]).

:- use_module(library(system),
	      [datime/1,
	       mktime/2]).

:- use_module(library(arg),
	      [genarg/3]).

:- use_module(library(terms),
	      [subsumes/2,
	       term_variables/2,
	       term_variables/3,
	       term_hash/2,
	       unifiable/3,
	       variant/2]).

:- unhide('$system_library_directories'),
	unhide('$dir_separator').

% make sure we also use 
:- user:library_directory(X),
	atom(X),
	atom_concat([X,'/swi'],SwiDir),
	\+ user:library_directory(SwiDir),
	asserta(user:library_directory(SwiDir)),
	fail
	;
	true.

:- use_module(library(maplist)).

:- multifile swi_predicate_table/4.

swi_predicate_table(_,maplist(X,Y),maplist,maplist(X,Y)).
swi_predicate_table(_,maplist(X,Y,Z),maplist,maplist(X,Y,Z)).
swi_predicate_table(_,maplist(X,Y,Z,W),maplist,maplist(X,Y,Z,W)).
swi_predicate_table(_,is_list(X),lists,is_list(X)).
swi_predicate_table(_,min_list(X,Y),lists,min_list(X,Y)).
swi_predicate_table(_,nth(X,Y,Z),lists,nth(X,Y,Z)).
swi_predicate_table(_,delete(X,Y,Z),lists,delete(X,Y,Z)).
swi_predicate_table(_,nth1(X,Y,Z),lists,nth(X,Y,Z)).
swi_predicate_table(_,memberchk(X,Y),lists,memberchk(X,Y)).
swi_predicate_table(_,member(X,Y),lists,member(X,Y)).
swi_predicate_table(_,append(X,Y),lists,append(X,Y)).
swi_predicate_table(_,append(X,Y,Z),lists,append(X,Y,Z)).
swi_predicate_table(_,select(X,Y,Z),lists,select(X,Y,Z)).
swi_predicate_table(_,hash_term(X,Y),terms,term_hash(X,Y)).
swi_predicate_table(_,term_hash(X,Y),terms,term_hash(X,Y)).
swi_predicate_table(_,term_variables(X,Y),terms,term_variables(X,Y)).
swi_predicate_table(_,term_variables(X,Y,Z),terms,term_variables(X,Y,Z)).
swi_predicate_table(_,subsumes(X,Y),terms,subsumes(X,Y)).
swi_predicate_table(_,unifiable(X,Y,Z),terms,unifiable(X,Y,Z)).
swi_predicate_table(_,genarg(X,Y,Z),arg,genarg(X,Y,Z)).
swi_predicate_table(_,tmp_file(X,Y),system,tmp_file(X,Y)).

:- dynamic
   prolog:message/3.

:- multifile
   prolog:message/3.

:- multifile
   user:file_search_path/2.

:- dynamic
   user:file_search_path/2.

user:file_search_path(swi, Home) :-
        current_prolog_flag(home, Home).
user:file_search_path(foreign, swi(ArchLib)) :-
        current_prolog_flag(arch, Arch),
        atom_concat('lib/', Arch, ArchLib).
user:file_search_path(foreign, swi(lib)).

:- meta_predicate prolog:predsort(:,+,-).

prolog:plus(X, Y, Z) :-
       integer(X),
       integer(Y), !,
       Z is X + Y.
prolog:plus(X, Y, Z) :-
       integer(X),
       integer(Z), !,
       Y is Z - X.
prolog:plus(X, Y, Z) :-
       integer(Y),
       integer(Z), !,
       X is Z - Y.

%%	predsort(:Compare, +List, -Sorted) is det.
%
%	 Sorts similar to sort/2, but determines  the order of two terms
%	 by calling Compare(-Delta, +E1,  +E2).   This  call  must unify
%	 Delta with one of <, > or =. If built-in predicate compare/3 is
%	 used, the result is the same as sort/2. See also keysort/2.

prolog:predsort(P, L, R) :-
	length(L, N), 
	predsort(P, N, L, _, R1), !, 
	R = R1.

predsort(P, 2, [X1, X2|L], L, R) :- !, 
	call(P, Delta, X1, X2),
	sort2(Delta, X1, X2, R).
predsort(_, 1, [X|L], L, [X]) :- !.
predsort(_, 0, L, L, []) :- !.
predsort(P, N, L1, L3, R) :-
	N1 is N // 2, 
	plus(N1, N2, N), 
	predsort(P, N1, L1, L2, R1), 
	predsort(P, N2, L2, L3, R2), 
	predmerge(P, R1, R2, R).

sort2(<, X1, X2, [X1, X2]).
sort2(=, X1, _,  [X1]).
sort2(>, X1, X2, [X2, X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
	call(P, Delta, H1, H2),
	predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
	predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
	predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
	predmerge(P, T1, [H2|T2], R).


%
% maybe a good idea to eventually support this in YAP.
% but for now just ignore it.
%
:- meta_predicate prolog:volatile(:).

:- op(1150, fx, 'volatile').

prolog:volatile(P) :- var(P),
	throw(error(instantiation_error,volatile(P))).
prolog:volatile(M:P) :-
	do_volatile(P,M).
prolog:volatile((G1,G2)) :-
	prolog:volatile(G1),
	prolog:volatile(G2).
prolog:volatile(P) :-
	do_volatile(P,_).

prolog:load_foreign_library(P,Command) :-
	absolute_file_name(P,[file_type(executable),solutions(first),file_errors(fail)],Lib),
	load_foreign_files([Lib],[],Command).

prolog:load_foreign_library(P) :-
	prolog:load_foreign_library(P,install).

do_volatile(_,_).

:- use_module(library(lists)).

prolog:term_to_atom(Term,Atom) :-
	nonvar(Atom), !,
	atom_codes(Atom,S),
	read_from_chars(S,Term).
prolog:term_to_atom(Term,Atom) :-
	write_to_chars(Term,S),
	atom_codes(Atom,S).

prolog:concat_atom([A|List], Separator, New) :- var(List), !,
	atom_codes(Separator,[C]),
	atom_codes(New, NewChars),
	split_atom_by_chars(NewChars,C,L,L,A,List).
prolog:concat_atom(List, Separator, New) :-
	add_separator_to_list(List, Separator, NewList),
	atomic_concat(NewList, New).

prolog:concat_atom(List, New) :-
	atomic_concat(List, New).


split_atom_by_chars([],_,[],L,A,[]):-
	atom_codes(A,L).
split_atom_by_chars([C|NewChars],C,[],L,A,[NA|Atoms]) :- !,
	atom_codes(A,L),
	split_atom_by_chars(NewChars,C,NL,NL,NA,Atoms).
split_atom_by_chars([C1|NewChars],C,[C1|LF],LAtom,Atom,Atoms) :-
	split_atom_by_chars(NewChars,C,LF,LAtom,Atom,Atoms).

add_separator_to_list([], _, []).
add_separator_to_list([T], _, [T]) :- !.
add_separator_to_list([H|T], Separator, [H,Separator|NT]) :-
	add_separator_to_list(T, Separator, NT).


prolog:setenv(X,Y) :- unix(putenv(X,Y)).

prolog:prolog_to_os_filename(X,X).

prolog:is_absolute_file_name(X) :-
	absolute_file_name(X,X).

prolog:read_clause(X,Y) :-
	read_term(X,Y,[singetons(warning)]).

prolog:string(_) :- fail.

prolog:between(I,_,I).
prolog:between(I0,I,J) :- I0 < I, 
	I1 is I0+1,
	prolog:between(I1,I,J).

% SWI has a dynamic attribute scheme

prolog:get_attr(Var, Mod, Att) :-
	      AttTerm =.. [Mod,_,Att],
	      attributes:get_module_atts(Var, AttTerm).

prolog:put_attr(Var, Mod, Att) :-
	      AttTerm =.. [Mod,_,Att],
	      attributes:put_module_atts(Var, AttTerm).

prolog:del_attr(Var, Mod) :-
	      AttTerm =.. [Mod,_,_],
	      attributes:del_all_module_atts(Var, AttTerm).

prolog:get_attrs(AttVar, SWIAtts) :-
	get_all_swi_atts(AttVar,SWIAtts).

prolog:put_attrs(_, []).
prolog:put_attrs(V, Atts) :-
	cvt_to_swi_atts(Atts, YapAtts),
	attributes:put_att_term(V, YapAtts).

cvt_to_swi_atts([], _).
cvt_to_swi_atts(att(Mod,Attribute,Atts), ModAttribute) :-
	ModAttribute =.. [Mod, YapAtts, Attribute],
	cvt_to_swi_atts(Atts, YapAtts).

bindings_message(V) -->
       { cvt_bindings(V, Bindings) },
       prolog:message(query(_YesNo,Bindings)), !.

cvt_bindings([],[]).
cvt_bindings([[Name|Value]|L],[AName=Value|Bindings]) :-
	atom_codes(AName, Name),
	cvt_bindings(L,Bindings).

'$messages':prolog_message(_,L,L).

prolog:working_directory(OCWD,NCWD) :-
	getcwd(OCWD),
	(var(NCWD) -> true ; cd(NCWD)).

prolog:chdir(X) :- cd(X).

% Time is given as int, not as float.
prolog:get_time(Secs) :- datime(Datime),  mktime(Datime, Secs).

% Time is received as int, and converted to "..."
prolog:convert_time(X,Y) :- swi:ctime(X,Y).

:- hide(atom_concat).

prolog:atom_concat(A,B) :- atomic_concat(A,B).

prolog:atom_concat(A,B,C) :- atomic_concat(A,B,C).

:- hide(create_mutable).

:- hide(get_mutable).

:- hide(update_mutable).

prolog:make.

prolog:source_location(File,Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position, '$stream_position'(_,Line,_)).

% copied from SWI lists library.
prolog:intersection([], _, []) :- !.
prolog:intersection([X|T], L, Intersect) :-
	memberchk(X, L), !, 
	Intersect = [X|R], 
	prolog:intersection(T, L, R).
prolog:intersection([_|T], L, R) :-
	prolog:intersection(T, L, R).


:- op(700, xfx, '=@=').

prolog:(Term1 =@= Term2) :-
	variant(Term1, Term2), !.

%%	flatten(+List1, ?List2) is det.
%
%	Is true it List2 is a non nested version of List1.
%	
%	@deprecated	Ending up needing flatten/3 often indicates,
%			like append/3 for appending two lists, a bad
%			design.  Efficient code that generates lists
%			from generated small lists must use difference
%			lists, often possible through grammar rules for
%			optimal readability.

prolog:flatten(List, FlatList) :-
	flatten(List, [], FlatList0), !,
	FlatList = FlatList0.

flatten(Var, Tl, [Var|Tl]) :-
	var(Var), !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :- !,
	flatten(Hd, FlatHeadTail, List), 
	flatten(Tl, Tail, FlatHeadTail).
flatten(NonList, Tl, [NonList|Tl]).

