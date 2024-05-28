% Author: Jan Kalenda
% Year: 2024
% Description: Turing machine simulator

% ==================== Input ====================
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs] ).


:- dynamic rule/4.
symbol(L) :- char_type(L, lower).
state(L) :- char_type(L, upper).
left('L').
right('R').
empty(' ').
final('F').

% ==================== Input Validation and Turing Machine initialization ====================

% creates a tape from a list of symbols
% create_tape(+Symbols, ?Tape)
create_tape([], []).
create_tape([H|T], [H|T1]) :- (empty(H); symbol(H)), create_tape(T, T1).
create_tape(_, _) :- write('Invalid tape'), nl, retractall(rule(_,_,_,_)), halt(2).

% creates rules from a list of lines
% create_rules(+Lines, +Tape)
create_rules([], _) :- write('No rules or tape provided'), nl, halt(3).
create_rules([H],['S'|Tape]) :- create_tape(H, Tape). % last line is the tape
create_rules([[H|T1]|T],T2) :- state(H), create_rules(T,T2), create_rule([H|T1]).

% creates a rule from a line and validates it
% create_rule(+Line)
create_rule([Stat1,Space,Symb1,Space,Stat2,Space,Symb2]) :- 
    state(Stat1),(empty(Symb1); symbol(Symb1)),state(Stat2),(left(Symb2); right(Symb2); empty(Symb2);symbol(Symb2)),
    empty(Space), assertz( rule( Stat1, Symb1, Stat2, Symb2) ).
create_rule([_|_]) :- write('Invalid rule definiton'), nl, retractall(rule(_,_,_,_)), halt(4).

% check if there is a start state
check_start :- rule('S', _, _, _), !.
check_start :- write('No start state defined'), nl, retractall(rule(_,_,_,_)), halt(10).

% check if there is a final state
check_final :- rule(_, _, 'F', _), !.
check_final :- write('No final state defined'), nl, retractall(rule(_,_,_,_)), halt(11).

% ==================== Turing machine simulation ====================

% apply_rule(+State, +NewState, +Tape, ?NewTape)
apply_rule(S, S1, [Stat], NEW) :- % only state, end of defined tape, going right
    state(Stat),
    once(rule(S, S1, NewStat, NewSym)), right(NewSym),
    non_deterministic(S, S1, NewStat, NewSym),
    NEW = [S1, NewStat].
apply_rule(S, S1, [Stat], NEW) :- % only state, end of defined tape
    state(Stat), 
    once(rule(S, S1, NewStat, NewSym)), (empty(NewSym); symbol(NewSym)),
    non_deterministic(S, S1, NewStat, NewSym),
    NEW = [NewStat, NewSym].
apply_rule(S, S1, [Stat|_], _) :- % invalid left, leftmost side of tape
    state(Stat), rule(S, S1, NewStat, NewSym), left(NewSym), non_deterministic(S, S1, NewStat, NewSym), fail.
apply_rule(S, S1, [Symb, Stat|T], NEW) :- % left final
    (empty(Symb); symbol(Symb)), state(Stat),
    once(rule(S, S1, 'F', NewSym)),
    left(NewSym),
    NEW = ['F', Symb|T].
apply_rule(S, S1, [Stat, Symb|T], NEW) :- % right final
    symbol(Symb), state(Stat),
    once(rule(S, S1, 'F', NewSym)),
    right(NewSym),
    NEW = [Symb, 'F'|T].
apply_rule(S, S1, [Stat, Symb|T], NEW) :- % final, greedy rule
    state(Stat), symbol(Symb),
    once(rule(S, S1, 'F', NewSym)), % must be symbol, no check needed
    NEW = ['F', NewSym|T].
apply_rule(S, S1, [Symb, Stat|T], NEW) :- % left
    (empty(Symb); symbol(Symb)), state(Stat),
    once(rule(S, S1, NewStat, NewSym)),
    left(NewSym),
    non_deterministic(S, S1, NewStat, NewSym),
    NEW = [NewStat, Symb|T].
apply_rule(S, S1, [Stat, Symb|T], NEW) :- % right
    symbol(Symb), state(Stat),
    once(rule(S, S1, NewStat, NewSym)),
    right(NewSym),
    non_deterministic(S, S1, NewStat, NewSym), 
    NEW = [Symb, NewStat|T].
apply_rule(S, S1, [Stat, Symb|T], NEW) :- % basic rule
    state(Stat), (empty(Symb); symbol(Symb)), % accept empty symbol as well
    once(rule(S, S1, NewStat, NewSym)),
    (empty(NewSym); symbol(NewSym)),
    non_deterministic(S, S1, NewStat, NewSym),
    NEW = [NewStat, NewSym|T].
apply_rule(S, S1, [H1|T], [H1|NEW]) :-
    (symbol(H1); empty(H1)), !,
    apply_rule(S, S1, T, NEW).


% runs the simulation
run_sim :- 
        once(init(R)),
        compute(R, [], Outcome),
        write_outcome(Outcome),
        retractall(rule(_,_,_,_)).
% run_sim :- write('No solution found'), nl, retractall(rule(_,_,_,_)), halt(1).


% ==================== Output ====================
% writes the outcome on the screen
% write_outcome(+Outcome)
write_outcome([]).
write_outcome([H|T]) :- print_list(H), nl, write_outcome(T).

% prints elements of a list on a single line
% print_list(+List)
print_list([]).
print_list([H|T]) :- write(H), print_list(T).

% ==================== Helpers ====================
% initialises the turing machine
% init(?Tape)
init(R) :- prompt(_, ''),
		read_lines(LL),
        create_rules(LL,R),
        % check_start,
        % check_final, 
        !.

% computes the outcome of the simulation
% compute(+Tape, +Tapes, ?Outcome)
compute([], [], []).
compute(Tape, Tapes, Outcome) :- 
    get_head(Tape, S, _),
    final(S),
    append(Tapes, [Tape], Outcome).
compute(Tape, Tapes, Outcome) :- 
    get_head(Tape, S, S1),
    apply_rule(S, S1, Tape, NEW), !, % this cut might even be considered harmful, but we are already deterministically choosing the rule
    append(Tapes, [Tape], NewOutcome),
    compute(NEW, NewOutcome, Outcome).

% get the turing machine head (state) and the following symbol
% get_head(+Tape, ?State, ?Symbol)
get_head([State], State, ' ') :- state(State).
get_head([State, Symbol| _], State, Symbol) :- state(State), symbol(Symbol).
get_head([_|T], State, Symbol) :- get_head(T, State, Symbol).

% non_deterministic(+State, +NewState, +NewState, +NewSymbol)
non_deterministic(S, S1, NewStat, NewSym) :- retract(rule(S, S1, NewStat, NewSym)), assertz(rule(S, S1, NewStat, NewSym)).

test(0).
test(X) :- test(Y), X is Y + 1.
