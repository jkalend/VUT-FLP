with open('out.txt', 'r') as file:
    data = file.read()

data = data.replace("""
:- meta_predicate'$syspreds':rule(:,-,?).

'$syspreds':rule(Head, Rule, Ref) :-
    '$rule'(Head, Rule0, Ref),
    conditional_rule(Rule0, Rule1),
    Rule=Rule1.

:- meta_predicate'$syspreds':rule(:,-).

'$syspreds':rule(Head, Rule) :-
    '$rule'(Head, Rule0),
    conditional_rule(Rule0, Rule1),
    Rule=Rule1.

:- dynamic rule/4.
""", "")

with open('out.txt', 'w') as file:
    file.write(data)
