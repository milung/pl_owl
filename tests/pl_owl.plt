:- begin_tests(pl_owl).
:- use_module(source(pl_owl)).

% test(predicate, []) :-
%     pl_owl:predicate(a, a).

% test('context variable retrievable ', 
%     [
%         nondet,
%         true(Value == my_value)
%     ]) 
% :-
%     % prepare
%     pl_owl:prepare(Something),    
%     % execute
%     pl_owl:predicate(Something, Value).

% test('multiple variables retrievable', 
%     [
%         all(Value == [my_value, my_value_2])
%     ]) 
% :-
%     % prepare
%     pl_owl:prepare(Something),
%     % execute
%     pl_owl:predicate(Something, Value).

% test('context_remove', 
%     [
%         fail
%     ]) :-
%     % prepare
%     pl_owl:prepare(Something),
%     % execute
%     pl_owl:predicate(Something).

:- end_tests(pl_owl).