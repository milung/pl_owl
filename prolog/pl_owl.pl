:- module(owl, [
    owl_assert_individual/4,    % +Subject:iri, +Class:iri, +Label:rdf_literal, +Graph
    owl_date_time/3,            % +TimeStamp:float, -DateTime:compound, +TimeZone:atom
    owl_format_date_time/2,     % +DateTime:compound, -Text:atom
    owl_object_atom/2,          % +Object:atom, -Atom:atom
    owl_reachable/3,            % +Subject, +Property, -Object
    owl_remove_individual/2     % +Individual:iri, +Graph
]).
%! <module> owl doing some cool things
%  Predicates for querieng RDF graphs using OWL Semantics

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(apply)).

:- reexport(library(semweb/rdfs) , [
    rdfs_individual_of/2 as owl_individual_of
]).
:- reexport(library(semweb/rdf11) , [
    rdf_has/3 as owl_has
]).

:- rdf_meta 
    owl_assert_individual(r,r,o,r),
    owl_has(r,r,o),    
    owl_individual_of(r,r),
    owl_object_atom(o,-),
    owl_reachable(r,r,o),
    owl_remove_individual(r, +).


%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! owl_assert_individual(+Subject:iri, +Class:iri, +Label:rdf_literal, +Graph) is det
%  asserts the named individual of `Class` with the `SubjectIri` IRI and
%  assigns it label `Label`. The individual is asserted into the graph `Graph`
owl_assert_individual(SubjectIri, Class, Label, Graph) :-
    rdf_assert(SubjectIri, rdf:type, owl:'NamedIndividual', Graph),
    rdf_assert(SubjectIri, rdf:type, Class, Graph),
    rdf_assert(SubjectIri, rdfs:label, Label, Graph).

%! owl_date_time(+TimeStamp:float, -DateTime:compound, +TimeZone:atom) is det
owl_date_time(TimeStamp, DateTime, TimeZone) :-
    TimeStamp == now,
    get_time(T),
    owl_date_time(T, DateTime, TimeZone),
    !.
 owl_date_time(TimeStamp, DateTime, TimeZone) :-
    number(TimeStamp),
    stamp_date_time(TimeStamp, DateTime0, TimeZone),
    DateTime0 =.. [_, YYYY, MM, DD, HH, Min, SSs, TZ | _],
    SS is floor(SSs),
    DateTime1 =.. [ date_time, YYYY, MM, DD, HH, Min, SS, TZ],
    rdf_lexical_form(DateTime1, DateTime).

%!  owl_format_date_time(+DateTime:compound, -Text:atom) is det
%   Unifies `Text` with the xsd representation of the `date_time/7` compound.
owl_format_date_time(date_time(Y,M,D,H,Min, S, Offset), Text) :-
    format_time(atom(Text), '%FT%T%:z', date(Y,M,D,H,Min, S, Offset, _, _)).

%! owl_object_atom(+Object:atom, -Atom:atom) is det
%  Unifies Atom with the atomic representation of the `Object`, which is either atomic representation of literal
%  or label of the resource represented by the `Object`.
owl_object_atom(Object, Atom) :-
    rdf_is_literal(Object),
    (        
        Object = ^^(Date,'http://www.w3.org/2001/XMLSchema#dateTime'),
        owl_format_date_time(Date, Value)
    ;
        Object = ^^(Value,_)
    ;
        Object = @(Value, _)
    ;   
        Value = ''
    ),
    atomic_list_concat([Value], Atom),

    !.
 owl_object_atom(Object, Atom):-
    rdf_is_iri(Object),
    rdfs_label(Object, Value),
    atomic_list_concat([Value], Atom),
    !.
 owl_object_atom(Object, Atom):-
    rdf_is_iri(Object),
    rdf_global_id(_:Atom, Object),
    !.
 owl_object_atom(Object, Object).

%! owl_reachable(+Subject, +Property, -Object) is nondet
%  owl_reachable(-Subject, +Property, +Object) is nondet
%  Succeeds if `Subject` is reachable to `Object` with respect to OWL semantics. 
%  It uses the semantics of rdf_reachable/3 but Resource is not considered to 
%  be reachable to itself unless:
%    *  the `Property` makes `Subject` or `Object˙ reacheble to itself by a  non- transistive 
%       closure, using `rdf_has/3` and the `Property` is not _irreflexive_; or
%    *  the `Property` us _reflexive_
% 
%  As such the predicate handles `subpropertyOf`, `inverseOf`, `transient`, `reflexive`, and `irreflexive` 
%  properties of the `Property`
owl_reachable(_, Property, _) :-
    var(Property),
    throw(instantiation_error('Property')).
 owl_reachable(Subject, _, Object) :-
    var(Subject), var(Object),
    throw(instantiation_error('Subject, Object')).
 owl_reachable(Subject, Property, Object) :- 
    copy_term(s(Subject, Property, Object), Call),
    owl_reachable_stack(Subject, Property, Object, [Call]).

%! owl_remove_individual(+Individual:iri, +Graph) is det
%  Removes all rdf entries from the `Graph` where `Individual` is either in the subject or object role. 
owl_remove_individual(Individual, Graph) :-
    rdf_retractall(Individual, _, _, Graph),
    rdf_retractall(_, _, Individual, Graph).
    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

non_circular(Call, Stack, [Call | Stack]) :-
    \+ memberchk(Call, Stack).

noncircular_reachable(S, P, O, Stack) :-
    copy_term(s(S,P,O), Call),
    (           
        memberchk(Call, Stack)
    ->  rdf_reachable(S,P,O) 
    ;   owl_reachable_stack(S,P,O, [ Call | Stack])
    ),
    S \== O.

owl_reachable_stack(Subject, Property, Object, _) :-
    rdf_reachable(Subject, Property, Object),
    (   Subject == Object
    ->  ( rdf_has(Subject, Property, Object)
        -> \+ rdf(Property, rdf:type, owl:'IrreflexiveProperty' )
        ;   rdf(Property, rdf:type, owl:'ReflexiveProperty' ) 
        )
    ;   true
    ).
 owl_reachable_stack(S, P, O, Stack) :-
    \+ var(S),    
    rdf(P, owl:propertyChainAxiom, Chain),
    rdf_list(Chain, PropertyChain),
    foldl(
        {Stack}/[Property, Subject, Object]
            >> noncircular_reachable(Subject, Property, Object, Stack),
        PropertyChain, S, O).
owl_reachable_stack(S, P, O, Stack) :-
    \+ var(O),
    rdf(P, owl:propertyChainAxiom, Chain),
    rdf_list(Chain, PropertyChain),
    reverse(PropertyChain, RevertedChain),
    foldl(
        {Stack}/[Property, Object, Subject] 
            >> noncircular_reachable(Subject, Property, Object, Stack), 
        RevertedChain, O, S).

