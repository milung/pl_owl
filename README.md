# OWL utility predicates over rdf library

Prolog module `pl_owl`. Predicates for querieng RDF graphs using OWL Semantics.

## USAGE

Use module `pl_owl` in your code.

Example:

```prolog
:- use_module(library(`pl_owl`)).

hallo :-
    owl_assert_individual(my_ns:my_individual, my_ns:'my-class', 'Some Individual', catalog_graph).
```

## Details

Few utilities predicate to create and remove owl individuals and resolve the owl relations with respect to the OWL semantics.

## Exported predicates

### `owl_assert_individual(+Subject:iri, +Class:iri, +Label:rdf_literal, +Graph) is det`

Asserts the named individual of `Class` with the `SubjectIri` IRI and
%  assigns it label `Label`. The individual is asserted into the graph `Graph`

### `owl_date_time(+TimeStamp:float, -DateTime:compound, +TimeZone:atom) is det`

Unifies the `Timestamp` with the `DateTime` compound coresponding to the lexical form of RDF. `TimeStamp` can be atom `now` to get compound for the current timestamp.

### `owl_format_date_time(+DateTime:compound, -Text:atom) is det`

Unifies `Text` with the xsd representation of the `date_time/7` compound.

### `owl_object_atom(+Object:atom, -Atom:atom) is det`

Unifies Atom with the atomic representation of the `Object`, which is either atomic representation of literal or a label of the resource represented by the `Object`.

### `owl_reachable(+Subject, +Property, -Object) is nondet`

Succeeds if `Subject` is reachable to `Object` with respect to OWL semantics. It uses the semantics of rdf_reachable/3 but Resource is not considered to be reachable to itself unless:

* the `Property` makes `Subject` or `Object` reacheble to itself by a  non- transistive closure, using `rdf_has/3` and the `Property` is not _irreflexive_; or
* the `Property` us _reflexive_

The honors handles `subpropertyOf`, `inverseOf`, `transient`, `reflexive`, and `irreflexive` properties of the `Property.

### `owl_remove_individual(+Individual:iri, +Graph) is det`

Removes all rdf entries from the `Graph` where `Individual` is either in the subject or object role.

## Testing

The script `run-tests.ps1` executes the tests

## Development

To debug the module, load the `debug.pl` file into prolog top.
