This document is for internal use, and is not an adequate presentation
for people who do not part of LIGO's core development team. It is not
an adequate exposition of the type system, type inference and type
checking of LIGO for researchers, users or anyone else.

----------------------------------------------------------------------

Overview
========

The type inference and checker of LIGO are split into several
components. These components interact through specific APIs, which
only expose as much information as is needed by a given
component. Components are largely unaware of most of the typing
constructs and of most of the other components. This makes it possible
to add features to the type system by only modifying a few components,
without significant changes to the rest of the system.

Data structures
===============

The following data structures are in use:

* `type_expression` which can be one of

    τ ::= ∀ ν, τ
       |  ν          (ν is a type variable bound by a ∀ quantifier)
       |  α          (α is a unification type variable)
       |  κ(β, …)    (κ is a type_constructor like list, map or option)
       |  R(ρ)       (ρ is a row and R is "record" or "variant")

    R ::= record | variant  (a type constructor whose argument is a row)

  Given a simple kinding system, it should be possible to merge R with
  κ, and apply both using type-level application τ(τ). The
  implementation of the AST currently does not distinguish between ν
  and α, instead the absence of unbound type variables is checked
  after the inference pass by `check_has_no_unification_vars` in
  src/passes/09-typing/08-typer-new/typer.ml

* rows (`p_row_args`) are mappings from labels (ℓ) to types

    ρ ::= { ℓ ↦ τ ; … }

* `type_constraint` which includes equality constraints and typeclass
  constraints:
   
    τ = τ

    indicates that once the unification type variables within are all
    instantiated, the two type_expression will have to represent
    equivalent types.

    (τ, … ) ∈ [ (τ, … ) ; … ]  

    indicates that the given tuple of types (once the unification type
    variables within are all instantiated) must belong to the type
    expressions accepted by the typeclass. The current implementation
    only allows a list of type expressions given in
    extension. Typeclasses which allow tuples of types following some
    recursive rule (e.g. a rule stating that pairs of Comparable types
    are also Comparable) should be allowed shortly, but without for
    type inference in the first release.

* `type_constraint_simpl`

  A smaller kernel to represent constraints. It is designed to
  minimize the numer of cases to be considered. In particular, it
  enables local reasoning, since simplified constraints are never
  deeply nested: a simplified constraint only refers to unification
  type variables (noted α, β, δ) and never directly contains a nested
  constraint. The following forms are allowed:

    SC_Constructor  α = type_constructor(β, …)                 e.g. α = map(β, δ)
    SC_Alias        α = β                                      e.g. α = β
    SC_Poly         α = forall β, δ                            e.g. α = forall β, (β → int → β * int)
                        (δ can be a complex type expression)
    SC_Typeclass    Typeclass (α, …) ∈ [(type, …); …]          e.g. (α, β, δ) ∈ [(int,   int,   int  );
                                                                                (float, float, float);
                                                                                (int,   float, float);
                                                                                (float, int,   float)]
    SC_Row          α = R ϱ

  The last case uses a `row_tag` (R) and a `type_variable_lmap` (ϱ)

     R ::= record | variant  is a type constructor whose argument is a row
     ϱ ::= { ℓ ↦ β ; … }     is a row of unification type variables


Overview of the components
==========================

The components of LIGO's typer are as follows:

* The `wrap` module annotates the AST nodes with unification type
  variables, and produces initial constraints, it corresponds to the
  typing rules of the language's semantics.

* The solver orchestrates the propagation of these constraints, until
  all unification variables have been assigned or generalized. The
  solver only delegates tasks to other components, and does not do
  interesting work by itself.

* The state of the solver contains in `all_constrants` a database
  storing all constraints known so far.

* The state of the solver also contains `aliases`, a Union-Find data
  structure which maps unification type variables to a representant
  for equivalence class formed by SC_Alias constraints (α = β).

* The database of constraints starts as a simple set of constraints,
  but indexes allow for fast answers to queries like "what are all the
  SC_Constructor constraints whose left-hand-side is the unification
  type variable γ?". These indexes are implemented by indexer
  plug-ins.

* An important index to the database is `assignments`, which indexes
  SC_Constructor constraints (of the form α = type_constructor(β,
  …)). It maps a unification type variable α to the concrete type
  assigned to it, once it is solved.

* The deduction of new constraints strarting from existing ones is
  done by heuristics. The heuristics are plug-ins, consisting of two
  subcomponents: a selector (which identifies adequate inputs) and a
  propagator (which propagates input constraints to produce new
  information).

* A selector is a query on the database of constraints and its
  indexes, written in a way to obtain the results incrementally as new
  constraints are added to the database.

* A propagator is a function which takes a single output of a seletor
  query, and uses the information within to deduce new
  constraints. Currently, propagators are in charge of throwing an
  error when they encounter an inconsistency. In the future, it would
  be desirable to move this responsibility in a separate component.

* Unification variables which remain unassigned after inference
  finished can be generalized.

* Finally, the substitution component replaces the unification
  variables in the AST according to the assignments deduced by the
  type inference.

Solver
------

The following diagram illustrates part of the workflow used to
propagate constraints. Most components in this diagram are iteratively
applied to collections of inputs, and produce collections of
outputs. For clarity, the details of collection management are
omitted.


           AST
            |
    ,-------+
    |       |
    |       v
    |   ___________
    |  |           |
    |  |  Wrapper  |
    |  |     &     |
    |  | scheduler |
    |  |___________|
    |       |
    |       |
    |       ,-----------------------------------------------------------------------------------------------,
    |       |                                  ,---------------------------------------------------------,  |
    |       |                                  |                                                         |  |
    |       |                                  |                                                         |  |
    |       v                                  v                                                         |  |
    |  Constraints                          Indexes  ,---------------- Aliases                           |  |
    |       |                                  |     |               (UnionFind)                         |  |
    |       |                                  |     |                    ^                              |  |
    |  _____|______                            |     |                    |                              |  |
    | |            |                           |     |                    |                              |  |
    | | Simplifier |                           |     |                    |                              |  |
    | |____________|                           |     |                    |                              |  |
    |       |                                  |     |                    |                              |  |
    |       |                                  |     |                    |                              |  |
    |       |                                  v     v                    |                              |  |
    |      / \                             ______________                 |                              |  |
    |     /   \                           |              |                |                              |  |
    |    /     \                          |  add_alias ----------> updated_aliases                       |  |
    |   / Alias \------------------------>|              |                                               |  |
    |   \   ?   /                         | MergeAliases |                                               |  |
    |    \     /                          |_______|______|                                               |  |
    |     \   /                                   |                                                      |  |
    |      \ /      _________________________     |                                                      |  |
    |       |      |         Indexers        |    |                                                      |  |
    |       |      |        _________        |    |                                                      |  |
    |       |      |       |         |       |    v                                                      |  |
    |       +-->---|---,-->| Indexer |--,----|--->♢                                                      |  |
    |       |      |   |   |_________|  |    |    |                                                      |  |
    |       v      |   |    _________   |    |    |                                                      |  |
    |       |      |   |   |         |  |    |    |                                                      |  |
    |       |      |   `-->| Indexer |--´    |    |                                                      |  |
    |       |      |   |   |_________|  |    |    |                                                      |  |
    |       C      |   |                |    |    |                                                      |  |
    |       o      |   !        …       !    |    |                                                      |  |
    |       n      |   :                :    |    I                                                      |  |
    |       s      |                         |    n                                                      |  |
    |       t      |_________________________|    d                                                      |  |
    |       r                                     e                                                      |  |
    |       a                                     x                                                      |  |
    |       i                                     e                                                      |  |
    |       n                                     s                                                      |  |
    |       t                                     |         __________________                           |  |
    |       s                                     |        |    Heuristic     |                          |  |
    |       |                                     |        |    __________    |                          |  |
    |       |                                     +--------|-->| Selector |---|---,                      |  |
    |       |                                     |        |   |          |   |   |                      |  |
    |       `-----------------------------------~ | ~------|-->|__________|   |   |                      |  |
    |       |                                     |        |                  |   |                      |  |
    |       |                                     |     ,--| - - - - - - - - -|---´                      |  |
    |       |                                     |     |  |                  |                          |  |
    |       |                                     |     |  |    __________    |                          |  |
    |       |                                     |     `--|-->|Propagator|---|---new-----------,        |  |
    |       |                                     |        |   |__________|---|---remove---,    |        |  |
    |       |                                     |        |__________________|            |    |        |  |
    |       |                                     |                                        |    |        |  |
    |       |                                 ____v___                                     |    |        |  |
    |       |                                |        |                                    |    |        |  |
    |       |                                | Remove |<-----------------------------------´    |        |  |
    |       |                                |________|                                         |        |  |
    |       |                                     |                                             |        |  |
    |       |                                     |                                             |        |  |
    |       |                                     |         __________________                  |        |  |
    |       |                                     |        |    Heuristic     |                 |        |  |
    |       |                                     |        |                  |                 |        |  |
    |       |                                     +------->|                  |                 |        |  |
    |       |                                     |        |        …         |---new-----------+        |  |
    |       `-----------------------------------~ | ~------|        …         |---remove---,    |        |  |
    |       |                                     |        |__________________|            |    |        |  |
    |       |                                     |                                        |    |        |  |
    |       |                                     |                        - - - ----------´    |        |  |
    |       !                                     !                                             !        |  |
    |       :                                     :                 …                           :        |  |
    |                                                                                                    |  |
    |                                             :                                             :        |  |
    |                                             ¡                                             ¡        |  |
    |                                             |                                             |        |  |
    |                                             v                                             v        |  |
    |                                      updated_indexes                               new_constraints |  |
    |                                             |                                             |        |  |
    |                                             /\                                            |        |  |
    |                                            /  \                                           |        |  |
    |                                           /    \                                          |        |  |
    |                                          /      \                                         |        |  |
    |                                         /finished\--------------------------------------~ | ~------´  |
    |                                         \   ?    /                                        |           |
    |                                          \      /                                         `-----------´
    |                                           \    /
    |                                            \  /
    |                                             \/
    |                                             |
    |                                       ______|_______ 
    |                                      |              |
    |                                      | Generalizers |
    |                                      |______________|
    |                                             |
    |                                             |
    |                                       .assignments
    |                                             |
    `------------------------------------------,  |
                                               |  |
                                               v  v
                                            ____________
                                           |            |
                                           | Substitute |
                                           |____________|



Wrap
----

The `wrap` module src/passes/09-typing/08-typer-new/wrap.ml annotates
the AST nodes with unification type variables, and produces zero or
more constraints for each node. For example, the AST node `f x` will
be annotated as `((f : α) (x : β)) : δ`, and a single type_constraint
`α = (β → δ)` will be returned.

Simplifier
----------

The simplifier breaks down the constraints from the higher-level
constraint language (`type_constraint`) to obtain constraints from the
solver's kernel constraint language (`type_constraint_simpl`).

Database
--------

The state of the solver contains in `all_constrants` a database
storing all constraints known so far.


Aliases
-------

An union-find data structure is used to represent equivalence classes
for unification type variawle which have been merged by constraints of
the form α = β.

Indexer plug-ins
----------------

Indexer plug-ins provide an initial state, e.g. a map from unification
type variables to the constraints referencing them, and update this
state when new constraints are added to the database.

An index is defined modulo the aliases of unification type
variables. The main solver loop informs the indexer of new aliases as
they are added to the system. The indexer can then e.g. merge the
corresponding keys/value pairs in a map, if its state uses type
variables as the keys of a map.

The main solver loop ensures that the indexer correctly updates ervey
part of its state which uses type variables, by keeping them opaque
during lookups (a function from type variables to their union-find
representant is supplied to the heuristic plug-ins, so that they may
perform these lookups) and during updates (a function to merge the
keys of maps and sets is supplied, the code is structured in a way to
ensure an OCaml type error is thrown if some of the maps or sets
within an indexer's state are not updated).

The following indexer plug-ins are implemented:
* `assignments` maps variables to their constructor constraints
* `grouped_by_variable` maps variables to the constraints referencing
  them
* each constraint is given a unique ID when it is added to the
  databse; by_constraint_identifier maps the unique ID back to the
  constraint.  
* `refined_typeclasses` maps the constraint identifiers of typeclass
  constraints to the refinement of the typeclass available so far. The
  tc_fundep heuristic, described below, refines typeclass constraints
  by removing impossible cases from the set of tuples of types allowed
  by the typeclass. It keeps the original typeclass constraint in the
  database (to keep a trace of the original constraint imposed by the
  source code or a library function), but updates a refined copy by
  removing the best refinement and replacing it with a smaller one
  every time.
* `refined_typeclasses_back` maps the constraint identifiers of refined
  typeclasses back to the constraint identifier of the original
  typeclass.
* `typeclasses_constraining` maps a type variable to the identifiers
  of typeclass constraints which constrain this type variable.


Assignments
-----------

In the spirit of keeping the code small by allowing for local
reasoning, `assignments` only use type constructors whose arguments
are all unification type variables.

assignments ::= { (α = type_constructor(β, …)) … }

This means that an assignment gives only the root of the type
expression assigned to that variable, and the subtrees are given by
further assignments, once they are known.

Heuristics
----------

Heuristics are defined using a selector (which finds sets of
constrains in the database on which the heuristic is applicable), and
a propagator (which is the part of the heuristic which actually
deduces new constraints).

For now, the following heuristics are implemented:

* `break_ctor` which finds two constraints of the form α = κ(β, …) and α
  = κ(δ, …) and deduces β = δ, …

* `specialize1` which is a rule for specializing polymorphic types. It
  finds two constraints of the form (α = ∀ β, δ ) and (α = κ(γ …)),
  and deduces (α = δ[β |-> fresh_β]) where [from |-> to] denotes
  substitution and fresh_β is a fresh unification variable.

* `tc_fundep` which restricts a typeclass constraint

    (α₁, α₂, …) ∈ { (τ₁₁, τ₁₂, …) , … }

  to the cases which remain given a second hypothesis of the form

    αᵢ = κ(β₁, β₂, …).

  It restricts the number of possible cases and replaces αᵢ in tuple
  of constrained variables so that the βⱼ are constrained instead.

  This rule can deduce a new assignment for other variables
  constrained by the typeclass if every possible type for that
  variable uses the same type constructor.


Selector
--------

A selector is a query on the database of constraints and its
indexes. It is written in a way to obtain the results incrementally as
new constraints are added to the database.
  
At any point in time the set obtained by successive calls to a
selector on a growing database should be the same as the set obtained
by calling the selector just once on the entire database.

The exception to this property is due to the non-monotonicity caused
by the removal of constraints. Removing a constraint (usually because
it has been broken down into one or more smaller, easier to process
constraints) can make it impossible to apply the strategy of a
heuristic that would have been applicable without the
removal.

Currently, only one heuristic requests removal of constraints, the
tc_fundep heuristic. It infers which tuples of types allowed by a
typeclass constraint are impossible (due to other constraints
conflicting with said tuples). It removes those from the typeclass
constraint, while simultaneously deducing which of the types
constrained by the typeclass have only one valid assignment (as noted
above, assignments only consider the root of the type, so "partial"
assignments like `α = list(β)` where `β` is left unknown are
possible). This indeed does break down the initial constraint into
smaller constraints which are easier to process. While it is possible
that another heuristic could in theory stop being applicable after
reducing the set of allowed type tuples, in practice the information
which gets deleted is uninteresting: it only relates some unification
variables with some candidate types which are in fact impossible.

Propagator
----------

Propagators do not have access to the database of constraints nor to
the indexers. A propagator represents the purely deductive part of a
heuristic, based on the corresponding selector's output. It returns a
list of new constraints, and a list of removal requests. Removals are
used to discard input constraints which are implied by the deduced
constraints. In a way, removal is similar to the following Coq tactic:

    cut …; [> solve […] | ].

It discard a goal in favour of simpler goals which can be used to
solve the former goal. A sketch of the `solve […]` part is included
with the removal requests using free-form human-readable text.

Generalizer
-----------

Unification variables which remain unassigned after inference finished
can be generalized. This would be a separate component which is not
implemented yet.

A generalizer heuristic can decide whether or not to generalize an
unassigned unification type variable, and where to place the ∀ in the
generalized type.

For example, a generalizer for bound expressions by let…in… could use
prenex quantifiers (all ∀ binders at the beginning of the type), e.g.

    ∀ a, ∀ b, (list a -> (a -> b) -> list b)`

Conversely, a generalizer for expressions assigned to the fields of a
record used as a module could place the ∀ inside each field, e.g.

    {
      id : (∀ a, a -> a) ;
      map : (∀ a, ∀ b, (list a -> (a -> b) -> list b)) ;
    }

In order to allow for multiple strategies, a generalizer consists of a
predicate deciding whether is is an adequate strategy for the current
situation (e.g. are we generalizing a let…in…-bound expression? Are we
generalizing the value assigned to a record field, for a record built
with the `module` keyword?), and of the generalization strategy
itself, which takes a type possibly containing unbound type variables,
and returns a (partially) generalized type.

Currently, polymorphism is only allowed for predefined standard
library functions, and there is no generalizer componnt.

Substitution
------------

The substitution transforms the AST in one pass. Every time a type
variable is encountered, it is looked up in the assignments, and
substituted if found. The assigned type can also contain type
variables, and substitution is performed recursively on it.
