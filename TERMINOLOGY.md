# Terminology

- Producers may produce blocks
- Validators may sign blocks
- Verifiers checks consensus
- Node currently verifier + compute state

## Things to do

- Upgreadability
- Shitton TPS
<<<<<<< Updated upstream
=======

## Simple one

Majority = 2f + 1

L = Majority - 1
R = f - 1
E = 1

L & R = level 0; round 0;

PA -> Propose A(l0 : r0) -> L & R
L & R -> Pre vote A -> L & R
L -> Pre commit A -> L & R
R -> Pre commit A -> R
R -> Commit A
E -> Pre commit Nil -> L & R
PB -> Propose B(l1 : r0) -> !L & R
R -> Pre vote B -> !L & R
L -> Pre commit timeout

L = level 0; round 1; block G;
R = level 1; round 0; block A;

PC -> Propose C(l0 : r1) -> L & !R
L & E -> Pre vote C -> L & !R
L & E -> Pre commit C -> L & !R
L -> Commit C
R -> Pre commit A -> L
E -> Quits

L = level 1; round 0; block C;
R = level 1; round 0; block A;

## Double vote one

L = 2f
R = f - 1
E = 1

PA -> Propose A(l1 : r0) :: G -> L
L -> Pre vote A -> L & R

- R -> Propose timeout
  R -> Pre vote Nil -> R
  PA -> Propose A -> R
  - R -> Pre vote timeout
    R -> Pre commit Nil -> L & R
    R -> Pre vote Nil -> L
    E -> Pre vote A -> L & R
    L -> Pre commit A -> L & R
    E -> Pre commit A -> R
    R -> Commit A
    PB -> Propose B(l2 : r0) :: A -> L & R
    R -> Pre vote B -> L & R
    R -> Stuck
    - L -> Pre commit timeout
      PC -> Propose C(l1 : r1) :: G -> L & R
      L & E -> Pre vote C -> L & R
      L & E -> Pre commit C -> L & R
      L -> Commit C
      ??? R -> Switches to C
      PD -> Propose D(l2 : r0) :: C -> L & R
      L -> Pre vote D -> L & R
      L -> Stuck
      E -> Pre commit A -> L

# Simple System

L = 2f
R = f - 1
E = 1

- PA -> Propose A(l1 : r0) :: G -> L
  L -> Vote A -> L & R

  - R -> Propose Timeout
    R -> Vote Nil -> L & R
    E -> Vote A -> R
    R -> Accept A
    PB -> Propose B(l2 : r0) :: A -> L & R
    R -> Vote B -> L & R
    - L -> Vote Timeout
      PC -> Propose C(l1 : r1) :: G -> L & R
      L -> Vote C -> L & R
      - E -> Stop attacking
        E -> Vote A -> L
        L -> Accept A
        L -> Vote B -> L & R
        L & R -> Commit A & Accept B

- PA -> Propose A(l1 : r0) :: G -> L
  L -> Vote A -> L & R

  - R -> Propose Timeout
    R -> Vote Nil -> L & R
    E -> Vote A -> R
    R -> Accept A
    PB -> Propose B(l2 : r0) :: A -> L & R
    R -> Vote B -> L & R
    - L -> Vote Timeout
      PC -> Propose C(l1 : r1) :: G -> L & R
      L & E -> Vote C -> L & R
      L -> Accept C
      R -> Switch C
      PD -> Propose D(l2 : r0) :: C -> L & R
      L & R -> Vote D -> L & R
      L & R -> Commit C & Accept D
      - E -> Stock attacking
        E -> Vote A -> L

L1 = f
L2 = f
R = f - 1
E = 1

- PA -> Propose A(l1 : r0) :: G -> L
  L -> Vote A -> L & R

  - R -> Propose Timeout
    R -> Vote Nil -> L & R
    E -> Vote A -> R
    R -> Accept A
    PB -> Propose B(l2 : r0) :: A -> L & R
    R -> Vote B -> L & R
    - L -> Vote Timeout
      PC -> Propose C(l1 : r1) :: G -> L & R
      L -> Vote C -> H
      E -> Vote A -> L1
      L1 -> Accept A
      L1 & E -> Vote B -> R
      R -> Commit A & Accept B
      H -> Vote C -> L
      E -> Vote C -> L
      L -> Accept C
      PD -> Propose D(l2 : r0) :: C -> L & R
      L & E -> Vote D -> L
      L -> Commit C & Accept D

_ -> Propose A(l1 : r0) :: G -> L & R
L & R -> Vote A -> L & R
L & R -> Commit A -> L & R
_ -> Propose B(l2 : r0) :: A -> L & R
L & R -> Vote B -> L & R
L & R -> Commit B -> L & R
>>>>>>> Stashed changes
