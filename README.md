# ProbalisticAlzheimer
## General Epic To-Do
* *Generation*
	- [x] Randomise Math Expression with only Numbers using Binary Tree and Polish Notation
    - [x] Randomise Math Expression with One Variable (e.g. x) using Binary Tree and Polish Notation
    - [x] Randomise Math Expression with Two Variables (e.g. x1 (predecessor) and x2(pred-predecessor)) and their iterator n using Binary Tree and Polish Notation
 to produce Recursive Expression
* *Auxiliary Functions*
    - [x] Counting Leaves (all numbers and variables)
    - [x] Counting Nodes (all operators (operations))
* *Interpretation*
    - [x] Interpretation of the first kind of Expression
    - [x] Interpretation of the second kind of Expression with input for x
    - [x] Interpretation of the third kind of Expression with input for Two Variables (e.g. x1 and x2) and their iterator n
* *Producing sequences*
    - [x] Using Math Expression with one Variable (e.g. x) to produce iterable sequences with length of input N
    - [x] Using Math Expression with Two Variables to produce iterable recursive sequence with length of input N 
* *Simple Rejection*
    - [x] for given sequence predict accuratly Exprerssion  with one variable using random
    - [x] for given sequence predict accuratly Recursive Expression with Two Variables using random
* *Genetic programming*
for given sequence predict accuratly Recursive Expression with Two Variables using Genetic Programming
    - [x] Mutation of Expression
        - [x] Changing operators and some of nums/vars but saving structure
        - [x] Completely change on of the branches in random way
    - [x] Crossbreeding two Expressions (Realization on only one of rules is permissible)
        - [x] Change random branch of parent1 to random branch of parent2
        - [ ] Change subtree of parent1 to subtree of parent2
    - [x] Parents Selection
        - [ ] distant relationship
        - [ ] closest relationship
        - [x] random selection
    - [x] Fitness Function (who is the best to be producer, or to be a candidate) - Mean Squared Error (MSE)
    - [x] Managing the population (love, death and KPI)
    - [ ] Benchmarks - time consuming of simple rejection and genetic one.

## Specific To-do
* trees_sequences_actual.rkt
    - [ ]  finish recursive sequences generation and prediction
* trees_sequence_genetic_generation.rkt - Potapov's solution + mutation
  - [x]  add crossbreeding (see rules in orangebook)
  - [x]  add parents selection rule
  - [x]  add population structure
  - [x]  add fitness function - MSE
  
