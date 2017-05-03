# part 1: The substitution model
I ran into dead ends a few times as a result of my misunderstanding of the spec. 
For example I thought `f f 3` would implicitly be the same as `f (f 3)`. 
I.e application would work its way from right to left. 
I initially thought this because `let f = fun x -> x in f f 3` was given as an example in the project spec.
However, this is a special case since f is the identity function. `f f 3` Should return an error for any function f expecting an int. To do this correctly we actually want to evaluate `f (f 3)`. TLDR parenthesis are important. 

# part 2: The dynamic environment model
I did not have as much trouble with this section of the project. Although for a while
I was using the wrong environment when evaluating App. I was subsituting into the environment associated with the first element in the App, rather than the environment 
which was passed to eval_d with the App. This caused some annoying bugs. It is also
important to note that the dynamic model will return different results from the substitution for certain expressions.
This happens when when variables are defined in multiple places. In a dynamic environment the variables used within a function are decided when the function is applied. In the substitution model the function uses the variables which are defined when the functions are defined. 
For example, a function which will return different results with the two models: 
```
Subst:
<== let x = 1 in let f = fun y -> x + y in let x = 2 in f 0;;
==> 1

Dynamic:
<== let x = 1 in let f = fun y -> x + y in let x = 2 in f 0;;
==> 2
```
and a function which will return the same result in both models.

```
Subst:
<== let x = 1 in let x = 2 in let f = fun y -> y * x in f 5;;
==> 5
Dynamic:
<== let x = 1 in let x = 2 in let f = fun y -> y * x in f 5;;
==> 10
```
# part 3: Lexical environment model
The lexical environment model evaluates expressions within the context of the environment they are defined in. 
In this way it is like the substitution model but more flexible, because if we extened the interpreter to have persistent state, or in other words keep track of the environment then we could do the following.
```
<== let f = fun x -> x + x;;
<== f 5;;
==> 10
```
If I had more time I would have liked to implement this extension. 

hi  
