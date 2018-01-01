---

title: "Erlang: First element in a list that matches some condition"
date: "2012-10-05"
comments: true
categories: erlang
---

### The short answer:

``` erlang 
first(L, Condition, Default) ->
  case lists:dropwhile(fun(E) -> not Condition(E) end, L) of
    [] -> Default;
    [F | _] -> F
  end.

3 = first([0, 1, 2, 3, 4, 5], fun(E) -> E > 2 end, 0).
0 = first([0, 1, 2, 3, 4, 5], fun(E) -> E < 0 end, 0).
```

Thanks to 
[Odobenus Rosmarus](http://stackoverflow.com/users/1092783/odobenus-rosmarus),
who provided this solution to my 
[Stack Overflow question](http://stackoverflow.com/questions/12657202).

### The long answer:

I recently needed an Erlang function that looks through a list and returns
the first element matching some condition, without looking through the rest
of the list.  

It's relatively straightforward to use list comprehension to find all of the 
elements in a list that match some condition:

``` erlang
5>  L = [0, 1, 2, 3, 4, 5].
[0,1,2,3,4,5]
6> Matches = [ E || E <- L, E > 2].
[3,4,5]
```

And then we can get the first element using matching:

``` erlang
7> [First | _Rest] = Matches.
[3,4,5]
8> First.
3
```

Using this strategy, we have to evaluate the test condition (`M > 2`) for
*every* element in the list before we can extract the first one.  That's 
very inefficient --- what if the list was very long?  

Here's a solution I came up with that uses recursion:

``` erlang
first([E | Rest], Condition, Default) ->
  case Condition(E) of
    true -> E;
    false -> first(Rest, Condition, Default)
  end;
first([], _Cond, Default) -> Default.
```

This function returns a match immediately when it is found, it lets us
provide the condition as a function, and it returns a default argument
if no match is found.  We can verify that it stops evaluting by
having the `Condition` function tell us when it's called:

``` erlang
5> first([1, 2, 3, 4, 5], fun(E) -> io:format("Evaluating for ~p~n", [E]), E > 2 end, 0).    
Evaluating for 1
Evaluating for 2
Evaluating for 3
3
```

I posted this as a question on Stack Overflow and 
[Odobenus Rosmarus](http://stackoverflow.com/users/1092783/odobenus-rosmarus) 
suggested a more elegant solution.  I wrapped that solution into
a more general version:

``` erlang
first(L, Condition, Default) ->
  case lists:dropwhile(fun(E) -> not Condition(E) end, L) of
    [] -> Default;
    [F | _] -> F
  end.
```

This solution uses the 
[`lists:dropwhile/2`](http://erldocs.com/R15B/stdlib/lists.html?i=0&search=lists:drop#dropwhile/2)
function to stop evaluation when a match is found.  I wasn't sure that was what 
it would do, so I verified using the same technique above.

One thing I'm still not sure about is how these solutions compare in terms
of memory usage, especially for very large lists.
