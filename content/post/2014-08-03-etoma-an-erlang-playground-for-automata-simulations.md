---

title: "etoma - An Erlang playground for automata simulations"
date: "2014-08-03"
comments: true
published: false
categories:
  - "erlang"
tags:
  - "websockets"
---

I've been playing around lately with a project in Erlang (and a little bit in
JavaScript) that I call [etoma](http://github.com/dantswain/etoma).  The name
etoma is a portmanteau of Erlang and automata, which suggests a far grander
scheme than the one that I actually envisioned when creating this project.
I'm not that great at naming things.

The basic idea of etoma is to provide an Erlang playground (calling it a
framework would imply more formality than I'm really aiming for) for exploring
2D planar motion rules (i.e., the automata part).  Etoma is still very much
a work in progress, but I've finally got (I think) enough to write up something
mildly interesting here.

etoma was intended from the get-go to be a somewhat academic exercise.  That is,
etoma is probably never going to be all that useful for anything other than playing
around with but I think it has the right combination of pedagogy and simplicity
to help someone understand some interesting concepts.  The concepts I was going
for are

  1. Using processes in Erlang to keep track of state.
  2. Using functions to represent behavior.

I'll write a little bit about both of those here and give a little example of how
etoma works.  If you have a reasonably new installation of Erlang, you should
be able to clone the repository and follow along.

## Using processes to keep state

Erlang derives a lot of its power from
[single assignment](http://learnyousomeerlang.com/starting-out-for-real#invariable-variables).
Once a value is assigned to a variable, no other value
may be assigned to the same variable.  This is a huge benefit in a language
designed for concurrency, because it reduces the possibility for function
calls to have <a href="http://en.wikipedia.org/wiki/Side_effect_(computer_science)">
side effects</a>.

Single assignment makes it impossible to store program state in the value of
a variable in the sense that we stuff a value into a block of memory and
update that value when we need to change the state of the program.  In
object-oriented programming, we often do this using class instances.
For example, a simple counter class in Ruby might look something like this:

```ruby
class Counter
  def initialize
    @count = 0
  end

  def click
    puts @count
    @count += 1
  end
end

c = Counter.new
c.click # => print 0, @count is now 1
c.click # => print 1, @count is now 2
```

Don't worry if you're unfamiliar with Ruby. `@count` is an instance variable that
gets initialized to a value of 0 in the class constructor (`initialize`).   Each
time we call the `click` method, the value of `@count` is printed out and then
incremented.  `@count` is a variable that holds *state*.

How do we accomplish this in Erlang?  We can't have a variable that gets
incremented on each successive function call, because that would require
multiple assignments to the same variable.  The answer is that in Erlang,
we use *processes* to store state.  Here's an example:

```erlang
-module(counter).

-export([new/0]).

new() ->
  count(0).

count(C) ->
  receive
    click ->
      io:format("~p~n", [C]),
      count(C + 1);
    _ ->
      ok
  end.
```

We can use this counter like so:

```
1> c(counter).
{ok,counter}
2> C = spawn(fun counter:new/0).
<0.39.0>
3> C ! click.
0
click
4> C ! click.
1
click
5> C ! click.
2
click
```

We use the [spawn/1](http://www.erlang.org/doc/man/erlang.html#spawn-1) function
to create a process and assign its PID to the variable C.  When `counter:new/0` is
callsed, it calls `count(0)` so that the value of `C` in `count/1` is 0.  The spawned
process then listens for a `click` message and prints out the value of `C` before
calling itself again with `C + 1` as an argument.  The `click` we see in the output
is the return value of the `!` operator.
