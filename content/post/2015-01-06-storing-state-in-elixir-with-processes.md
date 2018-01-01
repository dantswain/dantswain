---
layout: post
title: "Storing state in Elixir with processes"
date: "2015-01-06"
comments: true
published: true
categories:  [elixir]
---

I've been getting into [Elixir](http://elixir-lang.org) lately. I'm still
learning, and one thing that helps me learn new languages is to take a
problem I've solved in another language and solve it in the new language.
A few months ago I wrote about
[storing state in erlang with processes](/blog/2014/09/27/storing-state-in-erlang-with-processes/)
and I thought it would be an interesting exercise to convert that
discussion to Elixir.

I'll start out with a solution that's too simple to be useful.
Then, by adding functionality and refactoring,
show that a design pattern emerges that can be abstracted away from our
implementation details.  In this case, that pattern is a use-case for
[GenServer](http://elixir-lang.org/docs/v1.0/elixir/GenServer.html) or
[Agent](http://elixir-lang.org/docs/v1.0/elixir/Agent.html), and I'll
show how we can solve this problem using those tools.

## State

State is how we make programs do non-trivial things. A 
video game's states might include the position of the player, or a
count of the number of bad guys that have been defeated.  In most
programming languages, we could
count dead badguys by assigning the count to a variable and then updating
that value each time another bad guy is defeated.  Elixir, however, doesn't
allow us to keep around the value of a variable without actually carrying
around a reference to that variable, which would get very cumbersome.
So how do we keep track of state in Elixir?  The answer is by using
processes.

## State is easy in object-oriented languages

We'll use an example of a counter throughout the rest of this post.  By a counter,
I just mean a simple piece of code that's capable of keeping a count value and
provides an API to increment and/or access the value of the count.  This is a
very simple example of *state*; the state of the counter is the value of the
count.

Object-oriented languages make it really easy and natural to store state in
class instance variables.  For example, to implement a simple counter in
Ruby we could do something like this.

```ruby
# a simple object-oriented counter in Ruby
class Counter
  def initialize
    @count = 0
  end

  def click
    @count += 1
  end
end

c = Counter.new
c.click # => @count is now 1
c.click # => @count is now 2
```

`@count` is an instance variable of the `Counter` class.  When we create
a new instance of the `Counter` class by calling `Counter.new`, `@count`
gets initialized to a value of 0 in the constructor (`initialize`).   Each
time we call the `click` method, the value of `@count` is incremented by 1.
`@count` holds the *state* of the counter.  We can easily create multiple counters,
each with its own independent state.

```ruby
c1 = Counter.new
c2 = Counter.new
c1.click # => 1
c1.click # => 2
c2.click # => 1
```

In Elixir, we COULD do the following.

```elixir
defmodule Counter do
  def new do
    0
  end

  def click(counter) do
    counter += 1
  end
end

c = Counter.new       # c = 0
c = Counter.click(c)  # c = 1
c = Counter.click(c)  # c = 2
```

That is, we could just keep using the count value.  This has some obvious
limitations.  What if we want more than one process to be able to
increment the counter?  Each process would have to know the counter's value,
and keeping them all consistent would be very cumbersome.

## Recursion and state

So how do we keep state in Elixir?  We can get part of the way there using
recursion by passing state from the intial function call to the next and
so on.  Here is an example of a counter in Elixir that prints out the value
as it increments with each recursive function call.

{% include_code Recursive counter elixir_state/loop_counter.ex %}

Running this code from the iex shell will take over the shell
because the function `LoopCounter.go` never returns.  You can
either exit the shell with the usual Control-g q or kill
the job and connect to a new local shell with Control-g followed
by k (kill the job), s (start a new shell), c (connect to the new
shell).

```iex
iex(1)> c("loop_counter.ex")
[LoopCounter]
iex(2)> LoopCounter.go
n is 0
n is 1
n is 2
n is 3

User switch command
--> k
--> s
--> c
```

In `LoopCounter`, we use the input parameter `n` to the `go/1` function
to keep track of our state, and we increment it by recursively calling
the same function with `n + 1` as the parameter for the next call.  The
fact that we're able to increment the value means that we are keeping
track of the counter's state.  It's just not very useful at this point
because the recursive call loop completely takes over the current
Elixir process.

We could use [Kernel.spawn/1](http://elixir-lang.org/docs/v1.0/elixir/Kernel.html#spawn/1)
to break our counter loop away from the main process (our shell) and
[Process.exit/2](http://elixir-lang.org/docs/v1.0/elixir/Process.html#exit/2) to
stop it.

```iex
iex(1)> pid = spawn(&LoopCounter.go/0)
n is 0
#PID<0.55.0>
n is 1
n is 2
n is 3
n is 4
n is 5
n is 6
n is 7
iex(2)> Process.exit(pid, :exit)
true
```

We've got control of our shell back, but we still have no way to
control the counter or to get at its current value programmatically.

## Controlling and querying state with messages

We use messages to communicate between processes in Elixir.  The Elixir
[Getting Started](http://elixir-lang.org/getting-started/introduction.html)
guide's section on [processes](http://elixir-lang.org/getting-started/processes.html)
does a good job of explaining message handling in Elixir (and even has a section
that's similar to this post).

Here is an example that adds a click message to our counter.  We can use
the click message to increment the counter's value
and send the updated value back to the calling process
as a return message.

{% include_code Simple signal-based counter elixir_state/signal_counter.ex %}

We can create a counter using spawn and send messages to it. The counter
expects messages to be tuples: `{:click, from}`  where `from` should be
the pid of the caller so we can send back the state as a return message.

```iex
iex(1)> c("signal_counter.ex")
[SignalCounter]
iex(2)> pid = spawn(&SignalCounter.go/0)
#PID<0.61.0>
iex(3)> send(pid, {:click, self})
{:click, #PID<0.53.0>}
iex(4)> receive do x -> x end
1
iex(5)> send(pid, {:click, self})
{:click, #PID<0.53.0>}
iex(6)> receive do x -> x end
```

This is still a little messy because the caller (in this case, us via
the shell) has to know the message semantics and perform all of the
message receiving.  We can simplify this quite a bit by including the
message handling in the module and providing a simple API to the outside
world.  Here's a modified counter that does this, as well as adding
a few new messages.

{% include_code Signal-based counter with API layer elixir_state/counter.ex %}

This makes the API much more intuitive by adding methods like `Counter.click/0`.
Note that `Counter.new/0` returns the pid of the underlying process and that
we pass this pid to subsequent function calls.  That's because we're using
that process to hold the state of the counter!  This is a very common
Erlang and Elixir idiom.

```iex
iex(1)> c("counter.ex")
[Counter]
iex(2)> c = Counter.new
#PID<0.62.0>
iex(3)> Counter.click(c)
1
iex(4)> Counter.set(c, 42)
:ok
iex(5)> Counter.get(c)
42
iex(6)> c2 = Counter.new
#PID<0.67.0>
iex(7)> Counter.get(c2)
0
```

Note that the Counter API functions use `self/0` to get the PID of the _caller_.
 In Elixir, `self/0` 
always returns the pid of the current process (it's actually shorthand for
[Kernel.self/0](http://elixir-lang.org/docs/v1.0/elixir/Kernel.html#self/0)).
In the case of `Counter.click/0`,
`self/0` will be the pid of the process that called the function (the shell in
the example above), because `Counter.click/0` is just a simple function and will
therefore be executed by the process that calls it.

## Refactoring shared code and logical groups

There's a lot of repeated code in the counter example above. Also, the
implementataion of the counter logic is interspersed with lower-level message
handling.  You're a good programmer and this rubs you the wrong way.
So let's refactor the code to share some of the common bits and clean it up quite a lot.

{% include_code Refactored counter elixir_state/gen_counter.ex %}

This code has the same functionality as before, but is factored
to avoid repeated code and to hopefully group together the important bits of
our API and logic.  Our API consists of using the `make_call/2` helper
to send messages and receive the response, which effectively translates
our API into message semantics.  All of the actual counter logic boils down to
defining how the process should react to various messages,
which we implement in the `handle_msg/2` callbacks.

A pattern is starting to emerge here. We have an API that makes calls by
sending messages to an underlying process state loop, and that process
state loop handles those messages by delegating to message callbacks.  The
`loop/1` and `make_call/2` helpers are low-level and generic.  They also
don't handle a ton of edge cases: what happens if the process dies or takes
too long to respond to a message? What happens when we send an unexpected message?
What if we don't need to respond to a message (i.e., a broadcast)?  How does
the state loop handle shutdown?  What if we want our process not to respond
immediately but instead do some work and then respond when that work is done?

## Production-ready with GenServer

[GenServer](http://elixir-lang.org/docs/v1.0/elixir/GenServer.html) implements
the generic part of our counter process, and does it very well.  GenServer is an
Elixir wrapper around the Erlang
[gen_server](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)
module, which is part of the [OTP](http://www.erlang.org/doc/design_principles/des_princ.html)
library.

Rewriting our counter using GenServer is fairly straightforward.  Essentially,
we add `use GenServer` to import the GenServer behavior, use
[GenServer.start_link/3](http://elixir-lang.org/docs/v1.0/elixir/GenServer.html#start_link/3)
instead of `Kernel.spawn/1`,
[GenServer.call/3](http://elixir-lang.org/docs/v1.0/elixir/GenServer.html#call/3) instead of 
our `make_call/2` helper, and implement `init` and `handle_call` callbacks to define
our logic.

{% include_code GenServer-based counter elixir_state/counter_server.ex %}

By using GenServer, we get a ton of functionality for free.  For example,
`GenServer.call/3` takes an optional timeout argument which we can use to
guarantee that the calling process never hangs while waiting for the GenServer
to respond.  We haven't implemented it here, but GenServer can also handle
termination callbacks, broadcast messages, bare messages (via `Kernel.send/2`
as opposed to `GenServer.call/3` or `GenServer.cast/2`), process
registration, idle timeouts, and even hot code swapping.

## An Elixir bonus - using Agent

GenServer is essentially a direct adaptation of Erlang's gen_server.  The Elixir
developers realized that many times we use a fairly small subset of GenServer's
capabilities, more or less just to store some state and provide an
API for accessing that state.  The
[Agent](http://elixir-lang.org/docs/v1.0/elixir/Agent.html) module is designed
for just that use case.

Here is how we could rewrite our counter using the Agent module.

{% include_code Agent-based counter elixir_state/counter_agent.ex %}

This reduces our Counter to just four named functions.  Much of the work
is done by anonymous functions that we supply to the various
Agent functions.  For example, the initializer is simply `fn -> 0 end` to
set the initial value to zero, and the getter is `fn(n) -> n end` which
just returns the current state.

## The end

There it is.  We use processes to keep track of state in Elixir, and
messages between processes to access and control that state.  Elixir
is very similar to, and in fact 100% compatible with,
Erlang in this way.  Elixir provides the Agent module to handle many
of the most basic use cases, and the GenServer module to provide
more flexibility.

Solving problems by using processes and messaging can be hard to
wrap your head around at first.  However, once it clicks for you, many
problems become much easier to solve in this paradigm.
