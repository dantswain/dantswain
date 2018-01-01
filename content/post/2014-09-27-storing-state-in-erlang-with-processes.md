---

title: "Storing state in Erlang with processes"
date: "2014-09-27"
comments: true
categories:  [erlang]
---

State is how we make programs do non-trivial things.  If we're writing a
video game, one of its states might be a count of the number of bad guys
that have been defeated.  In most programming languages, we would
accomplish this by assigning the count to a variable and then updating
the value each time a new bad guy is defeated.  Erlang, however, doesn't
allow us to change the value of a variable once it has been
set (single assignment).  So how do we keep track of state in Erlang
if we're not able to change the value of a variable?  The answer is by
using processes.

## Single Assignment

Erlang derives a lot of its power from
[single assignment](http://learnyousomeerlang.com/starting-out-for-real#invariable-variables).
Single assignment means that once a value is assigned to a variable, no other value
may be assigned to the same variable.  You'll see something like the following in
most tutorials on Erlang.

```erlang
1> X = 1.
1
2> X = 2.
** exception error: no match of right hand side value 2
```

In the first step, Erlang tries to
[pattern match](http://learnyousomeerlang.com/syntax-in-functions) `X` with
the value `1`.  Because `X` is unbound, it gets bound to the value `1`.
From now on, when `X` is the left-hand side of the `=` pattern matching operator,
normal pattern matching will happen.  That's why we get an error in the second
step - Erlang is trying to match `X`, which has a value of `1`, with the value `2`
therefore causing a `no match` error.  We can still match `X` as long as there is a
corresponding `1` on the right-hand side, as in the following.

```erlang
3> X = 1.
1
4> {X, 2} = {1, 2}.
{1,2}
```

Single assignment is a huge benefit in Erlang because it reduces the possibility for function
calls to have <a href="http://en.wikipedia.org/wiki/Side_effect_(computer_science)">
side effects</a>.  Side effects can make writing highly concurrent code very difficult.

## State

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
each with their own independent state.

```ruby
c1 = Counter.new
c2 = Counter.new
c1.click # => 1
c1.click # => 2
c2.click # => 1
```

## Recursion and state

So how do we keep state in Erlang?  We can get part of the way there using
recursion by passing state from the intial function call to the next and
so on.  Here is an example of a counter in erlang that prints out the value
as it increments with each recursive function call.

```erlang
%%% loop_counter.erl
% a recursive loop counter
%  takes over the current process, and no access to the value
-module(loop_counter).
-export([go/0]).

go() ->
  go(0).

go(N) ->
  io:format("N is ~p~n", [N]),
  % just to make sure this doesn't blow up our terminal
  timer:sleep(1000),
  go(N + 1).

%%% Erlang shell
1> c(loop_counter).
{ok,loop_counter}
2> loop_counter:go().
N is 0
N is 1
N is 2
N is 3

(hit Ctrl-c)
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
```

In this example, we use the input parameter `N` to the `go/1` function
to keep track of our state, and we increment it by recursively calling
the same function with `N + 1` as the parameter for the next call.  The
fact that we're able to increment the value means that we are keeping
track of the counter's state.  It's just not very useful at this point
because the recursive call loop completely takes over the current
erlang process.

We can use [erlang:spawn/1](http://www.erlang.org/doc/man/erlang.html#spawn-1)
to break our counter loop away from the main process (our shell).

```erlang
1> c(loop_counter).
{ok,loop_counter}
2> Pid = spawn(fun loop_counter:go/0).
N is 0
<0.39.0>
N is 1
N is 2
N is 3
N is 4
3> q().
ok
```

We've got control of our shell back, but now we have no way to
control the counter or to get at its current value programmatically.

## Controlling and querying state with messages

In the previous section we created a counter that increments its own value
by calling itself recursively, but we had no way to control the counter
or access its state from any external code.  We can use *messages* to do this.
Here is an example that adds a click message to our counter.  We can use
the click message to increment the counter's value
and send the updated value back to the calling process
as a return message.

```erlang
%%% counter.erl
% spawns a counter loop that listens for a click message
-module(counter).
-export([new/0, click/1]).

% spawn a counter, returning the pid
new() ->
  spawn(fun() -> loop(0) end).

% recursive loop with receive block
loop(N) ->
  receive
    {click, From} ->
      From ! N + 1,
      loop(N + 1)
  end.

% API function to increment a counter given its pid
click(Pid) ->
  Pid ! {click, self()},
  receive V -> V end.

%%% in Erlang shell
1> c(counter).
{ok,counter}
2> C = counter:new().
<0.39.0>
3> counter:click(C).
1
4> counter:click(C).
2
5> counter:click(C).
3
```

When we call `counter:new/0`, the function spawns a new recursive loop with a
zero initial counter value and returns the pid of the spawned
process.  The loop function immediately enters a `receive` block, where it waits
indefinitely for messages from any process.  We've set it up to
only listen for one message: `{click, From}`, with the expectation
that `From` is the pid of the calling process.

Here is an example of creating a counter process from the terminal and then
directly sending a message to it and waiting to receive a message back.

```erlang
1> c(counter).
{ok,counter}
2> C = counter:new().
<0.39.0>
3> C ! {click, self()}.
{click,<0.32.0>}
4> receive V -> V end.
1
```

This is a little cumbersome.  We don't want to have to write the
message call and the receive block each time we want to increment the counter.
That would also be unsafe because it leaves a lot of room for error.  Thus, we
introduce the API method `counter:click/1` (see the code above) that takes a counter pid and
knows the correct way to send and receive a click message.

Note that we use `self/0` in the `counter:click/1` call to automatically get
the pid of the *calling* process.  This works because `counter:click/1` is being
evaluated *by* the calling process.  If we had a call to `self/0` in the
`counter:loop/1` function, it would return the value of the spawned process
that is evaluating the loop.

Using this paradigm, it's easy to create and control multiple counters by spawning
multiple processes.  Moreover, the API hides most of the underlying implementation
details and we can work with the counter instances without having to know that
their value is actually a process identifier.

```erlang
1> c(counter).
{ok,counter}
2> C1 = counter:new().
<0.39.0>
3> C2 = counter:new().
<0.41.0>
4> counter:click(C1).
1
5> counter:click(C1).
2
6> counter:click(C2).
1
```

We could also modify the loop function to listen for a `set` message
to allow us to set the value of the counter manually.

```erlang
% adding a message handler to set the value of the counter
loop(N) ->
  receive
    {click, From} ->
      From ! N + 1,
      loop(N + 1);
    {set, Value, From} ->
      From ! ok,
      loop(Value)
  end.

% API function to set the value of a counter, given its pid
set(Pid, Value) ->
  Pid ! {set, self(), Value},
  receive V -> V end.
```

## Production-ready using gen_server

Our counter from above has a lot of potential issues.  What happens when
a counter process recieves an unexpected message?  What happens when the system gets busy
and it is unable respond to a click message in a timely manner?  How do we
stop the counter and free up any system resources that it is using?  The code
also gets unmanageable quickly as we add more and more messages.

Erlang comes with a set of [behavior](http://www.erlang.org/doc/design_principles/des_princ.html#id60128)
interfaces that solve the kinds of problems that one generally comes across when trying to solve problems using Erlang.
This set of modules is called [OTP](http://www.erlang.org/doc/design_principles/des_princ.html) (formerly
the "open telecom platform", now just "OTP").

The OTP [gen_server](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)
behavior is designed and implemented to implement a design pattern that solves
the problems that we've been exploring so far in this post.  It implements a stateful
spawned process behavior in a robust and complete way, and solves many problems that
we haven't even thought about yet. 

I won't go into a lot of depth here because the point of this post is not to teach
you about behaviors or proper OTP design.  If you want more detail, see
[the gen_server man page](http://www.erlang.org/doc/man/gen_server.html)
and the Learn You Some Erlang
[Clients and Servers](http://learnyousomeerlang.com/clients-and-servers)
chapter.  However, the following code should give you a
decent idea of how a gen_server solves the problem of keeping state in Erlang.

Here is an implementation of a simple counter using gen_server.

```erlang
%%% counter_server.erl
% simple counter implemented as a gen_server
-module(counter_server).
-behavior(gen_server).

% API
-export([new/0, click/1]).

% required by gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% API methods
new() ->
  gen_server:start(?MODULE, [], []).

click(Pid) ->
  gen_server:call(Pid, click).

%%% gen_server callbacks
%%%   these are required to implement the gen_server behavior
%%%   we're really only using init and handle_call
init([]) ->
  % the second value is the initial counter state
  {ok, 0}.

handle_call(click, _From, N) ->
  % the second value is sent back to the caller
  % the third value is the new state
  {reply, N + 1, N + 1}.

% basically, we ignore these, but keep the same counter state
handle_cast(_Msg, N) ->
  {noreply, N}.
handle_info(_Msg, N) ->
  {noreply, N}.
code_change(_OldVsn, N, _Other) ->
  {ok, N}.
terminate(_Reason, _N) ->
  ok.

%%% Erlang console
1> c(counter_server).
{ok,counter_server}
2> {ok, C} = counter_server:new().
{ok,<0.39.0>}
3> counter_server:click(C).
1
4> counter_server:click(C).
2
5> counter_server:click(C).
3
```

This may seem like a lot of code to solve a simple problem, but we've
gained a ton of flexibility just by implementing within this framework.
There are also plenty of templates available to produce all of the
boilerplate code for you (in Emacs, you can use
`M-x tempo-template-erlang-generic-server` from the Erlang package).

As an example, adding a `set/2` method similar to the one above is just
a matter of adding an API method and a `handle_call` callback.

```erlang
%%% API
set(Value, Pid) ->
  gen_server:call(Pid, {set, Value}).

...

handle_call({set, Value}, _From, _N) ->
  {reply, ok, Value};

% existing callback from above, shown for reference
%  (the two have to appear together for pattern matching to work)
handle_call(click, _From, N) ->
  {reply, N + 1, N + 1}.
```

## The end

So there you have it.  We use processes to keep track
of state in Erlang, and we use messages to access and control that state.
In a lot of ways, processes in Erlang are similar to class instances
in other languages.  In many other ways, they are very different.
Solving problems in Erlang takes a different way of thinking: thinking
in terms of processes.  This can be difficult to grok at first, but
once you understand it, many problems are much more easily solved
this way.
