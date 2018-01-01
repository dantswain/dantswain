---
layout: post
title: "Integration Testing a JSON API in Phoenix"
date: "2015-04-19"
comments: true
categories:
  - "elixir"
tags: [phoenix]
---

# TL;DR

To do integration testing in Phoenix, first enable the app's content
serving in its config, then use an HTTP client library (I use
HTTPoison below) to make requests.  The source code corresponding to
this post is on my github at
[https://github.com/dantswain/calculon](https://github.com/dantswain/calculon).

#Introduction (plus a little discourse on testing)

I like integration-level testing, especially for web apps and doubly
so for APIs.  A good integration test suite can help you work out
usability issues with your API, give you confidence that your
deployment is stable, and provide a springboard for troubleshooting
issues down the road.  Publishing your test suite can also be a decent
way to document it.  If you're working in Ruby, there is actually a
gem to
[document your API through rspec](https://github.com/zipmark/rspec_api_documentation)
(which I highly recommend).  I'm not currently aware of an equivalent
package for Elixir, though that would be awesome.

I was recently working on an API for a personal project using the
[Phoenix framework](http://phoenixframework.org) for
[Elixir](http://elixir-lang.org) and realized there wasn't much
guidance (at least as of the time of writing) on writing integration
tests.  I was able to figure it out after a little googling, so I
thought I'd write up what I've learned.

This post covers integration testing of a JSON API.  It's
theoretically quite possible to extend the same ideas to test a
generic web app, though it could be tricky to handle Javascript.

<blockquote> Note: Phoenix hasn't yet reached a 1.0 release version
and is correspondingly in a little bit of flux between minor versions.
This post was written using Phoenix v0.11.0 running on Elixir v1.0.2.
I know there are a couple differences from v0.10.0 to v0.11.0 that
will probably break this; see the note at the bottom of this post for
at least one difference that I know of.  </blockquote>

# It's just curling (kind of)

In theory, you could write integration tests for your API using any
language and framework you want. You could script the tests in the
shell using [curl](http://curl.haxx.se/docs/manpage.html), or write a
test suite in Ruby with [rspec](http://rspec.info).  All we need to do
is make HTTP requests and validate the responses.

However, we're already using Elixir and Elixir provides a pretty good
testing framework called
[ExUnit](http://elixir-lang.org/docs/v1.0/ex_unit/ExUnit.html).
Furthermore, there's probably code in our application that will help
us a lot when writing tests - model code is useful for generating
fixture data, etc.  We may also want to validate the state of the
application after a request.

<blockquote> Aside: In an ideal world, integration tests should
(debatably) share nothing with your application code.  That's because
you want integration tests not to depend on your implementation
details.  Using model code to set up fixtures and validate application
state is a fairly reasonable compromise because it generally
makes us a lot more productive. </blockquote>

# Setting up an example app

Let's build an app that does something very simple so that we can
focus on the mechanics of integration testing rather than the
implementation details.  We aren't doing any database calls since that
would add a fair amount of complexity to the code and wouldn't help us
understand integration testing a Phoenix app.  Nothing that we do here
will preclude us from making and testing database calls later on, so
let's just avoid it.

We'll make an app that performs simple arithmetic calculations.  The
goal is to be able to POST a calculation to the server and have the
response include the result of the calculation. We'll call the app
[Calculon](http://futurama.wikia.com/wiki/Calculon).

Follow the usual process to set up a Phoenix app.  As of v0.11.0, you
have to run `mix phoenix.new` from within the installer directory of
the Phoenix repo (as of the time of writing, the documentation hasn't
been updated).

```bash
in ~/src/phoenix/installer (v0.11.0)
$ mix phoenix.new ../../calculon
```

Set up an endpoint so that we can `POST /api/v1/calculations`:

```elixir
# inside web/router.ex
scope "/api/v1", Calculon do
  pipe_through :api

  post "/calculations", CalculationsController, :create
end
```

Then add the corresponding controller:

```elixir
# web/controllers/calculations_controller.ex
defmodule Calculon.CalculationsController do
  use Phoenix.Controller
  
  plug :action
  
  def create(conn, _params) do
    conn
    |> put_status(201)
    |> json  %{ok: true}
  end
end
```

This controller doesn't really do anything at this point, it just
handles the request and sets the HTTP status code to 201.  We'll add
functionality later.

We can now run our app with `mix phoenix.server` and use curl to
verify that it's working:

```bash
$ curl -v -XPOST -d'{"calculation": { "input": "1 + 1" }}' 'http://127.0.0.1:4000/api/v1/calculations'
# (trimmed output)
# ...
* Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
> POST /api/v1/calculations HTTP/1.1
# ...
* upload completely sent off: 37 out of 37 bytes
< HTTP/1.1 201 Created
# ...
{"ok":true}% 
```

We get back an HTTP 201 with `{"ok": true}` as the body, which is
exactly what we should get.

# HTTPoison for requests

[HTTPoison](https://github.com/edgurgel/httpoison) is an Elixir
library for performing HTTP requests.  Add it to the deps function in
your `mix.exs`.

```elixir
# inside mix.exs
defp deps do
  [ #... other deps
  {:httpoison, "~> 0.6"}]
end
```

Then run `mix deps.get` and `mix deps.compile`.

Now let's add a helper module to make API calls, making heavy use of
HTTPoison's `HTTPoison.Base` macro and the callbacks that it
provides to shape requests and responses.

{% include_code HTTP Helper phoenix_integration_testing/api_call.ex %}

Now we can launch our app (`mix phoenix.server`) and in another
terminal do things like this:

```iex
iex(1)> Calculon.Support.APICall.post!("/calculations", %{calculation: %{input: "1 + 1"}})
%HTTPoison.Response{body: %{"ok" => true},
headers: %{"cache-control" => "max-age=0, private, must-revalidate",
"connection" => "keep-alive", "content-length" => "11",
"content-type" => "application/json; charset=utf-8",
"date" => "Sun, 19 Apr 2015 20:24:49 GMT", "server" => "Cowboy",
   "x-request-id" => "DkMB98DVLHafB7eWdXoS"}, status_code: 201}
```

Note: By putting this code in `lib/calculon/support`, it will be
available in all environments (test, development, production).  This
makes it useful for manual testing in development mode.
Alternatively, we could put it in `test/support`, but then we could
only use it from within our test suite.

# Launching the server during tests

If we use the APICall helper in our test code at this point, we'll get
an error: `** (HTTPoison.Error) :econnrefused`.  That's because
Phoenix does not enable the server portion of the application when
it's run in the test environment.  Fortunately, it's not too hard to
automate turning it on.

Let's add a test helper to launch the API when we need it.  We'll put
this code in the `test/support` directory because we don't really need
it outside of our test code.

{% include_code API Helper phoenix_integration_testing/helpers.ex %}

# Writing integration tests

We have all of the pieces now.  Let's write a test.

{% include_code Simple integration test phoenix_integration_testing/calculations_integration_test.exs %}

We use the `setup_all`
[ExUnit callback](http://elixir-lang.org/docs/v1.0/ex_unit/ExUnit.Callbacks.html)
to execute our helper that enables the API endpoints, and our HTTPoison
helper to make the actual request.  The HTTPoison response object has
a `status_code` property that we can use to validate the HTTP status
code returned by the server.

The test should pass.

```bash
$ mix test test/integration
.

Finished in 0.2 seconds (0.1s on load, 0.1s on tests)
1 tests, 0 failures

Randomized with seed 170902
```

Now let's add some actual calculator functionality to Calculon using
TDD.  We'll update our previous integration test to test for simple
addition. 

{% include_code Updated integration test phoenix_integration_testing/updated_calculations_integration_test.exs %}

This test should fail because we haven't built the
corresponding functionality yet.

```bash
$ mix test test/integration


1) test simple addition (CalculationsIntegrationTest)
test/integration/calculations_integration_test.exs:19
Assertion with == failed
code: response.body() == expected_response
lhs:  %{ok: true}
rhs:  %{calculation: %{input: "1 + 1", output: "2"}}
stacktrace:
test/integration/calculations_integration_test.exs:26

.

Finished in 0.3 seconds (0.1s on load, 0.1s on tests)
2 tests, 1 failures
```

Now we can update our controller to actually calculate output. 

```elixir
def create(conn, params) do
  input = params["calculation"]["input"]
  output = Calculon.Calculator.eval(input)

  conn
  |> put_status(201)
  |> json %{calculation: %{input: input, output: "#{output}"}}
end
```

The implementation of `Calculon.Calculator.eval` is included below.
It's similar in functionality to
[`Code.eval_string/3`](http://elixir-lang.org/docs/v1.0/elixir/Code.html#eval_string/3),
which evaluates arbitrary Elixir code strings, but it has heavy
restrictions on the input for reasons of safety.

Now the tests pass:

```bash
$ mix test test/integration
..

Finished in 0.2 seconds (0.1s on load, 0.1s on tests)
2 tests, 0 failures

Randomized with seed 777178
```

The controller implementation doesn't do any error checking, so let's
add a test for that.

```elixir
# in test/integration/calculations_integration_test.exs
test "invalid input returns HTTP 400" do
  response = APICall.post!("/calculations", %{calculation: %{input:
""}})
  assert response.status_code == 400
end 
```

This test fails because the controller responds with an HTTP status
code of 201 even though it failed to perform any calculation.  We'd
like to let an API consumer know that they are providing invalid
input, so let's refactor the controller to return a 400 error if the
calculator returns `nil`.

{% include_code Controller with some error handling phoenix_integration_testing/calculations_controller.ex %}

Now our tests pass again:

```bash
$ mix test test/integration
...

Finished in 0.3 seconds (0.1s on load, 0.1s on tests)
3 tests, 0 failures

Randomized with seed 826740
```

# Conclusion

These are the basic building blocks that you need in order to start
doing integration testing for your Phoenix app. It mostly boils down
to using something like HTTPoison to automate your HTTP requests and
adding a setup callback that ensures your application is serving
content during testing.  The code for Calculon is on my github:
[https://github.com/dantswain/calculon](https://github.com/dantswain/calculon).

## Phoenix v0.10.0 and earlier

In Phoenix v0.10.0 and before, you may need to add the following to your
`launch_api` helper:

```elixir
Application.put_env :phoenix, :serve_endpoints, true
```

I've gotten it to work with v0.9.0 and v0.10.0.  If you have trouble,
feel free to leave a comment below and I'll try to help.

## Calculator code

This is by no means bullet proof, or even probably "good". The basic
idea is to parse the code into an
[AST](http://elixir-lang.org/getting-started/meta/quote-and-unquote.html)
and then check the form of the AST to make sure that the input is only
doing one of a small list of allowed operations. My initial thought
when I started writing this was to just use
[`Code.eval_string/3`](http://elixir-lang.org/docs/v1.0/elixir/Code.html#eval_string/3),
with the massive caveat that that would be __very__ unsafe.  I just
couldn't do it.

In theory, this could be extended with recursion to allow more complex
operations.  This is also might not be a great solution for a
production app because the parsing and validation might be relatively
heavy on the CPU.

{% include_code Simplistic AST arithmetic parser phoenix_integration_testing/calculator.ex %}
