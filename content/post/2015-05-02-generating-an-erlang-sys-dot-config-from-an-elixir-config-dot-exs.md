---
title: "Generating an Erlang sys.config from an Elixir config.exs"
date: "2015-05-02"
comments: true
aliases:
  - "blog/2015/05/02/generating-an-erlang-sys-dot-config-from-an-elixir-config-dot-exs/"
categories: [erlang, elixir, deployment, chef]
---

This post describes Erlang's sys.config and the equivalent config.exs
in Elixir and how you can
[convert from config.exs to sys.config](https://gist.github.com/dantswain/fdfb1c2c86e4d940a8f5)
(the link is to a script that does this).  I also talk a little about
how you could use this to deploy Elixir projects using Chef.

## Erlang's sys.config

Erlang releases use a file called
[sys.config](http://www.erlang.org/doc/man/config.html) for
configuration values.  This is the place to put your OTP applications'
configuration in addition to configuration for system applications
like the logger and
[sasl](http://www.erlang.org/doc/man/sasl_app.html).  You can read
these values in your code using
[`application:get_env/2`](http://www.erlang.org/doc/apps/kernel/application.html#get_env-2).
A sys.config file is just an Erlang
[term](http://www.erlang.org/doc/reference_manual/data_types.html#id68423);
`application:get_env` assumes that it is a
[proplist](http://www.erlang.org/doc/man/proplists.html).

As an example, consider login credentials for an outside service.  We
could store these in the sys.config file (`rel/files/sys.config` if
we're building using [rebar](https://github.com/basho/rebar)) as
something like the following.

```erlang
% rel/files/sys.config
[
  {outside_service, [
      {host, <<"dantswain.com">>},
      {user_name, <<"dantswain">>},
      {password, <<"s3kr4t">>}
    ]}
].
```

We would then access this config in our application using the
following.

```erlang
get_credentials() ->
  Host     = application:get_env(outside_service, host),
  UserName = application:get_env(outside_service, user_name),
  Password = application:get_env(outside_service, password),
  {Host, UserName, Password}.
```

Note that this approach only works for one environment.  To use
different credentials for testing, development, and production, we'd
need to either come up with a config layout that allowed for multiple
environments or swap out different files depending on the
build environment.  Either way is a bit cumbersome.

## Elixir's config.exs

Elixir enriches our ability to configure OTP applications via the 
[`Mix.Config`](http://elixir-lang.org/docs/v1.0/mix/Mix.Config.html)
DSL.  This makes it very easy to have per-environment configurations
(e.g., testing, development, and production) and to compose config
files programmatically.

Since we're going to talk specifically about deployment to a
production environment, let's make our `config/config.exs` read from
different files depending on `Mix.env`, which we can set with the
`MIX_ENV` environment variable.

```elixir
# config/config.exs
use Mix.Config

import_config "#{Mix.env}.exs"
```

That is, we just defer to the per-environment config file. Our
development config might point to localhost.

```elixir
# config/dev.exs
use Mix.Config

config :outside_service,
  host: "localhost",
  user_name: "dantswain",
  password: "password"
```

Our testing config might use different credentials.

```elixir
# config/test.exs
use Mix.Config

config :outside_service,
  host: "localhost",
  user_name: "test",
  password: "test"
```

We have to at least have a `config/prod.exs` file to be able to
compile the project, but we'll end up using a different method below
to populate the production config at deployment time.  So we can have
a dummy config file in source control.

```elixir
# config/prod.exs
use Mix.Config

# We generate production config during deployment, but
#  this file needs to exist so that mix does not fail when
#  it tries to import_config("prod.exs")
```

Configuration values are accessed using
[`Application.get_env/3`](http://elixir-lang.org/docs/v1.0/elixir/Application.html#get_env/3).

```elixir
def get_credentials do
  host = Application.get_env(:outside_service, :host)
  user_name = Application.get_env(:outside_service, :user_name)
  password = Application.get_env(:outside_service, :password)
  {host, user_name, password}
end
```

## Configuration management

I highly recommend [exrm](https://github.com/bitwalker/exrm) to
build releases for deploying Elixir projects.  I highly
recommend using a
[configuration management](http://en.wikipedia.org/wiki/Configuration_management)
tool for deployment in general.  I'm a fan of
[Chef](https://www.chef.io/), but most of this post isn't
specific to Chef.

exrm adds a Mix task called `mix release` to your project.  To build a
production release, you just run `MIX_ENV=prod mix release` (after the
usual build cycle).  This step creates an Erlang
[release](http://www.erlang.org/doc/design_principles/release_structure.html),
which contains everything needed to run your app.  Inside the release
is a sys.config file (`releases/<version>/sys.config`).
`Application.get_env/3` will consult this file when your code is
deployed.

Using `mix release` out-of-the-box will convert your
`config/config.exs` into a `sys.config` file.  This is fine if you
are not using configuration management, but you should really be using
configuration management ;).  exrm handles this using a tool called
[conform](https://github.com/bitwalker/conform), which allows you to
place `.conf` files inside your release.  conform automatically builds
a new sys.config file each time you start your app (`bin/my_app
start`).

conform is probably The Right Way To Do It.  I'm going to present a
simpler method here that also works.  This might be helpful to you if
you don't have time to grok conform, or it might just be useful to you
as a way of understanding part of what exrm and conform do under the
hood.

If you look at the
[exrm source code](https://github.com/bitwalker/exrm/blob/43e41fc5b9b13e893ad515c5a3afe60fa632892a/lib/exrm/utils.ex#L29),
you'll see that exrm uses `Mix.Config.read!/1` to read in the
appropriate config file and then simply
[formats and writes the config term to a file](https://github.com/bitwalker/exrm/blob/bebc97c7707b6019a2790132b16653418f25afdc/lib/exrm/utils.ex#L164).
The implementation of
[`Mix.Config.read!/1`](https://github.com/elixir-lang/elixir/blob/aa9aa8084a861cd93dcbaba33b6ce46bc82e8a64/lib/mix/lib/mix/config.ex#L129)
is itself fairly straightforward and relies on
[`Code.eval_file/2`](http://elixir-lang.org/docs/v1.0/elixir/Code#eval_file/2)
to turn the `.exs` files into terms.

Converting a config.exs to a sys.config is essentially a matter of
using
[`Mix.Config.read!/1`](http://elixir-lang.org/docs/v1.0/mix/Mix.Config.html#read!/1)
to obtain a term and then writing it to disk.  It could be implemented
simply using something like this.

```elixir
config = Mix.Config.read!("config.exs")
sys_config_string = :io_lib.format('~p.~n', [config]) |> List.to_string
File.write("sys.config", sys_config_string)
```

I wrote a more fleshed-out
[Elixir script to generate a sys.config file from a config.exs file](https://gist.github.com/dantswain/fdfb1c2c86e4d940a8f5)
that includes some error handling and usage messages.

## Deploying an Elixir project using Chef

This is how I use Chef to deploy Elixir applications.  I won't go go
into detail on how to install Erlang, Elixir, Mix, etc.  I also won't
cover how to build your code, which is fairly straightforward (the
usual `mix deps.get`, `mix compile`, etc., but with `MIX_ENV=prod`).

1. Build your release using exrm's `MIX_ENV=prod mix release`.  Use a
   build server if you have one, otherwise build in a temporary
   location.  The output of the build process is a `.tar.gz` file of
   your release.
2. Untar the release file into the deployment location and set
   appropriate ownership and permissions.
3. Use a Chef template to generate a config.exs file for your
   particular deployment environment.  Have Chef install the
   config.exs file alongside the release in
   `releases/<version>/config.exs`.
4. Have Chef install the
   [conversion script](https://gist.github.com/dantswain/fdfb1c2c86e4d940a8f5)
   in the deployment's `PATH` and make sure that it is executable.
5. In the directory where the config.exs file lives (which is where
   you want the sys.config file to end up), execute `MIX_ENV=prod
   elixir_to_sys_config config.exs > sys.config`.

This way you can template your config.exs file in a way that is
familiar to Elixir developers, and end up with a sys.config file
that will work with your release app.  If you structure your Chef
cookbooks appropriately, you should be able to regenerate the config files
without rebuilding the entire application.
