---
layout: post
title: "Why I Want a New Rake Alternative"
date: "2013-01-12"
comments: true
published: false
categories: [ruby, rake, c++, c]
---

## TL;DR

What I'd really like is a tool for generating Makefiles from Ruby.  It's a
fundamentally different problem than the one that Rake solves.  

## Use the right tool for the job

I want a new Ruby-based build tool for C and C++.  Others have successfully
used [Rake](http://rake.rubyforge.org) for this.  There are lots of example
Rakefiles for compiling C and C++ projects, and even more examples of people
trying to find help because they tried it and couldn't get it to work right.
It's a task that's used as an example in parts of the [Rake documentation](http://rake.rubyforge.org/files/doc/rakefile_rdoc.html).  Martin
Fowler even used it as an example in his article about [Rake and dependency based programming](http://martinfowler.com/articles/rake.html).  But I argue that it's not
really the right tool for that job, and if I were to have a coding motto, it's
"use the right tool for the job".

I'm not saying that I don't like Rake.  Rake is great.  It's ubiquitous in
the Ruby world; it's an essential part of developing and managing Rails applications.  It's pretty good at what it does there.  There are other
things that Rake is great for, too.  I've happily used Rake as a tool to launch
and interact with Erlang applications.  My blog is powered by [Octopress](http://octopress.org), which uses Rake very effectively.  I won't stop using Rake any time soon.

I won't, however, be using Rake to build a C or C++ program again any time
soon.  I've tried this.  It's frustrating and the end product is,
IMHO, not maintainable.  Maybe now is a good point, too, to point out that to
build Ruby extensions, we don't use Rakefiles, we use `extconf.rb`.  That is,
the people who designed the mechanisms for extending Ruby didn't think Rake was
a good tool to manage C/C++ builds.  They wrote a tool to turn Ruby scripts into
specialized Makefiles.

Let's take a look at an example Rakefile from the
[Beluga](http://github.com/leonard-lab/Beluga) project.  I wrote this Rakefile
to build some test suites that tie into the rest of the C++ code base for this
project.

``` ruby
# Rakefile
require 'rake/clean'

CLEAN.include('*.o')
CLOBBER.include('test_BelugaIPCClient')

INC_DIR = "../src"

task :default => ["test_BelugaIPCClient", "test_Controller"]

rule '.o' => '.cpp' do |t|
  sh "g++ -I#{INC_DIR} -c #{t.source}"
end

# build targets
task "test_BelugaIPCClient" => ['../src/BelugaIPCClient.o', 'test_BelugaIPCClient.o'] do |t|
  objs = t.prerequisites.map{|p| p.pathmap("%f")}.join(' ')
  sh "g++ -o #{t.name} #{objs}"
end

task "test_Controller" => ["../src/BelugaControl.o",
                           "../src/ControlLaw.o",
                           "test_Controller.o"] do |t|
  objs = t.prerequisites.map{|p| p.pathmap("%f")}.join(' ')
  sh "g++ -o #{t.name} #{objs}"
end
```

Rake executes a sequence of tasks based on the rules laid out in this Rakefile.
I can use Ruby code to generate these tasks and to define a set of dependencies
that governs the order of task execution.  Essentially, we use Rake as an
alternative to a Makefile, and buy the ability to program this file in Ruby.
But therein lies the problem.  We're generating an *alternative* to a Makefile.
Therefore, we lose out on a whole ton of extra features that Make affords us
(for example, it's possible to achieve incremental builds with Rake, but that
features comes for free with Make), not to mention the long, proven, history of
Make.  That is, Make is _very_ good at building C and C++ programs, and we're
throwing all of that away.

Let me take a side step into the land of maintainability.  If I have a second
coding motto, it's "do not sacrifice maintainability for shorter code".  To me,
part of maintainability is readability.  Take a look at the example Rakefile
above.  See how long it takes to answer these basic questions:  Where will the
finished executables live in the directory tree?  What source code files are
required to build each executable?  What libraries do they link against?  Now
think about making updates to this project.  If you introduced a new C++ module
(i.e., a source file), how would you introduce it to the Rakefile?  If you
wanted to add an executable target, how many lines of code would you have to
duplicate?  How much of that duplicated code is specific to your target?  None
of these questions are, on their own, terribly troubling.  But they add up;
they add to the frustration level.

So why not just write Makefiles?  For small projects, this isn't a big deal at
all.  Make's syntax is a little funny, but it's not all that difficult to pick
up.  The problem is that Makefiles are static.  It's also very repetetive.
This is why tools like autoconf, [scons](http://scons.org), and (one of my
favorites) [CMake](http://cmake.org) exist: to generate Makefiles from
user-defined scripts that condense configuration logic and project structure
down into more easily-digestible pieces.  

There's nothing wrong with tools like autoconf, scons, and CMake; they're
great.  I've happily used CMake in the past.  However, every time I go back to
CMake, I have to spend time reminding myself of CMake's syntax and built-in
functions.  On the other hand, I use Ruby all the time.  It's my default
language of choice, especially for scripting.  


Rake interprets a Rakefile to generate a set of tasks.
