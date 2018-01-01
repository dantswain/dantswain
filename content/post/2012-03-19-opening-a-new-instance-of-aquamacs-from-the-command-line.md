---
layout: post
title: "Opening a new instance of Aquamacs from the command line"
date: "2012-03-19"
comments: true
categories: [bash, shell scripts, os x, open]
---

### The short answer:

```
#!/bin/bash
open -na Aquamacs $*
```

### The long answer: 

If you've used the OS X command line for any length of time, you've probably (hopefully) come across the `open` command.  Its job is (shockingly enough) opening files and applications using Finder.  It works for files:

```
$ open index.html
```

opens `index.html` in your browser.  It also works for applications:

```
$ open -a Mail
```

opens Mail.app.  It also works for opening files with an application:

```
$ open -a Aquamacs config/routes.rb
```

opens `config/routes.rb` in [Aquamacs](http://aquamacs.org).

<!-- more -->

In fact, I often find myself wanting to open a file in Aquamacs from the command line.  It's not hard at all to find and/or figure a shell script to do this:

```
#!/bin/bash
open -a Aquamacs $*
```

That is, if you save that script somewhere on your `PATH` as `amacs` and `chmod +x amacs`, then `amacs config/routes.rb` opens up `config/routes.rb` in Aquamacs.  Using `$*` makes it flexible enough to handle multiple files.

Once Aquamacs is open, additional executions of `open -a Aquamacs` will cause one of two things to happen:

  - If no additional arguments are provided, then the application is brought to the front of the window manager as if you had clicked on its icon in the Dock, or

  - If you provide a file name, i.e., `open -a Aquamacs Gemfile`, then that file is opened up in the application as if you had opened it through the application's menu.

Most of the time, this is the behavior that you want.  Most people who use OS X are accustomed to it reusing existing instances of an application.  There are, however, some perfectly reasonable circumstances for which you might want a *new* instance of the application to open.  For example, I typically have at least a couple projects open in Aquamacs: a Rails app, a Latex document, a blog post, etc.  Personally, I like to be able to `C-x C-c` occasionally and restart with a blank slate of buffers.  I also like to be able to close one project without having to `C-x C-f` each of the buffers independently.

Happily, it's incredibly easy to modify the shell script above to handle this case using open's `-n`option:

```
#!/bin/bash
open -na Aquamacs $*
```

Save this version to another file called `namacs` and run `namacs foo.tex` to open `foo.tex` in a new instance of Aquamacs.

Note:

  - The argument ordering matters.  `open -an` does *not* work.
  - This works for pretty much any appliation.  I'm just using Aquamacs as an example.
