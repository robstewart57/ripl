---
layout: page
title: "Installing RIPL"
category: install
date: 2016-10-22 17:29:05
---

### Compiling the RIPL compiler

This requires a number of Linux distribution packages. Use your
package manager to install:

- `bnfc`

    from http://bnfc.digitalgrammars.com

- `ghc`

    from your Linux distribution's package manager

- `stack`

    from www.haskellstack.org

Then use stack to install tools for the RIPL parser:

{% highlight bash %}
$ stack install happy
$ stack install alex
{% endhighlight %}

Finally, the compiler can now be compiled from the your _ripl_
repository clone:

{% highlight bash %}
$ make
{% endhighlight %}

To test that the RIPL parser works use the `TestRIPL` executable:

{% highlight bash %}
$ src/TestRIPL examples/map.ripl
{% endhighlight %}

If that works, you are ready to use `riplc` to compile RIPL programs
to dataflow program equivalents.

### Setting up the RIPL to C compiler (optional)

The `riplc` compiler above compiles RIPL programs to CAL dataflow
programs, that can be manually inspected and manually compiled with
the [Orcc compiler](http://orcc.sourceforge.net). If you want to
compile RIPL to C to run on CPUs to validate the outputs of programs,
follow these instructions.

Define these environment variables, either in `/.profile` or
`/.bash_profile`, depending on your Operating System:

{% highlight bash %}
$ export ORCC_HOME=~/path/of/your/choice
$ export WORK_DIR=$ORCC_HOME/orcc-build
$ export ORCC_DIR=$ORCC_HOME/orcc
$ export CI_SERVER_DIR=$ORCC_HOME/ci-server-scripts
$ export SCRIPTS_DIR=$CI_SERVER_DIR/headless_build
{% endhighlight %}

Then run the `source` command against that bash configuration file,
e.g.

{% highlight bash %}
$ source ~/.profile
{% endhighlight %}


Now clone the Orcc repository:

{% highlight bash %}
$ cd $ORCC_HOME
$ git clone https://github.com/orcc/orcc.git
$ git clone https://github.com/orcc/ci-server-scripts.git
$ mkdir $ORCC_HOME/orcc-build
{% endhighlight %}

Add the `bin/` directory in the RIPL repository to your `$PATH`
environment variable:

{% highlight bash %}
$ export PATH=~/path/to/ripl/bin:$PATH
{% endhighlight %}

You need to run the following command only once, to install the
dataflow compiler. This takes about 10 minutes:

{% highlight bash %}
$ build_orcc
{% endhighlight %}
