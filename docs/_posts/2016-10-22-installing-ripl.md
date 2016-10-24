---
layout: page
title: "Installing RIPL"
category: install
date: 2016-10-22 17:29:05
---

### Setting up the RIPL to C compiler

{% highlight bash %}
$ cd ~/path/of/your/choice/<br>
$ git clone https://github.com/orcc/orcc.git
$ git clone https://github.com/orcc/ci-server-scripts.git
{% endhighlight %}

Define these environment variables, either in `/.profile` or
`/.bash_profile`, depending on your Operating System:

{% highlight bash %}
$ mkdir ~/path/of/your/choice/orcc-build
$ export WORK_DIR=~/path/of/your/choice/orcc-build
$ export ORCC_DIR=~/path/of/your/choice/orcc
$ export CI_SERVER_DIR=~/path/of/your/choice/ci-server-scripts
$ export SCRIPTS_DIR=$CI_SERVER_DIR/headless_build
{% endhighlight %}

Add the ~bin/~ directory in the RIPL repository to your ~$PATH~
environment variable:

{% highlight bash %}
$ export PATH=~/path/to/ripl/bin:$PATH
{% endhighlight %}

You need to run the following command only once, to install the
dataflow compiler. This takes about 10 minutes:

{% highlight bash %}
$ build_orcc
{% endhighlight %}
