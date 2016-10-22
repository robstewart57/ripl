---
layout: page
title: "Installing RIPL"
category: install
date: 2016-10-22 17:29:05
---

### Setting up the RIPL to C compiler

```bash
$ cd ~/path/of/your/choice/
$ git clone https://github.com/orcc/orcc.git
$ git clone https://github.com/orcc/ci-server-scripts.git
```

Define these environment variables, either in `/.profile` or
`/.bash_profile`, depending on your Operating System:

```bash
$ mkdir ~/path/of/your/choice/orcc-build
$ export WORK_DIR=~/path/of/your/choice/orcc-build
$ export ORCC_DIR=~/path/of/your/choice/orcc
$ export CI_SERVER_DIR=~/path/of/your/choice/ci-server-scripts
$ export SCRIPTS_DIR=$CI_SERVER_DIR/headless_build
```

Add the ~bin/~ directory in the RIPL repository to your ~$PATH~
environment variable:

```bash
$ export PATH=~/path/to/ripl/bin:$PATH
```

You need to run the following command only once, to install the
dataflow compiler. This takes about 10 minutes:

```bash
$ build_orcc
```
