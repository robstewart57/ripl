---
layout: page
title: "Debugging the RIPL compiler"
category: install
date: 2016-10-22 17:29:05
---

### Debugging

The first thing to test is whether the syntax in your RIPL program is correct or not. From the RIPL repository's root directory, run:

```bash
$ src/TestRIPL myProgram.ripl
```

If it is syntactically valid, a linearised tree of your program will be shown.

If you get an _"unsupported"_ error message, then there is a missing piece in the compiler implementation. Submit your RIPL program as a GitHub issue, along with the error message.

Otherwise, create a stack trace when running `riplc`. Do this by:

```bash
$ riplc -c -o /some/path/ myProgram.ripl +RTS -xc -RTS
```

Submit the stack trace along with the RIPL program as a GitHub issue.
