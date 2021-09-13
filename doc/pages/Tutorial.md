---
title: Tutorial
---

# Getting Started With Vegetables

If this is your first encounter with a unit testing framework,
or even just your first encounter with vegetables,
I highly recommend cloning the vegetables repository and running its test suite.
You'll need to have the following installed to do that.

* git
* A modern Fortran compiler; newer versions of gfortran and nagfor are known to work
* The [Fortran Package Manager (fpm)](https://github.com/fortran-lang/fpm)

With those installed and configured, you should be able to open a terminal and issue the following commands.

```
git clone https://gitlab.com/everythingfunctional/vegetables.git
cd vegetables
fpm test
```

You should see output looking like the following.

```
Running Tests

Test that
    assert_doesnt_include
        passes with different strings
        fails with the same string
...
A total of 78 test cases

All Passed
Took 3.17154 seconds

A total of 78 test cases containing a total of 205 assertions
```

Congratulations, you've run your first suite of tests using the vegetables framework.

## Options for Running Test Suites

Some command line options are accepted by the default test suite runner.
To see the options available, pass the `-h` option to the runner, which can be done using fpm like

```
fpm test -- -h
```

You should see output like the following:

```
Usage: build/gfortran_2A42023B310FA28D/test/vegetables-test [-h] [-q] [-v] [-d] [-f string] [-n num] [-s num] [-c]
  options:
    -h, --help                    Output this message and exit
    -q, --quiet                   Don't print the test descriptions before
                                  running the tests
    -v, --verbose                 Print all of the assertion messages, not
                                  just the failing ones
    -d, --debug                   Report the beginning and end of execution
                                  of each test case or suite
    -f string, --filter string    Only run cases or collections whose
                                  description contains the given string
    -n num, --numrand num         Number of random values to use for each
                                  test with generated values (default = 100)
    -s num, --shrink-max num      Number of attempts to find a simpler value
                                  if a random value fails (default = 100)
    -c, --color-off               Don't colorize the output
```

The `-q`, `-v`, and `-d` options affect the verbosity of the output.
By default, vegetables will report the tests that will be run, before starting to run them.
The `-q` option suppresses this initial output.
By default, vegetables will only report assertions that fail.
The `-v` option will cause the passing assertions to be reported as well.
By default, vegetables does not produce any output during the execution of the tests.
If a test crashes, this can make it difficult to isolate the cause of the problem.
The `-d` option will cause the beginning and completion of execution of each test.

The `-f` options impacts what tests are run.
By passing this option, vegetables will filter the test suite before execution,
thus only executing tests that match the given string.

The `-n` and `-s` affect the behavior of generator tests (a more advanced concept that we'll get to later).
The `-n` option will set how many randomly generated values will be provided to a test.
The `-s` option will set how many attempts will be made to find the simplest possible input causing a failure, in the case that an input causing failure is found.

The `-c` option impacts the output color.
By default, vegetables attempts to color the messages from passing assertions green, and from failing assertions red.
It uses terminal escape sequences to do so, but under some terminals or environments these may not be interpreted correctly, or at all, leading to some visual clutter.

# Writing Your First Test

Coming Soon!

# Including Tests Into Your Test Suite

Coming Soon!

# More Advanced Testing Patterns

Coming Soon!

## Providing Example Inputs to a Test

Coming Soon!

## Generating Random Inputs for a Test

Coming Soon!

## Inputs for a Whole Test Suite

Coming Soon!

## Modifying Inputs Before Passing to a Test
