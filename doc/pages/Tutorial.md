---
title: Tutorial
---

# Prerequisites

This tutorial assumes that you have working knowledge of, and are comfortable using the following tools:

* The [Fortran Language](https://fortran-lang.org/)
* [A command line interface](https://en.wikipedia.org/wiki/Command-line_interface)
* [git](https://git-scm.com/)
* The [Fortran Package Manager](https://github.com/fortran-lang/fpm)

In addition, to understand some of the motivations behind this library,
and how to use it effectively, you will want to understand:

* [What unit testing is](https://everythingfunctional.wordpress.com/2021/09/09/a-what-test/)
* [How to write good unit tests](https://www.youtube.com/watch?v=tWn8RA_DEic)

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

The examples for this tutorial are stored in a public repository available [here](https://gitlab.com/everythingfunctional/vegetables_tutorial).
I'll link to tags in that repository for key milestones.

To get started, we're going to use fpm to build and run our tests.
In a terminal, move to a place you want to keep your project, and issue the command:

```
fpm new --lib --test vegetables_tutorial
```

This will generate a new project for us, with a template test and library.
Move into the newly created folder, and run the command `fpm test`, and you should see some output like the following:

```txt
$ fpm test
 + mkdir -p build/dependencies
 + mkdir -p build/gfortran_2A42023B310FA28D/vegetables_tutorial
 + gfortran -c test/check.f90 -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds -fcheck=array-temps -fbacktrace -fcoarray=single -J build/gfortran_2A42023B310FA28D/vegetables_tutorial -I build/gfortran_2A42023B310FA28D/vegetables_tutorial  -o build/gfortran_2A42023B310FA28D/vegetables_tutorial/test_check.f90.o
 + gfortran -c ././src/vegetables_tutorial.f90 -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds -fcheck=array-temps -fbacktrace -fcoarray=single -J build/gfortran_2A42023B310FA28D/vegetables_tutorial -I build/gfortran_2A42023B310FA28D/vegetables_tutorial  -o build/gfortran_2A42023B310FA28D/vegetables_tutorial/src_vegetables_tutorial.f90.o
 + ar -rs build/gfortran_2A42023B310FA28D/vegetables_tutorial/libvegetables_tutorial.a build/gfortran_2A42023B310FA28D/vegetables_tutorial/src_vegetables_tutorial.f90.o
ar: creating build/gfortran_2A42023B310FA28D/vegetables_tutorial/libvegetables_tutorial.a
 + mkdir -p build/gfortran_2A42023B310FA28D/test/
 + gfortran  -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds -fcheck=array-temps -fbacktrace -fcoarray=single -J build/gfortran_2A42023B310FA28D/vegetables_tutorial -I build/gfortran_2A42023B310FA28D/vegetables_tutorial  build/gfortran_2A42023B310FA28D/vegetables_tutorial/test_check.f90.o build/gfortran_2A42023B310FA28D/vegetables_tutorial/libvegetables_tutorial.a -o build/gfortran_2A42023B310FA28D/test/check
 Put some tests in here!
```

You can find the code at this stage [here](https://gitlab.com/everythingfunctional/vegetables_tutorial/-/tree/starting_point).

At this point, our code compiles and our tests run,
but the code doesn't do anything interesting, our tests aren't actually testing anything, and we aren't using vegetables yet.
Let's start by getting vegetables.
In the `fpm.toml` file, add the following section:

```toml
[dev-dependencies]
vegetables = { git = "https://gitlab.com/everythingfunctional/vegetables.git", tag = "v7.2.2" }
```

Now fpm will fetch and compile vegetables into our test suite.
Of course, we're still not actually using vegetables,
so running `fpm test` after this step will still produce the same message.

In order to write a test, we're going to need something to test.
To start with, we'll use a relatively simple function as an example.

```Fortran
module is_leap_year_m
    implicit none
    private
    public :: is_leap_year
contains
    pure function is_leap_year(year)
        integer, intent(in) :: year
        logical :: is_leap_year

        if (mod(year, 4) == 0) then
            if (mod(year, 100) == 0) then
                if (mod(year, 400) == 0) then
                    is_leap_year = .true.
                else
                    is_leap_year = .false.
                end if
            else
                is_leap_year = .true.
            end if
        else
            is_leap_year = .false.
        end if
    end function
end module
```

Save this in a file `is_leap_year_m.f90` in the `src` folder,
and delete the file put there when we created the project.
Next, we need a file to hold our test.
In the `test` folder, create a file named `is_leap_year_test.f90`,
and start by creating a module in it with the same name.

```Fortran
module is_leap_year_test
    implicit none
end module
```

The first thing we need to do when creating a test is write the specification about what it's going to test.
We do this in a function in this module, like so.

```Fortran
module is_leap_year_test
    use vegetables, only: test_item_t

    implicit none
    private
    public :: test_is_leap_year
contains
    function test_is_leap_year() result(tests)
        type(test_item_t) :: tests
    end function
end module
```

This is the only thing that need be public from this module,
and the function will return this section of our test suite as a `test_item_t`.
To start defining this section of our test suite,
we will use the `describe` and `it` functions, like so.

```Fortran
module is_leap_year_test
    use vegetables, only: test_item_t, describe, it

    implicit none
    private
    public :: test_is_leap_year
contains
    function test_is_leap_year() result(tests)
        type(test_item_t) :: tests

        tests = describe(&
                "is_leap_year", &
                [ it( &
                        "returns false for years that are not divisible by 4", &
                        check_not_divisible_by_4) &
                ])
    end function
end module
```

The `it` function takes a description, and a function to call to perform the test, and returns a `test_item_t`.
The `describe` function takes a description, and an array of `test_item_t`s, and returns a `test_item_t`.
This facilitates, and even encourages, documenting the expected behavior of the code under test.

Note that we needed to provide a function to the `it` function,
so now we must write the actual test.
A test is a function that returns a test `result_t`, like so.

```Fortran
module is_leap_year_test
use is_leap_year_m, only: is_leap_year
    use vegetables, only: result_t, test_item_t, assert_not, describe, it

    implicit none
    private
    public :: test_is_leap_year
contains
    function test_is_leap_year() result(tests)
        type(test_item_t) :: tests

        tests = describe(&
                "is_leap_year", &
                [ it( &
                        "returns false for years that are not divisible by 4", &
                        check_not_divisible_by_4) &
                ])
    end function

    function check_not_divisible_by_4() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_leap_year(2002))
    end function
end module
```

The `assert_not` function checks that its argument is `.false.`,
and records the results in its return value.
We've now written our first test.
So how do we run it?

# Including Tests Into Your Test Suite

We could actually run the command `fpm test` at this point and our test will compiler.
But nothing is calling it, so it won't run.
We need to write a main program that will gather up all our tests and run them.
Luckily, I've written a tool that can generate such a program for you,
provided you follow the convention we used above.
Write your test suites in modules with names ending `_test`,
in files with the same names,
and return the sections of your test suite from functions that start with `test_`.

To get this tool, clone the repository `https://gitlab.com/everythingfunctional/make_vegetable_driver.git`.
Change directories into that repository and run `fpm install`.
This should compile and put the executable for the tool somewhere that is (hopefully) on your path.
You can specify where you'd like it to be installed with the `--prefix` and/or `--bindir` options to `fpm` if the defaults don't work for you.

Now change directories back to your tutorial project and run the command
`make_vegetable_driver test/main.f90 test/*_test.f90`.
This should create a main program like the following.

```Fortran
! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use is_leap_year_test, only: &
                is_leap_year_is_leap_year => test_is_leap_year
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = is_leap_year_is_leap_year()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
```

You should also delete the original program in the `test` folder.
Now run `fpm test` and you should see output like the following.

```txt
$ fpm test
Running Tests

Test that
    is_leap_year
        returns false for years that are not divisible by 4

A total of 1 test cases

All Passed
Took 8.464e-6 seconds

A total of 1 test cases containing a total of 1 assertions
```

Congratulations, you've written and executed your first vegetables test suite!
You can find the code at this stage [here](https://gitlab.com/everythingfunctional/vegetables_tutorial/-/tree/first_test).

# More Advanced Testing Patterns

Coming Soon!

## Providing Example Inputs to a Test

Coming Soon!

## Generating Random Inputs for a Test

Coming Soon!

## Inputs for a Whole Test Suite

Coming Soon!

## Modifying Inputs Before Passing to a Test

Coming Soon!
