Vegetables
==========

[![pipeline status](https://gitlab.com/everythingfunctional/vegetables/badges/master/pipeline.svg)](https://gitlab.com/everythingfunctional/vegetables/commits/master)

For a healthier code base, eat your vegetables.

Vegetables is a Fortran testing framework written using functional programming
principles. As many of its procedures as possible are marked with the pure
keyword while still allowing the framework to test impure code. It's biggest
features are that it provides a readable, test/code specification as it's output
by default, and provides the option to output even the passing test results. It
makes writing tests in a Specification, BDD style as easy as possible, and has
all the features one would expect from a modern testing framework. It is also
flexible and extensible.

## Environment

In order to run this you will need:
* A reasonably recent version of gfortran
* The Fortran Package Manager (fpm)

Running the tests is then as simple as

```
git clone https://gitlab.com/everythingfunctional/vegetables.git
cd vegetables
fpm test
```

Using Vegetables
----------------

Using Vegetables is (almost) as easy as writing a specification. A great first
example are the tests for Vegetables themselves.

### Writing a Test Function

The simplest test function is one that takes no arguments and produces a
`Result_t` value as its output. The function some execute some portion of your
code, and make some assertion(s) about the result(s). Multiple assertsions can
be combined by using the `.and.` operator. Also, the `succeed` and `fail` functions
are provided if for some reason the provided assertions aren't quite sufficient.

The simplest example of a test function would be something like the following:

```Fortran
function simplest()
    use Vegetables_m, only: Result_t, succeed

    type(Result_t) :: simplest

    simplest = succeed("Successfully")
end function
```

Additionally, a test function can take a `class(Input_t)` value as an input.
Some simple types are provided that extend from `Input_t`, such as
`DoublePrecisionInput_t`, `IntegerInput_t` and `StringInput_t`. If you need other
inputs to your test, you can create a new type extended from `Input_t` to provide
whatever you need. A `select case` construct is then needed to make sure the right
type gets passed in at run time. An example of a test that takes an input is shown
below. The actual inputs passed to the test are determined by how it is included
into the test suite. Assembling the test suite is described below.

```Fortran
function inputTest(input) result(result_)
    use Vegetables_m, only: Input_t, IntegerInput_t, Result_t, assertEquals, fail

    class(Input_t), intent(in) :: input
    type(Result_t) :: result_

    select type (input)
    type is (IntegerInput_t)
        result_ = assertEquals(1, input%value_)
    class default
        result_ = fail("Didn't get an IntegerInput_t")
    end select
end function
```

### Assertions

An assertion is simply something to make sure that your code is behaving as
expected. There are multiple assert functions provided to check a variety of
types of values. They all return a value of type `Result_t` that can be returned
by your test function. Additionally, `Result_t` values can be combined using
the `.and.` operator to allow you to make multiple assertions within your
test and return the results of all of them. Additionally, the `succeed` and `fail`
functions are provided for situations where the provided assertions aren't quite
appropriate.

By convention, there are two final arguments to the assert functions which are
optional that represent an additional *user* message to be included. If both
arguments are included then the first is used in the case the assertion succeeds,
and the second is used in the case the assertion fails. If only one is provided,
then it is used whether the assertion passes or fails. Any argument of type
`character` can also accept a value of type `VARYING_STRING` from the
`iso_varying_string` module. The provided assertions are
list below:

* `assertThat` and `assertNot` accept a logical value and make sure it is either `.true.` or `.false.` respectively
* `assertEquals` accepts two values of integer, double precision, character, or VARYING_STRING, and ensures they are equal (Note that assertEquals with double precision values simply uses the assertEqualsWithinAbsolute with a tolerance of machine epsilon)
* `assertEqualsWithinAbsolute` and `assertEqualsWithinRelative` accept two values of double precision and a third value of double precision to use as the tolerance, and ensures the values are within that tolerance.
* `assertEmpty` ensures that the given string is of zero length
* `assertIncludes` and `assertDoesntInclude` ensure that the second string includes (or doesnt include) the first string
* `assertFasterThan` Has several variations. It accepts a subroutine with no arguments, and runs it the specified number of times to measure how long it takes to run. It then compares that to either a given number in seconds, or from running another provided subroutine and measuring it as well. Optionally, additional subroutines can be provided to be executed before and after the other subroutine(s) to function as setup and teardown, to avoid including that code in the measurement.

#### Writing Your Own Assertions

All of the code used to create the messages from the assertions is publicly callable.
The basic pattern used is:

```Fortran
if (some_condition) then
    result_ = succeed( &
            withUserMessage( &
                    makeSomeSuccessMessage(args), &
                    success_message))
else
    result_ = fail( &
            withUserMessage( &
                    makeSomeFailureMessage(args), &
                    failure_message))
end if
```

So if the provided assert functions don't quite do what you need, or don't accept
the type you need, it should be pretty easy to write your own, and have the style
of the message it produces match with the rest of them. The
[Quantities for Fortran](https://gitlab.com/everythingfunctional/quaff) is a great
example of where I've done just that.

### Assemble The Suite

Once you've written your test function, you'll need to include it into your
test suite. I've got a little tool in this repository that can be used
to do it, but you can also do it manually.

First, you'll need to write a function that defines a part of your test suite,
either spec or BDD style, using the provided functions `Describe` and `It` or
`Given`, `When` and `Then_`. If you're using the provided build system, then
this function should be in a module named `something`**`_test`**, in a file with the
same name. The function should be named **`test_`**`something`, and must take
no arguments, and return a value of type `TestItem_t`, which the above functions do.

The `Given`, `When` and `Describe` functions take a description string, and an
array of `TestItem_t`s. The `It` and `Then_` functions accept a string description,
and a function that takes no arguments and returns a `Result_t`.
The `It_` and `Then__` functions accept a function that takes one argument of
`class(Input_t)`. These are the
descriptions that are given in the output of Vegetables for each test.

The `Given`, `When` and `Describe` functions can also accept a `class(Input_t)`
value, to be passed to each of the tests they contain. Additionally, the `When`
function can accept a function from `class(Input_t)` to `type(Transformed_t)` (which
is just a wrapper for a `class(Input_t)` value) which will be called and then pass
its result down to the tests.

The `It` and `Then_` functions can also take an array of `Example_t`s (which is
just a wrapper for a `class(Input_t)`) which will each be passed to the test
function.  They can instead accept a value of `class(Generator_t)`, which will
be used to generate random values to be passed to the test. More information is
provided for `class(Generator_t)` below.

An example of a specification function would be as follows:

```Fortran
function test_assert_empty() result(tests)
    type(TestItem_t) :: tests

    type(TestItem_t) :: individual_tests(2)

    individual_tests(1) = it( &
            "passes with an empty string", &
            checkPassForEmptyChars)
    individual_tests(2) = it( &
            "fails with a non empty string", &
            checkFailsForNonemptyChars)
    tests = describe("assertEmpty", individual_tests)
end function test_assert_empty
```

The basic gist of this is that we are describing the requirements for the
`assertEmpty` function. The two requirements are that:

1. It passes with an empty string
2. It fails with a non-empty string

If you are using the provided build system, multiple **`test_`**`something` functions
can be provided within a module, and multiple `something`**`_test`** modules can be
provided in separate files. The build system will generate a driver program
that calls each **`test_`**`something` function it finds in order to build up the
test suite. It will then run all of the tests and report the results. The section
of the build system which generates this program can be used as a standalone
program that accepts as command line arguments the name of the generator program
and the list of files containing the tests, but it makes the same assumptions.

The generated driver program uses the function `testThat` to combine all of the
tests provided by the **`test_`**`something` functions into a single collection,
and then calls the subroutine `runTests` with that collection. So, even manually
writing and maintaining the driver program wouldn't be _too_ bad.

### Generating Random Inputs

By providing a type extended from `class(Generator_t)`, you can test fundamental
properties of your code that should hold for all values. A couple of types are
provided for simple types, but generally you'll want to provide your own. To do
so you'll need to overide the `generate` function. This function takes your
generator type as an input, and must produce a value of `type(Generated_t)`. This
is just a wrapper around a `class(Input_t)`. Several `getRandom*` functions are
provided to generate most primitive types with various ranges and/or lengths.

Additionally, you must overide the `shrink` function. This must take a
`class(Input_t)` value as input, and provide a `type(ShrinkResult_t)` as output.
This is just a wrapper around a `class(Input_t)`, with a flag for whether it is
the simplest possible value. The relavent code for one of the provided generators
is shown below.

```Fortran
type, public, extends(Generator_t) :: IntegerGenerator_t
contains
    private
    procedure, public :: generate => generateInteger
    procedure, nopass, public :: shrink => shrinkInteger
end type IntegerGenerator_t

function generateInteger(self) result(generated_value)
    class(IntegerGenerator_t), intent(in) :: self
    type(Generated_t) :: generated_value

    type(IntegerInput_t) :: the_input

    associate(a => self)
    end associate

    the_input%value_ = getRandomInteger()
    generated_value = Generated(the_input)
end function generateInteger

pure function shrinkInteger(input) result(shrunk)
    class(Input_t), intent(in) :: input
    type(ShrinkResult_t) :: shrunk

    type(IntegerInput_t) :: new_input

    select type (input)
    type is (IntegerInput_t)
        if (input%value_ == 0) then
            new_input%value_ = 0
            shrunk = SimplestValue(new_input)
        else
            new_input%value_ = input%value_ / 2
            shrunk = ShrunkValue(new_input)
        end if
    end select
end function shrinkInteger
```

When given a generator, the `generate` function will be called to generate up to
the number random values given on the command line (the default is 100). If the
test never fails, it passes. If the test fails on one of the inputs, then it will
successively call the `shrink` function with that input until either the test
passes again, or it gets to the simplest possible input. The last failing result
is then reported.

### The Command Line

The driver program accepts a handful of command line arguments for controlling
how the tests are run. This is done inside the `runTests` subroutine, so
even manually or otherwise generated driver programs can use this functionality.

```
Usage: tests_build/vegetable_driver [-h] [-q] [-v] [-f string] [-n num] [-c]
  options:
    -h, --help                    Output this message and exit
    -q, --quiet                   Don't print the test descriptions before
                                  running the tests
    -v, --verbose                 Print all of the assertion messages, not
                                  just the failing ones
    -f string, --filter string    Only run cases or collections whose
                                  description contains the given string
    -n num, --numrand num         Number of random values to use for each
                                  test with generated values (default = 100)
    -c, --color-off               Don't colorize the output
```

A common usage is to use the `-q`, `-v` and `-f` flags to run the tests you're
working on and make sure they're doing what you expect.
