Vegetables
==========

[![pipeline status](https://gitlab.com/everythingfunctional/vegetables/badges/master/pipeline.svg)](https://gitlab.com/everythingfunctional/vegetables/commits/master)

For a healthier code base, eat your vegetables.

## Environment

In order to run this you will need:
* A reasonably recent version of gcc and gfortran
* The Haskell Stack tool, with packages MissingH, shake and split installed
  * I.e. `stack install MissingH shake split`

Running the tests is then as simple as

```
git clone https://gitlab.com/everythingfunctional/vegetables.git
cd vegetables
./shake.sh
```

Using Vegetables
----------------

Using Vegetables is (almost) as easy as writing a specification. A great first
example or demo project can be found [here](https://gitlab.com/everythingfunctional/freshfizzbuzz),
but the tests for Vegetables are a pretty good example as well.

First, you'll need to write a function that defines a part of your test suite,
either spec or BDD style use the provided functions `describe` and `it` or
`Given`, `When` and `Then`. If you're using the provided build system, then
this function should be in a module name something*_test*, in a file with the
same name. The function should be named *test_*something, and must take
no arguments, and return a value of type `TestItem_t`, which the above functions do.

The `Given`, `When` and `describe` functions take a description string, and a
list of `TestItem_t`s. They can also accept a value, to be passed to each of
the tests they contain. This value will be passed as a `class(*)`. This is a
limitation of Fortran's type system, but is easily enough overcome with a
`select type` construct. Additionally, the `When` function can accept a function
from `class(*)` to `Transformed_t` (which is just a wrapper for a `class(*)` value)
which will be called before passing the input value down the tests.

The `it` and `Then` functions accept a string description, and a function that
takes no arguments and returns a `Result_t`. Additionally, the functions `it_`
and `Then_` can be used to provide a function that accepts a `class(*)` value
as input.

A variety of `assert` functions are provided to generate the `Result_t` values,
and these values can be combined using the `.and.` operator. The available
`assert` functions, which do what you would expect are, `assertThat`, `assertNot`,
`assertIncludes`, `assertDoesntInclude`, `assertEmpty`, `assertEquals`,
`assertEqualsWithinAbsolute`, and `assertEqualsWithinRelative`. These functions
also optionally accept one or two strings to be used as custom messages. If two
are provided, the first is used in the case of a success, and the second in the
case of a failure. If only one is provided, then it is used in either case.

If you are using the provided build system, multiple *test_*something functions
can be provided within a module, and multiple something*_test* modules can be
provided in separate files. The build system will generate a driver program
that calls each *test_*something function it finds in order to build up the
test suite. It will then run all of the tests and report the results. The section
of the build system which generates this program can be used as a standalone
program that accepts as command line arguments the name of the generator program
and the list of files containing the tests, but it makes the same assumptions.
