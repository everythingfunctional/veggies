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

# Writing Your First Test

# Including Tests Into Your Test Suite

# More Advanced Testing Patterns

## Providing Example Inputs to a Test

## Generating Random Inputs for a Test

## Inputs for a Whole Test Suite

## Modifying Inputs Before Passing to a Test
