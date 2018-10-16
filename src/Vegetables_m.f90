module Vegetables_m
    implicit none
    private

    type, abstract, public :: Test_t
    end type Test_t

    public :: runTests
contains
    subroutine runTests(tests)
        class(Test_t) :: tests

        associate(a => tests); end associate

        print *, "Running Tests"
    end subroutine
end module Vegetables_m
