module Vegetables_m
    implicit none
    private

    type, abstract, public :: Test_t
    end type Test_t

    type, public, extends(Test_t) :: TestCase_t
    end type TestCase_t

    type(TestCase_t), parameter :: SUCCEEDS = TestCase_t()

    public :: runTests, SUCCESSFUL, TODO
contains
    subroutine runTests(tests)
        class(Test_t) :: tests

        associate(a => tests); end associate

        print *, "Running Tests"
    end subroutine

    pure function SUCCESSFUL() result(test_case)
        type(TestCase_t) :: test_case

        test_case = SUCCEEDS
    end function SUCCESSFUL

    pure function TODO() result(test_case)
        type(TestCase_t) :: test_case

        test_case = TestCase_t()
    end function TODO
end module Vegetables_m
