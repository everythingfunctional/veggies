module Vegetables_m
    implicit none
    private

    type, public :: TestCase_t
        private
        character(len=:), allocatable :: description
        procedure(test), nopass, pointer :: case
    end type TestCase_t

    type, public :: TestCaseList_t
        private
        type(TestCase_t), allocatable :: cases(:)
    end type TestCaseList_t

    type, public :: TestSuite_t
        private
        character(len=:), allocatable :: suite_name
        type(TestCaseList_t) :: cases
    end type TestSuite_t

    type, public :: TestResult_t
        private
        logical :: passed
        character(len=:), allocatable :: message
    end type TestResult_t

    abstract interface
        function test() result(test_result)
            import TestResult_t

            type(TestResult_t) :: test_result
        end function test
    end interface

    interface assertEquals
        module procedure assertEqualsCharacter
    end interface

    public :: assertEquals, describe, it
contains
    pure function describe(suite_name, cases) result(test_suite)
        character(len=*), intent(in) :: suite_name
        type(TestCase_t), intent(in) :: cases(:)
        type(TestSuite_t) :: test_suite

        test_suite = TestSuite_t(suite_name, TestCaseList_t(cases))
    end function describe

    function it(description, case) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test) :: case
        type(TestCase_t) :: test_case

        test_case = TestCase_t(description, case)
    end function it

    pure function assertEqualsCharacter(expected, actual) result(test_result)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(TestResult_t) :: test_result

        character(len=:), allocatable :: message

        allocate(character(len=0)::message)

        if (expected == actual) then
            test_result = TestResult_t(.true., "")
        else
            message = formatMessage(expected, actual)
            test_result = TestResult_t(.false., message)
        end if
    end function assertEqualsCharacter

    pure function formatMessage(expected, actual) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=:), allocatable :: message

        message = "Expected: '" // expected // "' Actual: '" // actual // "'"
    end function formatMessage
end module Vegetables_m
