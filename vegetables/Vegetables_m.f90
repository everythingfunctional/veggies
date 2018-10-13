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
    end type TestResult_t

    abstract interface
        function test() result(test_result)
            import TestResult_t

            type(TestResult_t) :: test_result
        end function test
    end interface

    public :: describe, it
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
end module Vegetables_m
