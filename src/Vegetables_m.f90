module Vegetables_m
    implicit none
    private

    type :: VegetableString_t
        private
        character(len=:), allocatable :: string
    contains
        private
        generic, public :: WRITE(FORMATTED) => stringWrite
        procedure :: stringWrite
    end type VegetableString_t

    type, abstract, public :: Test_t
        private
        type(VegetableString_t) :: description_
    contains
        private
        procedure(description_), pass(self), public, deferred :: description
        procedure(run_), pass(self), public, deferred :: run
    end type Test_t

    type, abstract, public :: TestResult_t
    contains
        private
        procedure(statNum), pass(self), public, deferred :: numCases
        procedure(passed_), pass(self), public, deferred :: passed
    end type TestResult_t

    abstract interface
        pure function description_(self)
            import Test_t, VegetableString_t

            class(Test_t), intent(in) :: self
            type(VegetableString_t) :: description_
        end function description_

        pure function run_(self) result(test_result)
            import Test_t, TestResult_t

            class(Test_t), intent(in) :: self
            class(TestResult_t), allocatable :: test_result
        end function run_

        pure function passed_(self)
            import TestResult_t

            class(TestResult_t), intent(in) :: self
            logical :: passed_
        end function passed_

        pure function statNum(self) result(num)
            import TestResult_t

            class(TestResult_t), intent(in) :: self
            integer :: num
        end function statNum
    end interface

    type, public, extends(Test_t) :: TestCase_t
        private
        procedure(test_), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: run => runTestCase
    end type TestCase_t

    type, public, extends(TestResult_t) :: TestCaseResult_t
    contains
        private
        procedure, public :: numCases => testCaseNumCases
        procedure, public :: passed => testCasePassed
    end type TestCaseResult_t

    type :: Result_t
    end type Result_t

    interface
        pure function test_() result(result)
            import Result_t

            type(Result_t) :: result
        end function test_
    end interface

    public :: runTests, SUCCESSFUL, TODO
contains
    subroutine runTests(tests)
        use iso_fortran_env, only: output_unit

        class(Test_t) :: tests

        integer :: num_cases
        class(TestResult_t), allocatable :: test_result

        write(output_unit, *) "Running Tests"
        write(output_unit, *) tests%description()
        test_result = tests%run()
        if (test_result%passed()) then
            num_cases = test_result%numCases()
            call writePassingReport(num_cases)
        end if
    end subroutine

    subroutine writePassingReport(num_cases)
        use iso_fortran_env, only: output_unit

        integer, intent(in) :: num_cases

        write(output_unit, *) "All ", num_cases, " Passed"
    end subroutine writePassingReport

    pure function SUCCESSFUL() result(test_case)
        type(TestCase_t) :: test_case

        test_case = SUCCEEDS()
    end function SUCCESSFUL

    pure function SUCCEEDS()
        type(TestCase_t) :: SUCCEEDS

        SUCCEEDS = TestCase_t(description_ = toString("SUCCEEDS"), test = succeed)
    end function SUCCEEDS

    pure function succeed() result(test_result)
        type(Result_t) :: test_result

        test_result = Result_t()
    end function succeed

    pure function TODO() result(test_case)
        type(TestCase_t) :: test_case

        test_case = TestCase_t(description_ = toString("TODO"), test = fail)
    end function TODO

    pure function fail() result(test_result)
        type(Result_t) :: test_result

        test_result = Result_t()
    end function fail

    pure function toString(string_in) result(string_out)
        character(len=*), intent(in) :: string_in
        type(VegetableString_t) :: string_out

        string_out = VegetableString_t(string_in)
    end function toString

    subroutine stringWrite(string, unit, iotype, v_list, iostat, iomsg)
        class(VegetableString_t), intent(in) :: string
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        associate(a => iotype, b => v_list); end associate

        write(unit=unit, iostat=iostat, iomsg=iomsg, fmt='(A)') string%string
    end subroutine stringWrite

    pure function testCaseDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = self%description_
    end function testCaseDescription

    pure function runTestCase(self) result(test_result)
        class(TestCase_t), intent(in) :: self
        class(TestResult_t), allocatable :: test_result

        associate(a => self); end associate
        test_result = TestCaseResult_t()
    end function runTestCase

    pure function testCaseNumCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self); end associate
        num_cases = 1
    end function testCaseNumCases

    pure function testCasePassed(self) result(passed)
        class(TestCaseResult_t), intent(in) :: self
        logical :: passed

        associate(a => self); end associate
        passed = .true.
    end function testCasePassed
end module Vegetables_m
