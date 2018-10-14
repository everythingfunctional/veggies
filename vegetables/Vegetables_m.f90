module Vegetables_m
    implicit none
    private

    type, public :: TestCase_t
        private
        character(len=:), allocatable :: description
        procedure(test), nopass, pointer :: case_
    contains
        private
        procedure, public :: runCase
    end type TestCase_t

    type, public :: TestCaseList_t
        private
        type(TestCase_t), allocatable :: cases(:)
    contains
        private
        procedure, public :: length => TestCaseListLength
        procedure, public :: runCases
    end type TestCaseList_t

    type, public :: TestSuite_t
        private
        character(len=:), allocatable :: suite_name
        type(TestCaseList_t) :: cases
    contains
        private
        procedure, public :: numCases => testSuiteNumCases
        procedure, public :: runSuite
    end type TestSuite_t

    type, public :: TestResult_t
        private
        logical :: passed
        character(len=:), allocatable :: message
    end type TestResult_t

    type, public :: TestCaseResult_t
        private
        character(len=:), allocatable :: description
        type(TestResult_t) :: test_result
    end type TestCaseResult_t

    type, public :: TestSuiteResult_t
        private
        character(len=:), allocatable :: suite_name
        type(TestCaseResult_t), allocatable :: results(:)
    end type TestSuiteResult_t

    abstract interface
        pure function test() result(test_result)
            import TestResult_t

            type(TestResult_t) :: test_result
        end function test
    end interface

    interface assertEquals
        module procedure assertEqualsCharacter
        module procedure assertEqualsInteger
    end interface

    interface toCharacter
        module procedure integerToCharacter
    end interface

    public :: assertEquals, describe, it
contains
    pure function describe(suite_name, cases) result(test_suite)
        character(len=*), intent(in) :: suite_name
        type(TestCase_t), intent(in) :: cases(:)
        type(TestSuite_t) :: test_suite

        test_suite = TestSuite_t(suite_name, TestCaseList_t(cases))
    end function describe

    pure function it(description, case) result(test_case)
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

    pure function assertEqualsInteger(expected, actual) result(test_result)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(TestResult_t) :: test_result

        character(len=:), allocatable :: message

        allocate(character(len=0)::message)

        if (expected == actual) then
            test_result = TestResult_t(.true., "")
        else
            message = formatMessage(toCharacter(expected), toCharacter(actual))
            test_result = TestResult_t(.false., message)
        end if
    end function assertEqualsInteger

    pure function formatMessage(expected, actual) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=:), allocatable :: message

        message = "Expected: '" // expected // "' Actual: '" // actual // "'"
    end function formatMessage

    elemental function runCase(self) result(test_case_result)
        class(TestCase_t), intent(in) :: self
        type(TestCaseResult_t) :: test_case_result

        test_case_result = TestCaseResult_t( &
                description = self%description, &
                test_result = self%case_())
    end function runCase

    pure function runCases(self) result(test_case_results)
        class(TestCaseList_t), intent(in) :: self
        type(TestCaseResult_t), allocatable :: test_case_results(:)

        if (allocated(self%cases)) then
            allocate(test_case_results(size(self%cases)))
            test_case_results = self%cases%runCase()
        end if
    end function runCases

    elemental function runSuite(self) result(test_suite_result)
        class(TestSuite_t), intent(in) :: self
        type(TestSuiteResult_t) :: test_suite_result

        test_suite_result = TestSuiteResult_t( &
                suite_name = self%suite_name, &
                results = self%cases%runCases())
    end function runSuite

    pure function integerToCharacter(num) result(string)
        integer, intent(in) :: num
        character(len=:), allocatable :: string

        character(len=12) :: temp

        write(temp, '(I0)') num
        string = trim(temp)
    end function integerToCharacter

    elemental function testSuiteNumCases(self) result(num_cases)
        class(TestSuite_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%cases%length()
    end function testSuiteNumCases

    pure function testCaseListLength(self) result(length)
        class(TestCaseList_t), intent(in) :: self
        integer :: length

        if (allocated(self%cases)) then
            length = size(self%cases)
        else
            length = 0
        end if
    end function testCaseListLength
end module Vegetables_m
