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
        procedure, public :: getDescription => testCaseGetDescription
    end type TestCase_t

    type, public :: TestCaseList_t
        private
        type(TestCase_t), allocatable :: cases(:)
    contains
        private
        procedure, public :: length => TestCaseListLength
        procedure, public :: runCases
    end type TestCaseList_t

    type, public :: TestCollection_t
        private
        character(len=:), allocatable :: collection_name
        type(TestCaseList_t) :: cases
    contains
        private
        procedure, public :: numCases => TestCollectionNumCases
        procedure, public :: runCollection
    end type TestCollection_t

    type, public :: TestResult_t
        private
        logical :: passed
        character(len=:), allocatable :: message
    end type TestResult_t

    type, public :: TestCaseResult_t
        private
        character(len=:), allocatable :: description
        type(TestResult_t) :: test_result
    contains
        private
        procedure, public :: passed => TestCaseResultPassed
    end type TestCaseResult_t

    type, public :: TestCollectionResult_t
        private
        character(len=:), allocatable :: collection_name
        type(TestCaseResult_t), allocatable :: results(:)
    contains
        private
        procedure, public :: passed => TestCollectionResultPassed
    end type TestCollectionResult_t

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

    public :: assertEquals, assertThat, describe, it, executeEverything
contains
    pure function describe(collection_name, cases) result(test_collection)
        character(len=*), intent(in) :: collection_name
        type(TestCase_t), intent(in) :: cases(:)
        type(TestCollection_t) :: test_collection

        test_collection = TestCollection_t(collection_name, TestCaseList_t(cases))
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

    pure function assertThat(condition) result(test_result)
        logical, intent(in) :: condition
        type(TestResult_t) :: test_result

        test_result = TestResult_t(condition, "Wasn't True")
    end function assertThat

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

    pure function testCaseGetDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%description
    end function testCaseGetDescription

    pure function runCases(self) result(test_case_results)
        class(TestCaseList_t), intent(in) :: self
        type(TestCaseResult_t), allocatable :: test_case_results(:)

        if (allocated(self%cases)) then
            allocate(test_case_results(size(self%cases)))
            test_case_results = self%cases%runCase()
        end if
    end function runCases

    elemental function runCollection(self) result(test_collection_result)
        class(TestCollection_t), intent(in) :: self
        type(TestCollectionResult_t) :: test_collection_result

        test_collection_result = TestCollectionResult_t( &
                collection_name = self%collection_name, &
                results = self%cases%runCases())
    end function runCollection

    pure function integerToCharacter(num) result(string)
        integer, intent(in) :: num
        character(len=:), allocatable :: string

        character(len=12) :: temp

        write(temp, '(I0)') num
        string = trim(temp)
    end function integerToCharacter

    elemental function TestCollectionNumCases(self) result(num_cases)
        class(TestCollection_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%cases%length()
    end function TestCollectionNumCases

    pure function testCaseListLength(self) result(length)
        class(TestCaseList_t), intent(in) :: self
        integer :: length

        if (allocated(self%cases)) then
            length = size(self%cases)
        else
            length = 0
        end if
    end function testCaseListLength

    elemental function TestCaseResultPassed(self) result(passed)
        class(TestCaseResult_t), intent(in) :: self
        logical :: passed

        passed = self%test_result%passed
    end function TestCaseResultPassed

    elemental function TestCollectionResultPassed(self) result(passed)
        class(TestCollectionResult_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed())
    end function TestCollectionResultPassed

    subroutine executeEverything(test_collections)
        type(TestCollection_t), intent(in) :: test_collections(:)

        type(TestCollectionResult_t), allocatable :: test_collection_results(:)

        call reportTestsToRun(test_collections)

        allocate(test_collection_results(size(test_collections)))
        test_collection_results = test_collections%runCollection()

        call reportResults(test_collection_results)
    end subroutine

    subroutine reportTestsToRun(test_collections)
        use iso_fortran_env, only: output_unit

        type(TestCollection_t), intent(in) :: test_collections(:)

        integer :: num_collections
        integer :: num_tests

        num_collections = size(test_collections)
        num_tests = sum(test_collections%numCases())

        write(output_unit, '(A,I0,A,I0,A)') &
                "Running ", num_tests, " tests across ", num_collections, " collections"
    end subroutine

    subroutine reportResults(test_collection_results)
        use iso_fortran_env, only: error_unit, output_unit
        type(TestCollectionResult_t), intent(in) :: test_collection_results(:)

        integer :: num_collections

        num_collections = size(test_collection_results)
        if (all(test_collection_results%passed())) then
            write(output_unit, '(A,I0,A)') "All ", num_collections, " passed."
        else
            write(error_unit, '(A,I0,A,I0,A)') &
                    "Only ", count(test_collection_results%passed()), " of ", num_collections, " passed."
            stop 1
        end if
    end subroutine reportResults
end module Vegetables_m
