module Vegetables_m
    implicit none
    private

    type, public :: VegetableString_t
        private
        character(len=:), allocatable :: string
    contains
        private
        generic, public :: operator(//) => &
                concatCharsAndString, concatStringAndChars, concatStrings
        procedure, pass(string) :: concatCharsAndString
        procedure :: concatStringAndChars
        procedure :: concatStrings
        generic, public :: operator(.includes.) => stringIncludesString
        procedure :: stringIncludesString
    end type VegetableString_t

    type, public :: Result_t
        private
        type(VegetableString_t) :: message
        integer :: num_failling_asserts
        integer :: num_passing_asserts
        logical :: passed
    contains
        private
        generic, public :: operator(.and.) => combineResults
        procedure :: combineResults
    end type Result_t

    abstract interface
        function test_() result(result_)
            import :: Result_t
            type(Result_t) :: result_
        end function
    end interface

    type, abstract, public :: Test_t
        private
        type(VegetableString_t) :: description_
    end type Test_t

    type, public :: TestItem_t
        private
        class(Test_t), pointer :: test => null()
    contains
        private
        procedure, public :: run => runTestItem
    end type TestItem_t

    type, extends(Test_t), public :: TestCase_t
        private
        procedure(test_), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: run => runCase
        procedure, public :: numCases => testCaseNumCases
    end type TestCase_t

    type, extends(Test_t), public :: TestCollection_t
        private
        type(TestItem_t), allocatable :: tests(:)
    contains
        private
        procedure, public :: run => runCollection
    end type TestCollection_t

    type, abstract, public :: TestResult_t
        private
        type(VegetableString_t) :: description
    end type TestResult_t

    type, public :: TestResultItem_t
        private
        class(TestResult_t), pointer :: result_ => null()
    end type TestResultItem_t

    type, extends(TestResult_t), public :: TestCaseResult_t
        private
        type(Result_t) :: result_
    end type TestCaseResult_t

    type, extends(TestResult_t), public :: TestCollectionResult_t
        private
        type(TestResultItem_t), allocatable :: results(:)
    end type TestCollectionResult_t

    interface assertEquals
        module procedure assertEqualsInteger
    end interface assertEquals

    interface assertIncludes
        module procedure assertStringIncludesChars
        module procedure assertStringIncludesString
    end interface assertIncludes

    interface fail
        module procedure failWithChars
        module procedure failWithString
    end interface fail

    interface succeed
        module procedure succeedWithChars
        module procedure succeedWithString
    end interface succeed

    character(len=*), parameter :: NEWLINE = NEW_LINE('A')

    public :: &
            assertEquals, &
            assertIncludes, &
            describe, &
            fail, &
            it, &
            runTests, &
            succeed, &
            TestCase, &
            testThat, &
            toString
contains
    function assertEqualsInteger(expected, actual) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed("Expected and got")
        else
            result__ = fail("Expected but got")
        end if
    end function assertEqualsInteger

    function assertStringIncludesChars(search_for, string) result(result__)
        character(len=*), intent(in) :: search_for
        type(VegetableString_t), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes(toString(search_for), string)
    end function assertStringIncludesChars

    function assertStringIncludesString(search_for, string) result(result__)
        type(VegetableString_t), intent(in) :: search_for
        type(VegetableString_t), intent(in) :: string
        type(Result_t) :: result__

        if (string.includes.search_for) then
            result__ = succeed("'" // string // "' included '" // search_for // "'")
        else
            result__ = fail("Expected '" // string // "' to include '" // search_for // "'")
        end if
    end function assertStringIncludesString

    function combineResults(lhs, rhs) result(combined)
        class(Result_t), intent(in) :: lhs
        type(Result_t), intent(in) :: rhs
        type(Result_t) :: combined

        combined = Result_( &
                message = lhs%message // NEWLINE // rhs%message, &
                passed = lhs%passed .and. rhs%passed, &
                num_failling_asserts = lhs%num_failling_asserts + rhs%num_failling_asserts, &
                num_passing_asserts = lhs%num_passing_asserts + rhs%num_passing_asserts)
    end function combineResults

    function concatCharsAndString(chars, string) result(combined)
        character(len=*), intent(in) :: chars
        class(VegetableString_t), intent(in) :: string
        type(VegetableString_t) :: combined

        combined = toString(chars // string%string)
    end function concatCharsAndString

    function concatStringAndChars(string, chars) result(combined)
        class(VegetableString_t), intent(in) :: string
        character(len=*), intent(in) :: chars
        type(VegetableString_t) :: combined

        combined = toString(string%string // chars)
    end function concatStringAndChars

    function concatStrings(lhs, rhs) result(combined)
        class(VegetableString_t), intent(in) :: lhs
        type(VegetableString_t), intent(in) :: rhs
        type(VegetableString_t) :: combined

        combined = toString(lhs%string // rhs%string)
    end function concatStrings

    function failWithChars(message) result(failure)
        character(len=*), intent(in) :: message
        type(Result_t) :: failure

        failure = fail(toString(message))
    end function failWithChars

    function failWithString(message) result(failure)
        type(VegetableString_t), intent(in) :: message
        type(Result_t) :: failure

        failure = Result_( &
                passed = .false., &
                message = message, &
                num_failling_asserts = 1, &
                num_passing_asserts = 0)
    end function failWithString

    function describe(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        allocate(TestCollection_t :: test_collection%test)
        select type (test => test_collection%test)
        type is (TestCollection_t)
            test = TestCollection(description, tests)
        end select
    end function describe

    function it(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: func
        type(TestItem_t) :: test_case

        allocate(TestCase_t :: test_case%test)
        select type (test => test_case%test)
        type is (TestCase_t)
            test = TestCase(description, func)
        end select
    end function it

    function Result_(passed, message, num_failling_asserts, num_passing_asserts)
        logical, intent(in) :: passed
        type(VegetableString_t), intent(in) :: message
        integer, intent(in) :: num_failling_asserts
        integer, intent(in) :: num_passing_asserts
        type(Result_t) :: Result_

        Result_ = Result_t( &
                message = message, &
                num_failling_asserts = num_failling_asserts, &
                num_passing_asserts = num_passing_asserts, &
                passed = passed)
    end function Result_

    function runCase(self) result(result__)
        class(TestCase_t), intent(in) :: self
        type(TestCaseResult_t) :: result__

        result__ = TestCaseResult(self%description_, self%test())
    end function runCase

    function runCollection(self) result(result__)
        class(TestCollection_t), intent(in) :: self
        type(TestCollectionResult_t) :: result__

        integer :: i
        integer :: num_tests
        type(TestResultItem_t), allocatable :: results(:)

        num_tests = size(self%tests)
        allocate(results(num_tests))
        do i = 1, num_tests
            results(i) = self%tests(i)%run()
        end do
        result__ = TestCollectionResult(self%description_, results)
    end function runCollection

    function runTestItem(self) result(result_item)
        class(TestItem_t), intent(in) :: self
        type(TestResultItem_t) :: result_item

        select type (test => self%test)
        type is (TestCase_t)
            allocate(TestCaseResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCaseResult_t)
                result_ = test%run()
            end select
        type is (TestCollection_t)
            allocate(TestCollectionResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCollectionResult_t)
                result_ = test%run()
            end select
        end select
    end function runTestItem

    subroutine runTests(tests)
        type(TestItem_t) :: tests
        type(TestResultItem_t) :: results

        results = tests%run()
    end subroutine

    function stringIncludesString(string, search_for)
        class(VegetableString_t), intent(in) :: string
        type(VegetableString_t), intent(in) :: search_for
        logical :: stringIncludesString

        stringIncludesString = index(string%string, search_for%string) > 0
    end function stringIncludesString

    function succeedWithChars(message) result(success)
        character(len=*), intent(in) :: message
        type(Result_t) :: success

        success = succeed(toString(message))
    end function succeedWithChars

    function succeedWithString(message) result(success)
        type(VegetableString_t), intent(in) :: message
        type(Result_t) :: success

        success = Result_( &
                passed = .true., &
                message = message, &
                num_failling_asserts = 0, &
                num_passing_asserts = 1)
    end function succeedWithString

    function TestCase(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: func
        type(TestCase_t) :: test_case

        test_case%description_ = toString(description)
        test_case%test => func
    end function TestCase

    function TestCaseDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = self%description_
    end function TestCaseDescription

    function TestCaseNumCases(self) result(num_cases)
        class(TestCase_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function TestCaseNumCases

    function TestCaseResult(description, result__) result(test_case_result)
        type(VegetableString_t), intent(in) :: description
        type(Result_t), intent(in) :: result__
        type(TestCaseResult_t) :: test_case_result

        test_case_result%description = description
        test_case_result%result_ = result__
    end function TestCaseResult

    function TestCollection(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        test_collection%description_ = toString(description)
        allocate(test_collection%tests(size(tests)))
        test_collection%tests = tests
    end function TestCollection

    function TestCollectionResult(description, results) result(test_collection_result)
        type(VegetableString_t), intent(in) :: description
        type(TestResultItem_t), intent(in) :: results(:)
        type(TestCollectionResult_t) :: test_collection_result

        test_collection_result%description = description
        allocate(test_collection_result%results(size(results)))
        test_collection_result%results = results
    end function TestCollectionResult

    function testThat(tests) result(test_collection)
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        allocate(TestCollection_t :: test_collection%test)
        select type (test => test_collection%test)
        type is (TestCollection_t)
            test = TestCollection("Test that", tests)
        end select
    end function testThat

    function toString(chars) result(string)
        character(len=*), intent(in) :: chars
        type(VegetableString_t) :: string

        string%string = chars
    end function toString
end module Vegetables_m
