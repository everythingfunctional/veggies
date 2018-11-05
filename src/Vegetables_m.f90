module Vegetables_m
!   use cVegetables
    use iso_c_binding, only: c_bool, c_ptr

    implicit none
    private

    type, bind(c), public :: Result_t
        private
        type(c_ptr) :: contents
    end type Result_t

    type, public :: TestCase_t
        private
        type(c_ptr) :: contents
    contains
        procedure, public :: description => testCaseDescription
        procedure, public :: numCases => testCaseNumCases
        procedure, public :: run => runTestCase
    end type TestCase_t

    type, public :: TestCollection_t
        private
        type(c_ptr) :: contents
    contains
        private
        procedure, public :: description => testCollectionDescription
        procedure, public :: numCases => testCollectionNumCases
        procedure, public :: run => runTestCollection
    end type TestCollection_t

    type, public :: TestCaseResult_t
        private
        type(c_ptr) :: contents
    contains
        private
        procedure, public :: failureDescription => testCaseFailureDescription
        procedure, public :: numAsserts => testCaseNumAsserts
        procedure, public :: numCases => testCaseResultNumCases
        procedure, public :: numFailingAsserts => testCaseNumFailingAsserts
        procedure, public :: numPassingAsserts => testCaseNumPassingAsserts
        procedure, public :: passed => testCasePassed
        procedure, public :: verboseDescription => testCaseVerboseDescription
    end type TestCaseResult_t

    type, public :: TestCollectionResult_t
        private
        type(c_ptr) :: contents
    contains
        procedure, public :: numCases => testCollectionResultNumCases
        procedure, public :: passed => testCollectionPassed
        procedure, public :: verboseDescription => testCollectionVerboseDescription
    end type TestCollectionResult_t

    interface operator(.includes.)
        module procedure includes
    end interface

    interface
        subroutine cAddTest(collection, test) bind(C, name="cAddTest")
            use iso_c_binding, only: c_ptr

            type(c_ptr), value, intent(in) :: collection
            type(c_ptr), value, intent(in) :: test
        end subroutine cAddTest

        function cResult(message, passed) result(result_) bind(C, name="cResult")
            use iso_c_binding, only: c_bool, c_char, c_ptr

            character(len=1, kind=c_char), dimension(*), intent(in) :: message
            logical(kind=c_bool), value, intent(in) :: passed
            type(c_ptr) :: result_
        end function cResult

        function cTestCollection( &
                description) &
                result(test_collection) &
                bind(C, name="cTestCollection")
            use iso_c_binding, only: c_char, c_int, c_ptr

            character(len=1, kind=c_char), dimension(*), intent(in) :: description
            type(c_ptr) :: test_collection
        end function cTestCollection

        function test_() result(result_) bind(c)
            import Result_t

            type(Result_t) :: result_
        end function test_
    end interface

    logical(kind=c_bool), parameter :: CTRUE = .true.
    logical(kind=c_bool), parameter :: CFALSE = .false.

    interface operator(.and.)
        module procedure combineResults
    end interface

    interface assertEquals
        module procedure assertEqualsInteger
    end interface

    interface toString
        module procedure toStringInteger
    end interface toString

    public :: &
            operator(.and.), &
            assertEmpty, &
            assertEquals, &
            assertIncludes, &
            assertNot, &
            assertThat, &
            describe, &
            fail, &
            given, &
            it, &
            runATest, &
            runTests, &
            succeed, &
            testThat, &
            then, &
            when
contains
    function assertEmpty(string) result(result_)
        character(len=*), intent(in) :: string
        type(Result_t) :: result_

        if (string == "") then
            result_ = succeed()
        else
            result_ = fail("String '" // string // "' wasn't empty")
        end if
    end function assertEmpty

    function assertEqualsInteger(expected, actual) result(result_)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(Result_t) :: result_

        if (expected == actual) then
            result_ = succeed()
        else
            result_ = fail( &
                    "Expected '" // toString(expected) &
                    // "' but got '" // toString(actual) // "'")
        end if
    end function assertEqualsInteger

    function assertIncludes(search_for, string) result(result_)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result_

        if (string.includes.search_for) then
            result_ = succeed()
        else
            result_ = fail( &
                    "Expected '" // string &
                    // "' to include '" // search_for // "'")
        end if
    end function assertIncludes

    function assertNot(condition) result(result_)
        logical, intent(in) :: condition
        type(Result_t) :: result_

        if (.not.condition) then
            result_ = succeed()
        else
            result_ = fail("Expected to be FALSE")
        end if
    end function assertNot

    function assertThat(condition) result(result_)
        logical, intent(in) :: condition
        type(Result_t) :: result_

        if (condition) then
            result_ = succeed()
        else
            result_ = fail("Expected to be TRUE")
        end if
    end function assertThat

    function combineResults(lhs, rhs) result(combined)
        type(Result_t), intent(in) :: lhs
        type(Result_t), intent(in) :: rhs
        type(Result_t) :: combined

        interface
            function cCombineResults( &
                    lhs, rhs) result(combined) bind(C, name="cCombineResults")
                use iso_c_binding, only: c_ptr

                type(c_ptr), value, intent(in) :: lhs
                type(c_ptr), value, intent(in) :: rhs
                type(c_ptr) :: combined
            end function cCombineResults
        end interface

        combined%contents = cCombineResults(lhs%contents, rhs%contents)
    end function combineResults

    function cStringToF(c_string) result(f_string)
        character(len=*), intent(in) :: c_string
        character(len=:), allocatable :: f_string

        integer :: terminator_position

        terminator_position = index(c_string, char(0))
        if (terminator_position == 0) then
            f_string = c_string
        else
            f_string = c_string(1:terminator_position-1)
        end if
    end function cStringToF

    function describe(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestCase_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        integer :: i

        test_collection%contents = cTestCollection(fStringToC(description))
        do i = 1, size(tests)
            call cAddTest(test_collection%contents, tests(i)%contents)
        end do
    end function describe

    function fail(message) result(result_)
        character(len=*), intent(in) :: message
        type(Result_t) :: result_

        result_%contents = cResult(fStringToC(message), CFALSE)
    end function fail

    function fStringToC(f_string) result(c_string)
        character(len=*), intent(in) :: f_string
        character(len=:), allocatable :: c_string

        c_string = f_string // char(0)
    end function fStringToC

    function given(description, tests) result(collection)
        character(len=*), intent(in) :: description
        type(TestCollection_t), intent(in) :: tests(:)
        type(TestCollection_t) :: collection

        integer :: i

        collection%contents = cTestCollection(fStringToC("Given " // description))
        do i = 1, size(tests)
            call cAddTest(collection%contents, tests(i)%contents)
        end do
    end function given

    pure function includes(string, search_for)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: search_for
        logical :: includes

        includes = index(string, search_for) > 0
    end function includes

    function it(description, test) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: test
        type(TestCase_t) :: test_case

        interface
            function cTestCase(description, test) result(test_case) bind(C, name="cTestCase")
                use iso_c_binding, only: c_char, c_ptr

                character(len=1, kind=c_char), dimension(*), intent(in) :: description
                procedure(test_) :: test
                type(c_ptr) :: test_case
            end function cTestCase
        end interface

        test_case%contents = cTestCase(fStringToC(description), test)
    end function it

    function runATest(test) result(result_) bind(C, name="runATest")
        procedure(test_) :: test
        type(c_ptr) :: result_

        type(Result_t) :: test_result

        test_result = test()
        result_ = test_result%contents
    end function runATest

    function runTestCase(self) result(test_result)
        class(TestCase_t), intent(in) :: self
        type(TestCaseResult_t) :: test_result

        interface
            function cRunTestCase( &
                    test_case) &
                    result(result_) &
                    bind(C, name="cRunTestCase")
                use iso_c_binding, only: c_ptr

                type(c_ptr), value, intent(in) :: test_case
                type(c_ptr) :: result_
            end function cRunTestCase
        end interface

        test_result%contents = cRunTestCase(self%contents)
    end function runTestCase

    function runTestCollection(self) result(test_result)
        class(TestCollection_t), intent(in) :: self
        type(TestCollectionResult_t) :: test_result

        interface
            function cRunTestCollection( &
                    collection) &
                    result(results) &
                    bind(C, name="cRunTestCollection")
                use iso_c_binding, only: c_ptr

                type(c_ptr), value, intent(in) :: collection
                type(c_ptr) :: results
            end function cRunTestCollection
        end interface

        test_result%contents = cRunTestCollection(self%contents)
    end function runTestCollection

    subroutine runTests(tests)
        use iso_fortran_env, only: error_unit, output_unit
        type(TestCollection_t), intent(in) :: tests

        type(TestCollectionResult_t) :: test_results

        write(output_unit, '(A)') "Running Tests"
        write(output_unit, '(A)') tests%description()
        write(output_unit, '(A)') &
                "A total of " // toString(tests%numCases()) // " test cases"
        test_results = tests%run()
        if (test_results%passed()) then
            write(output_unit, '(A)') "All Passed"
        else
            write(error_unit, '(A)') "Failed"
            call exit(1)
        end if
    end subroutine runTests

    function succeed() result(result_)
        type(Result_t) :: result_

        result_%contents = cResult(fStringToC(""), CTRUE)
    end function succeed

    function testCaseDescription(self) result(description)
        use iso_c_binding, only: c_char

        class(TestCase_t), intent(in) :: self
        character(len=:), allocatable :: description

        interface
            subroutine cTestCaseDescription( &
                    test_case, &
                    description, &
                    max_length) &
                    bind(C, name="cTestCaseDescription")
                use iso_c_binding, only: c_char, c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                character(kind=c_char), dimension(*) :: description
                integer(kind=c_int), value, intent(in) :: max_length
            end subroutine cTestCaseDescription
        end interface

        integer, parameter :: MAX_STRING_LENGTH = 10000
        character(len=MAX_STRING_LENGTH, kind=c_char) :: description_

        call cTestCaseDescription(self%contents, description_, MAX_STRING_LENGTH)
        description = cStringToF(description_)
    end function testCaseDescription

    function testCaseFailureDescription(self) result(description)
        use iso_c_binding, only: c_char

        class(TestCaseResult_t), intent(in) :: self
        character(len=:), allocatable :: description

        interface
            subroutine cTestCaseResultFailureDescription( &
                    test_case, &
                    description, &
                    max_length) &
                    bind(C, name="cTestCaseResultFailureDescription")
                use iso_c_binding, only: c_char, c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                character(kind=c_char), dimension(*) :: description
                integer(kind=c_int), value, intent(in) :: max_length
            end subroutine cTestCaseResultFailureDescription
        end interface

        integer, parameter :: MAX_STRING_LENGTH = 10000
        character(len=MAX_STRING_LENGTH, kind=c_char) :: description_

        call cTestCaseResultFailureDescription(self%contents, description_, MAX_STRING_LENGTH)
        description = cStringToF(description_)
    end function testCaseFailureDescription

    function testCaseNumAsserts(self) result(num_asserts)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_asserts

        interface
            function cTestCaseNumAsserts( &
                    test_case) &
                    result(num_asserts) &
                    bind(C, name="cTestCaseNumAsserts")
                use iso_c_binding, only: c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                integer(kind=c_int) :: num_asserts
            end function cTestCaseNumAsserts
        end interface

        num_asserts = cTestCaseNumAsserts(self%contents)
    end function testCaseNumAsserts

    function testCaseNumCases(self) result(num_cases)
        class(TestCase_t), intent(in) :: self
        integer :: num_cases

        interface
            function cTestCaseNumCases( &
                    test_case) &
                    result(num_cases) &
                    bind(C, name="cTestCaseNumCases")
                use iso_c_binding, only: c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                integer(kind=c_int) :: num_cases
            end function cTestCaseNumCases
        end interface

        num_cases = cTestCaseNumCases(self%contents)
    end function

    function testCaseNumFailingAsserts(self) result(num_failing)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_failing

        interface
            function cTestCaseNumFailingAsserts( &
                    test_case) &
                    result(num_failing) &
                    bind(C, name="cTestCaseNumFailingAsserts")
                use iso_c_binding, only: c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                integer(kind=c_int) :: num_failing
            end function cTestCaseNumFailingAsserts
        end interface

        num_failing = cTestCaseNumFailingAsserts(self%contents)
    end function testCaseNumFailingAsserts

    function testCaseNumPassingAsserts(self) result(num_passing)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_passing

        interface
            function cTestCaseNumPassingAsserts( &
                    test_case) &
                    result(num_passing) &
                    bind(C, name="cTestCaseNumPassingAsserts")
                use iso_c_binding, only: c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                integer(kind=c_int) :: num_passing
            end function cTestCaseNumPassingAsserts
        end interface

        num_passing = cTestCaseNumPassingAsserts(self%contents)
    end function testCaseNumPassingAsserts

    function testCasePassed(self) result(passed)
        class(TestCaseResult_t), intent(in) :: self
        logical :: passed

        interface
            function cTestCasePassed( &
                    collection) &
                    result(passed) &
                    bind(C, name="cTestCasePassed")
                use iso_c_binding, only: c_bool, c_ptr

                type(c_ptr), value, intent(in) :: collection
                logical(kind=c_bool) :: passed
            end function cTestCasePassed
        end interface

        passed = cTestCasePassed(self%contents)
    end function testCasePassed

    function testCaseResultNumCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        interface
            function cTestCaseResultNumCases( &
                    test_case) &
                    result(num_cases) &
                    bind(C, name="cTestCaseResultNumCases")
                use iso_c_binding, only: c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                integer(kind=c_int) :: num_cases
            end function cTestCaseResultNumCases
        end interface

        num_cases = cTestCaseResultNumCases(self%contents)
    end function testCaseResultNumCases

    function testCaseVerboseDescription(self) result(description)
        use iso_c_binding, only: c_char

        class(TestCaseResult_t), intent(in) :: self
        character(len=:), allocatable :: description

        interface
            subroutine cTestCaseResultVerboseDescription( &
                    test_case, &
                    description, &
                    max_length) &
                    bind(C, name="cTestCaseResultVerboseDescription")
                use iso_c_binding, only: c_char, c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                character(kind=c_char), dimension(*) :: description
                integer(kind=c_int), value, intent(in) :: max_length
            end subroutine cTestCaseResultVerboseDescription
        end interface

        integer, parameter :: MAX_STRING_LENGTH = 10000
        character(len=MAX_STRING_LENGTH, kind=c_char) :: description_

        call cTestCaseResultVerboseDescription(self%contents, description_, MAX_STRING_LENGTH)
        description = cStringToF(description_)
    end function testCaseVerboseDescription

    function testCollectionDescription(self) result(description)
        use iso_c_binding, only: c_char

        class(TestCollection_t), intent(in) :: self
        character(len=:), allocatable :: description

        interface
            subroutine cTestCollectionDescription( &
                    test_collection, &
                    description, &
                    max_length) &
                    bind(C, name="cTestCollectionDescription")
                use iso_c_binding, only: c_char, c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_collection
                character(kind=c_char), dimension(*) :: description
                integer(kind=c_int), value, intent(in) :: max_length
            end subroutine cTestCollectionDescription
        end interface

        integer, parameter :: MAX_STRING_LENGTH = 10000
        character(len=MAX_STRING_LENGTH, kind=c_char) :: description_

        call cTestCollectionDescription( &
                self%contents, description_, MAX_STRING_LENGTH)
        description = cStringToF(description_)
    end function testCollectionDescription

    function testCollectionNumCases(self) result(num_cases)
        class(TestCollection_t), intent(in) :: self
        integer :: num_cases

        interface
            function cTestCollectionNumCases( &
                    collection) &
                    result(num_cases) &
                    bind(C, name="cTestCollectionNumCases")
                use iso_c_binding, only: c_int, c_ptr

                type(c_ptr), value, intent(in) :: collection
                integer(kind=c_int) :: num_cases
            end function cTestCollectionNumCases
        end interface

        num_cases = cTestCollectionNumCases(self%contents)
    end function testCollectionNumCases

    function testCollectionPassed(self) result(passed)
        class(TestCollectionResult_t), intent(in) :: self
        logical :: passed

        interface
            function cTestCollectionPassed( &
                    collection) &
                    result(passed) &
                    bind(C, name="cTestCollectionPassed")
                use iso_c_binding, only: c_bool, c_ptr

                type(c_ptr), value, intent(in) :: collection
                logical(kind=c_bool) :: passed
            end function cTestCollectionPassed
        end interface

        passed = cTestCollectionPassed(self%contents)
    end function testCollectionPassed

    function testCollectionVerboseDescription(self) result(description)
        use iso_c_binding, only: c_char

        class(TestCollectionResult_t), intent(in) :: self
        character(len=:), allocatable :: description

        interface
            subroutine cTestCollectionResultVerboseDescription( &
                    test_case, &
                    description, &
                    max_length) &
                    bind(C, name="cTestCollectionResultVerboseDescription")
                use iso_c_binding, only: c_char, c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                character(kind=c_char), dimension(*) :: description
                integer(kind=c_int), value, intent(in) :: max_length
            end subroutine cTestCollectionResultVerboseDescription
        end interface

        integer, parameter :: MAX_STRING_LENGTH = 10000
        character(len=MAX_STRING_LENGTH, kind=c_char) :: description_

        call cTestCollectionResultVerboseDescription( &
                self%contents, description_, MAX_STRING_LENGTH)
        description = cStringToF(description_)
    end function testCollectionVerboseDescription

    function testCollectionResultNumCases(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        interface
            function cTestCollectionResultNumCases( &
                    test_case) &
                    result(num_cases) &
                    bind(C, name="cTestCollectionResultNumCases")
                use iso_c_binding, only: c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                integer(kind=c_int) :: num_cases
            end function cTestCollectionResultNumCases
        end interface

        num_cases = cTestCollectionResultNumCases(self%contents)
    end function testCollectionResultNumCases

    function testThat(tests) result(test_collection)
        type(TestCollection_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        integer :: i

        test_collection%contents = cTestCollection(fStringToC("Test that"))
        do i = 1, size(tests)
            call cAddTest(test_collection%contents, tests(i)%contents)
        end do
    end function testThat

    function then(description, test) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: test
        type(TestCase_t) :: test_case

        test_case = it("Then " // description, test)
    end function then

    function toStringInteger(int) result(string)
        integer, intent(in) :: int
        character(len=:), allocatable :: string

        character(len=12) :: temp

        write(temp, '(I0)') int
        string = trim(temp)
    end function toStringInteger

    function when(description, tests) result(collection)
        character(len=*), intent(in) :: description
        type(TestCase_t), intent(in) :: tests(:)
        type(TestCollection_t) :: collection

        collection = describe("When " // description, tests)
    end function when
end module Vegetables_m
