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
        generic, public :: WRITE(FORMATTED) => stringWriteFormatted
        procedure :: stringWriteFormatted
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

    type, abstract, public :: Test_t
        private
        type(VegetableString_t) :: description_
    contains
        private
        procedure(testDescription), deferred, public :: description
        procedure(testNum), deferred, public :: numCases
    end type Test_t

    abstract interface
        function test_() result(result_)
            import :: Result_t
            type(Result_t) :: result_
        end function

        function testDescription(self) result(description)
            import :: Test_t, VegetableString_t
            class(Test_t), intent(in) :: self
            type(VegetableString_t) :: description
        end function testDescription

        function testNum(self) result(num)
            import :: Test_t
            class(Test_t), intent(in) :: self
            integer :: num
        end function testNum
    end interface

    type, public :: TestItem_t
        private
        class(Test_t), allocatable :: test
    contains
        private
        procedure, public :: description => testItemDescription
        procedure, public :: numCases => testItemNumCases
        procedure, public :: run => runTestItem
    end type TestItem_t

    type, extends(Test_t), public :: TestCase_t
        private
        procedure(test_), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: numCases => testCaseNumCases
        procedure, public :: run => runCase
    end type TestCase_t

    type, extends(Test_t), public :: TestCollection_t
        private
        type(TestItem_t), allocatable :: tests(:)
    contains
        private
        procedure, public :: description => testCollectionDescription
        procedure, public :: numCases => testCollectionNumCases
        procedure, public :: run => runCollection
    end type TestCollection_t

    type, abstract, public :: TestResult_t
        private
        type(VegetableString_t) :: description
    end type TestResult_t

    type, public :: TestResultItem_t
        private
        class(TestResult_t), pointer :: result_
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

    interface join
        module procedure joinWithCharacter
        module procedure joinWithString
    end interface

    interface splitAt
        module procedure splitAtBothCharacter
        module procedure splitAtStringCharacter
    end interface

    interface succeed
        module procedure succeedWithChars
        module procedure succeedWithString
    end interface succeed

    interface toString
        module procedure charsToString
        module procedure integerToString
    end interface toString

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
            TestCollection, &
            testThat, &
            toString
contains
    function assertEqualsInteger(expected, actual) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed("Expected and got '" // toString(expected) // "'")
        else
            result__ = fail( &
                    "Expected '" // toString(expected) &
                    // "' but got '" // toString(actual) // "'")
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
            result__ = succeed( &
                    "'" // string // "' included '" // search_for // "'")
        else
            result__ = fail( &
                    "Expected '" // string &
                    // "' to include '" // search_for // "'")
        end if
    end function assertStringIncludesString

    function charsToString(chars) result(string)
        character(len=*), intent(in) :: chars
        type(VegetableString_t) :: string

        string%string = chars
    end function charsToString

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

    function hangingIndent(string_) result(indented)
        type(VegetableString_t), intent(in) :: string_
        type(VegetableString_t), allocatable :: indented

        type(VegetableString_t), allocatable :: lines(:)

        lines = splitAt(string_, NEWLINE)
        indented = join(lines, NEWLINE // "    ")
    end function hangingIndent

    function integerToString(int) result(string)
        integer, intent(in) :: int
        type(VegetableString_t) :: string

        character(len=12) :: temp

        write(temp, '(I0)') int
        string = toString(trim(temp))
    end function integerToString

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

    function joinWithCharacter(strings, separator) result(string)
        type(VegetableString_t), intent(in) :: strings(:)
        character(len=*), intent(in) :: separator
        type(VegetableString_t) :: string

        string = join(strings, toString(separator))
    end function joinWithCharacter

    function joinWithString(strings, separator) result(string)
        type(VegetableString_t), intent(in) :: strings(:)
        type(VegetableString_t), intent(in) :: separator
        type(VegetableString_t) :: string

        integer :: i

        string = strings(1)
        do i = 2, size(strings)
            string = string // separator // strings(i)
        end do
    end function joinWithString

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
        use iso_fortran_env, only: error_unit, output_unit

        type(TestItem_t) :: tests
        type(TestResultItem_t) :: results

        write(output_unit, '(A)') "Running Tests"
        write(output_unit, '(A)')
        write(output_unit, '(DT)') tests%description()
        write(output_unit, '(A)')
        write(output_unit, '(DT)') &
                "A total of " // toString(tests%numCases()) // " test cases"
        write(output_unit, '(A)')
        results = tests%run()
    end subroutine

    recursive function splitAtBothCharacter(&
            string_, split_characters) result(strings)
        character(len=*), intent(in) :: string_
        character(len=*), intent(in) :: split_characters
        type(VegetableString_t), allocatable :: strings(:)

        if (len(split_characters) > 0) then
            if (len(string_) > 0) then
                if (index(split_characters, string_(1:1)) > 0) then
                    strings = splitAtBothCharacter(string_(2:), split_characters)
                else if (index(split_characters, string_(len(string_):len(string_))) > 0) then
                    strings = splitAtBothCharacter(string_(1:len(string_) - 1), split_characters)
                else
                    strings = doSplit(string_, split_characters)
                end if
            else
                allocate(strings(1))
                strings(1) = toString("")
            end if
        else
            allocate(strings(1))
            strings(1) = toString(string_)
        end if
    contains
        function doSplit(string__, split_characters_) result(strings_)
            character(len=*), intent(in) :: string__
            character(len=*), intent(in) :: split_characters_
            type(VegetableString_t), allocatable :: strings_(:)

            integer :: i
            type(VegetableString_t), allocatable :: rest(:)
            character(len=:), allocatable :: this_string
            allocate(character(len=0)::this_string)

            do i = 2, len(string__)
                if (index(split_characters_, string__(i:i)) > 0) exit
            end do
            if (i < len(string__)) then
                this_string = string__(1:i - 1)
                rest = splitAtBothCharacter(string__(i + 1:), split_characters_)
                allocate(strings_(size(rest) + 1))
                strings_(1) = toString(this_string)
                strings_(2:) = rest(:)
            else
                allocate(strings_(1))
                strings_(1) = toString(string__)
            end if
        end function doSplit
    end function splitAtBothCharacter

    function splitAtStringCharacter(string_, split_characters) result(strings)
        type(VegetableString_t), intent(in) :: string_
        character(len=*), intent(in) :: split_characters
        type(VegetableString_t), allocatable :: strings(:)

        strings = splitAt(string_%string, split_characters)
    end function splitAtStringCharacter

    function stringIncludesString(string, search_for)
        class(VegetableString_t), intent(in) :: string
        type(VegetableString_t), intent(in) :: search_for
        logical :: stringIncludesString

        stringIncludesString = index(string%string, search_for%string) > 0
    end function stringIncludesString

    subroutine stringWriteFormatted(string, unit, iotype, v_list, iostat, iomsg)
        class(VegetableString_t), intent(in) :: string
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        associate(a => iotype, b => v_list); end associate

        write(unit=unit, iostat=iostat, iomsg=iomsg, fmt='(A)') string%string
    end subroutine stringWriteFormatted

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

    function TestCollectionDescription(self) result(description)
        class(TestCollection_t), intent(in) :: self
        type(VegetableString_t) :: description

        type :: VegStringArray_t
            type(VegetableString_t), pointer :: strings(:) => null()
        end type VegStringArray_t

        integer, parameter :: MAX_STACK_SIZE = 100
        type(VegStringArray_t), save :: descriptions(MAX_STACK_SIZE)
        integer :: descriptions_location
        integer :: i
        integer :: num_cases

        num_cases = size(self%tests)
        do i = 1, MAX_STACK_SIZE
            if (.not.associated(descriptions(i)%strings)) then
                descriptions_location = i
                allocate(descriptions(descriptions_location)%strings(num_cases))
                exit
            end if
        end do
        if (i > MAX_STACK_SIZE) STOP "Test Collections Nested Too Deep!"
        do i = 1, num_cases
            descriptions(descriptions_location)%strings(i) = self%tests(i)%description()
        end do
        description = hangingIndent( &
                self%description_ // NEWLINE &
                // join(descriptions(descriptions_location)%strings, NEWLINE))
        deallocate(descriptions(descriptions_location)%strings)
        nullify(descriptions(descriptions_location)%strings)
    end function TestCollectionDescription

    function TestCollectionNumCases(self) result(num_cases)
        class(TestCollection_t), intent(in) :: self
        integer :: num_cases

        integer :: i
        integer, allocatable :: individual_nums(:)
        integer :: num_individual

        associate(a => self)
        end associate
        num_individual = size(self%tests)
        allocate(individual_nums(num_individual))
        do i = 1, num_individual
            individual_nums(i) = self%tests(i)%numCases()
        end do
        num_cases = sum(individual_nums)
    end function TestCollectionNumCases

    function TestCollectionResult(description, results) result(test_collection_result)
        type(VegetableString_t), intent(in) :: description
        type(TestResultItem_t), intent(in) :: results(:)
        type(TestCollectionResult_t) :: test_collection_result

        test_collection_result%description = description
        allocate(test_collection_result%results(size(results)))
        test_collection_result%results = results
    end function TestCollectionResult

    function testItemDescription(self) result(description)
        class(TestItem_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = self%test%description()
    end function testItemDescription

    function testItemNumCases(self) result(num_cases)
        class(TestItem_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%test%numCases()
    end function testItemNumCases

    function testThat(tests) result(test_collection)
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        allocate(TestCollection_t :: test_collection%test)
        select type (test => test_collection%test)
        type is (TestCollection_t)
            test = TestCollection("Test that", tests)
        end select
    end function testThat
end module Vegetables_m
