module Vegetables_m
    implicit none
    private

    type :: VegetableString_t
        private
        character(len=:), allocatable :: string
    contains
        private
        generic, public :: operator(//) => &
                concatCharacterAndString, &
                concatStringAndCharacter, &
                concatStrings
        procedure, pass(rhs) :: concatCharacterAndString
        procedure :: concatStringAndCharacter
        procedure :: concatStrings
        procedure, public :: includes
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
        private
        type(VegetableString_t) :: description_
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

    type, public :: Result_t
        private
        integer :: num_asserts
        logical :: passed
        type(VegetableString_t) :: message
    end type Result_t

    type, public, extends(Test_t) :: TestCase_t
        private
        procedure(test_), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: run => runTestCase
    end type TestCase_t

    type :: TestItem_t
        private
        class(Test_t), allocatable :: test
    contains
        procedure, public :: description => testItemDescription
        procedure, public :: run => runTestItem
    end type TestItem_t

    type, public, extends(Test_t) :: TestCollection_t
        private
        type(TestItem_t), allocatable :: tests(:)
    contains
        procedure, public :: description => testCollectionDescription
        procedure, public :: run => runTestCollection
    end type TestCollection_t

    type, public, extends(TestResult_t) :: TestCaseResult_t
        private
        type(Result_t) :: result_
    contains
        private
        procedure, public :: numCases => testCaseNumCases
        procedure, public :: passed => testCasePassed
    end type TestCaseResult_t

    type :: TestResultItem_t
        private
        class(TestResult_t), allocatable :: test_result
    contains
        private
        procedure :: passed => testItemPassed
    end type TestResultItem_t

    type, public, extends(TestResult_t) :: TestCollectionResult_t
        private
        type(TestResultItem_t), allocatable :: results(:)
    contains
        private
        procedure, public :: numCases => testCollectionNumCases
        procedure, public :: passed => testCollectionPassed
    end type TestCollectionResult_t

    interface
        pure function test_() result(result)
            import Result_t

            type(Result_t) :: result
        end function test_
    end interface

    interface operator(.and.)
        module procedure testCollectionAndTest
    end interface

    interface assertIncludes
        module procedure assertStringIncludesCharacter
    end interface

    interface fail
        module procedure failWithCharacter
        module procedure failWithString
    end interface

    interface join
        module procedure joinWithCharacter
        module procedure joinWithString
    end interface

    interface splitAt
        module procedure splitAtBothCharacter
        module procedure splitAtStringCharacter
    end interface

    character(len=*), parameter :: NEWLINE = NEW_LINE('A')

    public :: &
            operator(.and.), &
            assertIncludes, &
            assertNot, &
            Describe, &
            fail, &
            FAILING, &
            Given, &
            It, &
            runTests, &
            succeed, &
            SUCCEEDS, &
            testThat, &
            Then, &
            TODO, &
            When
contains
    pure function alwaysFail() result(test_result)
        type(Result_t) :: test_result

        test_result = fail("Intentional Failure")
    end function alwaysFail

    pure function assertNot(condition) result(result_)
        logical, intent(in) :: condition
        type(Result_t) :: result_

        if (condition) then
            result_ = fail("Wasn't False")
        else
            result_ = succeed()
        end if
    end function assertNot

    pure function assertStringIncludesCharacter(character_, string) result(result_)
        character(len=*), intent(in) :: character_
        type(VegetableString_t), intent(in) :: string
        type(Result_t) :: result_

        if (string%includes(character_)) then
            result_ = succeed()
        else
            result_ = fail( &
                    "'" // string%string // "' did not include '" &
                    // character_ // "'")
        end if
    end function assertStringIncludesCharacter

    pure function concatCharacterAndString(lhs, rhs) result(concatted)
        character(len=*), intent(in) :: lhs
        class(VegetableString_t), intent(in) :: rhs
        type(VegetableString_t) :: concatted

        concatted = toString(lhs // rhs%string)
    end function concatCharacterAndString

    pure function concatStringAndCharacter(lhs, rhs) result(concatted)
        class(VegetableString_t), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        type(VegetableString_t) :: concatted

        concatted = toString(lhs%string // rhs)
    end function concatStringAndCharacter

    pure function concatStrings(lhs, rhs) result(concatted)
        class(VegetableString_t), intent(in) :: lhs
        type(VegetableString_t), intent(in) :: rhs
        type(VegetableString_t) :: concatted

        concatted = toString(lhs%string // rhs%string)
    end function concatStrings

    pure function Describe(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestCase_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        test_collection = TestCollection_t( &
                description_ = toString(description), &
                tests = toItem(tests))
    end function Describe

    pure function FAILING() result(test_case)
        type(TestCase_t) :: test_case

        test_case = TestCase_t(description_ = toString("FAIL"), test = alwaysFail)
    end function FAILING

    pure function failWithCharacter(message) result(failure)
        character(len=*), intent(in) :: message
        type(Result_t) :: failure

        failure = fail(toString(message))
    end function failWithCharacter

    pure function failWithString(message) result(failure)
        type(VegetableString_t), intent(in) :: message
        type(Result_t) :: failure

        failure = Result_t(num_asserts = 1, passed = .false., message = message)
    end function failWithString

    pure function Given(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestCollection_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        test_collection = TestCollection_t( &
                description_ = toString("Given " // description), &
                tests = toItem(tests))
    end function Given

    pure function hangingIndent(string_) result(indented)
        type(VegetableString_t), intent(in) :: string_
        type(VegetableString_t), allocatable :: indented

        type(VegetableString_t), allocatable :: lines(:)

        lines = splitAt(string_, NEWLINE)
        indented = join(lines, NEWLINE // "    ")
    end function hangingIndent


    pure function includes(self, character_)
        class(VegetableString_t), intent(in) :: self
        character(len=*), intent(in) :: character_
        logical :: includes

        includes = index(self%string, character_) > 0
    end function includes

    pure function It(description, test) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: test
        type(TestCase_t) :: test_case

        test_case = TestCase_t( &
                description_ = toString(description), &
                test = test)
    end function It

    pure function joinWithCharacter(strings, separator) result(string)
        type(VegetableString_t), intent(in) :: strings(:)
        character(len=*), intent(in) :: separator
        type(VegetableString_t) :: string

        string = join(strings, toString(separator))
    end function joinWithCharacter

    pure function joinWithString(strings, separator) result(string)
        type(VegetableString_t), intent(in) :: strings(:)
        type(VegetableString_t), intent(in) :: separator
        type(VegetableString_t) :: string

        integer :: i

        string = strings(1)
        do i = 2, size(strings)
            string = string // separator // strings(i)
        end do
    end function joinWithString

    pure function runTestCase(self) result(test_result)
        class(TestCase_t), intent(in) :: self
        class(TestResult_t), allocatable :: test_result

        test_result = TestCaseResult_t( &
                description_ = self%description_, &
                result_ = self%test())
    end function runTestCase

    pure function runTestCollection(self) result(test_result)
        class(TestCollection_t), intent(in) :: self
        class(TestResult_t), allocatable :: test_result

        test_result = TestCollectionResult_t( &
                description_ = self%description_, &
                results = self%tests%run())
    end function runTestCollection

    elemental function runTestItem(self) result(test_result)
        class(TestItem_t), intent(in) :: self
        type(TestResultItem_t) :: test_result

        test_result = TestResultItem_t(self%test%run())
    end function runTestItem

    subroutine runTests(tests)
        use iso_fortran_env, only: error_unit, output_unit

        class(Test_t) :: tests

        class(TestResult_t), allocatable :: test_result

        write(output_unit, *) "Running Tests"
        write(output_unit, *) tests%description()
        test_result = tests%run()
        if (test_result%passed()) then
            write(output_unit, *) "Passed"
        else
            write(error_unit, *) "Failed"
            stop 1
        end if
    end subroutine runTests

    pure recursive function splitAtBothCharacter(&
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
        pure function doSplit(string__, split_characters_) result(strings_)
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

    pure function splitAtStringCharacter(string_, split_characters) result(strings)
        type(VegetableString_t), intent(in) :: string_
        character(len=*), intent(in) :: split_characters
        type(VegetableString_t), allocatable :: strings(:)

        strings = splitAt(string_%string, split_characters)
    end function splitAtStringCharacter

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

    pure function succeed() result(success)
        type(Result_t) :: success

        success = Result_t(num_asserts = 1, passed = .true., message = toString(""))
    end function succeed

    pure function SUCCEEDS()
        type(TestCase_t) :: SUCCEEDS

        SUCCEEDS = TestCase_t(description_ = toString("SUCCEEDS"), test = succeed)
    end function SUCCEEDS

    pure function testCaseDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = self%description_
    end function testCaseDescription

    pure function testCaseNumCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self); end associate
        num_cases = 1
    end function testCaseNumCases

    pure function testCasePassed(self) result(passed)
        class(TestCaseResult_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed
    end function testCasePassed

    pure function testCollectionAndTest(test_collection, test) result(new_collection)
        type(TestCollection_t), intent(in) :: test_collection
        class(Test_t), intent(in) :: test
        type(TestCollection_t) :: new_collection

        integer :: new_num_tests
        integer :: prev_num_tests

        prev_num_tests = size(test_collection%tests)
        new_num_tests = prev_num_tests + 1

        new_collection%description_ = test_collection%description_
        allocate(new_collection%tests(new_num_tests))
        new_collection%tests(1:prev_num_tests) = test_collection%tests(1:prev_num_tests)
        new_collection%tests(new_num_tests) = TestItem_t(test)
    end function testCollectionAndTest

    pure function testCollectionDescription(self) result(description)
        class(TestCollection_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = hangingIndent( &
                self%description_ // NEWLINE &
                // join(self%tests%description(), NEWLINE))
    end function testCollectionDescription

    pure function testCollectionNumCases(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self); end associate
        num_cases = 1
    end function testCollectionNumCases

    pure function testCollectionPassed(self) result(passed)
        class(TestCollectionResult_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed())
    end function testCollectionPassed

    elemental function testItemDescription(self) result(description)
        class(TestItem_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = self%test%description()
    end function testItemDescription

    elemental function testItemPassed(self) result(passed)
        class(TestResultItem_t), intent(in) :: self
        logical :: passed

        passed = self%test_result%passed()
    end function testItemPassed

    pure function testThat(test_case) result(test_collection)
        class(Test_t), intent(in) :: test_case
        type(TestCollection_t) :: test_collection

        test_collection = TestCollection_t( &
                description_ = toString("Test That"), &
                tests = [TestItem_t(test_case)])
    end function testThat

    pure function Then(description, test) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: test
        type(TestCase_t) :: test_case

        test_case = It("Then " // description, test)
    end function Then

    pure function TODO() result(test_case)
        type(TestCase_t) :: test_case

        test_case = TestCase_t(description_ = toString("TODO"), test = alwaysFail)
    end function TODO

    elemental function toItem(test) result(item)
        class(Test_t), intent(in) :: test
        type(TestItem_t) :: item

        item = TestItem_t(test)
    end function toItem

    pure function toString(string_in) result(string_out)
        character(len=*), intent(in) :: string_in
        type(VegetableString_t) :: string_out

        string_out = VegetableString_t(string_in)
    end function toString

    pure function When(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestCase_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        test_collection = Describe("When " // description, tests)
    end function When
end module Vegetables_m
