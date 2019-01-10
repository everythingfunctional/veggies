module Vegetables_m
    implicit none
    private

    type :: VegetableString_t
        private
        character(len=:), allocatable :: string
    end type VegetableString_t

    type, public, abstract :: Maybe_t
    end type Maybe_t

    type :: MaybeItem_t
        private
        class(Maybe_t), allocatable :: maybe
    contains
        private
        procedure, public :: hasValue
    end type MaybeItem_t

    type, public, extends(Maybe_t) :: Nothing_t
    end type Nothing_t

    type, public :: Result_t
        private
        character(len=:), allocatable :: all_message
        character(len=:), allocatable :: failing_message
        logical :: initialized = .false.
        integer :: num_failling_asserts
        integer :: num_passing_asserts
        logical :: passed_
    contains
        private
        generic, public :: operator(.and.) => combineResults
        procedure :: combineResults
        procedure, public :: failureDescription => resultFailureDescription
        procedure, public :: numAsserts => resultNumAsserts
        procedure, public :: numFailing => resultNumFailing
        procedure, public :: passed => resultPassed
        procedure, public :: verboseDescription => resultVerboseDescription
    end type Result_t

    type, abstract, public :: Test_t
        private
        character(len=:), allocatable :: description_
    contains
        private
        procedure(testDescription), deferred, public :: description
        procedure(filter_), deferred, public :: filter
        procedure(testNum), deferred, public :: numCases
    end type Test_t

    type, abstract, public :: TestResult_t
        private
        character(len=:), allocatable :: description
    contains
        private
        procedure(testResultDescription), deferred, public :: failureDescription
        procedure(testResultNum), deferred, public :: numAsserts
        procedure(testResultNum), deferred, public :: numCases
        procedure(testResultNum), deferred, public :: numFailingAsserts
        procedure(testResultNum), deferred, public :: numFailingCases
        procedure(testQuestion), deferred, public :: passed
        procedure(testResultDescription), deferred, public :: verboseDescription
    end type TestResult_t

    type, public :: Transformed_t
        private
        class(*), allocatable :: value_
    end type Transformed_t

    type, public :: Example_t
        private
        class(*), allocatable :: value_
    end type Example_t

    abstract interface
        pure function filter_(self, filter_string) result(maybe)
            import :: Test_t, Maybe_t
            class(Test_t), intent(in) :: self
            character(len=*), intent(in) :: filter_string
            class(Maybe_t), allocatable :: maybe
        end function filter_

        function inputTest(input) result(result_)
            import :: Result_t
            class(*), intent(in) :: input
            type(Result_t) :: result_
        end function inputTest

        function test_() result(result_)
            import :: Result_t
            type(Result_t) :: result_
        end function

        pure function testDescription(self) result(description)
            import :: Test_t
            class(Test_t), intent(in) :: self
            character(len=:), allocatable :: description
        end function testDescription

        pure function testNum(self) result(num)
            import :: Test_t
            class(Test_t), intent(in) :: self
            integer :: num
        end function testNum

        pure function testQuestion(self) result(answer)
            import :: TestResult_t
            class(TestResult_t), intent(in) :: self
            logical :: answer
        end function testQuestion

        pure function testResultDescription(self) result(description)
            import :: TestResult_t
            class(TestResult_t), intent(in) :: self
            character(len=:), allocatable :: description
        end function testResultDescription

        pure function testResultNum(self) result(num)
            import :: TestResult_t
            class(TestResult_t), intent(in) :: self
            integer :: num
        end function testResultNum

        function transformer_(input) result(output)
            import Transformed_t
            class(*), intent(in) :: input
            type(Transformed_t) :: output
        end function transformer_
    end interface

    type, public :: TestItem_t
        private
        class(Test_t), allocatable :: test
    contains
        private
        procedure, public :: description => testItemDescription
        procedure, public :: filter => filterTestItem
        procedure, public :: numCases => testItemNumCases
        procedure, public :: run => runTestItem
        procedure, public :: runWithInput => runTestItemWithInput
    end type TestItem_t

    type, extends(Test_t), public :: TestCase_t
        private
        procedure(test_), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: filter => filterTestCase
        procedure, public :: numCases => testCaseNumCases
        procedure, public :: run => runCase
    end type TestCase_t

    type, extends(Test_t), public :: InputTestCase_t
        private
        procedure(inputTest), nopass, pointer :: test
    contains
        private
        procedure, public :: description => inputTestCaseDescription
        procedure, public :: filter => filterInputTestCase
        procedure, public :: numCases => inputTestCaseNumCases
        procedure, public :: run => runCaseWithInput
    end type InputTestCase_t

    type, extends(Test_t), public :: TestCaseWithExamples_t
        private
        type(Example_t), allocatable :: examples(:)
        procedure(inputTest), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseWithExamplesDescription
        procedure, public :: filter => filterTestCaseWithExamples
        procedure, public :: numCases => testCaseWithExamplesNumCases
        procedure, public :: run => runCaseWithExamples
    end type TestCaseWithExamples_t

    type, extends(Test_t), public :: TestCollection_t
        private
        type(TestItem_t), allocatable :: tests(:)
    contains
        private
        procedure, public :: description => testCollectionDescription
        procedure, public :: filter => filterTestCollection
        procedure, public :: numCases => testCollectionNumCases
        procedure, public :: run => runCollection
    end type TestCollection_t

    type, extends(Test_t), public :: TestCollectionWithInput_t
        private
        type(TestItem_t), allocatable :: tests(:)
        class(*), allocatable :: input
    contains
        private
        procedure, public :: description => testCollectionWithInputDescription
        procedure, public :: filter => filterTestCollectionWithInput
        procedure, public :: numCases => testCollectionWithInputNumCases
        procedure, public :: run => runCollectionThatHasInput
    end type TestCollectionWithInput_t

    type, extends(Test_t), public :: TransformingTestCollection_t
        private
        type(TestItem_t), allocatable :: tests(:)
        procedure(transformer_), nopass, pointer :: transformer
    contains
        private
        procedure, public :: description => transformingTestCollectionDescription
        procedure, public :: filter => filterTransformingTestCollection
        procedure, public :: numCases => transformingTestCollectionNumCases
        procedure, public :: run => runTransformingCollection
    end type TransformingTestCollection_t

    type, public :: TestResultItem_t
        private
        class(TestResult_t), allocatable :: result_
    contains
        private
        procedure, public :: failureDescription => testResultItemFailureDescription
        procedure, public :: numAsserts => testResultItemNumAsserts
        procedure, public :: numCases => testResultItemNumCases
        procedure, public :: numFailingAsserts => testResultItemNumFailingAsserts
        procedure, public :: numFailingCases => testResultItemNumFailing
        procedure, public :: passed => testItemPassed
        procedure, public :: verboseDescription => testResultItemVerboseDescription
    end type TestResultItem_t

    type, extends(TestResult_t), public :: TestCaseResult_t
        private
        type(Result_t) :: result_
    contains
        private
        procedure, public :: failureDescription => testCaseFailureDescription
        procedure, public :: numAsserts => testCaseNumAsserts
        procedure, public :: numCases => testCaseResultNumCases
        procedure, public :: numFailingAsserts => testCaseNumFailingAsserts
        procedure, public :: numFailingCases => testCaseNumFailing
        procedure, public :: passed => testCasePassed
        procedure, public :: verboseDescription => testCaseVerboseDescription
    end type TestCaseResult_t

    type, extends(TestResult_t), public :: TestCollectionResult_t
        private
        type(TestResultItem_t), allocatable :: results(:)
    contains
        private
        procedure, public :: failureDescription => testCollectionFailureDescription
        procedure, public :: numAsserts => testCollectionNumAsserts
        procedure, public :: numCases => testCollectionResultNumCases
        procedure, public :: numFailingAsserts => testCollectionNumFailingAsserts
        procedure, public :: numFailingCases => testCollectionNumFailing
        procedure, public :: passed => testCollectionPassed
        procedure, public :: verboseDescription => testCollectionVerboseDescription
    end type TestCollectionResult_t

    type :: Options_t
        private
        logical :: quiet
        logical :: verbose
        logical :: filter_tests
        character(len=:), allocatable :: filter_string
    end type Options_t

    type, public, extends(Maybe_t) :: JustInputTestCase_t
        private
        type(InputTestCase_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueInputTestCase
    end type JustInputTestCase_t

    type, public, extends(Maybe_t) :: JustTestCase_t
        private
        type(TestCase_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueTestCase
    end type JustTestCase_t

    type, public, extends(Maybe_t) :: JustTestCaseWithExamples_t
        private
        type(TestCaseWithExamples_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueTestCaseWithExamples
    end type JustTestCaseWithExamples_t

    type, public, extends(Maybe_t) :: JustTestCollection_t
        private
        type(TestCollection_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueTestCollection
    end type JustTestCollection_t

    type, public, extends(Maybe_t) :: JustTestCollectionWithInput_t
        private
        type(TestCollectionWithInput_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueTestCollectionWithInput
    end type JustTestCollectionWithInput_t

    type, public, extends(Maybe_t) :: JustTestItem_t
        private
        type(TestItem_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueTestItem
    end type JustTestItem_t

    type, public, extends(Maybe_t) :: JustTransformingTestCollection_t
        private
        type(TransformingTestCollection_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueTransformingTestCollection
    end type JustTransformingTestCollection_t

    interface operator(.includes.)
        module procedure includes
    end interface operator(.includes.)

    interface assertDoesntInclude
        module procedure assertDoesntIncludeBasic
        module procedure assertDoesntIncludeWithMessage
        module procedure assertDoesntIncludeWithMessages
    end interface assertDoesntInclude

    interface assertEmpty
        module procedure assertEmptyBasic
        module procedure assertEmptyWithMessage
        module procedure assertEmptyWithMessages
    end interface assertEmpty

    interface assertEquals
        module procedure assertEqualsCharacters
        module procedure assertEqualsCharactersWithMessage
        module procedure assertEqualsCharactersWithMessages
        module procedure assertEqualsDoublePrecision
        module procedure assertEqualsDoublePrecisionWithMessage
        module procedure assertEqualsDoublePrecisionWithMessages
        module procedure assertEqualsInteger
        module procedure assertEqualsIntegerWithMessage
        module procedure assertEqualsIntegerWithMessages
    end interface assertEquals

    interface assertEqualsWithinAbsolute
        module procedure assertEqualsWithinAbsoluteBasic
        module procedure assertEqualsWithinAbsoluteWithMessage
        module procedure assertEqualsWithinAbsoluteWithMessages
    end interface assertEqualsWithinAbsolute

    interface assertEqualsWithinRelative
        module procedure assertEqualsWithinRelativeBasic
        module procedure assertEqualsWithinRelativeWithMessage
        module procedure assertEqualsWithinRelativeWithMessages
    end interface assertEqualsWithinRelative

    interface assertIncludes
        module procedure assertIncludesBasic
        module procedure assertIncludesWithMessage
        module procedure assertIncludesWithMessages
    end interface assertIncludes

    interface assertNot
        module procedure assertNotBasic
        module procedure assertNotWithMessage
        module procedure assertNotWithMessages
    end interface assertNot

    interface assertThat
        module procedure assertThatBasic
        module procedure assertThatWithMessage
        module procedure assertThatWithMessages
    end interface assertThat

    interface describe
        module procedure describeBasic
        module procedure describeWithInput
    end interface describe

    interface given
        module procedure givenBasic
        module procedure givenWithInput
    end interface given

    interface it
        module procedure itBasic
        module procedure itWithExamples
    end interface it

    interface Just
        module procedure JustInputTestCase
        module procedure JustTestCase
        module procedure JustTestCaseWithExamples
        module procedure JustTestCollection
        module procedure JustTestCollectionWithInput
        module procedure JustTestItem
        module procedure JustTransformingTestCollection
    end interface Just

    interface toCharacter
        module procedure doublePrecisionToCharacter
        module procedure integerToCharacter
    end interface toCharacter

    interface when
        module procedure whenBasic
        module procedure whenWithInput
        module procedure whenWithTransformer
    end interface

    integer, parameter :: dp = kind(0.0d0)
    character(len=*), parameter :: EMPTY_SUCCESS_MESSAGE = "String was empty"
    double precision, parameter :: MACHINE_EPSILON = EPSILON(0.0_dp)
    double precision, parameter :: MACHINE_TINY = TINY(0.0_dp)
    character(len=*), parameter :: NEWLINE = NEW_LINE('A')
    character(len=*), parameter :: NOT_FAILURE_MESSAGE = "Expected to not be true"
    character(len=*), parameter :: NOT_SUCCESS_MESSAGE = "Was not true"
    type(Nothing_t), parameter :: NOTHING = Nothing_t()
    character(len=*), parameter :: THAT_FAILURE_MESSAGE = "Expected to be true"
    character(len=*), parameter :: THAT_SUCCESS_MESSAGE = "Was true"

    public :: &
            assertDoesntInclude, &
            assertEmpty, &
            assertEquals, &
            assertEqualsWithinAbsolute, &
            assertEqualsWithinRelative, &
            assertIncludes, &
            assertNot, &
            assertThat, &
            describe, &
            Example, &
            fail, &
            given, &
            it, &
            it_, &
            runTests, &
            succeed, &
            TestCase, &
            TestCollection, &
            testThat, &
            then, &
            then_, &
            Transformed, &
            when
contains
    pure function assertDoesntIncludeBasic(search_for, string) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, "", "")
    end function assertDoesntIncludeBasic

    pure function assertDoesntIncludeWithMessage( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, message, message)
    end function assertDoesntIncludeWithMessage

    pure function assertDoesntIncludeWithMessages( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if(.not.(string.includes.search_for)) then
            result__ = succeed( &
                    makeDoesntIncludeSuccessMessage(search_for, string) &
                    // makeUserMessage(success_message))
        else
            result__ = fail( &
                    makeDoesntIncludeFailureMessage(search_for, string) &
                    // makeUserMessage(failure_message))
        end if
    end function assertDoesntIncludeWithMessages

    pure function assertEmptyBasic(string) result(result__)
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertEmpty(string, "", "")
    end function assertEmptyBasic

    pure function assertEmptyWithMessage(string, message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty(string, message, message)
    end function assertEmptyWithMessage

    pure function assertEmptyWithMessages(&
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (string == "") then
            result__ = succeed( &
                    EMPTY_SUCCESS_MESSAGE // makeUserMessage(success_message))
        else
            result__ = fail( &
                    makeEmptyFailureMessage(string) &
                    // makeUserMessage(failure_message))
        end if
    end function assertEmptyWithMessages

    pure function assertEqualsCharacters(expected, actual) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, "", "")
    end function assertEqualsCharacters

    pure function assertEqualsCharactersWithMessage( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, message, message)
    end function assertEqualsCharactersWithMessage

    pure function assertEqualsCharactersWithMessages( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed( &
                    makeEqualsSuccessMessage(expected) &
                    // makeUserMessage(success_message))
        else
            result__ = fail( &
                    makeEqualsFailureMessage( &
                            expected, actual) &
                    // makeUserMessage(failure_message))
        end if
    end function assertEqualsCharactersWithMessages

    pure function assertEqualsDoublePrecision(expected, actual) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, "", "")
    end function assertEqualsDoublePrecision

    pure function assertEqualsDoublePrecisionWithMessage( &
            expected, actual, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, message, message)
    end function assertEqualsDoublePrecisionWithMessage

    pure function assertEqualsDoublePrecisionWithMessages( &
            expected, actual, success_message, failure_message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, MACHINE_EPSILON, success_message, failure_message)
    end function assertEqualsDoublePrecisionWithMessages

    pure function assertEqualsWithinAbsoluteBasic( &
            expected, actual, tolerance) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute(expected, actual, tolerance, "", "")
    end function assertEqualsWithinAbsoluteBasic

    pure function assertEqualsWithinAbsoluteWithMessage( &
            expected, actual, tolerance, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinAbsoluteWithMessage

    pure function assertEqualsWithinAbsoluteWithMessages( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (equalsWithinAbsolute(expected, actual, tolerance)) then
            result__ = succeed( &
                    makeWithinSuccesMessage( &
                            toCharacter(expected), &
                            toCharacter(actual), &
                            toCharacter(tolerance) ) &
                    // makeUserMessage(success_message))
        else
            result__ = fail( &
                    makeWithinFailureMessage( &
                            toCharacter(expected), &
                            toCharacter(actual), &
                            toCharacter(tolerance)) &
                    // makeUserMessage(failure_message))
        end if
    end function assertEqualsWithinAbsoluteWithMessages

    pure function assertEqualsWithinRelativeBasic( &
            expected, actual, tolerance) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative(expected, actual, tolerance, "", "")
    end function assertEqualsWithinRelativeBasic

    pure function assertEqualsWithinRelativeWithMessage( &
            expected, actual, tolerance, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinRelativeWithMessage

    pure function assertEqualsWithinRelativeWithMessages( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (equalsWithinRelative(expected, actual, tolerance)) then
            result__ = succeed( &
                    makeWithinSuccesMessage( &
                            toCharacter(expected), &
                            toCharacter(actual), &
                            toCharacter(tolerance * 100.0_dp) // "%") &
                    // makeUserMessage(success_message))
        else
            result__ = fail( &
                    makeWithinFailureMessage( &
                            toCharacter(expected), &
                            toCharacter(actual), &
                            toCharacter(tolerance * 100.0_dp) // "%") &
                    // makeUserMessage(failure_message))
        end if
    end function assertEqualsWithinRelativeWithMessages

    pure function assertEqualsInteger(expected, actual) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, "", "")
    end function assertEqualsInteger

    pure function assertEqualsIntegerWithMessage( &
            expected, actual, message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, message, message)
    end function assertEqualsIntegerWithMessage

    pure function assertEqualsIntegerWithMessages( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed( &
                    makeEqualsSuccessMessage(toCharacter(expected)) &
                    // makeUserMessage(success_message))
        else
            result__ = fail( &
                    makeEqualsFailureMessage( &
                            toCharacter(expected), toCharacter(actual)) &
                    // makeUserMessage(failure_message))
        end if
    end function assertEqualsIntegerWithMessages

    pure function assertIncludesBasic(search_for, string) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, string, "", "")
    end function assertIncludesBasic

    pure function assertIncludesWithMessage( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, string, message, message)
    end function assertIncludesWithMessage

    pure function assertIncludesWithMessages( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (string.includes.search_for) then
            result__ = succeed( &
                    makeIncludesSuccessMessage(search_for, string) &
                    // makeUserMessage(success_message))
        else
            result__ = fail( &
                    makeIncludesFailureMessage(search_for, string) &
                    // makeUserMessage(failure_message))
        end if
    end function assertIncludesWithMessages

    pure function assertNotBasic(condition) result(result__)
        logical, intent(in) :: condition
        type(Result_t) :: result__

        result__ = assertNot(condition, "", "")
    end function assertNotBasic

    pure function assertNotWithMessage(condition, message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertNot(condition, message, message)
    end function assertNotWithMessage

    pure function assertNotWithMessages( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (.not. condition) then
            result__ = succeed( &
                    NOT_SUCCESS_MESSAGE // makeUserMessage(success_message))
        else
            result__ = fail( &
                    NOT_FAILURE_MESSAGE // makeUserMessage(failure_message))
        end if
    end function assertNotWithMessages

    pure function assertThatBasic(condition) result(result__)
        logical, intent(in) :: condition
        type(Result_t) :: result__

        result__ = assertThat(condition, "", "")
    end function assertThatBasic

    pure function assertThatWithMessage(condition, message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertThat(condition, message, message)
    end function assertThatWithMessage

    pure function assertThatWithMessages( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (condition) then
            result__ = succeed( &
                    THAT_SUCCESS_MESSAGE // makeUserMessage(success_message))
        else
            result__ = fail( &
                    THAT_FAILURE_MESSAGE // makeUserMessage(failure_message))
        end if
    end function assertThatWithMessages

    pure function combineResults(lhs, rhs) result(combined)
        class(Result_t), intent(in) :: lhs
        type(Result_t), intent(in) :: rhs
        type(Result_t) :: combined

        if (lhs%initialized .and. rhs%initialized) then
            combined = Result_( &
                    all_message = lhs%all_message // NEWLINE // rhs%all_message, &
                    failing_message = lhs%failing_message // NEWLINE // rhs%failing_message, &
                    passed = lhs%passed_ .and. rhs%passed_, &
                    num_failling_asserts = lhs%num_failling_asserts + rhs%num_failling_asserts, &
                    num_passing_asserts = lhs%num_passing_asserts + rhs%num_passing_asserts)
        else if (lhs%initialized) then
            combined = lhs
        else if (rhs%initialized) then
            combined = rhs
        end if
    end function combineResults

    pure function coverEmptyDecimal(number) result(fixed)
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: fixed

        if (lastCharacter(trim(number)) == ".") then
            fixed = trim(number) // "0"
        else if (firstCharacter(trim(number)) == ".") then
            fixed = "0" // trim(number)
        else
            fixed = trim(number)
        end if
    end function coverEmptyDecimal

    pure function delimit(string) result(delimited)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: delimited

        delimited = "[" // string // "]"
    end function delimit

    pure function describeBasic(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        allocate(TestCollection_t :: test_collection%test)
        select type (test => test_collection%test)
        type is (TestCollection_t)
            test = TestCollection(description, tests)
        end select
    end function describeBasic

    pure function describeWithInput(description, input, tests) result(test_collection)
        character(len=*), intent(in) :: description
        class(*), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        allocate(TestCollectionWithInput_t :: test_collection%test)
        select type (test => test_collection%test)
        type is (TestCollectionWithInput_t)
            test = TestCollectionWithInput(description, input, tests)
        end select
    end function describeWithInput

    pure function doublePrecisionToCharacter(number) result(string)
        double precision, intent(in) :: number
        character(len=:), allocatable :: string

        integer, parameter :: C_LEN = 32
        integer, parameter :: PRECISION = 15
        double precision :: abs_num
        character(len=C_LEN) :: exponent_part
        character(len=C_LEN) :: floating_part
        character(len=7) :: format_string
        character(len=C_LEN) :: intermediate
        integer :: scale_

        abs_num = abs(number)
        if (abs_num <= MACHINE_TINY) then
            string = "0.0"
            return
        end if
        scale_ = floor(log10(abs_num))
        if (scale_ >= PRECISION) then
            write(format_string, '(A,I0,A)') "(f0.", PRECISION-1, ")"
            write(floating_part, format_string) abs_num / 10.0_dp**scale_
            write(exponent_part, '(A,I0)') 'e', scale_
        else if (scale_ <= -2) then
            write(format_string, '(A,I0,A)') "(f0.", PRECISION-1, ")"
            write(floating_part, format_string) abs_num * 10.0_dp**(-scale_)
            write(exponent_part, '(A,I0)') 'e', scale_
        else
            write(format_string, '(A,I0,A)') "(f0.", PRECISION-scale_-1, ")"
            write(floating_part, format_string) abs_num
            exponent_part = ""
        end if
        floating_part = removeTrailingZeros(floating_part)
        floating_part = coverEmptyDecimal(floating_part)
        intermediate = trim(floating_part) // trim(exponent_part)
        if (number < 0.0_dp) then
            string = "-" // trim(intermediate)
        else
            string = trim(intermediate)
        end if
    end function doublePrecisionToCharacter

    pure function equalsWithinAbsolute(expected, actual, tolerance)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        logical :: equalsWithinAbsolute

        equalsWithinAbsolute = abs(expected - actual) <= tolerance
    end function equalsWithinAbsolute

    pure function equalsWithinRelative(expected, actual, tolerance)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        logical :: equalsWithinRelative

        equalsWithinRelative = &
                (abs(expected) <= MACHINE_TINY .and. abs(actual) <= MACHINE_TINY) &
                .or. (abs(expected - actual) / abs(expected) <= tolerance)
    end function equalsWithinRelative

    pure function Example(value_)
        class(*), intent(in) :: value_
        type(Example_t) :: Example

        Example%value_ = value_
    end function Example

    pure function fail(message) result(failure)
        character(len=*), intent(in) :: message
        type(Result_t) :: failure

        failure = Result_( &
                passed = .false., &
                all_message = message, &
                failing_message = message, &
                num_failling_asserts = 1, &
                num_passing_asserts = 0)
    end function fail

    pure function filterInputTestCase(self, filter_string) result(maybe)
        class(InputTestCase_t), intent(in) :: self
        character(len=*), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        if (self%description_.includes.filter_string) then
            maybe = Just(self)
        else
            maybe = NOTHING
        end if
    end function filterInputTestCase

    pure function filterTestCase(self, filter_string) result(maybe)
        class(TestCase_t), intent(in) :: self
        character(len=*), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        if (self%description_.includes.filter_string) then
            maybe = Just(self)
        else
            maybe = NOTHING
        end if
    end function filterTestCase

    pure function filterTestCaseWithExamples(self, filter_string) result(maybe)
        class(TestCaseWithExamples_t), intent(in) :: self
        character(len=*), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        if (self%description_.includes.filter_string) then
            maybe = Just(self)
        else
            maybe = NOTHING
        end if
    end function filterTestCaseWithExamples

    pure function filterTestCollection(self, filter_string) result(maybe)
        class(TestCollection_t), intent(in) :: self
        character(len=*), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        type(MaybeItem_t), allocatable :: filtered_tests(:)
        type(TestCollection_t) :: new_collection
        integer :: num_input_tests
        logical, allocatable :: passed_filter(:)

        if (self%description_.includes.filter_string) then
            maybe = Just(self)
        else
            num_input_tests = size(self%tests)
            allocate(filtered_tests(num_input_tests))
            filtered_tests = self%tests%filter(filter_string)
            allocate(passed_filter(num_input_tests))
            passed_filter = filtered_tests%hasValue()
            if (any(passed_filter)) then
                new_collection%description_ = self%description_
                allocate(new_collection%tests(count(passed_filter)))
                new_collection%tests = getTestItems(filtered_tests)
                maybe = Just(new_collection)
            else
                maybe = NOTHING
            end if
        end if
    end function filterTestCollection

    pure function filterTestCollectionWithInput(self, filter_string) result(maybe)
        class(TestCollectionWithInput_t), intent(in) :: self
        character(len=*), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        type(MaybeItem_t), allocatable :: filtered_tests(:)
        type(TestCollectionWithInput_t) :: new_collection
        integer :: num_input_tests
        logical, allocatable :: passed_filter(:)

        if (self%description_.includes.filter_string) then
            maybe = Just(self)
        else
            num_input_tests = size(self%tests)
            allocate(filtered_tests(num_input_tests))
            filtered_tests = self%tests%filter(filter_string)
            allocate(passed_filter(num_input_tests))
            passed_filter = filtered_tests%hasValue()
            if (any(passed_filter)) then
                new_collection%description_ = self%description_
                new_collection%input = self%input
                allocate(new_collection%tests(count(passed_filter)))
                new_collection%tests = getTestItems(filtered_tests)
                maybe = Just(new_collection)
            else
                maybe = NOTHING
            end if
        end if
    end function filterTestCollectionWithInput

    elemental function filterTestItem(self, filter_string) result(maybe)
        class(TestItem_t), intent(in) :: self
        character(len=*), intent(in) :: filter_string
        type(MaybeItem_t) :: maybe

        class(Maybe_t), allocatable :: filtered
        type(TestItem_t) :: test_item

        filtered = self%test%filter(filter_string)

        select type (filtered)
        type is (JustInputTestCase_t)
            allocate(InputTestCase_t :: test_item%test)
            select type (test => test_item%test)
            type is (InputTestCase_t)
                test = filtered%getValue()
                maybe%maybe = Just(test_item)
            end select
        type is (JustTestCase_t)
            allocate(TestCase_t :: test_item%test)
            select type (test => test_item%test)
            type is (TestCase_t)
                test = filtered%getValue()
                maybe%maybe = Just(test_item)
            end select
        type is (JustTestCaseWithExamples_t)
            allocate(TestCaseWithExamples_t :: test_item%test)
            select type (test => test_item%test)
            type is (TestCaseWithExamples_t)
                test = filtered%getValue()
                maybe%maybe = Just(test_item)
            end select
        type is (JustTestCollection_t)
            allocate(TestCollection_t :: test_item%test)
            select type (test => test_item%test)
            type is (TestCollection_t)
                test = filtered%getValue()
                maybe%maybe = Just(test_item)
            end select
        type is (JustTestCollectionWithInput_t)
            allocate(TestCollectionWithInput_t :: test_item%test)
            select type (test => test_item%test)
            type is (TestCollectionWithInput_t)
                test = filtered%getValue()
                maybe%maybe = Just(test_item)
            end select
        type is (JustTransformingTestCollection_t)
            allocate(TransformingTestCollection_t :: test_item%test)
            select type (test => test_item%test)
            type is (TransformingTestCollection_t)
                test = filtered%getValue()
                maybe%maybe = Just(test_item)
            end select
        type is (Nothing_t)
            maybe%maybe = NOTHING
        end select
    end function filterTestItem

    pure function filterTransformingTestCollection(self, filter_string) result(maybe)
        class(TransformingTestCollection_t), intent(in) :: self
        character(len=*), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        type(MaybeItem_t), allocatable :: filtered_tests(:)
        type(TransformingTestCollection_t) :: new_collection
        integer :: num_input_tests
        logical, allocatable :: passed_filter(:)

        if (self%description_.includes.filter_string) then
            maybe = Just(self)
        else
            num_input_tests = size(self%tests)
            allocate(filtered_tests(num_input_tests))
            filtered_tests = self%tests%filter(filter_string)
            allocate(passed_filter(num_input_tests))
            passed_filter = filtered_tests%hasValue()
            if (any(passed_filter)) then
                new_collection%description_ = self%description_
                new_collection%transformer => self%transformer
                allocate(new_collection%tests(count(passed_filter)))
                new_collection%tests = getTestItems(filtered_tests)
                maybe = Just(new_collection)
            else
                maybe = NOTHING
            end if
        end if
    end function filterTransformingTestCollection

    pure function firstCharacter(string) result(first)
        character(len=*), intent(in) :: string
        character(len=1) :: first

        character(len=:), allocatable :: trimmed

        allocate(character(len=0) :: trimmed)
        trimmed = trim(string)
        first = trimmed(1:1)
    end function firstCharacter

    function getOptions() result(options)
        use iso_fortran_env, only: error_unit, output_unit

        type(Options_t) :: options

        character(len=100) :: argument
        character(len=100) :: program_name
        integer :: i
        integer :: num_arguments

        options%quiet = .false.
        options%verbose = .false.
        options%filter_tests = .false.
        options%filter_string = ""

        call get_command_argument(0, program_name)
        num_arguments = command_argument_count()
        i = 1
        do while (i <= num_arguments)
            call get_command_argument(i, argument)
            select case (trim(argument))
            case ("-h", "--help")
                write(output_unit, '(A)') usageMessage(program_name)
                call exit(0)
            case ("-f", "--filter")
                options%filter_tests = .true.
                i = i + 1
                call get_command_argument(i, argument)
                options%filter_string = trim(argument)
            case ("-q", "--quiet")
                options%quiet = .true.
            case ("-v", "--verbose")
                options%verbose = .true.
            case default
                write(error_unit, '(A)') &
                        "Unknown argument: '" // trim(argument) // "'" // NEWLINE
                write(error_unit, '(A)') usageMessage(program_name)
                call exit(1)
            end select
            i = i + 1
        end do
    contains
        pure function usageMessage(program_name_)
            character(len=*), intent(in) :: program_name_
            character(len=:), allocatable :: usageMessage

            usageMessage = &
                    "Usage: " // trim(program_name_) // " [-h] [-q] [-v] [-f string]" // NEWLINE &
                    // "  options:" // NEWLINE &
                    // "    -h, --help                    Output this message and exit" // NEWLINE &
                    // "    -q, --quiet                   Don't print the test descriptions before" // NEWLINE &
                    // "                                  running the tests" // NEWLINE &
                    // "    -v, --verbose                 Print all of the assertion messages, not" // NEWLINE &
                    // "                                  just the failing ones" // NEWLINE &
                    // "    -f string, --filter string    Only run cases or collections whose" // NEWLINE &
                    // "                                  description contains the given string" // NEWLINE
        end function usageMessage
    end function getOptions

    pure function getTestItems(maybes) result(test_items)
        type(MaybeItem_t), intent(in) :: maybes(:)
        type(TestItem_t), allocatable :: test_items(:)

        logical, allocatable :: are_values(:)
        integer :: i
        type(MaybeItem_t), allocatable :: maybe_outputs(:)
        integer :: num_inputs
        integer :: num_outputs

        num_inputs = size(maybes)
        allocate(are_values(num_inputs))
        are_values = maybes%hasValue()
        num_outputs = count(are_values)
        allocate(maybe_outputs(num_outputs))
        maybe_outputs = pack(maybes, are_values)
        allocate(test_items(num_outputs))
        do i = 1, num_outputs
            select type (maybe => maybe_outputs(i)%maybe)
            type is (JustTestItem_t)
                test_items(i) = maybe%getValue()
            end select
        end do
    end function getTestItems

    pure function getValueInputTestCase(just_) result(value_)
        class(JustInputTestCase_t), intent(in) :: just_
        type(InputTestCase_t) :: value_

        value_ = just_%value_
    end function getValueInputTestCase

    pure function getValueTestCase(just_) result(value_)
        class(JustTestCase_t), intent(in) :: just_
        type(TestCase_t) :: value_

        value_ = just_%value_
    end function getValueTestCase

    pure function getValueTestCaseWithExamples(just_) result(value_)
        class(JustTestCaseWithExamples_t), intent(in) :: just_
        type(TestCaseWithExamples_t) :: value_

        value_ = just_%value_
    end function getValueTestCaseWithExamples

    pure function getValueTestCollection(just_) result(value_)
        class(JustTestCollection_t), intent(in) :: just_
        type(TestCollection_t) :: value_

        value_ = just_%value_
    end function getValueTestCollection

    pure function getValueTestCollectionWithInput(just_) result(value_)
        class(JustTestCollectionWithInput_t), intent(in) :: just_
        type(TestCollectionWithInput_t) :: value_

        value_ = just_%value_
    end function getValueTestCollectionWithInput

    pure function getValueTestItem(just_) result(value_)
        class(JustTestItem_t), intent(in) :: just_
        type(TestItem_t) :: value_

        value_ = just_%value_
    end function getValueTestItem

    pure function getValueTransformingTestCollection(just_) result(value_)
        class(JustTransformingTestCollection_t), intent(in) :: just_
        type(TransformingTestCollection_t) :: value_

        value_ = just_%value_
    end function getValueTransformingTestCollection

    pure function givenBasic(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        test_collection = describe("Given " // description, tests)
    end function givenBasic

    pure function givenWithInput(description, input, tests) result(test_collection)
        character(len=*), intent(in) :: description
        class(*), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        test_collection = describe("Given " // description, input, tests)
    end function givenWithInput

    pure function hangingIndent(string__) result(indented)
        character(len=*), intent(in) :: string__
        character(len=:), allocatable :: indented

        type(VegetableString_t), allocatable :: lines(:)

        lines = splitAt(string__, NEWLINE)
        indented = join(lines, NEWLINE // "    ")
    end function hangingIndent

    elemental function hasValue(self)
        class(MaybeItem_t), intent(in) :: self
        logical :: hasValue

        select type (maybe => self%maybe)
        type is (Nothing_t)
            hasValue = .false.
        class default
            hasValue = .true.
        end select
    end function hasValue

    pure function includes(string, search_for)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: search_for
        logical :: includes

        includes = index(string, search_for) > 0
    end function includes

    function InputTestCase(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(inputTest) :: func
        type(InputTestCase_t) :: test_case

        test_case%description_ = description
        test_case%test => func
    end function InputTestCase

    pure function inputTestCaseDescription(self) result(description)
        class(InputTestCase_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%description_
    end function inputTestCaseDescription

    pure function inputTestCaseNumCases(self) result(num_cases)
        class(InputTestCase_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function inputTestCaseNumCases

    pure function integerToCharacter(int) result(string)
        integer, intent(in) :: int
        character(len=:), allocatable :: string

        character(len=12) :: temp

        write(temp, '(I0)') int
        string = trim(temp)
    end function integerToCharacter

    function it_(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(inputTest) :: func
        type(TestItem_t) :: test_case

        allocate(InputTestCase_t :: test_case%test)
        select type (test => test_case%test)
        type is (InputTestCase_t)
            test = InputTestCase(description, func)
        end select
    end function it_

    function itBasic(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: func
        type(TestItem_t) :: test_case

        allocate(TestCase_t :: test_case%test)
        select type (test => test_case%test)
        type is (TestCase_t)
            test = TestCase(description, func)
        end select
    end function itBasic

    function itWithExamples(description, examples, func) result(test_case)
        character(len=*), intent(in) :: description
        type(Example_t), intent(in) :: examples(:)
        procedure(inputTest) :: func
        type(TestItem_t) :: test_case

        allocate(TestCaseWithExamples_t :: test_case%test)
        select type (test => test_case%test)
        type is (TestCaseWithExamples_t)
            test = TestCaseWithExamples(description, examples, func)
        end select
    end function itWithExamples

    pure function join(strings_, separator) result(string)
        type(VegetableString_t), intent(in) :: strings_(:)
        character(len=*), intent(in) :: separator
        character(len=:), allocatable :: string

        integer :: i

        string = strings_(1)%string
        do i = 2, size(strings_)
            string = string // separator // strings_(i)%string
        end do
    end function join

    pure function JustInputTestCase(value_) result(just_)
        type(InputTestCase_t), intent(in) :: value_
        type(JustInputTestCase_t) :: just_

        just_ = JustInputTestCase_t(value_)
    end function JustInputTestCase

    pure function JustTestCase(value_) result(just_)
        type(TestCase_t), intent(in) :: value_
        type(JustTestCase_t) :: just_

        just_ = JustTestCase_t(value_)
    end function JustTestCase

    pure function JustTestCaseWithExamples(value_) result(just_)
        type(TestCaseWithExamples_t), intent(in) :: value_
        type(JustTestCaseWithExamples_t) :: just_

        just_ = JustTestCaseWithExamples_t(value_)
    end function JustTestCaseWithExamples

    pure function JustTestCollection(value_) result(just_)
        type(TestCollection_t), intent(in) :: value_
        type(JustTestCollection_t) :: just_

        just_ = JustTestCollection_t(value_)
    end function JustTestCollection

    pure function JustTestCollectionWithInput(value_) result(just_)
        type(TestCollectionWithInput_t), intent(in) :: value_
        type(JustTestCollectionWithInput_t) :: just_

        just_ = JustTestCollectionWithInput_t(value_)
    end function JustTestCollectionWithInput

    pure function JustTestItem(value_) result(just_)
        type(TestItem_t), intent(in) :: value_
        type(JustTestItem_t) :: just_

        just_ = JustTestItem_t(value_)
    end function JustTestItem

    pure function JustTransformingTestCollection(value_) result(just_)
        type(TransformingTestCollection_t), intent(in) :: value_
        type(JustTransformingTestCollection_t) :: just_

        just_ = JustTransformingTestCollection_t(value_)
    end function JustTransformingTestCollection

    pure function lastCharacter(string) result(char_)
        character(len=*), intent(in) :: string
        character(len=1) :: char_

        integer :: length

        length = len(trim(string))
        char_ = string(length:length)
    end function lastCharacter

    pure function makeDoesntIncludeFailureMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: message

        message = &
                "Expected " // delimit(string) &
                // " to not include " // delimit(search_for)
    end function makeDoesntIncludeFailureMessage

    pure function makeDoesntIncludeSuccessMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: message

        message = &
                delimit(string) // " did not include " &
                // delimit(search_for)
    end function makeDoesntIncludeSuccessMessage

    pure function makeEmptyFailureMessage(string) result(message)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: message

        message = "String " // delimit(string) // " wasn't empty"
    end function makeEmptyFailureMessage

    pure function makeEqualsFailureMessage(expected, actual) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=:), allocatable :: message

        message = &
                "Expected " // delimit(expected) &
                // " but got " // delimit(actual)
    end function makeEqualsFailureMessage

    pure function makeEqualsSuccessMessage(expected) result(message)
        character(len=*), intent(in) :: expected
        character(len=:), allocatable :: message

        message = "Expected and got " // delimit(expected)
    end function makeEqualsSuccessMessage

    pure function makeIncludesFailureMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: message

        message = &
                "Expected " // delimit(string) &
                // " to include " // delimit(search_for)
    end function makeIncludesFailureMessage

    pure function makeIncludesSuccessMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: message

        message = &
                delimit(string) // " included " &
                // delimit(search_for)
    end function makeIncludesSuccessMessage

    pure function makeUserMessage(message) result(user_message)
        character(len=*), intent(in) :: message
        character(len=:), allocatable :: user_message

        if (message == "") then
            user_message = ""
        else
            user_message = "; User Message: " // delimit(message)
        end if
    end function makeUserMessage

    pure function makeWithinFailureMessage( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        character(len=:), allocatable :: message

        message = &
                "Expected " // delimit(actual) // " to be  within " &
                // delimit("±" // tolerance) // " of " // delimit(expected)
    end function makeWithinFailureMessage

    pure function makeWithinSuccesMessage( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        character(len=:), allocatable :: message

        message = &
                delimit(actual) // " was within " // delimit("±" // tolerance) &
                // " of " // delimit(expected)
    end function makeWithinSuccesMessage

    pure function removeTrailingZeros(number) result(trimmed)
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: trimmed

        trimmed = trim(number)
        do while (lastCharacter(trimmed) == "0")
            trimmed = withoutLastCharacter(trimmed)
        end do
    end function removeTrailingZeros

    pure function Result_( &
            passed, &
            all_message, &
            failing_message, &
            num_failling_asserts, &
            num_passing_asserts)
        logical, intent(in) :: passed
        character(len=*), intent(in) :: all_message
        character(len=*), intent(in) :: failing_message
        integer, intent(in) :: num_failling_asserts
        integer, intent(in) :: num_passing_asserts
        type(Result_t) :: Result_

        Result_ = Result_t( &
                all_message = all_message, &
                failing_message = failing_message, &
                initialized = .true., &
                num_failling_asserts = num_failling_asserts, &
                num_passing_asserts = num_passing_asserts, &
                passed_ = passed)
    end function Result_

    pure function resultFailureDescription(self) result(description)
        class(Result_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%failing_message
    end function resultFailureDescription

    pure function resultNumAsserts(self) result(num_asserts)
        class(Result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%num_passing_asserts + self%num_failling_asserts
    end function resultNumAsserts

    pure function resultNumFailing(self) result(num_asserts)
        class(Result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%num_failling_asserts
    end function resultNumFailing

    pure function resultPassed(self) result(passed)
        class(Result_t), intent(in) :: self
        logical :: passed

        passed = self%passed_
    end function resultPassed

    pure function resultVerboseDescription(self) result(description)
        class(Result_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%all_message
    end function resultVerboseDescription

    function runCase(self) result(result__)
        class(TestCase_t), intent(in) :: self
        type(TestCaseResult_t) :: result__

        result__ = TestCaseResult(self%description_, self%test())
    end function runCase

    function runCaseWithExamples(self) result(result__)
        class(TestCaseWithExamples_t), intent(in) :: self
        type(TestCaseResult_t) :: result__

        integer :: i
        type(Result_t) :: results

        do i = 1, size(self%examples)
            results = results.and.self%test(self%examples(i)%value_)
        end do
        result__ = TestCaseResult(self%description_, results)
    end function runCaseWithExamples

    function runCaseWithInput(self, input) result(result__)
        class(InputTestCase_t), intent(in) :: self
        class(*), intent(in) :: input
        type(TestCaseResult_t) :: result__

        result__ = TestCaseResult(self%description_, self%test(input))
    end function runCaseWithInput

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

    function runCollectionThatHasInput(self) result(result__)
        class(TestCollectionWithInput_t), intent(in) :: self
        type(TestCollectionResult_t) :: result__

        integer :: i
        integer :: num_tests
        type(TestResultItem_t), allocatable :: results(:)

        num_tests = size(self%tests)
        allocate(results(num_tests))
        do i = 1, num_tests
            results(i) = self%tests(i)%runWithInput(self%input)
        end do
        result__ = TestCollectionResult(self%description_, results)
    end function runCollectionThatHasInput

    function runTestItem(self) result(result_item)
        class(TestItem_t), intent(in) :: self
        type(TestResultItem_t) :: result_item

        select type (test => self%test)
        type is (InputTestCase_t)
            allocate(TestCaseResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCaseResult_t)
                result_ = TestCaseResult(test%description_, fail("No input provided"))
            end select
        type is (TestCase_t)
            allocate(TestCaseResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCaseResult_t)
                result_ = test%run()
            end select
        type is (TestCaseWithExamples_t)
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
        type is (TestCollectionWithInput_t)
            allocate(TestCollectionResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCollectionResult_t)
                result_ = test%run()
            end select
        type is (TransformingTestCollection_t)
            allocate(TestCaseResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCaseResult_t)
                result_ = TestCaseResult(test%description_, fail("No input provided"))
            end select
        end select
    end function runTestItem

    function runTestItemWithInput(self, input) result(result_item)
        class(TestItem_t), intent(in) :: self
        class(*), intent(in) :: input
        type(TestResultItem_t) :: result_item

        select type (test => self%test)
        type is (InputTestCase_t)
            allocate(TestCaseResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCaseResult_t)
                result_ = test%run(input)
            end select
        type is (TestCase_t)
            allocate(TestCaseResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCaseResult_t)
                result_ = test%run()
            end select
        type is (TestCaseWithExamples_t)
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
        type is (TestCollectionWithInput_t)
            allocate(TestCollectionResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCollectionResult_t)
                result_ = test%run()
            end select
        type is (TransformingTestCollection_t)
            allocate(TestCollectionResult_t :: result_item%result_)
            select type (result_ => result_item%result_)
            type is (TestCollectionResult_t)
                result_ = test%run(input)
            end select
        end select
    end function runTestItemWithInput

    subroutine runTests(tests)
        use iso_fortran_env, only: error_unit, output_unit

        type(TestItem_t) :: tests

        type(MaybeItem_t) :: maybe_tests
        type(Options_t) :: options
        type(TestResultItem_t) :: results
        type(TestItem_t) :: tests_to_run

        options = getOptions()

        if (options%filter_tests) then
            maybe_tests = tests%filter(options%filter_string)
            select type (maybe => maybe_tests%maybe)
            type is (JustTestItem_t)
                tests_to_run = maybe%getValue()
            type is (Nothing_t)
                write(error_unit, '(A)') "No matching tests found"
                call exit(1)
            end select
        else
            tests_to_run = tests
        end if
        write(output_unit, '(A)') "Running Tests"
        write(output_unit, '(A)')
        if (.not.options%quiet) then
            write(output_unit, '(A)') tests_to_run%description()
            write(output_unit, '(A)')
        end if
        write(output_unit, '(A)') &
                "A total of " // toCharacter(tests_to_run%numCases()) // " test cases"
        results = tests_to_run%run()
        if (results%passed()) then
            write(output_unit, '(A)')
            write(output_unit, '(A)') "All Passed"
            if (options%verbose) then
                write(output_unit, '(A)') results%verboseDescription()
            end if
            write(output_unit, '(A)') &
                    "A total of " // toCharacter(results%numCases()) &
                    // " test cases containg a total of " &
                    // toCharacter(results%numAsserts()) // " assertions"
            write(output_unit, '(A)')
        else
            write(error_unit, '(A)')
            write(error_unit, '(A)') "Failed"
            write(error_unit, '(A)')
            write(error_unit, '(A)') &
                    toCharacter(results%numFailingCases()) // " of " &
                    // toCharacter(results%numCases()) // " cases failed"
            write(error_unit, '(A)') &
                    toCharacter(results%numFailingAsserts()) // " of " &
                    // toCharacter(results%numAsserts()) // " assertions failed"
            write(error_unit, '(A)')
            if (options%verbose) then
                write(error_unit, '(A)') results%verboseDescription()
            else
                write(error_unit, '(A)') results%failureDescription()
            end if
            write(error_unit, '(A)')
            call exit(1)
        end if
    end subroutine

    function runTransformingCollection(self, input) result(result__)
        class(TransformingTestCollection_t), intent(in) :: self
        class(*), intent(in) :: input
        type(TestCollectionResult_t) :: result__

        integer :: i
        integer :: num_tests
        type(TestResultItem_t), allocatable :: results(:)
        type(Transformed_t) :: transformed_

        transformed_ = self%transformer(input)
        select type (next_input => transformed_%value_)
        type is (Result_t)
            allocate(results(1))
            allocate(TestCaseResult_t :: results(1)%result_)
            select type (the_result => results(1)%result_)
            type is (TestCaseResult_t)
                the_result = TestCaseResult("Transformation Failed", next_input)
            end select
            result__ = TestCollectionResult(self%description_, results)
        class default
            num_tests = size(self%tests)
            allocate(results(num_tests))
            do i = 1, num_tests
                results(i) = self%tests(i)%runWithInput(next_input)
            end do
            result__ = TestCollectionResult(self%description_, results)
        end select
    end function runTransformingCollection

    pure recursive function splitAt(&
            string_, split_characters) result(strings)
        character(len=*), intent(in) :: string_
        character(len=*), intent(in) :: split_characters
        type(VegetableString_t), allocatable :: strings(:)

        if (len(split_characters) > 0) then
            if (len(string_) > 0) then
                if (index(split_characters, string_(1:1)) > 0) then
                    strings = splitAt(string_(2:), split_characters)
                else if (index(split_characters, string_(len(string_):len(string_))) > 0) then
                    strings = splitAt(string_(1:len(string_) - 1), split_characters)
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
                rest = splitAt(string__(i + 1:), split_characters_)
                allocate(strings_(size(rest) + 1))
                strings_(1) = toString(this_string)
                strings_(2:) = rest(:)
            else
                allocate(strings_(1))
                strings_(1) = toString(string__)
            end if
        end function doSplit
    end function splitAt

    pure function succeed(message) result(success)
        character(len=*), intent(in) :: message
        type(Result_t) :: success

        success = Result_( &
                passed = .true., &
                all_message = message, &
                failing_message = "", &
                num_failling_asserts = 0, &
                num_passing_asserts = 1)
    end function succeed

    function TestCase(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: func
        type(TestCase_t) :: test_case

        test_case%description_ = description
        test_case%test => func
    end function TestCase

    pure function testCaseDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%description_
    end function testCaseDescription

    pure function testCaseFailureDescription(self) result(description)
        class(TestCaseResult_t), intent(in) :: self
        character(len=:), allocatable :: description

        if (self%passed()) then
            description = ""
        else
            description = hangingIndent( &
                    self%description // NEWLINE // self%result_%failureDescription())
        end if
    end function testCaseFailureDescription

    pure function testCaseNumAsserts(self) result(num_asserts)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%numAsserts()
    end function testCaseNumAsserts

    pure function testCaseNumCases(self) result(num_cases)
        class(TestCase_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function testCaseNumCases

    pure function testCaseNumFailing(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        if (self%passed()) then
            num_cases = 0
        else
            num_cases = 1
        end if
    end function testCaseNumFailing

    pure function testCaseNumFailingAsserts(self) result(num_asserts)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%numFailing()
    end function testCaseNumFailingAsserts

    pure function testCasePassed(self) result(passed)
        class(TestCaseResult_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function testCasePassed

    pure function TestCaseResult(description, result__) result(test_case_result)
        character(len=*), intent(in) :: description
        type(Result_t), intent(in) :: result__
        type(TestCaseResult_t) :: test_case_result

        test_case_result%description = description
        test_case_result%result_ = result__
    end function TestCaseResult

    pure function testCaseResultNumCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function testCaseResultNumCases

    pure function testCaseVerboseDescription(self) result(description)
        class(TestCaseResult_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = hangingIndent( &
                self%description // NEWLINE // self%result_%verboseDescription())
    end function testCaseVerboseDescription

    function TestCaseWithExamples(description, examples, func) result(test_case)
        character(len=*), intent(in) :: description
        type(Example_t), intent(in) :: examples(:)
        procedure(inputTest) :: func
        type(TestCaseWithExamples_t) :: test_case

        test_case%description_ = description
        allocate(test_case%examples(size(examples)))
        test_case%examples = examples
        test_case%test => func
    end function TestCaseWithExamples

    pure function testCaseWithExamplesDescription(self) result(description)
        class(TestCaseWithExamples_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%description_
    end function testCaseWithExamplesDescription

    pure function testCaseWithExamplesNumCases(self) result(num_cases)
        class(TestCaseWithExamples_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function testCaseWithExamplesNumCases

    pure function TestCollection(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        test_collection%description_ = description
        allocate(test_collection%tests(size(tests)))
        test_collection%tests = tests
    end function TestCollection

    pure function testCollectionDescription(self) result(description)
        class(TestCollection_t), intent(in) :: self
        character(len=:), allocatable :: description

        type(VegetableString_t), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%tests)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = toString(self%tests(i)%description())
        end do
        description = hangingIndent( &
                self%description_ // NEWLINE &
                // join(descriptions, NEWLINE))
    end function testCollectionDescription

    pure function testCollectionFailureDescription(self) result(description)
        class(TestCollectionResult_t), intent(in) :: self
        character(len=:), allocatable :: description

        type(VegetableString_t), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        if (self%passed()) then
            description = ""
        else
            num_cases = size(self%results)
            allocate(descriptions(num_cases))
            do concurrent (i = 1:num_cases)
                descriptions(i) = toString(self%results(i)%failureDescription())
            end do
            description = hangingIndent( &
                    self%description // NEWLINE &
                    // join(descriptions, NEWLINE))
        end if
    end function testCollectionFailureDescription

    pure function testCollectionNumAsserts(self) result(num_asserts)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = sum(self%results%numAsserts())
    end function testCollectionNumAsserts

    pure function testCollectionNumCases(self) result(num_cases)
        class(TestCollection_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%tests%numCases())
    end function testCollectionNumCases

    pure function testCollectionNumFailing(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%results%numFailingCases())
    end function testCollectionNumFailing

    pure function testCollectionNumFailingAsserts(self) result(num_asserts)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = sum(self%results%numFailingAsserts())
    end function testCollectionNumFailingAsserts

    pure function testCollectionPassed(self) result(passed)
        class(TestCollectionResult_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed())
    end function testCollectionPassed

    pure function TestCollectionResult(description, results) result(test_collection_result)
        character(len=*), intent(in) :: description
        type(TestResultItem_t), intent(in) :: results(:)
        type(TestCollectionResult_t) :: test_collection_result

        test_collection_result%description = description
        allocate(test_collection_result%results(size(results)))
        test_collection_result%results = results
    end function TestCollectionResult

    pure function testCollectionResultNumCases(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%results%numCases())
    end function testCollectionResultNumCases

    pure function testCollectionVerboseDescription(self) result(description)
        class(TestCollectionResult_t), intent(in) :: self
        character(len=:), allocatable :: description

        type(VegetableString_t), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%results)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = toString(self%results(i)%verboseDescription())
        end do
        description = hangingIndent( &
                self%description // NEWLINE &
                // join(descriptions, NEWLINE))
    end function testCollectionVerboseDescription

    pure function TestCollectionWithInput( &
            description, input, tests) result(test_collection)
        character(len=*), intent(in) :: description
        class(*), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestCollectionWithInput_t) :: test_collection

        test_collection%description_ = description
        test_collection%input = input
        allocate(test_collection%tests(size(tests)))
        test_collection%tests = tests
    end function TestCollectionWithInput

    pure function testCollectionWithInputDescription(self) result(description)
        class(TestCollectionWithInput_t), intent(in) :: self
        character(len=:), allocatable :: description

        type(VegetableString_t), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%tests)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = toString(self%tests(i)%description())
        end do
        description = hangingIndent( &
                self%description_ // NEWLINE &
                // join(descriptions, NEWLINE))
    end function testCollectionWithInputDescription

    pure function testCollectionWithInputNumCases(self) result(num_cases)
        class(TestCollectionWithInput_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%tests%numCases())
    end function testCollectionWithInputNumCases

    pure function testItemDescription(self) result(description)
        class(TestItem_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%test%description()
    end function testItemDescription

    elemental function testItemNumCases(self) result(num_cases)
        class(TestItem_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%test%numCases()
    end function testItemNumCases

    elemental function testItemPassed(self) result(passed)
        class(TestResultItem_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function testItemPassed

    pure function testResultItemFailureDescription(self) result(description)
        class(TestResultItem_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%result_%failureDescription()
    end function testResultItemFailureDescription

    elemental function testResultItemNumAsserts(self) result(num_asserts)
        class(TestResultItem_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%numAsserts()
    end function testResultItemNumAsserts

    elemental function testResultItemNumCases(self) result(num_cases)
        class(TestResultItem_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%numCases()
    end function testResultItemNumCases

    elemental function testResultItemNumFailing(self) result(num_cases)
        class(TestResultItem_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%numFailingCases()
    end function testResultItemNumFailing

    elemental function testResultItemNumFailingAsserts(self) result(num_asserts)
        class(TestResultItem_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%numFailingAsserts()
    end function testResultItemNumFailingAsserts

    pure function testResultItemVerboseDescription(self) result(description)
        class(TestResultItem_t), intent(in) :: self
        character(len=:), allocatable :: description

        description = self%result_%verboseDescription()
    end function testResultItemVerboseDescription

    pure function testThat(tests) result(test_collection)
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        allocate(TestCollection_t :: test_collection%test)
        select type (test => test_collection%test)
        type is (TestCollection_t)
            test = TestCollection("Test that", tests)
        end select
    end function testThat

    function then(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: func
        type(TestItem_t) :: test_case

        test_case = it("Then " // description, func)
    end function then

    function then_(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(inputTest) :: func
        type(TestItem_t) :: test_case

        test_case = it_("Then " // description, func)
    end function then_

    pure function toString(chars) result(string)
        character(len=*), intent(in) :: chars
        type(VegetableString_t) :: string

        string%string = chars
    end function toString

    pure function Transformed(input)
        class(*), intent(in) :: input
        type(Transformed_t) :: Transformed

        Transformed%value_ = input
    end function

    function TransformingTestCollection( &
            description, func, tests) result(test_collection)
        character(len=*), intent(in) :: description
        procedure(transformer_) :: func
        type(TestItem_t), intent(in) :: tests(:)
        type(TransformingTestCollection_t) :: test_collection

        test_collection%description_ = description
        test_collection%transformer => func
        allocate(test_collection%tests(size(tests)))
        test_collection%tests = tests
    end function TransformingTestCollection

    pure function transformingTestCollectionDescription(self) result(description)
        class(TransformingTestCollection_t), intent(in) :: self
        character(len=:), allocatable :: description

        type(VegetableString_t), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%tests)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = toString(self%tests(i)%description())
        end do
        description = hangingIndent( &
                self%description_ // NEWLINE &
                // join(descriptions, NEWLINE))
    end function transformingTestCollectionDescription

    pure function transformingTestCollectionNumCases(self) result(num_cases)
        class(TransformingTestCollection_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%tests%numCases())
    end function transformingTestCollectionNumCases

    pure function whenBasic(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        test_collection = describe("When " // description, tests)
    end function whenBasic

    pure function whenWithInput(description, input, tests) result(test_collection)
        character(len=*), intent(in) :: description
        class(*), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        test_collection = describe("When " // description, input, tests)
    end function whenWithInput

    function whenWithTransformer(description, func, tests) result(test_collection)
        character(len=*), intent(in) :: description
        procedure(transformer_) :: func
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: test_collection

        allocate(TransformingTestCollection_t :: test_collection%test)
        select type (test => test_collection%test)
        type is (TransformingTestCollection_t)
            test = TransformingTestCollection("When " // description, func, tests)
        end select
    end function whenWithTransformer

    pure function withoutLastCharacter(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: withoutLastCharacter

        character(len=:), allocatable :: trimmed

        allocate(character(len=0) :: trimmed)
        trimmed = trim(string)
        withoutLastCharacter = trimmed(1:len(trimmed)-1)
    end function withoutLastCharacter
end module Vegetables_m
