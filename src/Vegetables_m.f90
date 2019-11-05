module Vegetables_m
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type :: Options_t
        private
        logical :: colorize
        logical :: quiet
        logical :: verbose
        logical :: filter_tests
        type(VARYING_STRING) :: filter_string
    end type

    type, public, abstract :: Input_t
    end type Input_t

    type, public, extends(Input_t) :: DoublePrecisionInput_t
        double precision :: value_
    end type DoublePrecisionInput_t

    type, public, extends(Input_t) :: IntegerInput_t
        integer :: value_
    end type IntegerInput_t

    type, public, extends(Input_t) :: StringInput_t
        type(VARYING_STRING) :: value_
    end type StringInput_t

    type, public :: Transformed_t
        private
        class(Input_t), allocatable :: input
    end type Transformed_t

    type, public :: Example_t
        private
        class(Input_t), allocatable :: input
    end type Example_t

    type, public :: Generated_t
        private
        class(Input_t), allocatable :: input
    end type Generated_t

    type, public :: ShrinkResult_t
        private
        class(Input_t), allocatable :: input
        logical :: simplest
    end type ShrinkResult_t

    type, public, abstract :: Generator_t
    contains
        private
        procedure(generate_), public, deferred :: generate
        procedure(shrink_), nopass, public, deferred :: shrink
    end type Generator_t

    type, public, extends(Generator_t) :: AsciiStringGenerator_t
    contains
        private
        procedure, public :: generate => generateAsciiString
        procedure, nopass, public :: shrink => shrinkAsciiString
    end type AsciiStringGenerator_t

    type, public, extends(Generator_t) :: IntegerGenerator_t
    contains
        private
        procedure, public :: generate => generateInteger
        procedure, nopass, public :: shrink => shrinkInteger
    end type IntegerGenerator_t

    type, abstract :: Test_t
        private
        type(VARYING_STRING) :: description_
    contains
        private
        procedure(testDescription), public, deferred :: description
        procedure(filter_), public, deferred :: filter
        procedure(testCount), public, deferred :: numCases
        procedure(runWithInput_), deferred :: runWithInput
        procedure(runWithoutInput_), deferred :: runWithoutInput
        generic :: run => runWithInput, runWithoutInput
        procedure(testDescription), deferred :: repr
    end type Test_t

    type, public :: TestItem_t
        private
        class(Test_t), allocatable :: test
    contains
        private
        procedure, public :: description => testItemDescription
        procedure, public :: filter => testItemFilter
        procedure, public :: numCases => testItemNumCases
        procedure :: runWithInput => testItemRunWithInput
        procedure :: runWithoutInput => testItemRunWithoutInput
        generic, public :: run => runWithInput, runWithoutInput
        procedure :: repr => testItemRepr
    end type TestItem_t

    type, public, abstract, extends(Test_t) :: TestCase_t
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: filter => testCaseFilter
        procedure, public :: numCases => testCaseNumCases
    end type TestCase_t

    type, public, extends(TestCase_t) :: SimpleTestCase_t
        private
        procedure(simpleTest), nopass, pointer :: test
    contains
        private
        procedure :: runWithInput => simpleTestCaseRunWithInput
        procedure :: runWithoutInput => simpleTestCaseRunWithoutInput
        procedure :: repr => simpleTestCaseRepr
    end type SimpleTestCase_t

    type, public, extends(TestCase_t) :: InputTestCase_t
        private
        procedure(inputTest), nopass, pointer :: test
    contains
        private
        procedure :: runWithInput => inputTestCaseRunWithInput
        procedure :: runWithoutInput => inputTestCaseRunWithoutInput
        procedure :: repr => inputTestCaseRepr
    end type InputTestCase_t

    type, public, extends(TestCase_t) :: TestCaseWithExamples_t
        private
        type(Example_t), allocatable :: examples(:)
        procedure(inputTest), nopass, pointer :: test
    contains
        private
        procedure :: runWithInput => testCaseWithExamplesRunWithInput
        procedure :: runWithoutInput => testCaseWithExamplesRunWithoutInput
        procedure :: repr => testCaseWithExamplesRepr
    end type TestCaseWithExamples_t

    type, public, extends(TestCase_t) :: TestCaseWithGenerator_t
        private
        class(Generator_t), allocatable :: generator
        procedure(inputTest), nopass, pointer :: test
    contains
        private
        procedure :: runWithInput => testCaseWithGeneratorRunWithInput
        procedure :: runWithoutInput => testCaseWithGeneratorRunWithoutInput
        procedure :: repr => testCaseWithGeneratorRepr
    end type TestCaseWithGenerator_t

    type, public, abstract, extends(Test_t) :: TestCollection_t
        private
        type(TestItem_t), allocatable :: tests(:)
    contains
        private
        procedure, public :: description => testCollectionDescription
        procedure, public :: filter => testCollectionFilter
        procedure, public :: numCases => testCollectionNumCases
    end type TestCollection_t

    type, public, extends(TestCollection_t) :: SimpleTestCollection_t
    contains
        private
        procedure :: runWithInput => simpleTestCollectionRunWithInput
        procedure :: runWithoutInput => simpleTestCollectionRunWithoutInput
        procedure :: repr => simpleTestCollectionRepr
    end type SimpleTestCollection_t

    type, public, extends(TestCollection_t) :: TestCollectionWithInput_t
        private
        class(Input_t), allocatable :: input
    contains
        private
        procedure :: runWithInput => testCollectionWithInputRunWithInput
        procedure :: runWithoutInput => testCollectionWithInputRunWithoutInput
        procedure :: repr => testCollectionWithInputRepr
        final :: testCollectionWithInputDestructor
    end type TestCollectionWithInput_t

    type, public, extends(TestCollection_t) :: TransformingTestCollection_t
        private
        procedure(transformer_), nopass, pointer :: transformer
    contains
        private
        procedure :: runWithInput => transformingTestCollectionRunWithInput
        procedure :: runWithoutInput => transformingTestCollectionRunWithoutInput
        procedure :: repr => transformingTestCollectionRepr
    end type TransformingTestCollection_t

    type :: IndividualResult_t
        private
        type(VARYING_STRING) :: message
        logical :: passed_
    contains
        private
        procedure :: failureDescription => individualResultFailureDescription
        procedure :: verboseDescription => individualResultVerboseDescription
        procedure :: repr => individualResultRepr
    end type IndividualResult_t

    type, public :: Result_t
        private
        type(IndividualResult_t), allocatable :: results(:)
    contains
        private
        procedure :: combineResults
        generic, public :: operator(.and.) => combineResults
        procedure, public :: numAsserts => resultNumAsserts
        procedure, public :: numFailingAsserts => resultNumFailingAsserts
        procedure, public :: passed => resultPassed
        procedure, public :: failureDescription => resultFailureDescription
        procedure, public :: verboseDescription => resultVerboseDescription
        procedure :: repr => resultRepr
    end type Result_t

    type, abstract :: TestResult_t
        private
        type(VARYING_STRING) :: description
    contains
        private
        procedure(testResultCount), public, deferred :: numAsserts
        procedure(testResultCount), public, deferred :: numCases
        procedure(testResultCount), public, deferred :: numFailingAsserts
        procedure(testResultCount), public, deferred :: numFailingCases
        procedure(testResultPassed), public, deferred :: passed
        procedure(testResultColorizedDescription), public, deferred :: &
                failureDescription
        procedure(testResultColorizedDescription), public, deferred :: &
                verboseDescription
        procedure(testResultDescription), deferred :: repr
    end type TestResult_t

    type, public :: TestResultItem_t
        private
        class(TestResult_t), allocatable :: result_
    contains
        private
        procedure, public :: numAsserts => testResultItemNumAsserts
        procedure, public :: numCases => testResultItemNumCases
        procedure, public :: numFailingAsserts => testResultItemNumFailingAsserts
        procedure, public :: numFailingCases => testResultItemNumFailingCases
        procedure, public :: passed => testResultItemPassed
        procedure, public :: failureDescription => testResultItemFailureDescription
        procedure, public :: verboseDescription => testResultItemVerboseDescription
        procedure :: repr => testResultItemRepr
    end type TestResultItem_t

    type, public, extends(TestResult_t) :: TestCaseResult_t
        private
        type(Result_t) :: result_
    contains
        private
        procedure, public :: numAsserts => testCaseResultNumAsserts
        procedure, public :: numCases => testCaseResultNumCases
        procedure, public :: numFailingAsserts => testCaseResultNumFailingAsserts
        procedure, public :: numFailingCases => testCaseResultNumFailingCases
        procedure, public :: passed => testCaseResultPassed
        procedure, public :: failureDescription => &
                testCaseResultFailureDescription
        procedure, public :: verboseDescription => &
                testCaseResultVerboseDescription
        procedure :: repr => testCaseResultRepr
        final :: testCaseResultDestructor
    end type TestCaseResult_t

    type, public, extends(TestResult_t) :: TestCollectionResult_t
        private
        type(TestResultItem_t), allocatable :: results(:)
    contains
        private
        procedure, public :: numAsserts => testCollectionResultNumAsserts
        procedure, public :: numCases => testCollectionResultNumCases
        procedure, public :: numFailingAsserts => &
                testCollectionResultNumFailingAsserts
        procedure, public :: numFailingCases => &
                testCollectionResultNumFailingCases
        procedure, public :: passed => testCollectionResultPassed
        procedure, public :: failureDescription => &
                testCollectionResultFailureDescription
        procedure, public :: verboseDescription => &
                testCollectionResultVerboseDescription
        procedure :: repr => testCollectionResultRepr
    end type TestCollectionResult_t

    type :: FilterResult_t
        class(Test_t), allocatable :: test
        logical :: matched
    end type FilterResult_t

    type, public :: FilterItemResult_t
        type(TestItem_t) :: test
        logical :: matched
    end type FilterItemResult_t

    type, public, extends(Input_t) :: TransformationFailure_t
        type(Result_t) :: result_
    end type TransformationFailure_t

    abstract interface
        subroutine computation_
        end subroutine computation_

        function filter_(self, filter_string) result(filter_result)
            use iso_varying_string, only: VARYING_STRING
            import Test_t, FilterResult_t
            class(Test_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: filter_string
            type(FilterResult_t) :: filter_result
        end function filter_

        function generate_(self) result(generated_value)
            import :: Generated_t, Generator_t
            class(Generator_t), intent(in) :: self
            type(Generated_t) :: generated_value
        end function generate_

        function inputTest(input) result(result_)
            import Input_t, Result_t
            class(Input_t), intent(in) :: input
            type(Result_t) :: result_
        end function inputTest

        function runWithInput_(self, input) result(result_)
            import Input_t, Test_t, TestResultItem_t
            class(Test_t), intent(in) :: self
            class(Input_t), intent(in) :: input
            type(TestResultItem_t) :: result_
        end function runWithInput_

        function runWithoutInput_(self) result(result_)
            import Test_t, TestResultItem_t
            class(Test_t), intent(in) :: self
            type(TestResultItem_t) :: result_
        end function runWithoutInput_

        function shrink_(input) result(shrunk)
            import Input_t, ShrinkResult_t
            class(Input_t), intent(in) :: input
            type(ShrinkResult_t) :: shrunk
        end function shrink_

        function simpleTest() result(result_)
            import Result_t
            type(Result_t) :: result_
        end function simpleTest

        pure function testCount(self) result(num)
            import Test_t
            class(Test_t), intent(in) :: self
            integer :: num
        end function testCount

        function testDescription(self) result(description)
            use iso_varying_string, only: VARYING_STRING
            import Test_t
            class(Test_t), intent(in) :: self
            type(VARYING_STRING) :: description
        end function testDescription

        function testResultColorizedDescription( &
                self, colorize) result(description)
            use iso_varying_string, only: VARYING_STRING
            import TestResult_t
            class(TestResult_t), intent(in) :: self
            logical, intent(in) :: colorize
            type(VARYING_STRING) :: description
        end function testResultColorizedDescription

        pure function testResultCount(self) result(num)
            import TestResult_t
            class(TestResult_t), intent(in) :: self
            integer :: num
        end function testResultCount

        function testResultDescription(self) result(description)
            use iso_varying_string, only: VARYING_STRING
            import TestResult_t
            class(TestResult_t), intent(in) :: self
            type(VARYING_STRING) :: description
        end function testResultDescription

        pure function testResultPassed(self) result(passed)
            import TestResult_t
            class(TestResult_t), intent(in) :: self
            logical :: passed
        end function testResultPassed

        function transformer_(input) result(output)
            import Input_t, Transformed_t
            class(Input_t), intent(in) :: input
            type(Transformed_t) :: output
        end function transformer_
    end interface

    interface assertDoesntInclude
        module procedure assertDoesntIncludeBasicCC
        module procedure assertDoesntIncludeBasicCS
        module procedure assertDoesntIncludeBasicSC
        module procedure assertDoesntIncludeBasicSS
        module procedure assertDoesntIncludeWithMessageCCC
        module procedure assertDoesntIncludeWithMessageCCS
        module procedure assertDoesntIncludeWithMessageCSC
        module procedure assertDoesntIncludeWithMessageCSS
        module procedure assertDoesntIncludeWithMessageSCC
        module procedure assertDoesntIncludeWithMessageSCS
        module procedure assertDoesntIncludeWithMessageSSC
        module procedure assertDoesntIncludeWithMessageSSS
        module procedure assertDoesntIncludeWithMessagesCCCC
        module procedure assertDoesntIncludeWithMessagesCCCS
        module procedure assertDoesntIncludeWithMessagesCCSC
        module procedure assertDoesntIncludeWithMessagesCCSS
        module procedure assertDoesntIncludeWithMessagesCSCC
        module procedure assertDoesntIncludeWithMessagesCSCS
        module procedure assertDoesntIncludeWithMessagesCSSC
        module procedure assertDoesntIncludeWithMessagesCSSS
        module procedure assertDoesntIncludeWithMessagesSCCC
        module procedure assertDoesntIncludeWithMessagesSCCS
        module procedure assertDoesntIncludeWithMessagesSCSC
        module procedure assertDoesntIncludeWithMessagesSCSS
        module procedure assertDoesntIncludeWithMessagesSSCC
        module procedure assertDoesntIncludeWithMessagesSSCS
        module procedure assertDoesntIncludeWithMessagesSSSC
        module procedure assertDoesntIncludeWithMessagesSSSS
    end interface assertDoesntInclude

    interface assertEmpty
        module procedure assertEmptyBasicC
        module procedure assertEmptyBasicS
        module procedure assertEmptyWithMessageCC
        module procedure assertEmptyWithMessageCS
        module procedure assertEmptyWithMessageSC
        module procedure assertEmptyWithMessageSS
        module procedure assertEmptyWithMessagesCCC
        module procedure assertEmptyWithMessagesCCS
        module procedure assertEmptyWithMessagesCSC
        module procedure assertEmptyWithMessagesCSS
        module procedure assertEmptyWithMessagesSCC
        module procedure assertEmptyWithMessagesSCS
        module procedure assertEmptyWithMessagesSSC
        module procedure assertEmptyWithMessagesSSS
    end interface assertEmpty

    interface assertEquals
        module procedure assertEqualsDoublePrecision
        module procedure assertEqualsDoublePrecisionWithMessageC
        module procedure assertEqualsDoublePrecisionWithMessageS
        module procedure assertEqualsDoublePrecisionWithMessagesCC
        module procedure assertEqualsDoublePrecisionWithMessagesCS
        module procedure assertEqualsDoublePrecisionWithMessagesSC
        module procedure assertEqualsDoublePrecisionWithMessagesSS
        module procedure assertEqualsIntegerBasic
        module procedure assertEqualsIntegerWithMessageC
        module procedure assertEqualsIntegerWithMessageS
        module procedure assertEqualsIntegerWithMessagesCC
        module procedure assertEqualsIntegerWithMessagesCS
        module procedure assertEqualsIntegerWithMessagesSC
        module procedure assertEqualsIntegerWithMessagesSS
        module procedure assertEqualsStringsCC
        module procedure assertEqualsStringsCS
        module procedure assertEqualsStringsSC
        module procedure assertEqualsStringsSS
        module procedure assertEqualsStringsWithMessageCCC
        module procedure assertEqualsStringsWithMessageCCS
        module procedure assertEqualsStringsWithMessageCSC
        module procedure assertEqualsStringsWithMessageCSS
        module procedure assertEqualsStringsWithMessageSCC
        module procedure assertEqualsStringsWithMessageSCS
        module procedure assertEqualsStringsWithMessageSSC
        module procedure assertEqualsStringsWithMessageSSS
        module procedure assertEqualsStringsWithMessagesCCCC
        module procedure assertEqualsStringsWithMessagesCCCS
        module procedure assertEqualsStringsWithMessagesCCSC
        module procedure assertEqualsStringsWithMessagesCCSS
        module procedure assertEqualsStringsWithMessagesCSCC
        module procedure assertEqualsStringsWithMessagesCSCS
        module procedure assertEqualsStringsWithMessagesCSSC
        module procedure assertEqualsStringsWithMessagesCSSS
        module procedure assertEqualsStringsWithMessagesSCCC
        module procedure assertEqualsStringsWithMessagesSCCS
        module procedure assertEqualsStringsWithMessagesSCSC
        module procedure assertEqualsStringsWithMessagesSCSS
        module procedure assertEqualsStringsWithMessagesSSCC
        module procedure assertEqualsStringsWithMessagesSSCS
        module procedure assertEqualsStringsWithMessagesSSSC
        module procedure assertEqualsStringsWithMessagesSSSS
    end interface assertEquals

    interface assertEqualsWithinAbsolute
        module procedure assertEqualsWithinAbsoluteBasic
        module procedure assertEqualsWithinAbsoluteWithMessageC
        module procedure assertEqualsWithinAbsoluteWithMessageS
        module procedure assertEqualsWithinAbsoluteWithMessagesCC
        module procedure assertEqualsWithinAbsoluteWithMessagesCS
        module procedure assertEqualsWithinAbsoluteWithMessagesSC
        module procedure assertEqualsWithinAbsoluteWithMessagesSS
    end interface assertEqualsWithinAbsolute

    interface assertEqualsWithinRelative
        module procedure assertEqualsWithinRelativeBasic
        module procedure assertEqualsWithinRelativeWithMessageC
        module procedure assertEqualsWithinRelativeWithMessageS
        module procedure assertEqualsWithinRelativeWithMessagesCC
        module procedure assertEqualsWithinRelativeWithMessagesCS
        module procedure assertEqualsWithinRelativeWithMessagesSC
        module procedure assertEqualsWithinRelativeWithMessagesSS
    end interface assertEqualsWithinRelative

    interface assertFasterThan
        module procedure assertFasterThanAbsoluteBracketed
        module procedure assertFasterThanAbsoluteBracketedWithMessageC
        module procedure assertFasterThanAbsoluteBracketedWithMessageS
        module procedure assertFasterThanAbsoluteBracketedWithMessagesCC
        module procedure assertFasterThanAbsoluteBracketedWithMessagesCS
        module procedure assertFasterThanAbsoluteBracketedWithMessagesSC
        module procedure assertFasterThanAbsoluteBracketedWithMessagesSS
        module procedure assertFasterThanAbsoluteSimple
        module procedure assertFasterThanAbsoluteSimpleWithMessageC
        module procedure assertFasterThanAbsoluteSimpleWithMessageS
        module procedure assertFasterThanAbsoluteSimpleWithMessagesCC
        module procedure assertFasterThanAbsoluteSimpleWithMessagesCS
        module procedure assertFasterThanAbsoluteSimpleWithMessagesSC
        module procedure assertFasterThanAbsoluteSimpleWithMessagesSS
        module procedure assertFasterThanRelativeBracketed
        module procedure assertFasterThanRelativeBracketedWithMessageC
        module procedure assertFasterThanRelativeBracketedWithMessageS
        module procedure assertFasterThanRelativeBracketedWithMessagesCC
        module procedure assertFasterThanRelativeBracketedWithMessagesCS
        module procedure assertFasterThanRelativeBracketedWithMessagesSC
        module procedure assertFasterThanRelativeBracketedWithMessagesSS
        module procedure assertFasterThanRelativeSimple
        module procedure assertFasterThanRelativeSimpleWithMessageC
        module procedure assertFasterThanRelativeSimpleWithMessageS
        module procedure assertFasterThanRelativeSimpleWithMessagesCC
        module procedure assertFasterThanRelativeSimpleWithMessagesCS
        module procedure assertFasterThanRelativeSimpleWithMessagesSC
        module procedure assertFasterThanRelativeSimpleWithMessagesSS
    end interface assertFasterThan

    interface assertIncludes
        module procedure assertIncludesCC
        module procedure assertIncludesCS
        module procedure assertIncludesSC
        module procedure assertIncludesSS
        module procedure assertIncludesWithMessageCCC
        module procedure assertIncludesWithMessageCCS
        module procedure assertIncludesWithMessageCSC
        module procedure assertIncludesWithMessageCSS
        module procedure assertIncludesWithMessageSCC
        module procedure assertIncludesWithMessageSCS
        module procedure assertIncludesWithMessageSSC
        module procedure assertIncludesWithMessageSSS
        module procedure assertIncludesWithMessagesCCCC
        module procedure assertIncludesWithMessagesCCCS
        module procedure assertIncludesWithMessagesCCSC
        module procedure assertIncludesWithMessagesCCSS
        module procedure assertIncludesWithMessagesCSCC
        module procedure assertIncludesWithMessagesCSCS
        module procedure assertIncludesWithMessagesCSSC
        module procedure assertIncludesWithMessagesCSSS
        module procedure assertIncludesWithMessagesSCCC
        module procedure assertIncludesWithMessagesSCCS
        module procedure assertIncludesWithMessagesSCSC
        module procedure assertIncludesWithMessagesSCSS
        module procedure assertIncludesWithMessagesSSCC
        module procedure assertIncludesWithMessagesSSCS
        module procedure assertIncludesWithMessagesSSSC
        module procedure assertIncludesWithMessagesSSSS
    end interface assertIncludes

    interface assertNot
        module procedure assertNotBasic
        module procedure assertNotWithMessageC
        module procedure assertNotWithMessageS
        module procedure assertNotWithMessagesCC
        module procedure assertNotWithMessagesCS
        module procedure assertNotWithMessagesSC
        module procedure assertNotWithMessagesSS
    end interface assertNot

    interface assertThat
        module procedure assertThatBasic
        module procedure assertThatWithMessageC
        module procedure assertThatWithMessageS
        module procedure assertThatWithMessagesCC
        module procedure assertThatWithMessagesCS
        module procedure assertThatWithMessagesSC
        module procedure assertThatWithMessagesSS
    end interface assertThat

    interface delimit
        module procedure delimitC
        module procedure delimitS
    end interface delimit

    interface Describe
        module procedure DescribeBasicC
        module procedure DescribeBasicS
        module procedure DescribeWithInputC
        module procedure DescribeWithInputS
    end interface Describe

    interface fail
        module procedure failC
        module procedure failS
    end interface fail

    interface Given
        module procedure GivenWithInputC
        module procedure GivenWithInputS
    end interface Given

    interface It
        module procedure ItBasicC
        module procedure ItBasicS
        module procedure ItWithExamplesC
        module procedure ItWithExamplesS
        module procedure ItWithGeneratorC
        module procedure ItWithGeneratorS
    end interface It

    interface It_
        module procedure ItInputC
        module procedure ItInputS
    end interface It_

    interface makeDoesntIncludeFailureMessage
        module procedure makeDoesntIncludeFailureMessageCC
        module procedure makeDoesntIncludeFailureMessageCS
        module procedure makeDoesntIncludeFailureMessageSC
        module procedure makeDoesntIncludeFailureMessageSS
    end interface makeDoesntIncludeFailureMessage

    interface makeDoesntIncludeSuccessMessage
        module procedure makeDoesntIncludeSuccessMessageCC
        module procedure makeDoesntIncludeSuccessMessageCS
        module procedure makeDoesntIncludeSuccessMessageSC
        module procedure makeDoesntIncludeSuccessMessageSS
    end interface makeDoesntIncludeSuccessMessage

    interface makeEmptyFailureMessage
        module procedure makeEmptyFailureMessageC
        module procedure makeEmptyFailureMessageS
    end interface makeEmptyFailureMessage

    interface makeEqualsFailureMessage
        module procedure makeEqualsFailureMessageCC
        module procedure makeEqualsFailureMessageCS
        module procedure makeEqualsFailureMessageSC
        module procedure makeEqualsFailureMessageSS
    end interface makeEqualsFailureMessage

    interface makeEqualsSuccessMessage
        module procedure makeEqualsSuccessMessageC
        module procedure makeEqualsSuccessMessageS
    end interface makeEqualsSuccessMessage

    interface makeFasterThanFailureMessage
        module procedure makeFasterThanFailureMessageCCC
        module procedure makeFasterThanFailureMessageCCS
        module procedure makeFasterThanFailureMessageCSC
        module procedure makeFasterThanFailureMessageCSS
        module procedure makeFasterThanFailureMessageSCC
        module procedure makeFasterThanFailureMessageSCS
        module procedure makeFasterThanFailureMessageSSC
        module procedure makeFasterThanFailureMessageSSS
    end interface makeFasterThanFailureMessage

    interface makeFasterThanSuccessMessage
        module procedure makeFasterThanSuccessMessageCCC
        module procedure makeFasterThanSuccessMessageCCS
        module procedure makeFasterThanSuccessMessageCSC
        module procedure makeFasterThanSuccessMessageCSS
        module procedure makeFasterThanSuccessMessageSCC
        module procedure makeFasterThanSuccessMessageSCS
        module procedure makeFasterThanSuccessMessageSSC
        module procedure makeFasterThanSuccessMessageSSS
    end interface makeFasterThanSuccessMessage

    interface makeIncludesFailureMessage
        module procedure makeIncludesFailureMessageCC
        module procedure makeIncludesFailureMessageCS
        module procedure makeIncludesFailureMessageSC
        module procedure makeIncludesFailureMessageSS
    end interface makeIncludesFailureMessage

    interface makeIncludesSuccessMessage
        module procedure makeIncludesSuccessMessageCC
        module procedure makeIncludesSuccessMessageCS
        module procedure makeIncludesSuccessMessageSC
        module procedure makeIncludesSuccessMessageSS
    end interface makeIncludesSuccessMessage

    interface makeWithinFailureMessage
        module procedure makeWithinFailureMessageCCC
        module procedure makeWithinFailureMessageCCS
        module procedure makeWithinFailureMessageCSC
        module procedure makeWithinFailureMessageCSS
        module procedure makeWithinFailureMessageSCC
        module procedure makeWithinFailureMessageSCS
        module procedure makeWithinFailureMessageSSC
        module procedure makeWithinFailureMessageSSS
    end interface makeWithinFailureMessage

    interface makeWithinSuccessMessage
        module procedure makeWithinSuccessMessageCCC
        module procedure makeWithinSuccessMessageCCS
        module procedure makeWithinSuccessMessageCSC
        module procedure makeWithinSuccessMessageCSS
        module procedure makeWithinSuccessMessageSCC
        module procedure makeWithinSuccessMessageSCS
        module procedure makeWithinSuccessMessageSSC
        module procedure makeWithinSuccessMessageSSS
    end interface makeWithinSuccessMessage

    interface succeed
        module procedure succeedC
        module procedure succeedS
    end interface succeed

    interface Then__
        module procedure ThenInputC
        module procedure ThenInputS
    end interface Then__

    interface When
        module procedure whenWithTransformerC
        module procedure whenWithTransformerS
    end interface When

    interface withUserMessage
        module procedure withUserMessageCC
        module procedure withUserMessageCS
        module procedure withUserMessageSC
        module procedure withUserMessageSS
    end interface withUserMessage

    type(AsciiStringGenerator_t), parameter, public :: &
            ASCII_STRING_GENERATOR = AsciiStringGenerator_t()
    type(IntegerGenerator_t), parameter, public :: &
            INTEGER_GENERATOR = IntegerGenerator_t()

    character(len=*), parameter, public :: EMPTY_SUCCESS_MESSAGE = "String was empty"
    integer, parameter :: INDENTATION = 4
    double precision, parameter :: MACHINE_EPSILON = epsilon(0.0d0)
    double precision, parameter :: MACHINE_TINY = tiny(0.0d0)
    integer, parameter :: MAX_INT = HUGE(1)
    character(len=*), parameter, public :: NOT_FAILURE_MESSAGE = "Expected to not be true"
    character(len=*), parameter, public :: NOT_SUCCESS_MESSAGE = "Was not true"
    integer :: NUM_GENERATOR_TESTS = 100
    character(len=*), parameter, public :: THAT_FAILURE_MESSAGE = "Expected to be true"
    character(len=*), parameter, public :: THAT_SUCCESS_MESSAGE = "Was true"


    public :: &
            assertDoesntInclude, &
            assertEmpty, &
            assertEquals, &
            assertEqualsWithinAbsolute, &
            assertEqualsWithinRelative, &
            assertFasterThan, &
            assertIncludes, &
            assertNot, &
            assertThat, &
            delimit, &
            Describe, &
            Example, &
            fail, &
            Generated, &
            getRandomAsciiCharacter, &
            getRandomAsciiString, &
            getRandomAsciiStringWithMaxLength, &
            getRandomDoublePrecisionWithMagnitude, &
            getRandomDoublePrecisionWithRange, &
            getRandomInteger, &
            getRandomIntegerWithRange, &
            getRandomLogical, &
            Given, &
            It, &
            It_, &
            makeDoesntIncludeFailureMessage, &
            makeDoesntIncludeSuccessMessage, &
            makeEmptyFailureMessage, &
            makeEqualsFailureMessage, &
            makeEqualsSuccessMessage, &
            makeFasterThanFailureMessage, &
            makeFasterThanSuccessMessage, &
            makeIncludesFailureMessage, &
            makeIncludesSuccessMessage, &
            makeWithinFailureMessage, &
            makeWithinSuccessMessage, &
            runTests, &
            ShrunkValue, &
            SimplestValue, &
            succeed, &
            testThat, &
            Then__, &
            Transformed, &
            When, &
            withUserMessage
contains
    function assertDoesntIncludeBasicCC(search_for, string) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function assertDoesntIncludeBasicCC

    function assertDoesntIncludeBasicCS(search_for, string) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                string, &
                var_str(""), &
                var_str(""))
    end function assertDoesntIncludeBasicCS

    function assertDoesntIncludeBasicSC(search_for, string) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function assertDoesntIncludeBasicSC

    function assertDoesntIncludeBasicSS(search_for, string) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                string, &
                var_str(""), &
                var_str(""))
    end function assertDoesntIncludeBasicSS

    function assertDoesntIncludeWithMessageCCC( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function assertDoesntIncludeWithMessageCCC

    function assertDoesntIncludeWithMessageCCS( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                var_str(string), &
                message, &
                message)
    end function assertDoesntIncludeWithMessageCCS

    function assertDoesntIncludeWithMessageCSC( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                string, &
                var_str(message), &
                var_str(message))
    end function assertDoesntIncludeWithMessageCSC

    function assertDoesntIncludeWithMessageCSS( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                string, &
                message, &
                message)
    end function assertDoesntIncludeWithMessageCSS

    function assertDoesntIncludeWithMessageSCC( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function assertDoesntIncludeWithMessageSCC

    function assertDoesntIncludeWithMessageSCS( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                var_str(string), &
                message, &
                message)
    end function assertDoesntIncludeWithMessageSCS

    function assertDoesntIncludeWithMessageSSC( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                string, &
                var_str(message), &
                var_str(message))
    end function assertDoesntIncludeWithMessageSSC

    function assertDoesntIncludeWithMessageSSS( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                string, &
                message, &
                message)
    end function assertDoesntIncludeWithMessageSSS

    function assertDoesntIncludeWithMessagesCCCC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesCCCC

    function assertDoesntIncludeWithMessagesCCCS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function assertDoesntIncludeWithMessagesCCCS

    function assertDoesntIncludeWithMessagesCCSC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesCCSC

    function assertDoesntIncludeWithMessagesCCSS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                failure_message)
    end function assertDoesntIncludeWithMessagesCCSS

    function assertDoesntIncludeWithMessagesCSCC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesCSCC

    function assertDoesntIncludeWithMessagesCSCS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                failure_message)
    end function assertDoesntIncludeWithMessagesCSCS

    function assertDoesntIncludeWithMessagesCSSC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                string, &
                success_message, &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesCSSC

    function assertDoesntIncludeWithMessagesCSSS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                var_str(search_for), &
                string, &
                success_message, &
                failure_message)
    end function assertDoesntIncludeWithMessagesCSSS

    function assertDoesntIncludeWithMessagesSCCC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesSCCC

    function assertDoesntIncludeWithMessagesSCCS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function assertDoesntIncludeWithMessagesSCCS

    function assertDoesntIncludeWithMessagesSCSC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesSCSC

    function assertDoesntIncludeWithMessagesSCSS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                var_str(string), &
                success_message, &
                failure_message)
    end function assertDoesntIncludeWithMessagesSCSS

    function assertDoesntIncludeWithMessagesSSCC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesSSCC

    function assertDoesntIncludeWithMessagesSSCS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                string, &
                var_str(success_message), &
                failure_message)
    end function assertDoesntIncludeWithMessagesSSCS

    function assertDoesntIncludeWithMessagesSSSC( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude( &
                search_for, &
                string, &
                success_message, &
                var_str(failure_message))
    end function assertDoesntIncludeWithMessagesSSSC

    function assertDoesntIncludeWithMessagesSSSS( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: operator(.includes.)

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (string.includes.search_for) then
            result__ = fail(withUserMessage( &
                    makeDoesntIncludeFailureMessage(search_for, string), &
                    failure_message))
        else
            result__ = succeed(withUserMessage( &
                    makeDoesntIncludeSuccessMessage(search_for, string), &
                    success_message))
        end if
    end function assertDoesntIncludeWithMessagesSSSS

    function assertEmptyBasicC(string) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertEmpty( &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function assertEmptyBasicC

    function assertEmptyBasicS(string) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertEmpty( &
                string, &
                var_str(""), &
                var_str(""))
    end function assertEmptyBasicS

    function assertEmptyWithMessageCC(string, message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function assertEmptyWithMessageCC

    function assertEmptyWithMessageCS(string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                var_str(string), &
                message, &
                message)
    end function assertEmptyWithMessageCS

    function assertEmptyWithMessageSC(string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                string, &
                var_str(message), &
                var_str(message))
    end function assertEmptyWithMessageSC

    function assertEmptyWithMessageSS(string, message) result(result__)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                string, &
                message, &
                message)
    end function assertEmptyWithMessageSS

    function assertEmptyWithMessagesCCC( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEmptyWithMessagesCCC

    function assertEmptyWithMessagesCCS( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function assertEmptyWithMessagesCCS

    function assertEmptyWithMessagesCSC( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function assertEmptyWithMessagesCSC

    function assertEmptyWithMessagesCSS( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                var_str(string), &
                success_message, &
                failure_message)
    end function assertEmptyWithMessagesCSS

    function assertEmptyWithMessagesSCC( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEmptyWithMessagesSCC

    function assertEmptyWithMessagesSCS( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                string, &
                var_str(success_message), &
                failure_message)
    end function assertEmptyWithMessagesSCS

    function assertEmptyWithMessagesSSC( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty( &
                string, &
                success_message, &
                var_str(failure_message))
    end function assertEmptyWithMessagesSSC

    function assertEmptyWithMessagesSSS( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, operator(==)

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (string == "") then
            result__ = succeed(withUserMessage( &
                    EMPTY_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(withUserMessage( &
                    makeEmptyFailureMessage(string), failure_message))
        end if
    end function assertEmptyWithMessagesSSS

    function assertEqualsDoublePrecision( &
            expected, &
            actual) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function assertEqualsDoublePrecision

    function assertEqualsDoublePrecisionWithMessageC( &
            expected, &
            actual, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, actual, var_str(message), var_str(message))
    end function assertEqualsDoublePrecisionWithMessageC

    function assertEqualsDoublePrecisionWithMessageS( &
            expected, &
            actual, &
            message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, message, message)
    end function assertEqualsDoublePrecisionWithMessageS

    function assertEqualsDoublePrecisionWithMessagesCC( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsDoublePrecisionWithMessagesCC

    function assertEqualsDoublePrecisionWithMessagesCS( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, actual, var_str(success_message), failure_message)
    end function assertEqualsDoublePrecisionWithMessagesCS

    function assertEqualsDoublePrecisionWithMessagesSC( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, actual, success_message, var_str(failure_message))
    end function assertEqualsDoublePrecisionWithMessagesSC

    function assertEqualsDoublePrecisionWithMessagesSS( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                MACHINE_EPSILON, &
                success_message, &
                failure_message)
    end function assertEqualsDoublePrecisionWithMessagesSS

    function assertEqualsIntegerBasic(expected, actual) result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function assertEqualsIntegerBasic

    function assertEqualsIntegerWithMessageC( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(message), &
                var_str(message))
    end function assertEqualsIntegerWithMessageC

    function assertEqualsIntegerWithMessageS( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                message, &
                message)
    end function assertEqualsIntegerWithMessageS

    function assertEqualsIntegerWithMessagesCC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsIntegerWithMessagesCC

    function assertEqualsIntegerWithMessagesCS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, actual, var_str(success_message), failure_message)
    end function assertEqualsIntegerWithMessagesCS

    function assertEqualsIntegerWithMessagesSC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, actual, success_message, var_str(failure_message))
    end function assertEqualsIntegerWithMessagesSC

    function assertEqualsIntegerWithMessagesSS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed(withUserMessage( &
                    makeEqualsSuccessMessage(toString(expected)), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeEqualsFailureMessage( &
                            toString(expected), toString(actual)), &
                    failure_message))
        end if
    end function assertEqualsIntegerWithMessagesSS

    function assertEqualsStringsCC( &
            expected, actual) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                var_str(actual), &
                var_str(""), &
                var_str(""))
    end function assertEqualsStringsCC

    function assertEqualsStringsCS( &
            expected, actual) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                actual, &
                var_str(""), &
                var_str(""))
    end function assertEqualsStringsCS

    function assertEqualsStringsSC( &
            expected, actual) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                var_str(actual), &
                var_str(""), &
                var_str(""))
    end function assertEqualsStringsSC

    function assertEqualsStringsSS( &
            expected, actual) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function assertEqualsStringsSS

    function assertEqualsStringsWithMessageCCC( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                var_str(actual), &
                var_str(message), &
                var_str(message))
    end function assertEqualsStringsWithMessageCCC

    function assertEqualsStringsWithMessageCCS( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                var_str(actual), &
                message, &
                message)
    end function assertEqualsStringsWithMessageCCS

    function assertEqualsStringsWithMessageCSC( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                actual, &
                var_str(message), &
                var_str(message))
    end function assertEqualsStringsWithMessageCSC

    function assertEqualsStringsWithMessageCSS( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                actual, &
                message, &
                message)
    end function assertEqualsStringsWithMessageCSS

    function assertEqualsStringsWithMessageSCC( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                var_str(actual), &
                var_str(message), &
                var_str(message))
    end function assertEqualsStringsWithMessageSCC

    function assertEqualsStringsWithMessageSCS( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                var_str(actual), &
                message, &
                message)
    end function assertEqualsStringsWithMessageSCS

    function assertEqualsStringsWithMessageSSC( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(message), &
                var_str(message))
    end function assertEqualsStringsWithMessageSSC

    function assertEqualsStringsWithMessageSSS( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                message, &
                message)
    end function assertEqualsStringsWithMessageSSS

    function assertEqualsStringsWithMessagesCCCC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                var_str(actual), &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesCCCC

    function assertEqualsStringsWithMessagesCCCS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                var_str(actual), &
                var_str(success_message), &
                failure_message)
    end function assertEqualsStringsWithMessagesCCCS

    function assertEqualsStringsWithMessagesCCSC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                var_str(actual), &
                success_message, &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesCCSC

    function assertEqualsStringsWithMessagesCCSS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                var_str(actual), &
                success_message, &
                failure_message)
    end function assertEqualsStringsWithMessagesCCSS

    function assertEqualsStringsWithMessagesCSCC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesCSCC

    function assertEqualsStringsWithMessagesCSCS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                actual, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsStringsWithMessagesCSCS

    function assertEqualsStringsWithMessagesCSSC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                actual, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesCSSC

    function assertEqualsStringsWithMessagesCSSS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                var_str(expected), &
                actual, &
                success_message, &
                failure_message)
    end function assertEqualsStringsWithMessagesCSSS

    function assertEqualsStringsWithMessagesSCCC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                var_str(actual), &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesSCCC

    function assertEqualsStringsWithMessagesSCCS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                var_str(actual), &
                var_str(success_message), &
                failure_message)
    end function assertEqualsStringsWithMessagesSCCS

    function assertEqualsStringsWithMessagesSCSC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                var_str(actual), &
                success_message, &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesSCSC

    function assertEqualsStringsWithMessagesSCSS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                var_str(actual), &
                success_message, &
                failure_message)
    end function assertEqualsStringsWithMessagesSCSS

    function assertEqualsStringsWithMessagesSSCC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesSSCC

    function assertEqualsStringsWithMessagesSSCS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsStringsWithMessagesSSCS

    function assertEqualsStringsWithMessagesSSSC( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsStringsWithMessagesSSSC

    function assertEqualsStringsWithMessagesSSSS( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, operator(==)

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed(withUserMessage( &
                    makeEqualsSuccessMessage(expected), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeEqualsFailureMessage(expected, actual), &
                    failure_message))
        end if
    end function assertEqualsStringsWithMessagesSSSS

    function assertEqualsWithinAbsoluteBasic( &
            expected, &
            actual, &
            tolerance) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(""), &
                var_str(""))
    end function assertEqualsWithinAbsoluteBasic

    function assertEqualsWithinAbsoluteWithMessageC( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(message), &
                var_str(message))
    end function assertEqualsWithinAbsoluteWithMessageC

    function assertEqualsWithinAbsoluteWithMessageS( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                message, &
                message)
    end function assertEqualsWithinAbsoluteWithMessageS

    function assertEqualsWithinAbsoluteWithMessagesCC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesCC

    function assertEqualsWithinAbsoluteWithMessagesCS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithinAbsoluteWithMessagesCS

    function assertEqualsWithinAbsoluteWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesSC

    function assertEqualsWithinAbsoluteWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (equalsWithinAbsolute(expected, actual, tolerance)) then
            result__ = succeed(withUserMessage( &
                    makeWithinSuccessMessage( &
                            toString(expected), &
                            toString(actual), &
                            toString(tolerance)), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeWithinFailureMessage( &
                            toString(expected), &
                            toString(actual), &
                            toString(tolerance)), &
                    failure_message))
        end if
    end function assertEqualsWithinAbsoluteWithMessagesSS

    function assertEqualsWithinRelativeBasic( &
            expected, &
            actual, &
            tolerance) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(""), &
                var_str(""))
    end function assertEqualsWithinRelativeBasic

    function assertEqualsWithinRelativeWithMessageC( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(message), &
                var_str(message))
    end function assertEqualsWithinRelativeWithMessageC

    function assertEqualsWithinRelativeWithMessageS( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                message, &
                message)
    end function assertEqualsWithinRelativeWithMessageS

    function assertEqualsWithinRelativeWithMessagesCC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithinRelativeWithMessagesCC

    function assertEqualsWithinRelativeWithMessagesCS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithinRelativeWithMessagesCS

    function assertEqualsWithinRelativeWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithinRelativeWithMessagesSC

    function assertEqualsWithinRelativeWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (equalsWithinRelative(expected, actual, tolerance)) then
            result__ = succeed(withUserMessage( &
                    makeWithinSuccessMessage( &
                            toString(expected), &
                            toString(actual), &
                            toString(tolerance * 100.0d0) // "%"), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeWithinFailureMessage( &
                            toString(expected), &
                            toString(actual), &
                            toString(tolerance * 100.0d0) // "%"), &
                    failure_message))
        end if
    end function assertEqualsWithinRelativeWithMessagesSS

    function assertFasterThanAbsoluteBracketed( &
            reference, &
            before, &
            computation, &
            after, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function assertFasterThanAbsoluteBracketed

    function assertFasterThanAbsoluteBracketedWithMessageC( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function assertFasterThanAbsoluteBracketedWithMessageC

    function assertFasterThanAbsoluteBracketedWithMessageS( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                message, &
                message)
    end function assertFasterThanAbsoluteBracketedWithMessageS

    function assertFasterThanAbsoluteBracketedWithMessagesCC( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertFasterThanAbsoluteBracketedWithMessagesCC

    function assertFasterThanAbsoluteBracketedWithMessagesCS( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function assertFasterThanAbsoluteBracketedWithMessagesCS

    function assertFasterThanAbsoluteBracketedWithMessagesSC( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function assertFasterThanAbsoluteBracketedWithMessagesSC

    function assertFasterThanAbsoluteBracketedWithMessagesSS( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time

        total_time = 0.0d0
        do i = 1, iterations
            call before
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            call after
            total_time = total_time + (end_time - start_time)
        end do
        average_time = total_time / dble(iterations)
        if (average_time < reference) then
            result__ = succeed(withUserMessage( &
                    makeFasterThanSuccessMessage( &
                            toString(reference), &
                            toString(average_time), &
                            toString(iterations)), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeFasterThanFailureMessage( &
                            toString(reference), &
                            toString(average_time), &
                            toString(iterations)), &
                    failure_message))
        end if
    end function assertFasterThanAbsoluteBracketedWithMessagesSS

    function assertFasterThanAbsoluteSimple( &
            reference, &
            computation, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function assertFasterThanAbsoluteSimple

    function assertFasterThanAbsoluteSimpleWithMessageC( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function assertFasterThanAbsoluteSimpleWithMessageC

    function assertFasterThanAbsoluteSimpleWithMessageS( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                message, &
                message)
    end function assertFasterThanAbsoluteSimpleWithMessageS

    function assertFasterThanAbsoluteSimpleWithMessagesCC( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertFasterThanAbsoluteSimpleWithMessagesCC

    function assertFasterThanAbsoluteSimpleWithMessagesCS( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function assertFasterThanAbsoluteSimpleWithMessagesCS

    function assertFasterThanAbsoluteSimpleWithMessagesSC( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function assertFasterThanAbsoluteSimpleWithMessagesSC

    function assertFasterThanAbsoluteSimpleWithMessagesSS( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        double precision, intent(in) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time

        total_time = 0.0d0
        do i = 1, iterations
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
        end do
        average_time = total_time / dble(iterations)
        if (average_time < reference) then
            result__ = succeed(withUserMessage( &
                    makeFasterThanSuccessMessage( &
                            toString(reference), &
                            toString(average_time), &
                            toString(iterations)), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeFasterThanFailureMessage( &
                            toString(reference), &
                            toString(average_time), &
                            toString(iterations)), &
                    failure_message))
        end if
    end function assertFasterThanAbsoluteSimpleWithMessagesSS

    function assertFasterThanRelativeBracketed( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        procedure(computation_) :: reference_before
        procedure(computation_) :: reference
        procedure(computation_) :: reference_after
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function assertFasterThanRelativeBracketed

    function assertFasterThanRelativeBracketedWithMessageC( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        procedure(computation_) :: reference_before
        procedure(computation_) :: reference
        procedure(computation_) :: reference_after
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function assertFasterThanRelativeBracketedWithMessageC

    function assertFasterThanRelativeBracketedWithMessageS( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        procedure(computation_) :: reference_before
        procedure(computation_) :: reference
        procedure(computation_) :: reference_after
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                message, &
                message)
    end function assertFasterThanRelativeBracketedWithMessageS

    function assertFasterThanRelativeBracketedWithMessagesCC( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        procedure(computation_) :: reference_before
        procedure(computation_) :: reference
        procedure(computation_) :: reference_after
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertFasterThanRelativeBracketedWithMessagesCC

    function assertFasterThanRelativeBracketedWithMessagesCS( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        procedure(computation_) :: reference_before
        procedure(computation_) :: reference
        procedure(computation_) :: reference_after
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function assertFasterThanRelativeBracketedWithMessagesCS

    function assertFasterThanRelativeBracketedWithMessagesSC( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        procedure(computation_) :: reference_before
        procedure(computation_) :: reference
        procedure(computation_) :: reference_after
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function assertFasterThanRelativeBracketedWithMessagesSC

    function assertFasterThanRelativeBracketedWithMessagesSS( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        procedure(computation_) :: reference_before
        procedure(computation_) :: reference
        procedure(computation_) :: reference_after
        procedure(computation_) :: before
        procedure(computation_) :: computation
        procedure(computation_) :: after
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time
        double precision :: reference_start_time
        double precision :: reference_end_time
        double precision :: reference_total_time
        double precision :: reference_average_time

        total_time = 0.0d0
        reference_total_time = 0.0d0
        do i = 1, iterations
            call reference_before
            call cpu_time(reference_start_time)
            call reference
            call cpu_time(reference_end_time)
            call reference_after
            reference_total_time = &
                    reference_total_time &
                    + (reference_end_time - reference_start_time)
            call before
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            call after
            total_time = total_time + (end_time - start_time)
        end do
        reference_average_time = reference_total_time / dble(iterations)
        average_time = total_time / dble(iterations)
        if (average_time < reference_average_time) then
            result__ = succeed(withUserMessage( &
                    makeFasterThanSuccessMessage( &
                            toString(reference_average_time), &
                            toString(average_time), &
                            toString(iterations)), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeFasterThanFailureMessage( &
                            toString(reference_average_time), &
                            toString(average_time), &
                            toString(iterations)), &
                    failure_message))
        end if
    end function assertFasterThanRelativeBracketedWithMessagesSS

    function assertFasterThanRelativeSimple( &
            reference, &
            computation, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        procedure(computation_) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function assertFasterThanRelativeSimple

    function assertFasterThanRelativeSimpleWithMessageC( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        procedure(computation_) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function assertFasterThanRelativeSimpleWithMessageC

    function assertFasterThanRelativeSimpleWithMessageS( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        procedure(computation_) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                message, &
                message)
    end function assertFasterThanRelativeSimpleWithMessageS

    function assertFasterThanRelativeSimpleWithMessagesCC( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str
        use strff, only: toString

        procedure(computation_) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertFasterThanRelativeSimpleWithMessagesCC

    function assertFasterThanRelativeSimpleWithMessagesCS( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        procedure(computation_) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function assertFasterThanRelativeSimpleWithMessagesCS

    function assertFasterThanRelativeSimpleWithMessagesSC( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str
        use strff, only: toString

        procedure(computation_) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertFasterThan( &
                reference, &
                computation, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function assertFasterThanRelativeSimpleWithMessagesSC

    function assertFasterThanRelativeSimpleWithMessagesSS( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        procedure(computation_) :: reference
        procedure(computation_) :: computation
        integer, intent(in) :: iterations
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time
        double precision :: reference_start_time
        double precision :: reference_end_time
        double precision :: reference_total_time
        double precision :: reference_average_time

        total_time = 0.0d0
        reference_total_time = 0.0d0
        do i = 1, iterations
            call cpu_time(reference_start_time)
            call reference
            call cpu_time(reference_end_time)
            reference_total_time = &
                    reference_total_time &
                    + (reference_end_time - reference_start_time)
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
        end do
        reference_average_time = reference_total_time / dble(iterations)
        average_time = total_time / dble(iterations)
        if (average_time < reference_average_time) then
            result__ = succeed(withUserMessage( &
                    makeFasterThanSuccessMessage( &
                            toString(reference_average_time), &
                            toString(average_time), &
                            toString(iterations)), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeFasterThanFailureMessage( &
                            toString(reference_average_time), &
                            toString(average_time), &
                            toString(iterations)), &
                    failure_message))
        end if
    end function assertFasterThanRelativeSimpleWithMessagesSS

    function assertIncludesCC( &
                search_for, &
                string) &
                result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function assertIncludesCC

    function assertIncludesCS( &
                search_for, &
                string) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                string, &
                var_str(""), &
                var_str(""))
    end function assertIncludesCS

    function assertIncludesSC( &
                search_for, &
                string) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function assertIncludesSC

    function assertIncludesSS( &
                search_for, &
                string) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                string, &
                var_str(""), &
                var_str(""))
    end function assertIncludesSS

    function assertIncludesWithMessageCCC( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function assertIncludesWithMessageCCC

    function assertIncludesWithMessageCCS( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                var_str(string), &
                message, &
                message)
    end function assertIncludesWithMessageCCS

    function assertIncludesWithMessageCSC( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                string, &
                var_str(message), &
                var_str(message))
    end function assertIncludesWithMessageCSC

    function assertIncludesWithMessageCSS( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                string, &
                message, &
                message)
    end function assertIncludesWithMessageCSS

    function assertIncludesWithMessageSCC( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function assertIncludesWithMessageSCC

    function assertIncludesWithMessageSCS( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                var_str(string), &
                message, &
                message)
    end function assertIncludesWithMessageSCS

    function assertIncludesWithMessageSSC( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                string, &
                var_str(message), &
                var_str(message))
    end function assertIncludesWithMessageSSC

    function assertIncludesWithMessageSSS( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                string, &
                message, &
                message)
    end function assertIncludesWithMessageSSS

    function assertIncludesWithMessagesCCCC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function assertIncludesWithMessagesCCCC

    function assertIncludesWithMessagesCCCS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function assertIncludesWithMessagesCCCS

    function assertIncludesWithMessagesCCSC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function assertIncludesWithMessagesCCSC

    function assertIncludesWithMessagesCCSS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                failure_message)
    end function assertIncludesWithMessagesCCSS

    function assertIncludesWithMessagesCSCC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertIncludesWithMessagesCSCC

    function assertIncludesWithMessagesCSCS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                failure_message)
    end function assertIncludesWithMessagesCSCS

    function assertIncludesWithMessagesCSSC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                string, &
                success_message, &
                var_str(failure_message))
    end function assertIncludesWithMessagesCSSC

    function assertIncludesWithMessagesCSSS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                var_str(search_for), &
                string, &
                success_message, &
                failure_message)
    end function assertIncludesWithMessagesCSSS

    function assertIncludesWithMessagesSCCC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function assertIncludesWithMessagesSCCC

    function assertIncludesWithMessagesSCCS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function assertIncludesWithMessagesSCCS

    function assertIncludesWithMessagesSCSC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function assertIncludesWithMessagesSCSC

    function assertIncludesWithMessagesSCSS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                var_str(string), &
                success_message, &
                failure_message)
    end function assertIncludesWithMessagesSCSS

    function assertIncludesWithMessagesSSCC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertIncludesWithMessagesSSCC

    function assertIncludesWithMessagesSSCS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                string, &
                var_str(success_message), &
                failure_message)
    end function assertIncludesWithMessagesSSCS

    function assertIncludesWithMessagesSSSC( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, &
                string, &
                success_message, &
                var_str(failure_message))
    end function assertIncludesWithMessagesSSSC

    function assertIncludesWithMessagesSSSS( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: operator(.includes.)

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (string.includes.search_for) then
            result__ = succeed(withUserMessage( &
                    makeIncludesSuccessMessage(search_for, string), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeIncludesFailureMessage(search_for, string), &
                    failure_message))
        end if
    end function assertIncludesWithMessagesSSSS

    function assertNotBasic(condition) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        type(Result_t) :: result__

        result__ = assertNot(condition, var_str(""), var_str(""))
    end function assertNotBasic

    function assertNotWithMessageC(condition, message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertNot(condition, var_str(message), var_str(message))
    end function assertNotWithMessageC

    function assertNotWithMessageS(condition, message) result(result__)
        use iso_varying_string, only: VARYING_STRING

        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertNot(condition, message, message)
    end function assertNotWithMessageS

    function assertNotWithMessagesCC( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertNot( &
                condition, var_str(success_message), var_str(failure_message))
    end function assertNotWithMessagesCC

    function assertNotWithMessagesCS( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertNot( &
                condition, var_str(success_message), failure_message)
    end function assertNotWithMessagesCS

    function assertNotWithMessagesSC( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertNot( &
                condition, success_message, var_str(failure_message))
    end function assertNotWithMessagesSC

    function assertNotWithMessagesSS( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING

        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (condition) then
            result__ = fail(withUserMessage( &
                    NOT_FAILURE_MESSAGE, failure_message))
        else
            result__ = succeed(withUserMessage( &
                    NOT_SUCCESS_MESSAGE, success_message))
        end if
    end function assertNotWithMessagesSS

    function assertThatBasic(condition) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        type(Result_t) :: result__

        result__ = assertThat(condition, var_str(""), var_str(""))
    end function assertThatBasic

    function assertThatWithMessageC(condition, message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertThat(condition, var_str(message), var_str(message))
    end function assertThatWithMessageC

    function assertThatWithMessageS(condition, message) result(result__)
        use iso_varying_string, only: VARYING_STRING

        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertThat(condition, message, message)
    end function assertThatWithMessageS

    function assertThatWithMessagesCC( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertThat( &
                condition, var_str(success_message), var_str(failure_message))
    end function assertThatWithMessagesCC

    function assertThatWithMessagesCS( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertThat( &
                condition, var_str(success_message), failure_message)
    end function assertThatWithMessagesCS

    function assertThatWithMessagesSC( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING, var_str

        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertThat( &
                condition, success_message, var_str(failure_message))
    end function assertThatWithMessagesSC

    function assertThatWithMessagesSS( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: VARYING_STRING

        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (condition) then
            result__ = succeed(withUserMessage( &
                    THAT_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(withUserMessage( &
                    THAT_FAILURE_MESSAGE, failure_message))
        end if
    end function assertThatWithMessagesSS

    pure function combineResults(lhs, rhs) result(combined)
        class(Result_t), intent(in) :: lhs
        type(Result_t), intent(in) :: rhs
        type(Result_t) :: combined

        integer :: num_lhs
        integer :: num_rhs

        if (allocated(lhs%results) .and. allocated(rhs%results)) then
            num_lhs = size(lhs%results)
            num_rhs = size(rhs%results)
            allocate(combined%results(num_lhs + num_rhs))
            combined%results(1:num_lhs) = lhs%results(:)
            combined%results(num_lhs+1:) = rhs%results(:)
        else if (allocated(lhs%results)) then
            combined = lhs
        else if (allocated(rhs%results)) then
            combined = rhs
        end if
    end function combineResults

    pure function delimitC(string) result(delimited)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: delimited

        delimited = delimit(var_str(string))
    end function delimitC

    pure function delimitS(string) result(delimited)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: delimited

        delimited = "|" // string // "|"
    end function delimitS

    function DescribeBasicC(description, tests) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        allocate(item%test, source = SimpleTestCollection( &
                var_str(description), tests))
    end function DescribeBasicC

    function DescribeBasicS(description, tests) result(item)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        allocate(item%test, source = SimpleTestCollection( &
                description, tests))
    end function DescribeBasicS

    function DescribeWithInputC(description, input, tests) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        class(Input_t), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        allocate(item%test, source = TestCollectionWithInput( &
                var_str(description), input, tests))
    end function DescribeWithInputC

    function DescribeWithInputS(description, input, tests) result(item)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        class(Input_t), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        allocate(item%test, source = TestCollectionWithInput( &
                description, input, tests))
    end function DescribeWithInputS

    function equalsWithinAbsolute(expected, actual, tolerance)
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

    function Example(input)
        class(Input_t), intent(in) :: input
        type(Example_t) :: Example

        allocate(Example%input, source = input)
    end function Example

    pure function failC(message) result(failure)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: message
        type(Result_t) :: failure

        failure = fail(var_str(message))
    end function failC

    pure function failS(message) result(failure)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: failure

        allocate(failure%results(1))
        failure%results(1) = IndividualResult(message, .false.)
    end function failS

    function generateAsciiString(self) result(generated_value)
        class(AsciiStringGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        type(StringInput_t) :: the_input

        associate(a => self)
        end associate

        the_input%value_ = getRandomAsciiString()
        generated_value = Generated(the_input)
    end function generateAsciiString

    function generateInteger(self) result(generated_value)
        class(IntegerGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        type(IntegerInput_t) :: the_input

        associate(a => self)
        end associate

        the_input%value_ = getRandomInteger()
        generated_value = Generated(the_input)
    end function generateInteger

    function Generated(value_)
        class(Input_t), intent(in) :: value_
        type(Generated_t) :: generated

        allocate(Generated%input, source = value_)
    end function Generated

    function getOptions() result(options)
        use iso_fortran_env, only: error_unit, output_unit
        use iso_varying_string, only: assignment(=), put_line
        use strff, only: NEWLINE

        type(Options_t) :: options

        character(len=100) :: argument
        character(len=100) :: program_name
        integer :: i
        integer :: iostat
        integer :: num_arguments

        options%colorize = .true.
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
            case ("-c", "--color-off")
                options%colorize = .false.
            case ("-h", "--help")
                call put_line(output_unit, usageMessage(program_name))
                call exit(0)
            case ("-f", "--filter")
                options%filter_tests = .true.
                i = i + 1
                call get_command_argument(i, argument)
                options%filter_string = trim(argument)
            case ("-n", "--numrand")
                i = i + 1
                call get_command_argument(i, argument)
                read(argument, *, iostat=iostat) NUM_GENERATOR_TESTS
                if (iostat /= 0) then
                    call put_line( &
                            error_unit, &
                            'Unable to read "' // trim(argument) // '" as an integer' // NEWLINE)
                    call put_line(error_unit, usageMessage(program_name))
                    call exit(1)
                end if
                if (NUM_GENERATOR_TESTS <= 0) then
                    call put_line(error_unit, "Number of random values must be >0")
                    call exit(1)
                end if
            case ("-q", "--quiet")
                options%quiet = .true.
            case ("-v", "--verbose")
                options%verbose = .true.
            case default
                call put_line( &
                        error_unit, &
                        "Unknown argument: '" // trim(argument) // "'" // NEWLINE)
                call put_line(error_unit, usageMessage(program_name))
                call exit(1)
            end select
            i = i + 1
        end do
    contains
        pure function usageMessage(program_name_)
            use iso_varying_string, only: VARYING_STRING, assignment(=)
            use strff, only: NEWLINE

            character(len=*), intent(in) :: program_name_
            type(VARYING_STRING) :: usageMessage

            usageMessage = &
                    "Usage: " // trim(program_name_) // " [-h] [-q] [-v] [-f string] [-n num] [-c]" // NEWLINE &
                    // "  options:" // NEWLINE &
                    // "    -h, --help                    Output this message and exit" // NEWLINE &
                    // "    -q, --quiet                   Don't print the test descriptions before" // NEWLINE &
                    // "                                  running the tests" // NEWLINE &
                    // "    -v, --verbose                 Print all of the assertion messages, not" // NEWLINE &
                    // "                                  just the failing ones" // NEWLINE &
                    // "    -f string, --filter string    Only run cases or collections whose" // NEWLINE &
                    // "                                  description contains the given string" // NEWLINE &
                    // "    -n num, --numrand num         Number of random values to use for each" // NEWLINE &
                    // "                                  test with generated values (default = 100)" // NEWLINE &
                    // "    -c, --color-off               Don't colorize the output"
        end function usageMessage
    end function getOptions

    function getRandomAsciiCharacter() result(random_character)
        character(len=1) :: random_character

        character(len=*), parameter :: ASCII_CHARACTERS = &
        '  !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~'
        integer :: which_character

        which_character = getRandomIntegerWithRange(0, len(ASCII_CHARACTERS))
        random_character = ASCII_CHARACTERS(which_character:which_character)
    end function getRandomAsciiCharacter

    function getRandomAsciiString() result(random_string)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING) :: random_string

        random_string = getRandomAsciiStringWithMaxLength(1024)
    end function getRandomAsciiString

    function getRandomAsciiStringWithMaxLength(max_length) result(random_string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        integer, intent(in) :: max_length
        type(VARYING_STRING) :: random_string

        character(len=max_length) :: characters
        integer :: i
        integer :: num_characters

        num_characters = getRandomIntegerWithRange(0, max_length)
        !$omp parallel do
        do i = 1, num_characters
            characters(i:i) = getRandomAsciiCharacter()
        end do
        !$omp end parallel do
        random_string = characters(1:num_characters)
    end function getRandomAsciiStringWithMaxLength

    function getRandomDoublePrecisionWithMagnitude(magnitude) result(random_double)
        double precision, intent(in) :: magnitude
        double precision :: random_double

        call random_number(random_double)
        random_double = random_double * magnitude
        if (getRandomLogical()) random_double = -random_double
    end function getRandomDoublePrecisionWithMagnitude

    function getRandomDoublePrecisionWithRange(start, end_) result(random_double)
        double precision, intent(in) :: start
        double precision, intent(in) :: end_
        double precision :: random_double

        call random_number(random_double)
        random_double = start + (end_ - start) * random_double
    end function getRandomDoublePrecisionWithRange

    function getRandomInteger() result(random_integer)
        integer :: random_integer

        double precision :: random_real

        call random_number(random_real)
        random_integer = floor(random_real*MAX_INT)
        if (getRandomLogical()) random_integer = -random_integer
    end function getRandomInteger

    function getRandomIntegerWithRange(start, end_) result(random_integer)
        integer, intent(in) :: start
        integer, intent(in) :: end_
        integer :: random_integer

        double precision :: random_real

        call random_number(random_real)
        random_integer = start + floor((end_ + 1 - start) * random_real)
    end function getRandomIntegerWithRange

    function getRandomLogical() result(random_logical)
        logical :: random_logical

        if (getRandomIntegerWithRange(0, 1) == 0) then
            random_logical = .TRUE.
        else
            random_logical = .FALSE.
        end if
    end function getRandomLogical

    function GivenWithInputC(description, input, tests) result(item)
        character(len=*), intent(in) :: description
        class(Input_t), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        item = Describe("Given " // description, input, tests)
    end function GivenWithInputC

    function GivenWithInputS(description, input, tests) result(item)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: description
        class(Input_t), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        item = Describe("Given " // description, input, tests)
    end function GivenWithInputS

    pure function IndividualResult(message, passed)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: message
        logical, intent(in) :: passed
        type(IndividualResult_t) :: IndividualResult

        IndividualResult%message = message
        IndividualResult%passed_ = passed
    end function IndividualResult

    function individualResultFailureDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: &
                VARYING_STRING, assignment(=), operator(//)

        class(IndividualResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        if (self%passed_) then
            description = ""
        else
            if (colorize) then
                description = char(27) // "[31m" // self%message // char(27) // "[0m"
            else
                description = self%message
            end if
        end if
    end function individualResultFailureDescription

    function individualResultRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, toString, NEWLINE

        class(IndividualResult_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "IndividualResult(" // NEWLINE &
                    // "message = """ // self%message // """," // NEWLINE &
                    // "passed = " // toString(self%passed_), &
                INDENTATION) // NEWLINE // ")"
    end function individualResultRepr

    function individualResultVerboseDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(IndividualResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        if (colorize) then
            if (self%passed_) then
                description = char(27) // "[32m" // self%message // char(27) // "[0m"
            else
                description = char(27) // "[31m" // self%message // char(27) // "[0m"
            end if
        else
            description = self%message
        end if
    end function individualResultVerboseDescription

    function InputTestCase(description, test)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        procedure(inputTest) :: test
        type(InputTestCase_t) :: InputTestCase

        InputTestCase%description_ = description
        InputTestCase%test => test
    end function InputTestCase

    function inputTestCaseRunWithInput(self, input) result(result_)
        class(InputTestCase_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        allocate(result_%result_, source = TestCaseResult( &
                self%description_, self%test(input)))
    end function inputTestCaseRunWithInput

    function inputTestCaseRunWithoutInput(self) result(result_)
        class(InputTestCase_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        allocate(result_%result_, source = TestCaseResult( &
                self%description_, fail("No input provided")))
    end function inputTestCaseRunWithoutInput

    function inputTestCaseRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, toString, NEWLINE

        class(InputTestCase_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "InputTestCase_t(" // NEWLINE &
                    // "description = """ // self%description_ // """," // NEWLINE &
                    // "test @ " // toString(loc(self%test)), &
                INDENTATION) // NEWLINE // ")"
    end function inputTestCaseRepr

    function ItBasicC(description, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        procedure(simpleTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = SimpleTestCase(var_str(description), test))
    end function ItBasicC

    function ItBasicS(description, test) result(item)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        procedure(simpleTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = SimpleTestCase(description, test))
    end function ItBasicS

    function ItInputC(description, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = InputTestCase(var_str(description), test))
    end function ItInputC

    function ItInputS(description, test) result(item)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = InputTestCase(description, test))
    end function ItInputS

    function ItWithExamplesC(description, examples, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        type(Example_t), intent(in) :: examples(:)
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = TestCaseWithExamples( &
                var_str(description), examples, test))
    end function ItWithExamplesC

    function ItWithExamplesS(description, examples, test) result(item)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        type(Example_t), intent(in) :: examples(:)
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = TestCaseWithExamples( &
                description, examples, test))
    end function ItWithExamplesS

    function ItWithGeneratorC(description, generator, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        class(Generator_t), intent(in) :: generator
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = TestCaseWithGenerator( &
                var_str(description), generator, test))
    end function ItWithGeneratorC

    function ItWithGeneratorS(description, generator, test) result(item)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        class(Generator_t), intent(in) :: generator
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        allocate(item%test, source = TestCaseWithGenerator( &
                description, generator, test))
    end function ItWithGeneratorS

    pure function makeDoesntIncludeFailureMessageCC( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeDoesntIncludeFailureMessage( &
                var_str(search_for), var_str(string))
    end function makeDoesntIncludeFailureMessageCC

    pure function makeDoesntIncludeFailureMessageCS( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeDoesntIncludeFailureMessage( &
                var_str(search_for), string)
    end function makeDoesntIncludeFailureMessageCS

    pure function makeDoesntIncludeFailureMessageSC( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeDoesntIncludeFailureMessage( &
                search_for, var_str(string))
    end function makeDoesntIncludeFailureMessageSC

    pure function makeDoesntIncludeFailureMessageSS( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "to not include" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function makeDoesntIncludeFailureMessageSS

    pure function makeDoesntIncludeSuccessMessageCC( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeDoesntIncludeSuccessMessage( &
                var_str(search_for), var_str(string))
    end function makeDoesntIncludeSuccessMessageCC

    pure function makeDoesntIncludeSuccessMessageCS( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeDoesntIncludeSuccessMessage( &
                var_str(search_for), string)
    end function makeDoesntIncludeSuccessMessageCS

    pure function makeDoesntIncludeSuccessMessageSC( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeDoesntIncludeSuccessMessage( &
                search_for, var_str(string))
    end function makeDoesntIncludeSuccessMessageSC

    pure function makeDoesntIncludeSuccessMessageSS( &
            search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "did not include" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function makeDoesntIncludeSuccessMessageSS

    pure function makeEmptyFailureMessageC(string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeEmptyFailureMessage(var_str(string))
    end function makeEmptyFailureMessageC

    pure function makeEmptyFailureMessageS(string) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "wasn't empty", &
                INDENTATION)
    end function makeEmptyFailureMessageS

    pure function makeEqualsFailureMessageCC(expected, actual) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING) :: message

        message = makeEqualsFailureMessage(var_str(expected), var_str(actual))
    end function makeEqualsFailureMessageCC

    pure function makeEqualsFailureMessageCS(expected, actual) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING) :: message

        message = makeEqualsFailureMessage(var_str(expected), actual)
    end function makeEqualsFailureMessageCS

    pure function makeEqualsFailureMessageSC(expected, actual) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING) :: message

        message = makeEqualsFailureMessage(expected, var_str(actual))
    end function makeEqualsFailureMessageSC

    pure function makeEqualsFailureMessageSS(expected, actual) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(expected, 1)), &
                            INDENTATION) // NEWLINE &
                    // "but got" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(actual, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function makeEqualsFailureMessageSS

    pure function makeEqualsSuccessMessageC(expected) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING) :: message

        message = makeEqualsSuccessMessage(var_str(expected))
    end function makeEqualsSuccessMessageC

    pure function makeEqualsSuccessMessageS(expected) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "Expected and got" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(expected, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function makeEqualsSuccessMessageS

    pure function makeFasterThanFailureMessageCCC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanFailureMessage( &
                var_str(reference), var_str(actual), var_str(iterations))
    end function makeFasterThanFailureMessageCCC

    pure function makeFasterThanFailureMessageCCS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanFailureMessage( &
                var_str(reference), var_str(actual), iterations)
    end function makeFasterThanFailureMessageCCS

    pure function makeFasterThanFailureMessageCSC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanFailureMessage( &
                var_str(reference), actual, var_str(iterations))
    end function makeFasterThanFailureMessageCSC

    pure function makeFasterThanFailureMessageCSS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanFailureMessage( &
                var_str(reference), actual, iterations)
    end function makeFasterThanFailureMessageCSS

    pure function makeFasterThanFailureMessageSCC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanFailureMessage( &
                reference, var_str(actual), var_str(iterations))
    end function makeFasterThanFailureMessageSCC

    pure function makeFasterThanFailureMessageSCS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanFailureMessage( &
                reference, var_str(actual), iterations)
    end function makeFasterThanFailureMessageSCS

    pure function makeFasterThanFailureMessageSSC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanFailureMessage( &
                reference, actual, var_str(iterations))
    end function makeFasterThanFailureMessageSSC

    pure function makeFasterThanFailureMessageSSS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = &
                "Computation took " // actual &
                // ", which was slower than the reference time of " &
                // reference // ", averaged over " // iterations // " iterations."
    end function makeFasterThanFailureMessageSSS

    pure function makeFasterThanSuccessMessageCCC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanSuccessMessage( &
                var_str(reference), var_str(actual), var_str(iterations))
    end function makeFasterThanSuccessMessageCCC

    pure function makeFasterThanSuccessMessageCCS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanSuccessMessage( &
                var_str(reference), var_str(actual), iterations)
    end function makeFasterThanSuccessMessageCCS

    pure function makeFasterThanSuccessMessageCSC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanSuccessMessage( &
                var_str(reference), actual, var_str(iterations))
    end function makeFasterThanSuccessMessageCSC

    pure function makeFasterThanSuccessMessageCSS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanSuccessMessage( &
                var_str(reference), actual, iterations)
    end function makeFasterThanSuccessMessageCSS

    pure function makeFasterThanSuccessMessageSCC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanSuccessMessage( &
                reference, var_str(actual), var_str(iterations))
    end function makeFasterThanSuccessMessageSCC

    pure function makeFasterThanSuccessMessageSCS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanSuccessMessage( &
                reference, var_str(actual), iterations)
    end function makeFasterThanSuccessMessageSCS

    pure function makeFasterThanSuccessMessageSSC( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = makeFasterThanSuccessMessage( &
                reference, actual, var_str(iterations))
    end function makeFasterThanSuccessMessageSSC

    pure function makeFasterThanSuccessMessageSSS( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: reference
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: iterations
        type(VARYING_STRING) :: message

        message = &
                "Computation took " // actual &
                // ", which was faster than the reference time of " &
                // reference // ", averaged over " // iterations // " iterations."
    end function makeFasterThanSuccessMessageSSS

    pure function makeIncludesFailureMessageCC(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeIncludesFailureMessage( &
                var_str(search_for), var_str(string))
    end function makeIncludesFailureMessageCC

    pure function makeIncludesFailureMessageCS(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeIncludesFailureMessage(var_str(search_for), string)
    end function makeIncludesFailureMessageCS

    pure function makeIncludesFailureMessageSC(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeIncludesFailureMessage(search_for, var_str(string))
    end function makeIncludesFailureMessageSC

    pure function makeIncludesFailureMessageSS(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "to include" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function makeIncludesFailureMessageSS

    pure function makeIncludesSuccessMessageCC(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeIncludesSuccessMessage( &
                var_str(search_for), var_str(string))
    end function makeIncludesSuccessMessageCC

    pure function makeIncludesSuccessMessageCS(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeIncludesSuccessMessage(var_str(search_for), string)
    end function makeIncludesSuccessMessageCS

    pure function makeIncludesSuccessMessageSC(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = makeIncludesSuccessMessage(search_for, var_str(string))
    end function makeIncludesSuccessMessageSC

    pure function makeIncludesSuccessMessageSS(search_for, string) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "included" // NEWLINE &
                    // indent( &
                            delimit(hangingIndent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function makeIncludesSuccessMessageSS

    pure function makeWithinFailureMessageCCC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinFailureMessage( &
                var_str(expected), var_str(actual), var_str(tolerance))
    end function makeWithinFailureMessageCCC

    pure function makeWithinFailureMessageCCS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinFailureMessage( &
                var_str(expected), var_str(actual), tolerance)
    end function makeWithinFailureMessageCCS

    pure function makeWithinFailureMessageCSC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinFailureMessage( &
                var_str(expected), actual, var_str(tolerance))
    end function makeWithinFailureMessageCSC

    pure function makeWithinFailureMessageCSS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinFailureMessage( &
                var_str(expected), actual, tolerance)
    end function makeWithinFailureMessageCSS

    pure function makeWithinFailureMessageSCC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinFailureMessage( &
                expected, var_str(actual), var_str(tolerance))
    end function makeWithinFailureMessageSCC

    pure function makeWithinFailureMessageSCS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinFailureMessage( &
                expected, var_str(actual), tolerance)
    end function makeWithinFailureMessageSCS

    pure function makeWithinFailureMessageSSC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinFailureMessage( &
                expected, actual, var_str(tolerance))
    end function makeWithinFailureMessageSSC

    pure function makeWithinFailureMessageSSS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = &
                "Expected " // delimit(actual) // " to be within " &
                // delimit("±" // tolerance) // " of " // delimit(expected)
    end function makeWithinFailureMessageSSS

    pure function makeWithinSuccessMessageCCC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinSuccessMessage( &
                var_str(expected), var_str(actual), var_str(tolerance))
    end function makeWithinSuccessMessageCCC

    pure function makeWithinSuccessMessageCCS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinSuccessMessage( &
                var_str(expected), var_str(actual), tolerance)
    end function makeWithinSuccessMessageCCS

    pure function makeWithinSuccessMessageCSC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinSuccessMessage( &
                var_str(expected), actual, var_str(tolerance))
    end function makeWithinSuccessMessageCSC

    pure function makeWithinSuccessMessageCSS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinSuccessMessage( &
                var_str(expected), actual, tolerance)
    end function makeWithinSuccessMessageCSS

    pure function makeWithinSuccessMessageSCC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinSuccessMessage( &
                expected, var_str(actual), var_str(tolerance))
    end function makeWithinSuccessMessageSCC

    pure function makeWithinSuccessMessageSCS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinSuccessMessage( &
                expected, var_str(actual), tolerance)
    end function makeWithinSuccessMessageSCS

    pure function makeWithinSuccessMessageSSC( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = makeWithinSuccessMessage( &
                expected, actual, var_str(tolerance))
    end function makeWithinSuccessMessageSSC

    pure function makeWithinSuccessMessageSSS( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = &
                delimit(actual) // " was within " // delimit("±" // tolerance) &
                // " of " // delimit(expected)
    end function makeWithinSuccessMessageSSS

    function resultFailureDescription(self, colorize) result(description)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: join, NEWLINE

        class(Result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        integer :: i
        type(VARYING_STRING) :: individual_descriptions(size(self%results))

        !$omp parallel do
        do i = 1, size(self%results)
            individual_descriptions(i) =  &
                    self%results(i)%failureDescription(colorize)
        end do
        !$omp end parallel do
        description = join(individual_descriptions, NEWLINE)
    end function resultFailureDescription

    pure function resultNumAsserts(self) result(num_asserts)
        class(Result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = size(self%results)
    end function resultNumAsserts

    pure function resultNumFailingAsserts(self) result(num_asserts)
        class(Result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = count(.not.self%results%passed_)
    end function resultNumFailingAsserts

    pure function resultPassed(self) result(passed)
        class(Result_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed_)
    end function resultPassed

    function resultRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, indent, join, NEWLINE

        class(Result_t), intent(in) :: self
        type(VARYING_STRING) :: string

        type(VARYING_STRING) :: strings(size(self%results))
        integer :: i

        !$omp parallel do
        do i = 1, size(self%results)
            strings(i) = self%results(i)%repr()
        end do
        !$omp end parallel do
        string = hangingIndent( &
                "Result_t(" // NEWLINE &
                    // "results = [" // NEWLINE &
                    // indent( &
                            join(strings, "," // NEWLINE), &
                            INDENTATION) // NEWLINE // "]", &
                INDENTATION) // NEWLINE // ")"
    end function resultRepr

    function resultVerboseDescription(self, colorize) result(description)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: join, NEWLINE

        class(Result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        integer :: i
        type(VARYING_STRING) :: individual_descriptions(size(self%results))

        !$omp parallel do
        do i = 1, size(self%results)
            individual_descriptions(i) =  &
                    self%results(i)%verboseDescription(colorize)
        end do
        !$omp end parallel do
        description = join(individual_descriptions, NEWLINE)
    end function resultVerboseDescription

    subroutine runTests(tests)
        use iso_fortran_env, only: error_unit, output_unit
        use iso_varying_string, only: operator(//), put_line
        use strff, only: toString

        type(TestItem_t), intent(in) :: tests

        type(FilterItemResult_t) :: filtered_tests
        type(Options_t) :: options
        type(TestResultItem_t) :: results
        type(TestItem_t) :: tests_to_run

        options = getOptions()

        if (options%filter_tests) then
            filtered_tests = tests%filter(options%filter_string)
            if (filtered_tests%matched) then
                tests_to_run = filtered_tests%test
            else
                call put_line(error_unit, "No matching tests found")
                call exit(1)
            end if
        else
            tests_to_run = tests
        end if

        call put_line(output_unit, "Running Tests")
        call put_line(output_unit, "")

        if(.not.options%quiet) then
            call put_line(output_unit, tests_to_run%description())
            call put_line(output_unit, "")
        end if

        call put_line( &
                output_unit, &
                "A total of " // toString(tests_to_run%numCases()) // " test cases")
        call put_line(output_unit, "")

        results = tests_to_run%run()

        if (results%passed()) then
            call put_line(output_unit, "All Passed")
            call put_line(output_unit, "")
            if (options%verbose) then
                call put_line( &
                        output_unit, &
                        results%verboseDescription(options%colorize))
                call put_line(output_unit, "")
            end if
            call put_line( &
                    output_unit, &
                    "A total of " // toString(results%numCases()) &
                        // " test cases containing a total of " &
                        // toString(results%numAsserts()) // " assertions")
            call put_line(output_unit, "")
        else
            call put_line(error_unit, "Failed")
            call put_line(error_unit, "")
            if (options%verbose) then
                call put_line( &
                        error_unit, &
                        results%verboseDescription(options%colorize))
            else
                call put_line( &
                        error_unit, &
                        results%failureDescription(options%colorize))
            end if
            call put_line(error_unit, "")
            call put_line( &
                    error_unit, &
                    toString(results%numFailingCases()) // " of " &
                        // toString(results%numCases()) // " cases failed")
            call put_line( &
                    error_unit, &
                    toString(results%numFailingAsserts()) // " of " &
                        // toString(results%numAsserts()) // " assertions failed")
            call put_line(error_unit, "")
            call exit(1)
        end if
    end subroutine runTests

    function shrinkAsciiString(input) result(shrunk)
        use iso_varying_string, only: assignment(=), extract, len

        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(StringInput_t) :: new_input

        select type (input)
        type is (StringInput_t)
            if (len(input%value_) <= 1) then
                new_input%value_ = ""
                shrunk = SimplestValue(new_input)
            else
                new_input%value_ = extract( &
                        input%value_, 1, len(input%value_) - 1)
                shrunk = ShrunkValue(new_input)
            end if
        end select
    end function shrinkAsciiString

    function shrinkInteger(input) result(shrunk)
        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(IntegerInput_t) :: new_input

        select type (input)
        type is (IntegerInput_t)
            if (input%value_ == 0) then
                new_input%value_ = 0
                shrunk = SimplestValue(new_input)
            else
                new_input%value_ = input%value_ / 2
                shrunk = ShrunkValue(new_input)
            end if
        end select
    end function shrinkInteger

    function ShrinkResult(value_, simplest)
        class(Input_t), intent(in) :: value_
        logical, intent(in) :: simplest
        type(ShrinkResult_t) :: ShrinkResult

        allocate(ShrinkResult%input, source = value_)
        ShrinkResult%simplest = simplest
    end function ShrinkResult

    function ShrunkValue(value_)
        class(Input_t), intent(in) :: value_
        type(ShrinkResult_t) :: ShrunkValue

        ShrunkValue = ShrinkResult(value_, .false.)
    end function ShrunkValue

    function SimplestValue(value_)
        class(Input_t), intent(in) :: value_
        type(ShrinkResult_t) :: SimplestValue

        SimplestValue = ShrinkResult(value_, .true.)
    end function SimplestValue

    function SimpleTestCase(description, test)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        procedure(simpleTest) :: test
        type(SimpleTestCase_t) :: SimpleTestCase

        SimpleTestCase%description_ = description
        SimpleTestCase%test => test
    end function SimpleTestCase

    function simpleTestCaseRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, toString, NEWLINE

        class(SimpleTestCase_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "SimpleTestCase_t(" // NEWLINE &
                    // "description = """ // self%description_ // """," // NEWLINE &
                    // "test @ " // toString(loc(self%test)), &
                INDENTATION) // NEWLINE // ")"
    end function simpleTestCaseRepr

    function simpleTestCaseRunWithInput(self, input) result(result_)
        class(SimpleTestCase_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function simpleTestCaseRunWithInput

    function simpleTestCaseRunWithoutInput(self) result(result_)
        class(SimpleTestCase_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        allocate(result_%result_, source = TestCaseResult( &
                self%description_, self%test()))
    end function simpleTestCaseRunWithoutInput

    function SimpleTestCollection(description, tests)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        type(TestItem_t), intent(in) :: tests(:)
        type(SimpleTestCollection_t) :: SimpleTestCollection

        SimpleTestCollection%description_ = description
        allocate(SimpleTestCollection%tests, source = tests)
    end function SimpleTestCollection

    function simpleTestCollectionRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, indent, join, NEWLINE

        class(SimpleTestCollection_t), intent(in) :: self
        type(VARYING_STRING) :: string

        type(VARYING_STRING) :: strings(size(self%tests))
        integer :: i

        !$omp parallel do
        do i = 1, size(self%tests)
            strings(i) = self%tests(i)%repr()
        end do
        !$omp end parallel do
        string = hangingIndent( &
                "SimpleTestCollection_t(" // NEWLINE &
                    // "description = """ // self%description_ // """," // NEWLINE &
                    // "tests = [" // NEWLINE &
                    // indent( &
                            join(strings, "," // NEWLINE), &
                            INDENTATION) // NEWLINE // "]", &
                INDENTATION) // NEWLINE // ")"
    end function simpleTestCollectionRepr

    function simpleTestCollectionRunWithInput(self, input) result(result_)
        class(SimpleTestCollection_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function simpleTestCollectionRunWithInput

    function simpleTestCollectionRunWithoutInput(self) result(result_)
        class(SimpleTestCollection_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        integer :: i
        type(TestResultItem_t) :: results(size(self%tests))

        !$omp parallel do
        do i = 1, size(self%tests)
            results(i) = self%tests(i)%run()
        end do
        !$omp end parallel do
        allocate(result_%result_, source = TestCollectionResult( &
                self%description_, results))
    end function simpleTestCollectionRunWithoutInput

    pure function succeedC(message) result(success)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: message
        type(Result_t) :: success

        success = succeed(var_str(message))
    end function succeedC

    pure function succeedS(message) result(success)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: success

        allocate(success%results(1))
        success%results(1) = IndividualResult(message, .true.)
    end function succeedS

    function testCaseDescription(self) result(description)
        use iso_varying_string, only: VARYING_STRING

        class(TestCase_t), intent(in) :: self
        type(VARYING_STRING) :: description

        description = self%description_
    end function testCaseDescription

    function testCaseFilter(self, filter_string) result(filter_result)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: operator(.includes.)

        class(TestCase_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        type(FilterResult_t) :: filter_result

        if (self%description_.includes.filter_string) then
            filter_result%matched = .true.
            allocate(filter_result%test, source = self)
        else
            filter_result%matched = .false.
        end if
    end function testCaseFilter

    pure function testCaseNumCases(self) result(num_cases)
        class(TestCase_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate

        num_cases = 1
    end function testCaseNumCases

    function TestCaseResult(description, result_)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        type(Result_t), intent(in) :: result_
        type(TestCaseResult_t) :: TestCaseResult

        TestCaseResult%description = description
        TestCaseResult%result_ = result_
    end function TestCaseResult

    subroutine testCaseResultDestructor(self)
        type(TestCaseResult_t), intent(inout) :: self

        deallocate(self%result_%results)
    end subroutine testCaseResultDestructor

    function testCaseResultFailureDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: &
                VARYING_STRING, assignment(=), operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(TestCaseResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        if (self%passed()) then
            description = ""
        else
            description = hangingIndent( &
                    self%description // NEWLINE &
                        // self%result_%failureDescription(colorize), &
                    INDENTATION)
        end if
    end function testCaseResultFailureDescription

    pure function testCaseResultNumAsserts(self) result(num_asserts)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%numAsserts()
    end function testCaseResultNumAsserts

    pure function testCaseResultNumCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate

        num_cases = 1
    end function testCaseResultNumCases

    pure function testCaseResultNumFailingAsserts(self) result(num_asserts)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%numFailingAsserts()
    end function testCaseResultNumFailingAsserts

    pure function testCaseResultNumFailingCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        if (self%passed()) then
            num_cases = 0
        else
            num_cases = 1
        end if
    end function testCaseResultNumFailingCases

    pure function testCaseResultPassed(self) result(passed)
        class(TestCaseResult_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function testCaseResultPassed

    function testCaseResultRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(TestCaseResult_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "TestCaseResult_t(" // NEWLINE &
                    // "description = """ // self%description // """," // NEWLINE &
                    // "result = " // self%result_%repr(), &
                INDENTATION) // NEWLINE // ")"
    end function testCaseResultRepr

    function testCaseResultVerboseDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(TestCaseResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        description = hangingIndent( &
                self%description // NEWLINE &
                    // self%result_%verboseDescription(colorize), &
                INDENTATION)
    end function testCaseResultVerboseDescription

    function TestCaseWithExamples(description, examples, test)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        type(Example_t), intent(in) :: examples(:)
        procedure(inputTest) :: test
        type(TestCaseWithExamples_t) :: TestCaseWithExamples

        TestCaseWithExamples%description_ = description
        allocate(TestCaseWithExamples%examples, source = examples)
        TestCaseWithExamples%test => test
    end function TestCaseWithExamples

    function testCaseWithExamplesRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, toString, NEWLINE

        class(TestCaseWithExamples_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "TestCaseWithExamples_t(" // NEWLINE &
                    // "description = """ // self%description_ // """," // NEWLINE &
                    // "num_examples = " // toString(size(self%examples)) // "," // NEWLINE &
                    // "test @ " // toString(loc(self%test)), &
                INDENTATION) // NEWLINE // ")"
    end function testCaseWithExamplesRepr

    function testCaseWithExamplesRunWithInput(self, input) result(result_)
        class(TestCaseWithExamples_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function testCaseWithExamplesRunWithInput

    function testCaseWithExamplesRunWithoutInput(self) result(result_)
        class(TestCaseWithExamples_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        integer :: i
        type(Result_t) :: results

        !$omp parallel do
        do i = 1, size(self%examples)
            results = results.and.self%test(self%examples(i)%input)
        end do
        !$omp end parallel do
        allocate(result_%result_, source = TestCaseResult( &
                self%description_, results))
    end function testCaseWithExamplesRunWithoutInput

    function TestCaseWithGenerator(description, generator, test)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        class(Generator_t), intent(in) :: generator
        procedure(inputTest) :: test
        type(TestCaseWithGenerator_t) :: TestCaseWithGenerator

        TestCaseWithGenerator%description_ = description
        allocate(TestCaseWithGenerator%generator, source = generator)
        TestCaseWithGenerator%test => test
    end function TestCaseWithGenerator

    function testCaseWithGeneratorRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, toString, NEWLINE

        class(TestCaseWithGenerator_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "TestCaseWithGenerator_t(" // NEWLINE &
                    // "description = """ // self%description_ // """," // NEWLINE &
                    // "generator @ " // toString(loc(self%generator)) // "," // NEWLINE &
                    // "test @ " // toString(loc(self%test)), &
                INDENTATION) // NEWLINE // ")"
    end function testCaseWithGeneratorRepr

    function testCaseWithGeneratorRunWithInput(self, input) result(result_)
        class(TestCaseWithGenerator_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function testCaseWithGeneratorRunWithInput

    function testCaseWithGeneratorRunWithoutInput(self) result(result_)
        use iso_varying_string, only: operator(//)
        use strff, only: toString

        class(TestCaseWithGenerator_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        type(Generated_t) :: generated_value
        integer :: i
        type(Result_t) :: new_result
        type(Result_t) :: previous_result
        type(ShrinkResult_t) :: simpler_value

        do i = 1, NUM_GENERATOR_TESTS
            generated_value = self%generator%generate()
            previous_result = self%test(generated_value%input)
            if (.not.previous_result%passed()) exit
        end do
        if (i > NUM_GENERATOR_TESTS) then
            allocate(result_%result_, source = TestCaseResult( &
                    self%description_, &
                    succeed("Passed after " // toString(NUM_GENERATOR_TESTS) // " examples")))
        else
            do
                simpler_value = self%generator%shrink(generated_value%input)
                if (simpler_value%simplest) then
                    new_result = self%test(simpler_value%input)
                    if (new_result%passed()) then
                        allocate(result_%result_, source = TestCaseResult( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result))
                        return
                    else
                        allocate(result_%result_, source = TestCaseResult( &
                                self%description_, &
                                fail('Fails with the simplest possible example').and.new_result))
                        return
                    end if
                else
                    new_result = self%test(simpler_value%input)
                    if (new_result%passed()) then
                        allocate(result_%result_, source = TestCaseResult( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result))
                        return
                    else
                        previous_result = new_result
                        generated_value = Generated(simpler_value%input)
                    end if
                end if
            end do
        end if
    end function testCaseWithGeneratorRunWithoutInput

    function testCollectionDescription(self) result(description)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, join, NEWLINE

        class(TestCollection_t), intent(in) :: self
        type(VARYING_STRING) :: description

        type(VARYING_STRING) :: descriptions(size(self%tests))
        integer :: i

        !$omp parallel do
        do i = 1, size(self%tests)
            descriptions(i) = self%tests(i)%description()
        end do
        !$omp end parallel do
        description = hangingIndent( &
                self%description_ // NEWLINE // join(descriptions, NEWLINE), &
                INDENTATION)
    end function testCollectionDescription

    function testCollectionFilter(self, filter_string) result(filter_result)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: operator(.includes.)

        class(TestCollection_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        type(FilterResult_t) :: filter_result

        class(TestCollection_t), allocatable :: new_collection
        type(FilterItemResult_t) :: filter_results(size(self%tests))
        integer :: i

        if (self%description_.includes.filter_string) then
            filter_result%matched = .true.
            allocate(filter_result%test, source = self)
        else
            !$omp parallel do
            do i = 1, size(self%tests)
                filter_results(i) = self%tests(i)%filter(filter_string)
            end do
            !$omp end parallel do
            if (any(filter_results%matched)) then
                allocate(new_collection, source = self)
                deallocate(new_collection%tests)
                allocate(new_collection%tests, source = &
                        pack(filter_results%test, mask=filter_results%matched))
                filter_result%matched = .true.
                allocate(filter_result%test, source = new_collection)
            else
                filter_result%matched = .false.
            end if
        end if
    end function testCollectionFilter

    pure function testCollectionNumCases(self) result(num_cases)
        class(TestCollection_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%tests%numCases())
    end function testCollectionNumCases

    function TestCollectionResult(description, results)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        type(TestResultItem_t), intent(in) :: results(:)
        type(TestCollectionResult_t) :: TestCollectionResult

        TestCollectionResult%description = description
        allocate(TestCollectionResult%results, source = results)
    end function TestCollectionResult

    function testCollectionResultFailureDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: &
                VARYING_STRING, assignment(=), operator(//)
        use strff, only: hangingIndent, join, NEWLINE

        class(TestCollectionResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        type(VARYING_STRING) :: descriptions(size(self%results))
        integer :: i

        if (self%passed()) then
            description = ""
        else
            !$omp parallel do
            do i = 1, size(self%results)
                descriptions(i) = self%results(i)%failureDescription(colorize)
            end do
            !$omp end parallel do
            description = hangingIndent( &
                    self%description // NEWLINE // join(descriptions, NEWLINE), &
                    INDENTATION)
        end if
    end function testCollectionResultFailureDescription

    pure function testCollectionResultNumAsserts(self) result(num_asserts)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = sum(self%results%numAsserts())
    end function testCollectionResultNumAsserts

    pure function testCollectionResultNumCases(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%results%numCases())
    end function testCollectionResultNumCases

    pure function testCollectionResultNumFailingAsserts(self) result(num_asserts)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = sum(self%results%numFailingAsserts())
    end function testCollectionResultNumFailingAsserts

    pure function testCollectionResultNumFailingCases(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%results%numFailingCases())
    end function testCollectionResultNumFailingCases

    pure function testCollectionResultPassed(self) result(passed)
        class(TestCollectionResult_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed())
    end function testCollectionResultPassed

    function testCollectionResultRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, indent, join, NEWLINE

        class(TestCollectionResult_t), intent(in) :: self
        type(VARYING_STRING) :: string

        type(VARYING_STRING) :: strings(size(self%results))
        integer :: i

        !$omp parallel do
        do i = 1, size(self%results)
            strings(i) = self%results(i)%repr()
        end do
        !$omp end parallel do
        string = hangingIndent( &
                "TestCollectionResult_t(" // NEWLINE &
                    // "description = """ // self%description // """," // NEWLINE &
                    // "tests = [" // NEWLINE &
                    // indent( &
                            join(strings, "," // NEWLINE), &
                            INDENTATION) // NEWLINE // "]", &
                INDENTATION) // NEWLINE // ")"
    end function testCollectionResultRepr

    function testCollectionResultVerboseDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, join, NEWLINE

        class(TestCollectionResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        type(VARYING_STRING) :: descriptions(size(self%results))
        integer :: i

        !$omp parallel do
        do i = 1, size(self%results)
            descriptions(i) = self%results(i)%verboseDescription(colorize)
        end do
        !$omp end parallel do
        description = hangingIndent( &
                self%description // NEWLINE // join(descriptions, NEWLINE), &
                INDENTATION)
    end function testCollectionResultVerboseDescription

    function TestCollectionWithInput(description, input, tests)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        class(Input_t), intent(in) :: input
        type(TestItem_t), intent(in) :: tests(:)
        type(TestCollectionWithInput_t) :: TestCollectionWithInput

        TestCollectionWithInput%description_ = description
        allocate(TestCollectionWithInput%input, source = input)
        allocate(TestCollectionWithInput%tests, source = tests)
    end function TestCollectionWithInput

    subroutine testCollectionWithInputDestructor(self)
        type(TestCollectionWithInput_t), intent(inout) :: self

        deallocate(self%input)
        deallocate(self%tests)
    end subroutine testCollectionWithInputDestructor

    function testCollectionWithInputRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, indent, join, toString, NEWLINE

        class(TestCollectionWithInput_t), intent(in) :: self
        type(VARYING_STRING) :: string

        type(VARYING_STRING) :: strings(size(self%tests))
        integer :: i

        !$omp parallel do
        do i = 1, size(self%tests)
            strings(i) = self%tests(i)%repr()
        end do
        !$omp end parallel do
        string = hangingIndent( &
                "TestCollectionWithInput_t(" // NEWLINE &
                    // "description = """ // self%description_ // """," // NEWLINE &
                    // "input @ " // toString(loc(self%input)) // "," // NEWLINE &
                    // "tests = [" // NEWLINE &
                    // indent( &
                            join(strings, "," // NEWLINE), &
                            INDENTATION) // NEWLINE // "]", &
                INDENTATION) // NEWLINE // ")"
    end function testCollectionWithInputRepr

    function testCollectionWithInputRunWithInput(self, input) result(result_)
        class(TestCollectionWithInput_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function testCollectionWithInputRunWithInput

    function testCollectionWithInputRunWithoutInput(self) result(result_)
        class(TestCollectionWithInput_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        integer :: i
        type(TestResultItem_t) :: results(size(self%tests))

        !$omp parallel do
        do i = 1, size(self%tests)
            results(i) = self%tests(i)%run(self%input)
        end do
        !$omp end parallel do
        allocate(result_%result_, source = TestCollectionResult( &
                self%description_, results))
    end function testCollectionWithInputRunWithoutInput

    function testItemDescription(self) result(description)
        use iso_varying_string, only: VARYING_STRING

        class(TestItem_t), intent(in) :: self
        type(VARYING_STRING) :: description

        description = self%test%description()
    end function testItemDescription

    function testItemFilter(self, filter_string) result(filter_result)
        use iso_varying_string, only: VARYING_STRING

        class(TestItem_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        type(FilterItemResult_t) :: filter_result

        type(FilterResult_t) :: test_filter_result

        test_filter_result = self%test%filter(filter_string)
        if (test_filter_result%matched) then
            filter_result%matched = .true.
            allocate(filter_result%test%test, source = test_filter_result%test)
        else
            filter_result%matched = .false.
        end if
    end function testItemFilter

    elemental function testItemNumCases(self) result(num_cases)
        class(TestItem_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%test%numCases()
    end function testItemNumCases

    function testItemRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(TestItem_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "TestItem_t(" // NEWLINE &
                    // "test = " // self%test%repr(), &
                INDENTATION) // NEWLINE // ")"
    end function testItemRepr

    function testItemRunWithInput(self, input) result(result_)
        class(TestItem_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        result_ = self%test%run(input)
    end function testItemRunWithInput

    function testItemRunWithoutInput(self) result(result_)
        class(TestItem_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        result_ = self%test%run()
    end function testItemRunWithoutInput

    function testResultItemFailureDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: VARYING_STRING

        class(TestResultItem_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        description = self%result_%failureDescription(colorize)
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

    elemental function testResultItemNumFailingAsserts(self) result(num_asserts)
        class(TestResultItem_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%numFailingAsserts()
    end function testResultItemNumFailingAsserts

    elemental function testResultItemNumFailingCases(self) result(num_cases)
        class(TestResultItem_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%numFailingCases()
    end function testResultItemNumFailingCases

    elemental function testResultItemPassed(self) result(passed)
        class(TestResultItem_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function testResultItemPassed

    function testResultItemRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(TestResultItem_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                "TestResultItem_t(" // NEWLINE &
                    // "result = " // self%result_%repr(), &
                INDENTATION) // NEWLINE // ")"
    end function testResultItemRepr

    function testResultItemVerboseDescription( &
            self, colorize) result(description)
        use iso_varying_string, only: VARYING_STRING

        class(TestResultItem_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        description = self%result_%verboseDescription(colorize)
    end function testResultItemVerboseDescription

    function testThat(tests) result(item)
        type(TestItem_t) :: tests(:)
        type(TestItem_t) :: item

        item = Describe("Test that", tests)
    end function testThat

    function ThenInputC(description, test) result(item)
        character(len=*), intent(in) :: description
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        item = It_("Then " // description, test)
    end function ThenInputC

    function ThenInputS(description, test) result(item)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: description
        procedure(inputTest) :: test
        type(TestItem_t) :: item

        item = It_("Then " // description, test)
    end function ThenInputS

    function Transformed(input)
        class(Input_t), intent(in) :: input
        type(Transformed_t) :: Transformed

        allocate(Transformed%input, source = input)
    end function Transformed

    function TransformingTestCollection(description, transformer, tests)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: description
        procedure(transformer_) :: transformer
        type(TestItem_t), intent(in) :: tests(:)
        type(TransformingTestCollection_t) :: TransformingTestCollection

        TransformingTestCollection%description_ = description
        TransformingTestCollection%transformer => transformer
        allocate(TransformingTestCollection%tests, source = tests)
    end function TransformingTestCollection

    function transformingTestCollectionRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, indent, join, toString, NEWLINE

        class(TransformingTestCollection_t), intent(in) :: self
        type(VARYING_STRING) :: string

        type(VARYING_STRING) :: strings(size(self%tests))
        integer :: i

        !$omp parallel do
        do i = 1, size(self%tests)
            strings(i) = self%tests(i)%repr()
        end do
        !$omp end parallel do
        string = hangingIndent( &
                "TransformingTestCollection_t(" // NEWLINE &
                    // "description = """ // self%description_ // """," // NEWLINE &
                    // "transformer @ " // toString(loc(self%transformer)) // "," // NEWLINE &
                    // "tests = [" // NEWLINE &
                    // indent( &
                            join(strings, "," // NEWLINE), &
                            INDENTATION) // NEWLINE // "]", &
                INDENTATION) // NEWLINE // ")"
    end function transformingTestCollectionRepr

    function transformingTestCollectionRunWithInput(self, input) result(result_)
        class(TransformingTestCollection_t), intent(in) :: self
        class(Input_t), intent(in) :: input
        type(TestResultItem_t) :: result_

        integer :: i
        type(TestResultItem_t) :: results(size(self%tests))
        type(Transformed_t) :: transformed_

        transformed_ = self%transformer(input)
        select type (transformed_input => transformed_%input)
        type is (TransformationFailure_t)
            allocate(result_%result_, source = testCaseResult( &
                    self%description_, transformed_input%result_))
        class default
            !$omp parallel do
            do i = 1, size(self%tests)
                results(i) = self%tests(i)%run(transformed_input)
            end do
            !$omp end parallel do
            allocate(result_%result_, source = TestCollectionResult( &
                    self%description_, results))
        end select
    end function transformingTestCollectionRunWithInput

    function transformingTestCollectionRunWithoutInput(self) result(result_)
        class(TransformingTestCollection_t), intent(in) :: self
        type(TestResultItem_t) :: result_

        allocate(result_%result_, source = TestCaseResult( &
                self%description_, fail("No input provided")))
    end function transformingTestCollectionRunWithoutInput

    function whenWithTransformerC(description, transformer, tests) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        procedure(transformer_) :: transformer
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        allocate(item%test, source = TransformingTestCollection( &
                var_str("When " // description), transformer, tests))
    end function whenWithTransformerC

    function whenWithTransformerS(description, transformer, tests) result(item)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: description
        procedure(transformer_) :: transformer
        type(TestItem_t), intent(in) :: tests(:)
        type(TestItem_t) :: item

        allocate(item%test, source = TransformingTestCollection( &
                "When " // description, transformer, tests))
    end function whenWithTransformerS

    pure function withUserMessageCC(message, user_message) result(whole_message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        whole_message = withUserMessage( &
                var_str(message), var_str(user_message))
    end function withUserMessageCC

    pure function withUserMessageCS(message, user_message) result(whole_message)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: message
        type(VARYING_STRING), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        whole_message = withUserMessage(var_str(message), user_message)
    end function withUserMessageCS

    pure function withUserMessageSC(message, user_message) result(whole_message)
        use iso_varying_string, only: VARYING_STRING, var_str

        type(VARYING_STRING), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        whole_message = withUserMessage( &
                message, var_str(user_message))
    end function withUserMessageSC

    pure function withUserMessageSS(message, user_message) result(whole_message)
        use iso_varying_string, only: &
                VARYING_STRING, operator(//), operator(==)
        use strff, only: indent, hangingIndent, NEWLINE

        type(VARYING_STRING), intent(in) :: message
        type(VARYING_STRING), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        if (user_message == "") then
            whole_message = message
        else
            whole_message = &
                    message // NEWLINE &
                    // indent( &
                            hangingIndent( &
                                    "User Message:" // NEWLINE &
                                        // delimit(hangingIndent( &
                                                user_message, 1)), &
                                    INDENTATION), &
                            INDENTATION)
        end if
    end function withUserMessageSS
end module Vegetables_m
