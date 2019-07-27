module Vegetables_m
    use iso_varying_string

    implicit none
    private

    type :: VegetableString_t
        private
        type(VARYING_STRING) :: string
    end type VegetableString_t

    type, public :: Generated_t
        private
        class(*), allocatable :: value_
    end type Generated_t

    type, public, abstract :: ShrinkResult_t
        private
        class(*), allocatable :: value_
    end type ShrinkResult_t

    type, public, extends(ShrinkResult_t) :: ShrunkValue_t
    end type ShrunkValue_t

    type, public, extends(ShrinkResult_t) :: SimplestValue_t
    end type SimplestValue_t

    type, public, abstract :: Generator_t
    contains
        private
        procedure(generate_), public, deferred :: generate
        procedure(shrink_), public, nopass, deferred :: shrink
    end type Generator_t

    type, public, extends(Generator_t) :: AsciiCharactersGenerator_t
    contains
        private
        procedure, public :: generate => generateAsciiCharacters
        procedure, public, nopass :: shrink => shrinkAsciiCharacters
    end type AsciiCharactersGenerator_t

    type, public, extends(Generator_t) :: AsciiStringGenerator_t
    contains
        private
        procedure, public :: generate => generateAsciiString
        procedure, public, nopass :: shrink => shrinkAsciiString
    end type AsciiStringGenerator_t

    type, public, extends(Generator_t) :: IntegerGenerator_t
    contains
        private
        procedure, public :: generate => generateInteger
        procedure, public, nopass :: shrink => shrinkInteger
    end type IntegerGenerator_t

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

    type :: IndividualResult_t
        private
        type(VARYING_STRING) :: message
        logical :: passed_
    contains
        private
        procedure :: failureDescription => individualResultFailureDescription
        procedure :: verboseDescription => individualResultVerboseDescription
    end type IndividualResult_t

    type, public :: Result_t
        private
        type(IndividualResult_t), allocatable :: results(:)
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
        type(VARYING_STRING) :: description_
    contains
        private
        procedure(testDescription), deferred, public :: description
        procedure(filter_), deferred, public :: filter
        procedure(testNum), deferred, public :: numCases
    end type Test_t

    type, abstract, public :: TestResult_t
        private
        type(VARYING_STRING) :: description
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
            import :: Test_t, Maybe_t, VARYING_STRING
            class(Test_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: filter_string
            class(Maybe_t), allocatable :: maybe
        end function filter_

        function generate_(self) result(generated_value)
            import :: Generated_t, Generator_t
            class(Generator_t), intent(in) :: self
            type(Generated_t) :: generated_value
        end function generate_

        function inputTest(input) result(result_)
            import :: Result_t
            class(*), intent(in) :: input
            type(Result_t) :: result_
        end function inputTest

        function shrink_(value_) result(shrunk_value)
            import :: ShrinkResult_t
            class(*), intent(in) :: value_
            class(ShrinkResult_t), allocatable :: shrunk_value
        end function shrink_

        function test_() result(result_)
            import :: Result_t
            type(Result_t) :: result_
        end function

        pure function testDescription(self) result(description)
            import :: Test_t, VARYING_STRING
            class(Test_t), intent(in) :: self
            type(VARYING_STRING) :: description
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

        pure function testResultDescription(self, colorize) result(description)
            import :: TestResult_t, VARYING_STRING
            class(TestResult_t), intent(in) :: self
            logical, intent(in) :: colorize
            type(VARYING_STRING) :: description
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

    type, extends(Test_t), public :: TestCaseWithGenerator_t
        private
        class(Generator_t), allocatable :: generator
        procedure(inputTest), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseWithGeneratorDescription
        procedure, public :: filter => filterTestCaseWithGenerator
        procedure, public :: numCases => testCaseWithGeneratorNumCases
        procedure, public :: run => runCaseWithGenerator
    end type TestCaseWithGenerator_t

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
        logical :: colorize
        logical :: quiet
        logical :: verbose
        logical :: filter_tests
        type(VARYING_STRING) :: filter_string
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

    type, public, extends(Maybe_t) :: JustTestCaseWithGenerator_t
        private
        type(TestCaseWithGenerator_t) :: value_
    contains
        private
        procedure, public :: getValue => getValueTestCaseWithGenerator
    end type JustTestCaseWithGenerator_t

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
        module procedure includesCC
        ! module procedure includesCS
        ! module procedure includesSC
        module procedure includesSS
    end interface operator(.includes.)

    interface assertDoesntInclude
        module procedure assertDoesntIncludeCC
        module procedure assertDoesntIncludeCS
        module procedure assertDoesntIncludeSC
        module procedure assertDoesntIncludeSS
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
        module procedure assertEmptyC
        module procedure assertEmptyS
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
        module procedure assertEqualsInteger
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

    interface describe
        module procedure describeBasic
        module procedure describeWithInput
    end interface describe

    interface fail
        module procedure failC
        module procedure failS
    end interface fail

    interface given
        module procedure givenBasic
        module procedure givenWithInput
    end interface given

    interface hangingIndent
        module procedure hangingIndentC
        module procedure hangingIndentS
    end interface hangingIndent

    interface indent
        ! module procedure indentC
        module procedure indentS
    end interface indent

    interface it
        module procedure itBasic
        module procedure itWithExamples
        module procedure itWithGenerator
    end interface it

    interface join
        module procedure joinC
        ! module procedure joinS
    end interface join

    interface Just
        module procedure JustInputTestCase
        module procedure JustTestCase
        module procedure JustTestCaseWithExamples
        module procedure JustTestCaseWithGenerator
        module procedure JustTestCollection
        module procedure JustTestCollectionWithInput
        module procedure JustTestItem
        module procedure JustTransformingTestCollection
    end interface Just

    interface lastCharacter
        module procedure lastCharacterC
        module procedure lastCharacterS
    end interface lastCharacter

    interface splitAt
        module procedure splitAtCC
        module procedure splitAtSC
    end interface splitAt

    interface succeed
        module procedure succeedC
        module procedure succeedS
    end interface succeed

    interface TestCaseResult
        module procedure TestCaseResultC
        module procedure TestCaseResultS
    end interface TestCaseResult

    interface TestCollectionResult
        ! module procedure TestCollectionResultC
        module procedure TestCollectionResultS
    end interface TestCollectionResult

    interface toString
        module procedure doublePrecisionToString
        module procedure integerToString
    end interface toString

    interface withUserMessage
        module procedure withUserMessageCC
        module procedure withUserMessageCS
        module procedure withUserMessageSC
        module procedure withUserMessageSS
    end interface withUserMessage

    interface when
        module procedure whenBasic
        module procedure whenWithInput
        module procedure whenWithTransformer
    end interface

    type(AsciiCharactersGenerator_t), parameter, public :: &
            ASCII_CHARACTERS_GENERATOR = AsciiCharactersGenerator_t()
    type(AsciiStringGenerator_t), parameter, public :: &
            ASCII_STRING_GENERATOR = AsciiStringGenerator_t()
    type(IntegerGenerator_t), parameter, public :: &
            INTEGER_GENERATOR = IntegerGenerator_t()

    integer, parameter :: dp = kind(0.0d0)
    character(len=*), parameter, public :: EMPTY_SUCCESS_MESSAGE = "String was empty"
    integer, parameter :: INDENTATION = 4
    double precision, parameter :: MACHINE_EPSILON = EPSILON(0.0_dp)
    double precision, parameter :: MACHINE_TINY = TINY(0.0_dp)
    integer, parameter :: MAX_INT = HUGE(1)
    character(len=*), parameter :: NEWLINE = NEW_LINE('A')
    character(len=*), parameter, public :: NOT_FAILURE_MESSAGE = "Expected to not be true"
    character(len=*), parameter, public :: NOT_SUCCESS_MESSAGE = "Was not true"
    type(Nothing_t), parameter :: NOTHING = Nothing_t()
    integer :: NUM_GENERATOR_TESTS = 100
    character(len=*), parameter, public :: THAT_FAILURE_MESSAGE = "Expected to be true"
    character(len=*), parameter, public :: THAT_SUCCESS_MESSAGE = "Was true"

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
            Generated, &
            given, &
            getRandomAsciiCharacter, &
            getRandomAsciiCharacters, &
            getRandomAsciiCharactersWithMaxLength, &
            getRandomAsciiString, &
            getRandomAsciiStringWithMaxLength, &
            getRandomDoublePrecisionWithMagnitude, &
            getRandomDoublePrecisionWithRange, &
            getRandomInteger, &
            getRandomIntegerWithRange, &
            getRandomLogical, &
            it, &
            it_, &
            makeDoesntIncludeFailureMessage, &
            makeDoesntIncludeSuccessMessage, &
            makeEmptyFailureMessage, &
            makeEqualsFailureMessage, &
            makeEqualsSuccessMessage, &
            makeIncludesFailureMessage, &
            makeIncludesSuccessMessage, &
            makeWithinFailureMessage, &
            makeWithinSuccesMessage, &
            runTests, &
            ShrunkValue, &
            SimplestValue, &
            succeed, &
            TestCase, &
            TestCollection, &
            testThat, &
            then, &
            then_, &
            Transformed, &
            withUserMessage, &
            when
contains
    pure function assertDoesntIncludeCC(search_for, string) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, "", "")
    end function assertDoesntIncludeCC

    pure function assertDoesntIncludeCS(search_for, string) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, char(string), "", "")
    end function assertDoesntIncludeCS

    pure function assertDoesntIncludeSC(search_for, string) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), string, "", "")
    end function assertDoesntIncludeSC

    pure function assertDoesntIncludeSS(search_for, string) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), char(string), "", "")
    end function assertDoesntIncludeSS

    pure function assertDoesntIncludeWithMessageCCC( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, message, message)
    end function assertDoesntIncludeWithMessageCCC

    pure function assertDoesntIncludeWithMessageCCS( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, char(message), char(message))
    end function assertDoesntIncludeWithMessageCCS

    pure function assertDoesntIncludeWithMessageCSC( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, char(string), message, message)
    end function assertDoesntIncludeWithMessageCSC

    pure function assertDoesntIncludeWithMessageCSS( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, char(string), char(message), char(message))
    end function assertDoesntIncludeWithMessageCSS

    pure function assertDoesntIncludeWithMessageSCC( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), string, message, message)
    end function assertDoesntIncludeWithMessageSCC

    pure function assertDoesntIncludeWithMessageSCS( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), string, char(message), char(message))
    end function assertDoesntIncludeWithMessageSCS

    pure function assertDoesntIncludeWithMessageSSC( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), char(string), message, message)
    end function assertDoesntIncludeWithMessageSSC

    pure function assertDoesntIncludeWithMessageSSS( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), char(string), char(message), char(message))
    end function assertDoesntIncludeWithMessageSSS

    pure function assertDoesntIncludeWithMessagesCCCC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if(.not.(string.includes.search_for)) then
            result__ = succeed(withUserMessage(&
                    makeDoesntIncludeSuccessMessage(search_for, string), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeDoesntIncludeFailureMessage(search_for, string), &
                    failure_message))
        end if
    end function assertDoesntIncludeWithMessagesCCCC

    pure function assertDoesntIncludeWithMessagesCCCS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, success_message, char(failure_message))
    end function assertDoesntIncludeWithMessagesCCCS

    pure function assertDoesntIncludeWithMessagesCCSC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, char(success_message), failure_message)
    end function assertDoesntIncludeWithMessagesCCSC

    pure function assertDoesntIncludeWithMessagesCCSS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, string, char(success_message), char(failure_message))
    end function assertDoesntIncludeWithMessagesCCSS

    pure function assertDoesntIncludeWithMessagesCSCC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, char(string), success_message, failure_message)
    end function assertDoesntIncludeWithMessagesCSCC

    pure function assertDoesntIncludeWithMessagesCSCS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, char(string), success_message, char(failure_message))
    end function assertDoesntIncludeWithMessagesCSCS

    pure function assertDoesntIncludeWithMessagesCSSC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, char(string), char(success_message), failure_message)
    end function assertDoesntIncludeWithMessagesCSSC

    pure function assertDoesntIncludeWithMessagesCSSS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(search_for, char(string), char(success_message), char(failure_message))
    end function assertDoesntIncludeWithMessagesCSSS

    pure function assertDoesntIncludeWithMessagesSCCC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), string, success_message, failure_message)
    end function assertDoesntIncludeWithMessagesSCCC

    pure function assertDoesntIncludeWithMessagesSCCS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), string, success_message, char(failure_message))
    end function assertDoesntIncludeWithMessagesSCCS

    pure function assertDoesntIncludeWithMessagesSCSC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), string, char(success_message), failure_message)
    end function assertDoesntIncludeWithMessagesSCSC

    pure function assertDoesntIncludeWithMessagesSCSS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), string, char(success_message), char(failure_message))
    end function assertDoesntIncludeWithMessagesSCSS

    pure function assertDoesntIncludeWithMessagesSSCC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), char(string), success_message, failure_message)
    end function assertDoesntIncludeWithMessagesSSCC

    pure function assertDoesntIncludeWithMessagesSSCS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), char(string), success_message, char(failure_message))
    end function assertDoesntIncludeWithMessagesSSCS

    pure function assertDoesntIncludeWithMessagesSSSC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), char(string), char(success_message), failure_message)
    end function assertDoesntIncludeWithMessagesSSSC

    pure function assertDoesntIncludeWithMessagesSSSS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertDoesntInclude(char(search_for), char(string), char(success_message), char(failure_message))
    end function assertDoesntIncludeWithMessagesSSSS

    pure function assertEmptyC(string) result(result__)
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertEmpty(string, "", "")
    end function assertEmptyC

    pure function assertEmptyS(string) result(result__)
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertEmpty(char(string), "", "")
    end function assertEmptyS

    pure function assertEmptyWithMessageCC(string, message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty(string, message, message)
    end function assertEmptyWithMessageCC

    pure function assertEmptyWithMessageCS(string, message) result(result__)
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty(string, char(message), char(message))
    end function assertEmptyWithMessageCS

    pure function assertEmptyWithMessageSC(string, message) result(result__)
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty(char(string), message, message)
    end function assertEmptyWithMessageSC

    pure function assertEmptyWithMessageSS(string, message) result(result__)
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEmpty(char(string), char(message), char(message))
    end function assertEmptyWithMessageSS

    pure function assertEmptyWithMessagesCCC(&
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (string == "") then
            result__ = succeed(withUserMessage( &
                    EMPTY_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(withUserMessage( &
                    makeEmptyFailureMessage(string), &
                    failure_message))
        end if
    end function assertEmptyWithMessagesCCC

    pure function assertEmptyWithMessagesCCS(&
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty(string, success_message, char(failure_message))
    end function assertEmptyWithMessagesCCS

    pure function assertEmptyWithMessagesCSC(&
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty(string, char(success_message), failure_message)
    end function assertEmptyWithMessagesCSC

    pure function assertEmptyWithMessagesCSS(&
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty(string, char(success_message), char(failure_message))
    end function assertEmptyWithMessagesCSS

    pure function assertEmptyWithMessagesSCC(&
            string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty(char(string), success_message, failure_message)
    end function assertEmptyWithMessagesSCC

    pure function assertEmptyWithMessagesSCS(&
            string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty(char(string), success_message, char(failure_message))
    end function assertEmptyWithMessagesSCS

    pure function assertEmptyWithMessagesSSC(&
            string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty(char(string), char(success_message), failure_message)
    end function assertEmptyWithMessagesSSC

    pure function assertEmptyWithMessagesSSS(&
            string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEmpty(char(string), char(success_message), char(failure_message))
    end function assertEmptyWithMessagesSSS

    pure function assertEqualsDoublePrecision(expected, actual) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, "", "")
    end function assertEqualsDoublePrecision

    pure function assertEqualsDoublePrecisionWithMessageC( &
            expected, actual, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, message, message)
    end function assertEqualsDoublePrecisionWithMessageC

    pure function assertEqualsDoublePrecisionWithMessageS( &
            expected, actual, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(message), char(message))
    end function assertEqualsDoublePrecisionWithMessageS

    pure function assertEqualsDoublePrecisionWithMessagesCC( &
            expected, actual, success_message, failure_message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, MACHINE_EPSILON, success_message, failure_message)
    end function assertEqualsDoublePrecisionWithMessagesCC

    pure function assertEqualsDoublePrecisionWithMessagesCS( &
            expected, actual, success_message, failure_message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, success_message, char(failure_message))
    end function assertEqualsDoublePrecisionWithMessagesCS

    pure function assertEqualsDoublePrecisionWithMessagesSC( &
            expected, actual, success_message, failure_message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(success_message), failure_message)
    end function assertEqualsDoublePrecisionWithMessagesSC

    pure function assertEqualsDoublePrecisionWithMessagesSS( &
            expected, actual, success_message, failure_message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(success_message), char(failure_message))
    end function assertEqualsDoublePrecisionWithMessagesSS

    pure function assertEqualsStringsCC(expected, actual) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, "", "")
    end function assertEqualsStringsCC

    pure function assertEqualsStringsCS(expected, actual) result(result__)
        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, char(actual), "", "")
    end function assertEqualsStringsCS

    pure function assertEqualsStringsSC(expected, actual) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), actual, "", "")
    end function assertEqualsStringsSC

    pure function assertEqualsStringsSS(expected, actual) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), char(actual), "", "")
    end function assertEqualsStringsSS

    pure function assertEqualsStringsWithMessageCCC( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, message, message)
    end function assertEqualsStringsWithMessageCCC

    pure function assertEqualsStringsWithMessageCCS( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(message), char(message))
    end function assertEqualsStringsWithMessageCCS

    pure function assertEqualsStringsWithMessageCSC( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, char(actual), message, message)
    end function assertEqualsStringsWithMessageCSC

    pure function assertEqualsStringsWithMessageCSS( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, char(actual), char(message), char(message))
    end function assertEqualsStringsWithMessageCSS

    pure function assertEqualsStringsWithMessageSCC( &
            expected, actual, message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), actual, message, message)
    end function assertEqualsStringsWithMessageSCC

    pure function assertEqualsStringsWithMessageSCS( &
            expected, actual, message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), actual, char(message), char(message))
    end function assertEqualsStringsWithMessageSCS

    pure function assertEqualsStringsWithMessageSSC( &
            expected, actual, message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), char(actual), message, message)
    end function assertEqualsStringsWithMessageSSC

    pure function assertEqualsStringsWithMessageSSS( &
            expected, actual, message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), char(actual), char(message), char(message))
    end function assertEqualsStringsWithMessageSSS

    pure function assertEqualsStringsWithMessagesCCCC( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed(withUserMessage( &
                    makeEqualsSuccessMessage(var_str(expected)), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeEqualsFailureMessage(var_str(expected), var_str(actual)), &
                    failure_message))
        end if
    end function assertEqualsStringsWithMessagesCCCC

    pure function assertEqualsStringsWithMessagesCCCS( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, success_message, char(failure_message))
    end function assertEqualsStringsWithMessagesCCCS

    pure function assertEqualsStringsWithMessagesCCSC( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(success_message), failure_message)
    end function assertEqualsStringsWithMessagesCCSC

    pure function assertEqualsStringsWithMessagesCCSS( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(success_message), char(failure_message))
    end function assertEqualsStringsWithMessagesCCSS

    pure function assertEqualsStringsWithMessagesCSCC( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, char(actual), success_message, failure_message)
    end function assertEqualsStringsWithMessagesCSCC

    pure function assertEqualsStringsWithMessagesCSCS( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, char(actual), success_message, char(failure_message))
    end function assertEqualsStringsWithMessagesCSCS

    pure function assertEqualsStringsWithMessagesCSSC( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, char(actual), char(success_message), failure_message)
    end function assertEqualsStringsWithMessagesCSSC

    pure function assertEqualsStringsWithMessagesCSSS( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, char(actual), char(success_message), char(failure_message))
    end function assertEqualsStringsWithMessagesCSSS

    pure function assertEqualsStringsWithMessagesSCCC( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), actual, success_message, failure_message)
    end function assertEqualsStringsWithMessagesSCCC

    pure function assertEqualsStringsWithMessagesSCCS( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), actual, success_message, char(failure_message))
    end function assertEqualsStringsWithMessagesSCCS

    pure function assertEqualsStringsWithMessagesSCSC( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), actual, char(success_message), failure_message)
    end function assertEqualsStringsWithMessagesSCSC

    pure function assertEqualsStringsWithMessagesSCSS( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), actual, char(success_message), char(failure_message))
    end function assertEqualsStringsWithMessagesSCSS

    pure function assertEqualsStringsWithMessagesSSCC( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), char(actual), success_message, failure_message)
    end function assertEqualsStringsWithMessagesSSCC

    pure function assertEqualsStringsWithMessagesSSCS( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), char(actual), success_message, char(failure_message))
    end function assertEqualsStringsWithMessagesSSCS

    pure function assertEqualsStringsWithMessagesSSSC( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), char(actual), char(success_message), failure_message)
    end function assertEqualsStringsWithMessagesSSSC

    pure function assertEqualsStringsWithMessagesSSSS( &
            expected, actual, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(char(expected), char(actual), char(success_message), char(failure_message))
    end function assertEqualsStringsWithMessagesSSSS

    pure function assertEqualsWithinAbsoluteBasic( &
            expected, actual, tolerance) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute(expected, actual, tolerance, "", "")
    end function assertEqualsWithinAbsoluteBasic

    pure function assertEqualsWithinAbsoluteWithMessageC( &
            expected, actual, tolerance, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinAbsoluteWithMessageC

    pure function assertEqualsWithinAbsoluteWithMessageS( &
            expected, actual, tolerance, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, char(message), char(message))
    end function assertEqualsWithinAbsoluteWithMessageS

    pure function assertEqualsWithinAbsoluteWithMessagesCC( &
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
            result__ = succeed(withUserMessage( &
                    makeWithinSuccesMessage( &
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
    end function assertEqualsWithinAbsoluteWithMessagesCC

    pure function assertEqualsWithinAbsoluteWithMessagesCS( &
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
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, success_message, char(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesCS

    pure function assertEqualsWithinAbsoluteWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, char(success_message), failure_message)
    end function assertEqualsWithinAbsoluteWithMessagesSC

    pure function assertEqualsWithinAbsoluteWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, char(success_message), char(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesSS

    pure function assertEqualsWithinRelativeBasic( &
            expected, actual, tolerance) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative(expected, actual, tolerance, "", "")
    end function assertEqualsWithinRelativeBasic

    pure function assertEqualsWithinRelativeWithMessageC( &
            expected, actual, tolerance, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinRelativeWithMessageC

    pure function assertEqualsWithinRelativeWithMessageS( &
            expected, actual, tolerance, message) result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, char(message), char(message))
    end function assertEqualsWithinRelativeWithMessageS

    pure function assertEqualsWithinRelativeWithMessagesCC( &
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
            result__ = succeed(withUserMessage( &
                    makeWithinSuccesMessage( &
                            toString(expected), &
                            toString(actual), &
                            toString(tolerance * 100.0_dp) // "%"), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeWithinFailureMessage( &
                            toString(expected), &
                            toString(actual), &
                            toString(tolerance * 100.0_dp) // "%"), &
                    failure_message))
        end if
    end function assertEqualsWithinRelativeWithMessagesCC

    pure function assertEqualsWithinRelativeWithMessagesCS( &
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
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, success_message, char(failure_message))
    end function assertEqualsWithinRelativeWithMessagesCS

    pure function assertEqualsWithinRelativeWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, char(success_message), failure_message)
    end function assertEqualsWithinRelativeWithMessagesSC

    pure function assertEqualsWithinRelativeWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, char(success_message), char(failure_message))
    end function assertEqualsWithinRelativeWithMessagesSS

    pure function assertEqualsInteger(expected, actual) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, "", "")
    end function assertEqualsInteger

    pure function assertEqualsIntegerWithMessageC( &
            expected, actual, message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, message, message)
    end function assertEqualsIntegerWithMessageC

    pure function assertEqualsIntegerWithMessageS( &
            expected, actual, message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(message), char(message))
    end function assertEqualsIntegerWithMessageS

    pure function assertEqualsIntegerWithMessagesCC( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
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
    end function assertEqualsIntegerWithMessagesCC

    pure function assertEqualsIntegerWithMessagesCS( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, success_message, char(failure_message))
    end function assertEqualsIntegerWithMessagesCS

    pure function assertEqualsIntegerWithMessagesSC( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(success_message), failure_message)
    end function assertEqualsIntegerWithMessagesSC

    pure function assertEqualsIntegerWithMessagesSS( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, char(success_message), char(failure_message))
    end function assertEqualsIntegerWithMessagesSS

    pure function assertIncludesCC(search_for, string) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, string, "", "")
    end function assertIncludesCC

    pure function assertIncludesCS(search_for, string) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, char(string), "", "")
    end function assertIncludesCS

    pure function assertIncludesSC(search_for, string) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes(char(search_for), string, "", "")
    end function assertIncludesSC

    pure function assertIncludesSS(search_for, string) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(Result_t) :: result__

        result__ = assertIncludes(char(search_for), char(string), "", "")
    end function assertIncludesSS

    pure function assertIncludesWithMessageCCC( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, string, message, message)
    end function assertIncludesWithMessageCCC

    pure function assertIncludesWithMessageCCS( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, string, char(message), char(message))
    end function assertIncludesWithMessageCCS

    pure function assertIncludesWithMessageCSC( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, char(string), message, message)
    end function assertIncludesWithMessageCSC

    pure function assertIncludesWithMessageCSS( &
            search_for, string, message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(search_for, char(string), char(message), char(message))
    end function assertIncludesWithMessageCSS

    pure function assertIncludesWithMessageSCC( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(char(search_for), string, message, message)
    end function assertIncludesWithMessageSCC

    pure function assertIncludesWithMessageSCS( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(char(search_for), string, char(message), char(message))
    end function assertIncludesWithMessageSCS

    pure function assertIncludesWithMessageSSC( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(char(search_for), char(string), message, message)
    end function assertIncludesWithMessageSSC

    pure function assertIncludesWithMessageSSS( &
            search_for, string, message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertIncludes(char(search_for), char(string), char(message), char(message))
    end function assertIncludesWithMessageSSS

    pure function assertIncludesWithMessagesCCCC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
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
    end function assertIncludesWithMessagesCCCC

    pure function assertIncludesWithMessagesCCCS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, string, success_message, char(failure_message))
    end function assertIncludesWithMessagesCCCS

    pure function assertIncludesWithMessagesCCSC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, string, char(success_message), failure_message)
    end function assertIncludesWithMessagesCCSC

    pure function assertIncludesWithMessagesCCSS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, string, char(success_message), char(failure_message))
    end function assertIncludesWithMessagesCCSS

    pure function assertIncludesWithMessagesCSCC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, char(string), success_message, failure_message)
    end function assertIncludesWithMessagesCSCC

    pure function assertIncludesWithMessagesCSCS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, char(string), success_message, char(failure_message))
    end function assertIncludesWithMessagesCSCS

    pure function assertIncludesWithMessagesCSSC( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, char(string), char(success_message), failure_message)
    end function assertIncludesWithMessagesCSSC

    pure function assertIncludesWithMessagesCSSS( &
            search_for, string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                search_for, char(string), char(success_message), char(failure_message))
    end function assertIncludesWithMessagesCSSS

    pure function assertIncludesWithMessagesSCCC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), string, success_message, failure_message)
    end function assertIncludesWithMessagesSCCC

    pure function assertIncludesWithMessagesSCCS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), string, success_message, char(failure_message))
    end function assertIncludesWithMessagesSCCS

    pure function assertIncludesWithMessagesSCSC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), string, char(success_message), failure_message)
    end function assertIncludesWithMessagesSCSC

    pure function assertIncludesWithMessagesSCSS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), string, char(success_message), char(failure_message))
    end function assertIncludesWithMessagesSCSS

    pure function assertIncludesWithMessagesSSCC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), char(string), success_message, failure_message)
    end function assertIncludesWithMessagesSSCC

    pure function assertIncludesWithMessagesSSCS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), char(string), success_message, char(failure_message))
    end function assertIncludesWithMessagesSSCS

    pure function assertIncludesWithMessagesSSSC( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), char(string), char(success_message), failure_message)
    end function assertIncludesWithMessagesSSSC

    pure function assertIncludesWithMessagesSSSS( &
            search_for, string, success_message, failure_message) result(result__)
        type(VARYING_STRING), intent(in) :: search_for
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertIncludes( &
                char(search_for), char(string), char(success_message), char(failure_message))
    end function assertIncludesWithMessagesSSSS

    pure function assertNotBasic(condition) result(result__)
        logical, intent(in) :: condition
        type(Result_t) :: result__

        result__ = assertNot(condition, "", "")
    end function assertNotBasic

    pure function assertNotWithMessageC(condition, message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertNot(condition, message, message)
    end function assertNotWithMessageC

    pure function assertNotWithMessageS(condition, message) result(result__)
        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertNot(condition, char(message), char(message))
    end function assertNotWithMessageS

    pure function assertNotWithMessagesCC( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (.not. condition) then
            result__ = succeed(withUserMessage( &
                    NOT_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(withUserMessage( &
                    NOT_FAILURE_MESSAGE, failure_message))
        end if
    end function assertNotWithMessagesCC

    pure function assertNotWithMessagesCS( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertNot(condition, success_message, char(failure_message))
    end function assertNotWithMessagesCS

    pure function assertNotWithMessagesSC( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertNot(condition, char(success_message), failure_message)
    end function assertNotWithMessagesSC

    pure function assertNotWithMessagesSS( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertNot(condition, char(success_message), char(failure_message))
    end function assertNotWithMessagesSS

    pure function assertThatBasic(condition) result(result__)
        logical, intent(in) :: condition
        type(Result_t) :: result__

        result__ = assertThat(condition, "", "")
    end function assertThatBasic

    pure function assertThatWithMessageC(condition, message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertThat(condition, message, message)
    end function assertThatWithMessageC

    pure function assertThatWithMessageS(condition, message) result(result__)
        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertThat(condition, char(message), char(message))
    end function assertThatWithMessageS

    pure function assertThatWithMessagesCC( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        if (condition) then
            result__ = succeed(withUserMessage( &
                    THAT_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(withUserMessage( &
                    THAT_FAILURE_MESSAGE, failure_message))
        end if
    end function assertThatWithMessagesCC

    pure function assertThatWithMessagesCS( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertThat(condition, success_message, char(failure_message))
    end function assertThatWithMessagesCS

    pure function assertThatWithMessagesSC( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertThat(condition, char(success_message), failure_message)
    end function assertThatWithMessagesSC

    pure function assertThatWithMessagesSS( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertThat(condition, char(success_message), char(failure_message))
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

    pure function coverEmptyDecimal(number) result(fixed)
        character(len=*), intent(in) :: number
        type(VARYING_STRING) :: fixed

        if (lastCharacter(trim(number)) == ".") then
            fixed = trim(number) // "0"
        else if (firstCharacter(trim(number)) == ".") then
            fixed = "0" // trim(number)
        else
            fixed = trim(number)
        end if
    end function coverEmptyDecimal

    pure function delimit(string) result(delimited)
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: delimited

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

    pure function doublePrecisionToString(number) result(string)
        double precision, intent(in) :: number
        type(VARYING_STRING) :: string

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
    end function doublePrecisionToString

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

    pure function failC(message) result(failure)
        character(len=*), intent(in) :: message
        type(Result_t) :: failure

        failure = Result_(IndividualResult(message, .false.))
    end function failC

    pure function failS(message) result(failure)
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: failure

        failure = Result_(IndividualResult(char(message), .false.))
    end function failS

    pure function filterInputTestCase(self, filter_string) result(maybe)
        class(InputTestCase_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        if (self%description_.includes.filter_string) then
            allocate(maybe, source = Just(self))
        else
            allocate(maybe, source = NOTHING)
        end if
    end function filterInputTestCase

    pure function filterTestCase(self, filter_string) result(maybe)
        class(TestCase_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        if (self%description_.includes.filter_string) then
            allocate(maybe, source = Just(self))
        else
            allocate(maybe, source = NOTHING)
        end if
    end function filterTestCase

    pure function filterTestCaseWithExamples(self, filter_string) result(maybe)
        class(TestCaseWithExamples_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        if (self%description_.includes.filter_string) then
            allocate(maybe, source = Just(self))
        else
            allocate(maybe, source = NOTHING)
        end if
    end function filterTestCaseWithExamples

    pure function filterTestCaseWithGenerator(self, filter_string) result(maybe)
        class(TestCaseWithGenerator_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        if (self%description_.includes.filter_string) then
            allocate(maybe, source = Just(self))
        else
            allocate(maybe, source = NOTHING)
        end if
    end function filterTestCaseWithGenerator

    pure function filterTestCollection(self, filter_string) result(maybe)
        class(TestCollection_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        type(MaybeItem_t), allocatable :: filtered_tests(:)
        type(TestCollection_t) :: new_collection
        integer :: num_input_tests
        logical, allocatable :: passed_filter(:)

        if (self%description_.includes.filter_string) then
            allocate(maybe, source = Just(self))
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
                allocate(maybe, source = Just(new_collection))
            else
                allocate(maybe, source = NOTHING)
            end if
        end if
    end function filterTestCollection

    pure function filterTestCollectionWithInput(self, filter_string) result(maybe)
        class(TestCollectionWithInput_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        type(MaybeItem_t), allocatable :: filtered_tests(:)
        type(TestCollectionWithInput_t) :: new_collection
        integer :: num_input_tests
        logical, allocatable :: passed_filter(:)

        if (self%description_.includes.filter_string) then
            allocate(maybe, source = Just(self))
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
                allocate(maybe, source = Just(new_collection))
            else
                allocate(maybe, source = NOTHING)
            end if
        end if
    end function filterTestCollectionWithInput

    elemental function filterTestItem(self, filter_string) result(maybe)
        class(TestItem_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        type(MaybeItem_t) :: maybe

        class(Maybe_t), allocatable :: filtered
        type(TestItem_t) :: test_item

        select type (test => self%test)
        type is (InputTestCase_t)
            allocate(filtered, source = test%filter(filter_string))
        type is (TestCase_t)
            allocate(filtered, source = test%filter(filter_string))
        type is (TestCaseWithExamples_t)
            allocate(filtered, source = test%filter(filter_string))
        type is (TestCaseWithGenerator_t)
            allocate(filtered, source = test%filter(filter_string))
        type is (TestCollection_t)
            allocate(filtered, source = test%filter(filter_string))
        type is (TestCollectionWithInput_t)
            allocate(filtered, source = test%filter(filter_string))
        type is (TransformingTestCollection_t)
            allocate(filtered, source = test%filter(filter_string))
        end select

        select type (filtered)
        type is (JustInputTestCase_t)
            allocate(test_item%test, source = filtered%getValue())
            allocate(maybe%maybe, source = Just(test_item))
        type is (JustTestCase_t)
            allocate(test_item%test, source = filtered%getValue())
            allocate(maybe%maybe, source = Just(test_item))
        type is (JustTestCaseWithExamples_t)
            allocate(test_item%test, source = filtered%getValue())
            allocate(maybe%maybe, source = Just(test_item))
        type is (JustTestCaseWithGenerator_t)
            allocate(test_item%test, source = filtered%getValue())
            allocate(maybe%maybe, source = Just(test_item))
        type is (JustTestCollection_t)
            allocate(test_item%test, source = filtered%getValue())
            allocate(maybe%maybe, source = Just(test_item))
        type is (JustTestCollectionWithInput_t)
            allocate(test_item%test, source = filtered%getValue())
            allocate(maybe%maybe, source = Just(test_item))
        type is (JustTransformingTestCollection_t)
            allocate(test_item%test, source = filtered%getValue())
            allocate(maybe%maybe, source = Just(test_item))
        type is (Nothing_t)
            allocate(maybe%maybe, source = NOTHING)
        end select
    end function filterTestItem

    pure function filterTransformingTestCollection(self, filter_string) result(maybe)
        class(TransformingTestCollection_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: filter_string
        class(Maybe_t), allocatable :: maybe

        type(MaybeItem_t), allocatable :: filtered_tests(:)
        type(TransformingTestCollection_t) :: new_collection
        integer :: num_input_tests
        logical, allocatable :: passed_filter(:)

        if (self%description_.includes.filter_string) then
            allocate(maybe, source = Just(self))
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
                allocate(maybe, source = Just(new_collection))
            else
                allocate(maybe, source = NOTHING)
            end if
        end if
    end function filterTransformingTestCollection

    pure function firstCharacter(string) result(first)
        character(len=*), intent(in) :: string
        character(len=1) :: first

        type(VARYING_STRING) :: trimmed

        trimmed = trim(string)
        first = extract(trimmed, 1, 1)
    end function firstCharacter

    pure function Generated(value_)
        class(*), intent(in) :: value_
        type(Generated_t) :: Generated

        select type (value_)
        type is (character(len=*))
            allocate(Generated%value_, source = VString(value_))
        class default
            allocate(Generated%value_, source = value_)
        end select
    end function Generated

    function generateAsciiCharacters(self) result(generated_value)
        class(AsciiCharactersGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        associate(a => self)
        end associate
        generated_value = Generated(getRandomAsciiCharacters())
    end function generateAsciiCharacters

    function generateAsciiString(self) result(generated_value)
        class(AsciiStringGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        associate(a => self)
        end associate
        generated_value = Generated(getRandomAsciiString())
    end function generateAsciiString

    function generateInteger(self) result(generated_value)
        class(IntegerGenerator_t), intent(in) :: self
        type(Generated_t) :: generated_value

        associate(a => self)
        end associate
        generated_value = Generated(getRandomInteger())
    end function generateInteger

    function getOptions() result(options)
        use iso_fortran_env, only: error_unit, output_unit

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

    function getRandomAsciiCharacters() result(random_string)
        character(len=:), allocatable :: random_string

        random_string = getRandomAsciiCharactersWithMaxLength(1024)
    end function getRandomAsciiCharacters

    function getRandomAsciiCharactersWithMaxLength(max_length) result(random_string)
        integer, intent(in) :: max_length
        character(len=:), allocatable :: random_string

        integer :: i
        integer :: num_characters

        num_characters = getRandomIntegerWithRange(0, max_length)
        allocate(character(len=num_characters) :: random_string)
        do i = 1, num_characters
            random_string(i:i) = getRandomAsciiCharacter()
        end do
    end function getRandomAsciiCharactersWithMaxLength

    function getRandomAsciiString() result(random_string)
        type(VARYING_STRING) :: random_string

        random_string = getRandomAsciiStringWithMaxLength(1024)
    end function getRandomAsciiString

    function getRandomAsciiStringWithMaxLength(max_length) result(random_string)
        integer, intent(in) :: max_length
        type(VARYING_STRING) :: random_string

        random_string = getRandomAsciiCharactersWithMaxLength(max_length)
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

    pure function getValueTestCaseWithGenerator(just_) result(value_)
        class(JustTestCaseWithGenerator_t), intent(in) :: just_
        type(TestCaseWithGenerator_t) :: value_

        value_ = just_%value_
    end function getValueTestCaseWithGenerator

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

    pure function hangingIndentC(string__, spaces) result(indented)
        character(len=*), intent(in) :: string__
        integer, intent(in) :: spaces
        type(VARYING_STRING) :: indented

        type(VARYING_STRING), allocatable :: lines(:)

        allocate(lines, source = splitAt(string__, NEWLINE))
        indented = join(lines, NEWLINE // repeat(" ", spaces))
    end function hangingIndentC

    pure function hangingIndentS(string__, spaces) result(indented)
        type(VARYING_STRING), intent(in) :: string__
        integer, intent(in) :: spaces
        type(VARYING_STRING) :: indented

        type(VARYING_STRING), allocatable :: lines(:)

        allocate(lines, source = splitAt(string__, NEWLINE))
        indented = join(lines, NEWLINE // repeat(" ", spaces))
    end function hangingIndentS

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

    pure function includesCC(string, search_for) result(includes)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: search_for
        logical :: includes

        includes = index(string, search_for) > 0
    end function includesCC

    ! pure function includesCS(string, search_for) result(includes)
    !     character(len=*), intent(in) :: string
    !     type(VARYING_STRING), intent(in) :: search_for
    !     logical :: includes
    !
    !     includes = index(string, search_for) > 0
    ! end function includesCS

    ! pure function includesSC(string, search_for) result(includes)
    !     type(VARYING_STRING), intent(in) :: string
    !     character(len=*), intent(in) :: search_for
    !     logical :: includes
    !
    !     includes = index(string, search_for) > 0
    ! end function includesSC

    pure function includesSS(string, search_for) result(includes)
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: search_for
        logical :: includes

        includes = index(string, search_for) > 0
    end function includesSS

    ! pure function indentC(string, spaces) result(indented)
    !     character(len=*), intent(in) :: string
    !     integer, intent(in) :: spaces
    !     type(VARYING_STRING) :: indented
    !
    !     indented = repeat(" ", spaces) // hangingIndent(string, spaces)
    ! end function indentC

    pure function indentS(string, spaces) result(indented)
        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: spaces
        type(VARYING_STRING) :: indented

        indented = repeat(" ", spaces) // hangingIndent(string, spaces)
    end function indentS

    pure function IndividualResult(message, passed_)
        character(len=*), intent(in) :: message
        logical, intent(in) :: passed_
        type(IndividualResult_t) :: IndividualResult

        IndividualResult%message = message
        IndividualResult%passed_ = passed_
    end function IndividualResult

    pure function individualResultFailureDescription(self, colorize) result(description)
        class(IndividualResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        if (colorize) then
            description = char(27) // "[31m" // self%message // char(27) // "[0m"
        else
            description = self%message
        end if
    end function individualResultFailureDescription

    pure function individualResultVerboseDescription(self, colorize) result(description)
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

    function InputTestCase(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(inputTest) :: func
        type(InputTestCase_t) :: test_case

        test_case%description_ = description
        test_case%test => func
    end function InputTestCase

    pure function inputTestCaseDescription(self) result(description)
        class(InputTestCase_t), intent(in) :: self
        type(VARYING_STRING) :: description

        description = self%description_
    end function inputTestCaseDescription

    pure function inputTestCaseNumCases(self) result(num_cases)
        class(InputTestCase_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function inputTestCaseNumCases

    pure function integerToString(int) result(string)
        integer, intent(in) :: int
        type(VARYING_STRING) :: string

        character(len=12) :: temp

        write(temp, '(I0)') int
        string = trim(temp)
    end function integerToString

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

    function itWithGenerator(description, generator, func) result(test_case)
        character(len=*), intent(in) :: description
        class(Generator_t), intent(in) :: generator
        procedure(inputTest) :: func
        type(TestItem_t) :: test_case

        allocate(TestCaseWithGenerator_t :: test_case%test)
        select type (test => test_case%test)
        type is (TestCaseWithGenerator_t)
            test = TestCaseWithGenerator(description, generator, func)
        end select
    end function itWithGenerator

    pure function joinC(strings_, separator) result(string)
        type(VARYING_STRING), intent(in) :: strings_(:)
        character(len=*), intent(in) :: separator
        type(VARYING_STRING) :: string

        integer :: i

        string = strings_(1)
        do i = 2, size(strings_)
            string = string // separator // strings_(i)
        end do
    end function joinC

    ! pure function joinS(strings_, separator) result(string)
    !     type(VARYING_STRING), intent(in) :: strings_(:)
    !     type(VARYING_STRING), intent(in) :: separator
    !     type(VARYING_STRING) :: string
    !
    !     string = join(strings_, char(separator))
    ! end function joinS

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

    pure function JustTestCaseWithGenerator(value_) result(just_)
        type(TestCaseWithGenerator_t), intent(in) :: value_
        type(JustTestCaseWithGenerator_t) :: just_

        just_ = JustTestCaseWithGenerator_t(value_)
    end function JustTestCaseWithGenerator

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

    pure function lastCharacterC(string) result(char_)
        character(len=*), intent(in) :: string
        character(len=1) :: char_

        integer :: length

        length = len(trim(string))
        char_ = string(length:length)
    end function lastCharacterC

    pure function lastCharacterS(string) result(char_)
        type(VARYING_STRING), intent(in) :: string
        character(len=1) :: char_

        char_ = lastCharacter(char(string))
    end function lastCharacterS

    pure function makeDoesntIncludeFailureMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
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
    end function makeDoesntIncludeFailureMessage

    pure function makeDoesntIncludeSuccessMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
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
    end function makeDoesntIncludeSuccessMessage

    pure function makeEmptyFailureMessage(string) result(message)
        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "The string" // NEWLINE &
                // indent( &
                        delimit(hangingIndent(string, 1)), &
                        INDENTATION) // NEWLINE &
                // "wasn't empty", &
                INDENTATION)
    end function makeEmptyFailureMessage

    pure function makeEqualsFailureMessage(expected, actual) result(message)
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
    end function makeEqualsFailureMessage

    pure function makeEqualsSuccessMessage(expected) result(message)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING) :: message

        message = hangingIndent( &
                "Expected and got" // NEWLINE &
                // indent( &
                        delimit(hangingIndent(expected, 1)), &
                        INDENTATION), &
                INDENTATION)
    end function makeEqualsSuccessMessage

    pure function makeIncludesFailureMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
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
    end function makeIncludesFailureMessage

    pure function makeIncludesSuccessMessage(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
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
    end function makeIncludesSuccessMessage

    pure function makeWithinFailureMessage( &
            expected, actual, tolerance) result(message)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = &
                "Expected " // delimit(actual) // " to be  within " &
                // delimit("±" // tolerance) // " of " // delimit(expected)
    end function makeWithinFailureMessage

    pure function makeWithinSuccesMessage( &
            expected, actual, tolerance) result(message)
        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: tolerance
        type(VARYING_STRING) :: message

        message = &
                delimit(actual) // " was within " // delimit("±" // tolerance) &
                // " of " // delimit(expected)
    end function makeWithinSuccesMessage

    pure function removeTrailingZeros(number) result(trimmed)
        character(len=*), intent(in) :: number
        type(VARYING_STRING) :: trimmed

        trimmed = trim(number)
        do while (lastCharacter(trimmed) == "0")
            trimmed = withoutLastCharacter(trimmed)
        end do
    end function removeTrailingZeros

    pure function Result_(individual_result)
        type(IndividualResult_t), intent(in) :: individual_result
        type(Result_t) :: Result_

        allocate(Result_%results(1))
        Result_%results(1) = individual_result
    end function Result_

    pure function resultFailureDescription(self, colorize) result(description)
        class(Result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        logical, allocatable :: failed_mask(:)
        type(IndividualResult_t), allocatable :: failed_results(:)
        integer :: i
        type(VARYING_STRING), allocatable :: individual_descriptions(:)

        allocate(failed_mask(size(self%results)))
        failed_mask = .not.self%results%passed_
        allocate(failed_results(count(failed_mask)))
        failed_results = pack(self%results, mask=failed_mask)
        allocate(individual_descriptions(size(failed_results)))
        do i = 1, size(failed_results)
            individual_descriptions(i) = failed_results(i)%failureDescription(colorize)
        end do
        if (size(failed_results) > 0) then
            description = join(individual_descriptions, NEWLINE)
        else
            description = ""
        end if
    end function resultFailureDescription

    pure function resultNumAsserts(self) result(num_asserts)
        class(Result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = size(self%results)
    end function resultNumAsserts

    pure function resultNumFailing(self) result(num_asserts)
        class(Result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = count(.not.self%results%passed_)
    end function resultNumFailing

    pure function resultPassed(self) result(passed)
        class(Result_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed_)
    end function resultPassed

    pure function resultVerboseDescription(self, colorize) result(description)
        class(Result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        integer :: i
        type(VARYING_STRING), allocatable :: individual_descriptions(:)

        allocate(individual_descriptions(size(self%results)))
        do i = 1, size(self%results)
            individual_descriptions(i) = self%results(i)%verboseDescription(colorize)
        end do
        description = join(individual_descriptions, NEWLINE)
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

    function runCaseWithGenerator(self) result(result__)
        class(TestCaseWithGenerator_t), intent(in) :: self
        type(TestCaseResult_t) :: result__

        type(Generated_t) :: generated_value
        integer :: i
        class(ShrinkResult_t), allocatable :: simpler_value
        type(Result_t) :: new_result
        type(Result_t) :: previous_result

        do i = 1, NUM_GENERATOR_TESTS
            generated_value = self%generator%generate()
            select type (the_value => generated_value%value_)
            type is (VegetableString_t)
                previous_result = self%test(the_value%string)
            class default
                previous_result = self%test(the_value)
            end select
            if (.NOT.previous_result%passed()) exit
        end do
        if (i > NUM_GENERATOR_TESTS) then
            result__ = TestCaseResult( &
                    self%description_, &
                    succeed("Passed after " // toString(NUM_GENERATOR_TESTS) // " examples"))
        else
            do
                select type (the_value => generated_value%value_)
                type is (VegetableString_t)
                    allocate(simpler_value, source = self%generator%shrink(the_value%string))
                class default
                    allocate(simpler_value, source = self%generator%shrink(the_value))
                end select
                select type (simpler_value)
                type is (ShrunkValue_t)
                    select type (the_value => simpler_value%value_)
                    type is (VegetableString_t)
                        new_result = self%test(the_value%string)
                    class default
                        new_result = self%test(the_value)
                    end select
                    if (new_result%passed()) then
                        result__ = TestCaseResult( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result)
                        return
                    else
                        previous_result = new_result
                        generated_value = Generated(simpler_value%value_)
                    end if
                type is (SimplestValue_t)
                    select type (the_value => simpler_value%value_)
                    type is (VegetableString_t)
                        new_result = self%test(the_value%string)
                    class default
                        new_result = self%test(the_value)
                    end select
                    if (new_result%passed()) then
                        result__ = TestCaseResult( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result)
                        return
                    else
                        result__ = TestCaseResult( &
                                self%description_, &
                                fail('Fails with the simplest possible example').and.new_result)
                        return
                    end if
                class default
                    result__ = TestCaseResult( &
                            self%description_, &
                            fail("Got an unknown type when trying to shrink last value").and.previous_result)
                    return
                end select
                deallocate(simpler_value)
            end do
        end if
    end function runCaseWithGenerator

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
            allocate( &
                    result_item%result_, &
                    source = TestCaseResult( &
                            test%description_, fail("No input provided")))
        type is (TestCase_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCaseWithExamples_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCaseWithGenerator_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCollection_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCollectionWithInput_t)
            allocate(result_item%result_, source = test%run())
        type is (TransformingTestCollection_t)
            allocate( &
                    result_item%result_, &
                    source = TestCaseResult( &
                            test%description_, fail("No input provided")))
        end select
    end function runTestItem

    function runTestItemWithInput(self, input) result(result_item)
        class(TestItem_t), intent(in) :: self
        class(*), intent(in) :: input
        type(TestResultItem_t) :: result_item

        select type (test => self%test)
        type is (InputTestCase_t)
            allocate(result_item%result_, source = test%run(input))
        type is (TestCase_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCaseWithExamples_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCaseWithGenerator_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCollection_t)
            allocate(result_item%result_, source = test%run())
        type is (TestCollectionWithInput_t)
            allocate(result_item%result_, source = test%run())
        type is (TransformingTestCollection_t)
            allocate(result_item%result_, source = test%run(input))
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
                call put_line(error_unit, "No matching tests found")
                call exit(1)
            end select
        else
            tests_to_run = tests
        end if
        call put_line(output_unit, "Running Tests")
        call put_line(output_unit, "")
        if (.not.options%quiet) then
            call put_line(output_unit, tests_to_run%description())
            call put_line(output_unit, "")
        end if
        call put_line( &
                output_unit, &
                "A total of " // toString(tests_to_run%numCases()) // " test cases")
        results = tests_to_run%run()
        if (results%passed()) then
            call put_line(output_unit, "")
            call put_line(output_unit, "All Passed")
            if (options%verbose) then
                call put_line( &
                        output_unit, results%verboseDescription(options%colorize))
            end if
            call put_line( &
                    output_unit, &
                    "A total of " // toString(results%numCases()) &
                    // " test cases containg a total of " &
                    // toString(results%numAsserts()) // " assertions")
            call put_line(output_unit, "")
        else
            call put_line(error_unit, "")
            call put_line(error_unit, "Failed")
            if (options%verbose) then
                call put_line( &
                        error_unit, results%verboseDescription(options%colorize))
            else
                call put_line( &
                        error_unit, results%failureDescription(options%colorize))
            end if
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

    pure function shrinkAsciiCharacters(value_) result(shrunk)
        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        select type (value_)
        type is (character(len=*))
            if (len(value_) <= 1) then
                allocate(shrunk, source = SimplestValue(""))
            else
                allocate(shrunk, source = ShrunkValue(value_(1:len(value_)-1)))
            end if
        end select
    end function shrinkAsciiCharacters

    pure function shrinkAsciiString(value_) result(shrunk)
        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        select type (value_)
        type is (VARYING_STRING)
            if (len(value_) <= 1) then
                allocate(shrunk, source = SimplestValue(var_str("")))
            else
                allocate(shrunk, source = ShrunkValue(extract(value_, 1, len(value_)-1)))
            end if
        end select
    end function shrinkAsciiString

    pure function shrinkInteger(value_) result(shrunk)
        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        select type (value_)
        type is (integer)
            if (value_ == 0) then
                allocate(shrunk, source = SimplestValue(0))
            else
                allocate(shrunk, source = ShrunkValue(value_/2))
            end if
        end select
    end function

    pure function ShrunkValue(value_)
        class(*), intent(in) :: value_
        type(ShrunkValue_t) :: ShrunkValue

        select type (value_)
        type is (character(len=*))
            allocate(ShrunkValue%value_, source = vString(value_))
        class default
            allocate(ShrunkValue%value_, source = value_)
        end select
    end function ShrunkValue

    pure function SimplestValue(value_)
        class(*), intent(in) :: value_
        type(SimplestValue_t) :: SimplestValue

        select type (value_)
        type is (character(len=*))
            allocate(SimplestValue%value_, source = VString(value_))
        class default
            allocate(SimplestValue%value_, source = value_)
        end select
    end function SimplestValue

    pure recursive function splitAtCC(&
            string_, split_characters) result(strings)
        character(len=*), intent(in) :: string_
        character(len=*), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

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
                strings(1) = ""
            end if
        else
            allocate(strings(1))
            strings(1) = string_
        end if
    contains
        pure function doSplit(string__, split_characters_) result(strings_)
            character(len=*), intent(in) :: string__
            character(len=*), intent(in) :: split_characters_
            type(VARYING_STRING), allocatable :: strings_(:)

            integer :: i
            type(VARYING_STRING), allocatable :: rest(:)
            type(VARYING_STRING) :: this_string

            do i = 2, len(string__)
                if (index(split_characters_, string__(i:i)) > 0) exit
            end do
            if (i < len(string__)) then
                this_string = string__(1:i - 1)
                allocate(rest, source = splitAt(string__(i + 1:), split_characters_))
                allocate(strings_(size(rest) + 1))
                strings_(1) = this_string
                strings_(2:) = rest(:)
            else
                allocate(strings_(1))
                strings_(1) = string__
            end if
        end function doSplit
    end function splitAtCC

    pure function splitAtSC(string_, split_characters) result(strings)
        type(VARYING_STRING), intent(in) :: string_
        character(len=*), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(char(string_), split_characters))
    end function splitAtSC

    pure function succeedC(message) result(success)
        character(len=*), intent(in) :: message
        type(Result_t) :: success

        success = Result_(IndividualResult(message, .true.))
    end function succeedC

    pure function succeedS(message) result(success)
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: success

        success = Result_(IndividualResult(char(message), .true.))
    end function succeedS

    function TestCase(description, func) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: func
        type(TestCase_t) :: test_case

        test_case%description_ = description
        test_case%test => func
    end function TestCase

    pure function testCaseDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        type(VARYING_STRING) :: description

        description = self%description_
    end function testCaseDescription

    pure function testCaseFailureDescription(self, colorize) result(description)
        class(TestCaseResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        if (self%passed()) then
            description = ""
        else
            description = hangingIndent( &
                    self%description // NEWLINE // self%result_%failureDescription(colorize), &
                    INDENTATION)
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

    pure function TestCaseResultC(description, result__) result(test_case_result)
        character(len=*), intent(in) :: description
        type(Result_t), intent(in) :: result__
        type(TestCaseResult_t) :: test_case_result

        test_case_result%description = description
        test_case_result%result_ = result__
    end function TestCaseResultC

    pure function TestCaseResultS(description, result__) result(test_case_result)
        type(VARYING_STRING), intent(in) :: description
        type(Result_t), intent(in) :: result__
        type(TestCaseResult_t) :: test_case_result

        test_case_result%description = description
        test_case_result%result_ = result__
    end function TestCaseResultS

    pure function testCaseResultNumCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function testCaseResultNumCases

    pure function testCaseVerboseDescription(self, colorize) result(description)
        class(TestCaseResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        description = hangingIndent( &
                self%description // NEWLINE // self%result_%verboseDescription(colorize), &
                INDENTATION)
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

    function TestCaseWithGenerator(description, generator, func) result(test_case)
        character(len=*), intent(in) :: description
        class(Generator_t), intent(in) :: generator
        procedure(inputTest) :: func
        type(TestCaseWithGenerator_t) :: test_case

        test_case%description_ = description
        allocate(test_case%generator, source = generator)
        test_case%test => func
    end function TestCaseWithGenerator

    pure function testCaseWithExamplesDescription(self) result(description)
        class(TestCaseWithExamples_t), intent(in) :: self
        type(VARYING_STRING) :: description

        description = self%description_
    end function testCaseWithExamplesDescription

    pure function testCaseWithExamplesNumCases(self) result(num_cases)
        class(TestCaseWithExamples_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function testCaseWithExamplesNumCases

    pure function testCaseWithGeneratorDescription(self) result(description)
        class(TestCaseWithGenerator_t), intent(in) :: self
        type(VARYING_STRING) :: description

        description = self%description_
    end function testCaseWithGeneratorDescription

    pure function testCaseWithGeneratorNumCases(self) result(num_cases)
        class(TestCaseWithGenerator_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate
        num_cases = 1
    end function testCaseWithGeneratorNumCases

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
        type(VARYING_STRING) :: description

        type(VARYING_STRING), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%tests)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = self%tests(i)%description()
        end do
        description = hangingIndent( &
                self%description_ // NEWLINE // join(descriptions, NEWLINE), &
                INDENTATION)
    end function testCollectionDescription

    pure function testCollectionFailureDescription(self, colorize) result(description)
        class(TestCollectionResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        type(VARYING_STRING), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        if (self%passed()) then
            description = ""
        else
            num_cases = size(self%results)
            allocate(descriptions(num_cases))
            do concurrent (i = 1:num_cases)
                descriptions(i) = self%results(i)%failureDescription(colorize)
            end do
            description = hangingIndent( &
                    self%description // NEWLINE // join(descriptions, NEWLINE), &
                    INDENTATION)
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

    ! pure function TestCollectionResultC(description, results) result(test_collection_result)
    !     character(len=*), intent(in) :: description
    !     type(TestResultItem_t), intent(in) :: results(:)
    !     type(TestCollectionResult_t) :: test_collection_result
    !
    !     test_collection_result%description = description
    !     allocate(test_collection_result%results(size(results)))
    !     test_collection_result%results = results
    ! end function TestCollectionResultC

    pure function TestCollectionResultS(description, results) result(test_collection_result)
        type(VARYING_STRING), intent(in) :: description
        type(TestResultItem_t), intent(in) :: results(:)
        type(TestCollectionResult_t) :: test_collection_result

        test_collection_result%description = description
        allocate(test_collection_result%results(size(results)))
        test_collection_result%results = results
    end function TestCollectionResultS

    pure function testCollectionResultNumCases(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%results%numCases())
    end function testCollectionResultNumCases

    pure function testCollectionVerboseDescription(self, colorize) result(description)
        class(TestCollectionResult_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        type(VARYING_STRING), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%results)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = self%results(i)%verboseDescription(colorize)
        end do
        description = hangingIndent( &
                self%description // NEWLINE // join(descriptions, NEWLINE), &
                INDENTATION)
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
        type(VARYING_STRING) :: description

        type(VARYING_STRING), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%tests)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = self%tests(i)%description()
        end do
        description = hangingIndent( &
                self%description_ // NEWLINE // join(descriptions, NEWLINE), &
                INDENTATION)
    end function testCollectionWithInputDescription

    pure function testCollectionWithInputNumCases(self) result(num_cases)
        class(TestCollectionWithInput_t), intent(in) :: self
        integer :: num_cases

        num_cases = sum(self%tests%numCases())
    end function testCollectionWithInputNumCases

    pure function testItemDescription(self) result(description)
        class(TestItem_t), intent(in) :: self
        type(VARYING_STRING) :: description

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

    pure function testResultItemFailureDescription(self, colorize) result(description)
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

    pure function testResultItemVerboseDescription(self, colorize) result(description)
        class(TestResultItem_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(VARYING_STRING) :: description

        description = self%result_%verboseDescription(colorize)
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

    pure function VString(chars) result(string)
        character(len=*), intent(in) :: chars
        type(VegetableString_t) :: string

        string%string = chars
    end function VString

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
        type(VARYING_STRING) :: description

        type(VARYING_STRING), allocatable :: descriptions(:)
        integer :: i
        integer :: num_cases

        num_cases = size(self%tests)
        allocate(descriptions(num_cases))
        do concurrent (i = 1:num_cases)
            descriptions(i) = self%tests(i)%description()
        end do
        description = hangingIndent( &
                self%description_ // NEWLINE // join(descriptions, NEWLINE), &
                INDENTATION)
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
        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: withoutLastCharacter

        type(VARYING_STRING) :: trimmed

        trimmed = trim(string)
        withoutLastCharacter = extract(trimmed, 1, len(trimmed)-1)
    end function withoutLastCharacter

    pure function withUserMessageCC(message, user_message) result(whole_message)
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        if (user_message == "") then
            whole_message = message
        else
            whole_message = &
                    message // NEWLINE &
                    // indent( &
                            hangingIndent( &
                                    "User Message:" // NEWLINE &
                                    // delimit(hangingIndent(user_message, 1)), &
                                    INDENTATION), &
                            INDENTATION)
        end if
    end function withUserMessageCC

    pure function withUserMessageCS(message, user_message) result(whole_message)
        character(len=*), intent(in) :: message
        type(VARYING_STRING), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        whole_message = withUserMessage(message, char(user_message))
    end function withUserMessageCS

    pure function withUserMessageSC(message, user_message) result(whole_message)
        type(VARYING_STRING), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        whole_message = withUserMessage(char(message), user_message)
    end function withUserMessageSC

    pure function withUserMessageSS(message, user_message) result(whole_message)
        type(VARYING_STRING), intent(in) :: message
        type(VARYING_STRING), intent(in) :: user_message
        type(VARYING_STRING) :: whole_message

        whole_message = withUserMessage(char(message), char(user_message))
    end function withUserMessageSS
end module Vegetables_m
