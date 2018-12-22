module assert_equals_strings_test
    implicit none
    private

    public :: &
            test_assert_equals_strings, &
            test_equal_with_newlines, &
            test_not_equal_with_newlines
contains
    function test_assert_equals_strings() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertEquals with strings", &
                [it("passes with the same strings", checkPassForSameStrings), &
                it("fails with different strings", checkFailForDifferentStrings)])
    end function test_assert_equals_strings

    function test_equal_with_newlines() result(tests)
        use Vegetables_m, only: TestItem_t, Given, Then, When

        type(TestItem_t) :: tests

        tests = Given("a string with newlines", &
                [When("it is assertEquals with itself", &
                        [Then("the messages don't contain newlines", checkEqualStringsNoNewlines)])])
    end function test_equal_with_newlines

    function test_not_equal_with_newlines() result(tests)
        use Vegetables_m, only: TestItem_t, Given, Then, When

        type(TestItem_t) :: tests

        tests = Given("different strings with newlines", &
                [When("they are assertEquals", &
                        [Then("the messages don't contain newlines", checkNotEqualStringsNoNewlines)])])
    end function test_not_equal_with_newlines

    function checkPassForSameStrings() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertThat, toString

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE_STRING = "Example String"
        type(Result_t) :: example_result_chars
        type(Result_t) :: example_result_mix1
        type(Result_t) :: example_result_mix2
        type(Result_t) :: example_result_strings

        example_result_chars = assertEquals(EXAMPLE_STRING, EXAMPLE_STRING)
        example_result_mix1 = assertEquals(toString(EXAMPLE_STRING), EXAMPLE_STRING)
        example_result_mix2 = assertEquals(EXAMPLE_STRING, toString(EXAMPLE_STRING))
        example_result_strings = assertEquals(toString(EXAMPLE_STRING), toString(EXAMPLE_STRING))

        result_ = &
                assertThat(example_result_chars%passed()) &
                .and.assertThat(example_result_mix1%passed()) &
                .and.assertThat(example_result_mix2%passed()) &
                .and.assertThat(example_result_strings%passed())
    end function checkPassForSameStrings

    function checkFailForDifferentStrings() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertNot, toString

        type(Result_t) :: result_

        character(len=*), parameter :: ONE_STRNIG = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
        type(Result_t) :: example_result_chars
        type(Result_t) :: example_result_mix1
        type(Result_t) :: example_result_mix2
        type(Result_t) :: example_result_strings

        example_result_chars = assertEquals(ONE_STRNIG, OTHER_STRING)
        example_result_mix1 = assertEquals(toString(ONE_STRNIG), OTHER_STRING)
        example_result_mix2 = assertEquals(ONE_STRNIG, toString(OTHER_STRING))
        example_result_strings = assertEquals(toString(ONE_STRNIG), toString(OTHER_STRING))

        result_ = &
                assertNot(example_result_chars%passed()) &
                .and.assertNot(example_result_mix1%passed()) &
                .and.assertNot(example_result_mix2%passed()) &
                .and.assertNot(example_result_strings%passed())
    end function checkFailForDifferentStrings

    function checkEqualStringsNoNewlines() result(result_)
        use Vegetables_m, only: &
                Result_t, assertDoesntInclude, assertEquals, toString

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: EXAMPLE_STRING = &
                "Example" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_result_chars
        type(Result_t) :: example_result_mix1
        type(Result_t) :: example_result_mix2
        type(Result_t) :: example_result_strings

        example_result_chars = assertEquals(EXAMPLE_STRING, EXAMPLE_STRING)
        example_result_mix1 = assertEquals(toString(EXAMPLE_STRING), EXAMPLE_STRING)
        example_result_mix2 = assertEquals(EXAMPLE_STRING, toString(EXAMPLE_STRING))
        example_result_strings = assertEquals(toString(EXAMPLE_STRING), toString(EXAMPLE_STRING))

        result_ = &
                assertDoesntInclude(NEWLINE, example_result_chars%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_chars%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix1%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix1%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix2%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix2%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_strings%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_strings%verboseDescription())
    end function checkEqualStringsNoNewlines

    function checkNotEqualStringsNoNewlines() result(result_)
        use Vegetables_m, only: &
                Result_t, assertDoesntInclude, assertEquals, toString

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: ONE_STRING = &
                "One" // NEWLINE // "With" // NEWLINE // "Newlines"
        character(len=*), parameter :: OTHER_STRING = &
                "Other" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_result_chars
        type(Result_t) :: example_result_mix1
        type(Result_t) :: example_result_mix2
        type(Result_t) :: example_result_strings

        example_result_chars = assertEquals(ONE_STRING, OTHER_STRING)
        example_result_mix1 = assertEquals(toString(ONE_STRING), OTHER_STRING)
        example_result_mix2 = assertEquals(ONE_STRING, toString(OTHER_STRING))
        example_result_strings = assertEquals(toString(ONE_STRING), toString(OTHER_STRING))

        result_ = &
                assertDoesntInclude(NEWLINE, example_result_chars%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_chars%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix1%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix1%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix2%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_mix2%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_strings%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result_strings%verboseDescription())
    end function checkNOTEqualStringsNoNewlines
end module assert_equals_strings_test
