module assert_empty_test
    implicit none
    private

    public :: test_assert_empty, test_with_newlines
contains
    function test_assert_empty() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertEmpty", &
                [it("passes with an empty character", checkPassForEmptyChars), &
                it("passes with an empty string", checkPassForEmptyString), &
                it("fails with a non empty character", checkFailsForNonemptyChars), &
                it("fails with a non empty string", checkFailsForNonemptyString)])
    end function test_assert_empty

    function test_with_newlines() result(tests)
        use Vegetables_m, only: TestItem_t, Given, When, Then

        type(TestItem_t) :: tests

        tests = Given("a string with newlines", &
                [When("it is asserted to be empty", &
                        [Then("the the result messages don't contain newlines", checkMessageHasNoNewlines)])])
    end function test_with_newlines

    function checkPassForEmptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("")

        result_ = assertThat(example_result%passed())
    end function checkPassForEmptyChars

    function checkPassForEmptyString() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertThat, toString

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty(toString(""))

        result_ = assertThat(example_result%passed())
    end function checkPassForEmptyString

    function checkFailsForNonemptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("Not Empty")

        result_ = assertNot(example_result%passed())
    end function checkFailsForNonemptyChars

    function checkFailsForNonemptyString() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertNot, toString

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty(toString("Not Empty"))

        result_ = assertNot(example_result%passed())
    end function checkFailsForNonemptyString

    function checkMessageHasNoNewlines() result(result_)
        use Vegetables_m, only: &
                Result_t, assertEmpty, assertDoesntInclude, toString

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: EXAMPLE_STRING = &
                "Example" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_chars_result
        type(Result_t) :: example_string_result

        example_chars_result = assertEmpty(EXAMPLE_STRING)
        example_string_result = assertEmpty(toString(EXAMPLE_STRING))

        result_ = &
                assertDoesntInclude(NEWLINE, example_chars_result%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_chars_result%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_string_result%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_string_result%failureDescription())
    end function checkMessageHasNoNewlines
end module assert_empty_test
