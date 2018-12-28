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
                it("fails with a non empty character", checkFailsForNonemptyChars)])
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

    function checkFailsForNonemptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("Not Empty")

        result_ = assertNot(example_result%passed())
    end function checkFailsForNonemptyChars

    function checkMessageHasNoNewlines() result(result_)
        use Vegetables_m, only: &
                Result_t, assertEmpty, assertDoesntInclude, toString

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: EXAMPLE_STRING = &
                "Example" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_result

        example_result = assertEmpty(EXAMPLE_STRING)

        result_ = &
                assertDoesntInclude(NEWLINE, example_result%verboseDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result%failureDescription())
    end function checkMessageHasNoNewlines
end module assert_empty_test
