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
        use Vegetables_m, only: Result_t, assertEquals, assertThat

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE_STRING = "Example String"
        type(Result_t) :: example_result

        example_result = assertEquals(EXAMPLE_STRING, EXAMPLE_STRING)

        result_ = assertThat(example_result%passed())
    end function checkPassForSameStrings

    function checkFailForDifferentStrings() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertNot

        type(Result_t) :: result_

        character(len=*), parameter :: ONE_STRNIG = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
        type(Result_t) :: example_result

        example_result = assertEquals(ONE_STRNIG, OTHER_STRING)

        result_ = assertNot(example_result%passed())
    end function checkFailForDifferentStrings

    function checkEqualStringsNoNewlines() result(result_)
        use Vegetables_m, only: Result_t, assertDoesntInclude, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: EXAMPLE_STRING = &
                "Example" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_result

        example_result = assertEquals(EXAMPLE_STRING, EXAMPLE_STRING)

        result_ = &
                assertDoesntInclude(NEWLINE, example_result%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result%verboseDescription())
    end function checkEqualStringsNoNewlines

    function checkNotEqualStringsNoNewlines() result(result_)
        use Vegetables_m, only: Result_t, assertDoesntInclude, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: ONE_STRING = &
                "One" // NEWLINE // "With" // NEWLINE // "Newlines"
        character(len=*), parameter :: OTHER_STRING = &
                "Other" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_result

        example_result = assertEquals(ONE_STRING, OTHER_STRING)

        result_ = &
                assertDoesntInclude(NEWLINE, example_result%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result%verboseDescription())
    end function checkNOTEqualStringsNoNewlines
end module assert_equals_strings_test
