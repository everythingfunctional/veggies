module assert_includes_test
    implicit none
    private

    public :: &
            test_assert_includes, &
            test_included_with_newlines, &
            test_not_included_with_newlines
contains
    function test_assert_includes() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertIncludes", &
                [it("passes with the same strings", checkPassForSameStrings), &
                it("fails when the string isn't included", checkFailForDifferentStrings)])
    end function test_assert_includes

    function test_included_with_newlines() result(tests)
        use Vegetables_m, only: TestItem_t, Given, Then, When

        type(TestItem_t) :: tests

        tests = Given("a string with newlines", &
                [When("it is assertIncludes with itself", &
                        [Then("the messages don't contain newlines", checkIncludeStringsNoNewlines)])])
    end function test_included_with_newlines

    function test_not_included_with_newlines() result(tests)
        use Vegetables_m, only: TestItem_t, Given, Then, When

        type(TestItem_t) :: tests

        tests = Given("different strings with newlines", &
                [When("they are assertIncludes", &
                        [Then("the messages don't contain newlines", checkNotIncludedStringsNoNewlines)])])
    end function test_not_included_with_newlines

    function checkPassForSameStrings() result(result_)
        use Vegetables_m, only: Result_t, assertIncludes, assertThat, toString

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE_STRING = "Example String"
        type(Result_t) :: example_result

        example_result = assertIncludes(EXAMPLE_STRING, EXAMPLE_STRING)

        result_ = assertThat(example_result%passed())
    end function checkPassForSameStrings

    function checkFailForDifferentStrings() result(result_)
        use Vegetables_m, only: Result_t, assertIncludes, assertNot, toString

        type(Result_t) :: result_

        character(len=*), parameter :: ONE_STRNIG = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
        type(Result_t) :: example_result

        example_result = assertIncludes(ONE_STRNIG, OTHER_STRING)

        result_ = assertNot(example_result%passed())
    end function checkFailForDifferentStrings

    function checkIncludeStringsNoNewlines() result(result_)
        use Vegetables_m, only: &
                Result_t, assertDoesntInclude, assertIncludes, toString

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: EXAMPLE_STRING = &
                "Example" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_result

        example_result = assertIncludes(EXAMPLE_STRING, EXAMPLE_STRING)

        result_ = &
                assertDoesntInclude(NEWLINE, example_result%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result%verboseDescription())
    end function checkIncludeStringsNoNewlines

    function checkNotIncludedStringsNoNewlines() result(result_)
        use Vegetables_m, only: &
                Result_t, assertDoesntInclude, assertIncludes, toString

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: ONE_STRING = &
                "One" // NEWLINE // "With" // NEWLINE // "Newlines"
        character(len=*), parameter :: OTHER_STRING = &
                "Other" // NEWLINE // "With" // NEWLINE // "Newlines"
        type(Result_t) :: example_result

        example_result = assertIncludes(ONE_STRING, OTHER_STRING)

        result_ = &
                assertDoesntInclude(NEWLINE, example_result%failureDescription()) &
                .and.assertDoesntInclude(NEWLINE, example_result%verboseDescription())
    end function checkNotIncludedStringsNoNewlines
end module assert_includes_test
