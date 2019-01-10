module assert_equals_strings_test
    implicit none
    private

    public :: test_assert_equals_strings
contains
    function test_assert_equals_strings() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertEquals with strings", &
                [it("passes with the same strings", checkPassForSameStrings), &
                it("fails with different strings", checkFailForDifferentStrings)])
    end function test_assert_equals_strings

    function checkPassForSameStrings() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertThat

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE_STRING = "Example String"
        type(Result_t) :: example_result

        example_result = assertEquals(EXAMPLE_STRING, EXAMPLE_STRING)

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription())
    end function checkPassForSameStrings

    function checkFailForDifferentStrings() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertNot

        type(Result_t) :: result_

        character(len=*), parameter :: ONE_STRNIG = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
        type(Result_t) :: example_result

        example_result = assertEquals(ONE_STRNIG, OTHER_STRING)

        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription())
    end function checkFailForDifferentStrings
end module assert_equals_strings_test
