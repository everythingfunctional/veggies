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
end module assert_equals_strings_test
