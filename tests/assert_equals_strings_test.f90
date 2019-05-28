module assert_equals_strings_test
    implicit none
    private

    public :: test_assert_equals_strings
contains
    function test_assert_equals_strings() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with the same strings", ASCII_STRING_GENERATOR, checkPassForSameStrings)
        individual_tests(2) = it("fails with different strings", checkFailForDifferentStrings)
        tests = describe("assertEquals with strings", individual_tests)
    end function test_assert_equals_strings

    function checkPassForSameStrings(example) result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertThat, fail

        class(*), intent(in) :: example
        type(Result_t) :: result_

        type(Result_t) :: example_result

        select type (example)
        type is (character(len=*))
            example_result = assertEquals(example, example)
            result_ = assertThat( &
                    example_result%passed(), example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected a character string")
        end select
    end function checkPassForSameStrings

    function checkFailForDifferentStrings() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertNot

        type(Result_t) :: result_

        character(len=*), parameter :: ONE_STRNIG = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
        type(Result_t) :: example_result

        example_result = assertEquals(ONE_STRNIG, OTHER_STRING)

        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription(.false.))
    end function checkFailForDifferentStrings
end module assert_equals_strings_test
