module assert_equals_integers_test
    implicit none
    private

    public :: test_assert_equals_integers
contains
    function test_assert_equals_integers() result(tests)
        use Vegetables_m, only: Example_t, TestItem_t, describe, Example, it

        type(TestItem_t) :: tests

        type(Example_t) :: examples(2)
        type(TestItem_t) :: individual_tests(2)

        examples(1) = Example(1)
        examples(2) = Example(2)
        individual_tests(1) = it("passes with the same integer", examples, checkPassForSameInteger)
        individual_tests(2) = it("fails with different integers", checkFailForDifferentIntegers)
        tests = describe("assertEquals with integers", individual_tests)
    end function test_assert_equals_integers

    function checkPassForSameInteger(input) result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertThat, fail

        class(*), intent(in) :: input
        type(Result_t) :: result_

        type(Result_t) :: example_result

        select type (input)
        type is (integer)
            example_result = assertEquals(input, input)
            result_ = assertThat( &
                    example_result%passed(), example_result%verboseDescription())
        class default
            result_ = fail("Expected to get an integer")
        end select
    end function checkPassForSameInteger

    function checkFailForDifferentIntegers() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEquals(1, 2)

        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription())
    end function checkFailForDifferentIntegers
end module assert_equals_integers_test
