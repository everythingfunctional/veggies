module assert_equals_integers_test
    implicit none
    private

    public :: test_assert_equals_integers
contains
    function test_assert_equals_integers() result(tests)
        use Vegetables_m, only: TestItem_t, describe, Example, it

        type(TestItem_t) :: tests

        tests = describe("assertEquals with integers", &
                [it( &
                        "passes with the same integer", &
                        [Example(1), Example(2)],&
                        checkPassForSameInteger), &
                it("fails with different integers", checkFailForDifferentIntegers)])
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
