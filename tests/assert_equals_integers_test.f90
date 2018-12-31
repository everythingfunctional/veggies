module assert_equals_integers_test
    implicit none
    private

    public :: test_assert_equals_integers
contains
    function test_assert_equals_integers() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertEquals with integers", &
                [it("passes with the same integer", checkPassForSameInteger), &
                it("fails with different integers", checkFailForDifferentIntegers)])
    end function test_assert_equals_integers

    function checkPassForSameInteger() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEquals(1, 1)

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription())
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
