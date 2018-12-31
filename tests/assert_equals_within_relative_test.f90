module assert_equals_within_relative_test
    implicit none
    private

    public :: test_assert_equals_within_relative
contains
    function test_assert_equals_within_relative() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertEqualsWithinRelative", &
                [it("passes with the same number even with very small tolerance", checkPassForSameNumber), &
                it("fails with sufficiently different numbers", checkFailForDifferentNumbers), &
                it("passes with sufficiently close numbers", checkPassForCloseNumbers)])
    end function test_assert_equals_within_relative

    function checkPassForSameNumber() result(result_)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEqualsWithinRelative(1.0d0, 1.0d0, TINY(0.0d0))

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription())
    end function checkPassForSameNumber

    function checkFailForDifferentNumbers() result(result_)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEqualsWithinRelative(1.0d0, 2.0d0, 0.1d0)

        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription())
    end function checkFailForDifferentNumbers

    function checkPassForCloseNumbers() result(result_)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEqualsWithinRelative(1.0d0, 1.1d0, 0.1d0)

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription())
    end function checkPassForCloseNumbers
end module assert_equals_within_relative_test
