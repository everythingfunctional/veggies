module assert_equals_within_absolute_test
    implicit none
    private

    public :: test_assert_equals_within_relative
contains
    function test_assert_equals_within_relative() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "passes with the same number even with very small tolerance", &
                DOUBLE_PRECISION_GENERATOR, &
                checkPassForSameNumber)
        individual_tests(2) = it( &
                "fails with sufficiently different numbers", &
                DOUBLE_PRECISION_GENERATOR, &
                checkFailForDifferentNumbers)
        individual_tests(3) = it( &
                "passes with sufficiently close numbers", &
                DOUBLE_PRECISION_GENERATOR, &
                checkPassForCloseNumbers)
        tests = describe("assertEqualsWithinAbsolute", individual_tests)
    end function test_assert_equals_within_relative

    function checkPassForSameNumber(example) result(result_)
        use Vegetables_m, only: &
                Result_t, assertEqualsWithinAbsolute, assertThat, fail

        class(*), intent(in) :: example
        type(Result_t) :: result_

        type(Result_t) :: example_result

        select type (example)
        type is (double precision)
            example_result = assertEqualsWithinAbsolute(example, example, TINY(0.0d0))
            result_ = assertThat( &
                    example_result%passed(), example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function checkPassForSameNumber

    function checkFailForDifferentNumbers(example) result(result_)
        use Vegetables_m, only: &
                Result_t, assertEqualsWithinAbsolute, assertNot, fail

        class(*), intent(in) :: example
        type(Result_t) :: result_

        type(Result_t) :: example_result

        select type (example)
        type is (double precision)
            example_result = assertEqualsWithinAbsolute(example, example+0.2d0, 0.1d0)
            result_ = assertNot( &
                    example_result%passed(), example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function checkFailForDifferentNumbers

    function checkPassForCloseNumbers(example) result(result_)
        use Vegetables_m, only: &
                Result_t, assertEqualsWithinAbsolute, assertThat, fail

        class(*), intent(in) :: example
        type(Result_t) :: result_

        type(Result_t) :: example_result

        select type (example)
        type is (double precision)
            example_result = assertEqualsWithinAbsolute(example, example+0.05d0, 0.1d0)
            result_ = assertThat( &
                    example_result%passed(), example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function checkPassForCloseNumbers
end module assert_equals_within_absolute_test
