module assert_equals_double_precision_test
    implicit none
    private

    public :: test_assert_equals_integers
contains
    function test_assert_equals_integers() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with the same number", DOUBLE_PRECISION_GENERATOR, checkPassForSameNumber)
        individual_tests(2) = it("fails with different numbers", checkFailForDifferentNumbers)
        tests = describe("assertEquals with double precision values", individual_tests)
    end function test_assert_equals_integers

    function checkPassForSameNumber(example) result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertThat, fail

        class(*), intent(in) :: example
        type(Result_t) :: result_

        type(Result_t) :: example_result

        select type (example)
        type is (double precision)
                example_result = assertEquals(example, example)
                result_ = assertThat( &
                        example_result%passed(), example_result%verboseDescription())
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function checkPassForSameNumber

    function checkFailForDifferentNumbers() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEquals(1.0d0, 2.0d0)

        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription())
    end function checkFailForDifferentNumbers
end module assert_equals_double_precision_test
