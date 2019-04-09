module assert_equals_double_precision_test
    implicit none
    private

    public :: test_assert_equals_integers
contains
    function test_assert_equals_integers() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with the same number", checkPassForSameNumber)
        individual_tests(2) = it("fails with different numbers", checkFailForDifferentNumbers)
        tests = describe("assertEquals with double precision values", individual_tests)
    end function test_assert_equals_integers

    function checkPassForSameNumber() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEquals(1.0d0, 1.0d0)

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription())
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
