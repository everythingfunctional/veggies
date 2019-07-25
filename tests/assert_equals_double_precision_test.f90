module assert_equals_double_precision_test
    implicit none
    private

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"

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
        use iso_varying_string, only: var_str
        use Vegetables_m, only: Result_t, assertEquals, assertThat, fail

        class(*), intent(in) :: example
        type(Result_t) :: result_

        type(Result_t) :: example_result
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss

        select type (example)
        type is (double precision)
                example_result = assertEquals(example, example)
                example_result_c = assertEquals(example, example, BOTH_MESSAGE)
                example_result_s = assertEquals(example, example, var_str(BOTH_MESSAGE))
                example_result_cc = assertEquals( &
                        example, example, SUCCESS_MESSAGE, FAILURE_MESSAGE)
                example_result_cs = assertEquals( &
                        example, example, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
                example_result_sc = assertEquals( &
                        example, example, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
                example_result_ss = assertEquals( &
                        example, example, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
                result_ = &
                        assertThat( &
                                example_result%passed(), &
                                example_result%verboseDescription(.false.)) &
                        .and.assertThat( &
                                example_result_c%passed(), &
                                example_result_c%verboseDescription(.false.)) &
                        .and.assertThat( &
                                example_result_s%passed(), &
                                example_result_s%verboseDescription(.false.)) &
                        .and.assertThat( &
                                example_result_cc%passed(), &
                                example_result_cc%verboseDescription(.false.)) &
                        .and.assertThat( &
                                example_result_cs%passed(), &
                                example_result_cs%verboseDescription(.false.)) &
                        .and.assertThat( &
                                example_result_sc%passed(), &
                                example_result_sc%verboseDescription(.false.)) &
                        .and.assertThat( &
                                example_result_ss%passed(), &
                                example_result_ss%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function checkPassForSameNumber

    function checkFailForDifferentNumbers() result(result_)
        use iso_varying_string, only: var_str
        use Vegetables_m, only: Result_t, assertEquals, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss

        example_result = assertEquals(1.0d0, 2.0d0)
        example_result_c = assertEquals(1.0d0, 2.0d0, BOTH_MESSAGE)
        example_result_s = assertEquals(1.0d0, 2.0d0, var_str(BOTH_MESSAGE))
        example_result_cc = assertEquals( &
                1.0d0, 2.0d0, SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_cs = assertEquals( &
                1.0d0, 2.0d0, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_sc = assertEquals( &
                1.0d0, 2.0d0, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_ss = assertEquals( &
                1.0d0, 2.0d0, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))

        result_ = &
                assertNot( &
                        example_result%passed(), &
                        example_result%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_c%passed(), &
                        example_result_c%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_s%passed(), &
                        example_result_s%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_cc%passed(), &
                        example_result_cc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_cs%passed(), &
                        example_result_cs%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_sc%passed(), &
                        example_result_sc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_ss%passed(), &
                        example_result_ss%verboseDescription(.false.))
    end function checkFailForDifferentNumbers
end module assert_equals_double_precision_test
