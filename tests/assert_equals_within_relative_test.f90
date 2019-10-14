module assert_equals_within_relative_test
    implicit none
    private

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"

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
        tests = describe("assertEqualsWithinRelative", individual_tests)
    end function test_assert_equals_within_relative

    function checkPassForSameNumber(the_example) result(result_)
        use iso_varying_string, only: var_str
        use Vegetables_m, only: &
                DoublePrecisionInput_t, &
                Input_t, &
                Result_t, &
                assertEqualsWithinRelative, &
                assertThat, &
                fail

        class(Input_t), intent(in) :: the_example
        type(Result_t) :: result_

        double precision :: example
        type(Result_t) :: example_result
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss

        select type (the_example)
        type is (DoublePrecisionInput_t)
            example = the_example%value_
            example_result = assertEqualsWithinRelative( &
                    example, example, TINY(0.0d0))
            example_result_c = assertEqualsWithinRelative( &
                    example, example, TINY(0.0d0), BOTH_MESSAGE)
            example_result_s = assertEqualsWithinRelative( &
                    example, example, TINY(0.0d0), var_str(BOTH_MESSAGE))
            example_result_cc = assertEqualsWithinRelative( &
                    example, &
                    example, &
                    TINY(0.0d0), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assertEqualsWithinRelative( &
                    example, &
                    example, &
                    TINY(0.0d0), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assertEqualsWithinRelative( &
                    example, &
                    example, &
                    TINY(0.0d0), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assertEqualsWithinRelative( &
                    example, &
                    example, &
                    TINY(0.0d0), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
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

    function checkFailForDifferentNumbers(the_example) result(result_)
        use iso_varying_string, only: var_str
        use Vegetables_m, only: &
                DoublePrecisionInput_t, &
                Input_t, &
                Result_t, &
                assertEqualsWithinRelative, &
                assertNot, &
                fail

        class(Input_t), intent(in) :: the_example
        type(Result_t) :: result_

        double precision :: example
        type(Result_t) :: example_result
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss

        select type (the_example)
        type is (DoublePrecisionInput_t)
            example = the_example%value_
            example_result = assertEqualsWithinRelative( &
                    example, example*1.11d0, 0.1d0)
            example_result_c = assertEqualsWithinRelative( &
                    example, example*1.11d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assertEqualsWithinRelative( &
                    example, example*1.11d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assertEqualsWithinRelative( &
                    example, &
                    example*1.11d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assertEqualsWithinRelative( &
                    example, &
                    example*1.11d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assertEqualsWithinRelative( &
                    example, &
                    example*1.11d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assertEqualsWithinRelative( &
                    example, &
                    example*1.11d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
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
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function checkFailForDifferentNumbers

    function checkPassForCloseNumbers(the_example) result(result_)
        use iso_varying_string, only: var_str
        use Vegetables_m, only: &
                DoublePrecisionInput_t, &
                Input_t, &
                Result_t, &
                assertEqualsWithinRelative, &
                assertThat, &
                fail

        class(Input_t), intent(in) :: the_example
        type(Result_t) :: result_

        double precision :: example
        type(Result_t) :: example_result
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss

        select type (the_example)
        type is (DoublePrecisionInput_t)
            example = the_example%value_
            example_result = assertEqualsWithinRelative( &
                    example, example*1.09d0, 0.1d0)
            example_result_c = assertEqualsWithinRelative( &
                    example, example*1.09d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assertEqualsWithinRelative( &
                    example, example*1.09d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assertEqualsWithinRelative( &
                    example, &
                    example*1.09d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assertEqualsWithinRelative( &
                    example, &
                    example*1.09d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assertEqualsWithinRelative( &
                    example, &
                    example*1.09d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assertEqualsWithinRelative( &
                    example, &
                    example*1.09d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
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
    end function checkPassForCloseNumbers
end module assert_equals_within_relative_test
