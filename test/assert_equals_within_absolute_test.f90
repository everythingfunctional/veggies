module assert_equals_within_absolute_test
    implicit none
    private
    public :: test_assert_equals_within_relative

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_equals_within_relative() result(tests)
        use double_precision_generator_m, only: DOUBLE_PRECISION_GENERATOR
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "passes with the same number even with very small tolerance", &
                DOUBLE_PRECISION_GENERATOR, &
                check_pass_for_same_number)
        individual_tests(2) = it( &
                "fails with sufficiently different numbers", &
                DOUBLE_PRECISION_GENERATOR, &
                check_fail_for_different_numbers)
        individual_tests(3) = it( &
                "passes with sufficiently close numbers", &
                DOUBLE_PRECISION_GENERATOR, &
                check_pass_for_close_numbers)
        tests = describe("assert_equals_within_absolute", individual_tests)
    end function

    pure function check_pass_for_same_number(the_example) result(result_)
        use iso_varying_string, only: var_str
        use vegetables, only: &
                double_precision_input_t, &
                input_t, &
                result_t, &
                assert_equals_within_absolute, &
                assert_that, &
                fail

        class(input_t), intent(in) :: the_example
        type(result_t) :: result_

        double precision :: example
        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        select type (the_example)
        type is (double_precision_input_t)
            example = the_example%value_
            example_result = assert_equals_within_absolute( &
                    example, example, tiny(0.0d0))
            example_result_c = assert_equals_within_absolute( &
                    example, example, tiny(0.0d0), BOTH_MESSAGE)
            example_result_s = assert_equals_within_absolute( &
                    example, example, tiny(0.0d0), var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            result_ = &
                    assert_that( &
                            example_result%passed(), &
                            example_result%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_c%passed(), &
                            example_result_c%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_s%passed(), &
                            example_result_s%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_cc%passed(), &
                            example_result_cc%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_cs%passed(), &
                            example_result_cs%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_sc%passed(), &
                            example_result_sc%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_ss%passed(), &
                            example_result_ss%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function

    pure function check_fail_for_different_numbers(the_example) result(result_)
        use iso_varying_string, only: var_str
        use vegetables, only: &
                double_precision_input_t, &
                input_t, &
                result_t, &
                assert_equals_within_absolute, &
                assert_not, &
                fail

        class(input_t), intent(in) :: the_example
        type(result_t) :: result_

        double precision :: example
        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        select type (the_example)
        type is (double_precision_input_t)
            example = the_example%value_
            example_result = assert_equals_within_absolute( &
                    example, example+0.2d0, 0.1d0)
            example_result_c = assert_equals_within_absolute( &
                    example, example+0.2d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_absolute( &
                    example, example+0.2d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            result_ = &
                    assert_not( &
                            example_result%passed(), &
                            example_result%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_c%passed(), &
                            example_result_c%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_s%passed(), &
                            example_result_s%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_cc%passed(), &
                            example_result_cc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_cs%passed(), &
                            example_result_cs%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_sc%passed(), &
                            example_result_sc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_ss%passed(), &
                            example_result_ss%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function

    pure function check_pass_for_close_numbers(the_example) result(result_)
        use iso_varying_string, only: var_str
        use vegetables, only: &
                double_precision_input_t, &
                input_t, &
                result_t, &
                assert_equals_within_absolute, &
                assert_that, &
                fail

        class(input_t), intent(in) :: the_example
        type(result_t) :: result_

        double precision :: example
        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        select type (the_example)
        type is (double_precision_input_t)
            example = the_example%value_
            example_result = assert_equals_within_absolute( &
                    example, example+0.05d0, 0.1d0)
            example_result_c = assert_equals_within_absolute( &
                    example, example+0.05d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_absolute( &
                    example, example+0.05d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            result_ = &
                    assert_that( &
                            example_result%passed(), &
                            example_result%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_c%passed(), &
                            example_result_c%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_s%passed(), &
                            example_result_s%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_cc%passed(), &
                            example_result_cc%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_cs%passed(), &
                            example_result_cs%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_sc%passed(), &
                            example_result_sc%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_ss%passed(), &
                            example_result_ss%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a double precision value")
        end select
    end function
end module
