module assert_equals_within_absolute_test
    use ieee_arithmetic, only: &
            ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf
    use double_precision_generator_m, only: DOUBLE_PRECISION_GENERATOR
    use iso_varying_string, only: var_str
    use veggies, only: &
            double_precision_input_t, &
            input_t, &
            result_t, &
            test_item_t, &
            assert_equals_within_absolute, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it

    implicit none
    private
    public :: test_assert_equals_within_absolute

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_equals_within_absolute() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "assert_equals_within_absolute", &
                [ it( &
                        "passes with the same number even with very small tolerance", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_pass_for_same_number) &
                , it( &
                        "fails with sufficiently different numbers", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_fail_for_different_numbers) &
                , it( &
                        "passes with sufficiently close numbers", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_pass_for_close_numbers) &
                , it(&
                        "all comparisons with exceptional values fail", &
                        check_exceptional) &
                ])
    end function

    pure function check_pass_for_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        double precision :: example
        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        select type (input)
        type is (double_precision_input_t)
            example = input%input()
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
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_fail_for_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        double precision :: example
        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        select type (input)
        type is (double_precision_input_t)
            example = input%input()
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
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_pass_for_close_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        double precision :: example
        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        select type (input)
        type is (double_precision_input_t)
            example = input%input()
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
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_exceptional() result(result_)
        type(result_t) :: result_

        type(result_t) :: nan_vs_nan
        type(result_t) :: nan_vs_num
        type(result_t) :: num_vs_nan
        type(result_t) :: inf_vs_inf
        type(result_t) :: inf_vs_nan
        type(result_t) :: nan_vs_inf
        type(result_t) :: inf_vs_num
        type(result_t) :: num_vs_inf
        type(result_t) :: neg_inf_vs_neg_inf
        type(result_t) :: neg_inf_vs_inf
        type(result_t) :: inf_vs_neg_inf
        type(result_t) :: neg_inf_vs_nan
        type(result_t) :: nan_vs_neg_inf
        type(result_t) :: neg_inf_vs_num
        type(result_t) :: num_vs_neg_inf
        double precision :: nan, inf, neg_inf, num

        nan = ieee_value(nan, ieee_quiet_nan)
        inf = ieee_value(inf, ieee_positive_inf)
        neg_inf = ieee_value(neg_inf, ieee_negative_inf)
        num = 1.d0

        nan_vs_nan = assert_equals_within_absolute(nan, nan, tiny(num))
        nan_vs_num = assert_equals_within_absolute(nan, num, tiny(num))
        num_vs_nan = assert_equals_within_absolute(num, nan, tiny(num))
        inf_vs_inf = assert_equals_within_absolute(inf, inf, tiny(num))
        inf_vs_nan = assert_equals_within_absolute(inf, nan, tiny(num))
        nan_vs_inf = assert_equals_within_absolute(nan, inf, tiny(num))
        inf_vs_num = assert_equals_within_absolute(inf, num, tiny(num))
        num_vs_inf = assert_equals_within_absolute(num, inf, tiny(num))
        neg_inf_vs_neg_inf = assert_equals_within_absolute(neg_inf, neg_inf, tiny(num))
        neg_inf_vs_inf = assert_equals_within_absolute(neg_inf, inf, tiny(num))
        inf_vs_neg_inf = assert_equals_within_absolute(inf, neg_inf, tiny(num))
        neg_inf_vs_nan = assert_equals_within_absolute(neg_inf, nan, tiny(num))
        nan_vs_neg_inf = assert_equals_within_absolute(nan, neg_inf, tiny(num))
        neg_inf_vs_num = assert_equals_within_absolute(neg_inf, num, tiny(num))
        num_vs_neg_inf = assert_equals_within_absolute(num, neg_inf, tiny(num))

        result_ = &
                assert_not( &
                        nan_vs_nan%passed(), &
                        nan_vs_nan%verbose_description(.false.)) &
                .and.assert_not( &
                        nan_vs_num%passed(), &
                        nan_vs_num%verbose_description(.false.)) &
                .and.assert_not( &
                        num_vs_nan%passed(), &
                        num_vs_nan%verbose_description(.false.)) &
                .and.assert_not( &
                        inf_vs_inf%passed(), &
                        inf_vs_inf%verbose_description(.false.)) &
                .and.assert_not( &
                        inf_vs_nan%passed(), &
                        inf_vs_nan%verbose_description(.false.)) &
                .and.assert_not( &
                        nan_vs_inf%passed(), &
                        nan_vs_inf%verbose_description(.false.)) &
                .and.assert_not( &
                        inf_vs_num%passed(), &
                        inf_vs_num%verbose_description(.false.)) &
                .and.assert_not( &
                        num_vs_inf%passed(), &
                        num_vs_inf%verbose_description(.false.)) &
                .and.assert_not( &
                        neg_inf_vs_neg_inf%passed(), &
                        neg_inf_vs_neg_inf%verbose_description(.false.)) &
                .and.assert_not( &
                        neg_inf_vs_inf%passed(), &
                        neg_inf_vs_inf%verbose_description(.false.)) &
                .and.assert_not( &
                        inf_vs_neg_inf%passed(), &
                        inf_vs_neg_inf%verbose_description(.false.)) &
                .and.assert_not( &
                        neg_inf_vs_nan%passed(), &
                        neg_inf_vs_nan%verbose_description(.false.)) &
                .and.assert_not( &
                        nan_vs_neg_inf%passed(), &
                        nan_vs_neg_inf%verbose_description(.false.)) &
                .and.assert_not( &
                        neg_inf_vs_num%passed(), &
                        neg_inf_vs_num%verbose_description(.false.)) &
                .and.assert_not( &
                        num_vs_neg_inf%passed(), &
                        num_vs_neg_inf%verbose_description(.false.))
    end function
end module
