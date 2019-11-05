module assert_includes_test
    implicit none
    private

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"

    public :: test_assert_includes
contains
    function test_assert_includes() result(tests)
        use iso_varying_string ! To make compiler happy
        use Vegetables_m, only: TestItem_t, describe, it, ASCII_STRING_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with the same strings", ASCII_STRING_GENERATOR, checkPassForSameStrings)
        individual_tests(2) = it("fails when the string isn't included", checkFailForDifferentStrings)
        tests = describe("assertIncludes", individual_tests)
    end function test_assert_includes

    pure function checkPassForSameStrings(the_example) result(result_)
        use iso_varying_string, only: VARYING_STRING, char, var_str
        use Vegetables_m, only: &
                Input_t, &
                Result_t, &
                StringInput_t, &
                assertIncludes, &
                assertThat, &
                fail

        class(Input_t), intent(in) :: the_example
        type(Result_t) :: result_

        type(VARYING_STRING) :: example
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss
        type(Result_t) :: example_result_ccc
        type(Result_t) :: example_result_ccs
        type(Result_t) :: example_result_csc
        type(Result_t) :: example_result_css
        type(Result_t) :: example_result_scc
        type(Result_t) :: example_result_scs
        type(Result_t) :: example_result_ssc
        type(Result_t) :: example_result_sss
        type(Result_t) :: example_result_cccc
        type(Result_t) :: example_result_cccs
        type(Result_t) :: example_result_ccsc
        type(Result_t) :: example_result_ccss
        type(Result_t) :: example_result_cscc
        type(Result_t) :: example_result_cscs
        type(Result_t) :: example_result_cssc
        type(Result_t) :: example_result_csss
        type(Result_t) :: example_result_sccc
        type(Result_t) :: example_result_sccs
        type(Result_t) :: example_result_scsc
        type(Result_t) :: example_result_scss
        type(Result_t) :: example_result_sscc
        type(Result_t) :: example_result_sscs
        type(Result_t) :: example_result_sssc
        type(Result_t) :: example_result_ssss

        select type (the_example)
        type is (StringInput_t)
            example = the_example%value_
            example_result_cc = assertIncludes(char(example), char(example))
            example_result_cs = assertIncludes(char(example), example)
            example_result_sc = assertIncludes(example, char(example))
            example_result_ss = assertIncludes(example, example)
            example_result_ccc = assertIncludes( &
                    char(example), char(example), BOTH_MESSAGE)
            example_result_ccs = assertIncludes( &
                    char(example), char(example), var_str(BOTH_MESSAGE))
            example_result_csc = assertIncludes( &
                    char(example), example, BOTH_MESSAGE)
            example_result_css = assertIncludes( &
                    char(example), example, var_str(BOTH_MESSAGE))
            example_result_scc = assertIncludes( &
                    example, char(example), BOTH_MESSAGE)
            example_result_scs = assertIncludes( &
                    example, char(example), var_str(BOTH_MESSAGE))
            example_result_ssc = assertIncludes( &
                    example, example, BOTH_MESSAGE)
            example_result_sss = assertIncludes( &
                    example, example, var_str(BOTH_MESSAGE))
            example_result_cccc = assertIncludes( &
                    char(example), &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cccs = assertIncludes( &
                    char(example), &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_ccsc = assertIncludes( &
                    char(example), &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ccss = assertIncludes( &
                    char(example), &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_cscc = assertIncludes( &
                    char(example), &
                    example, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cscs = assertIncludes( &
                    char(example), &
                    example, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_cssc = assertIncludes( &
                    char(example), &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_csss = assertIncludes( &
                    char(example), &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_sccc = assertIncludes( &
                    example, &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_sccs = assertIncludes( &
                    example, &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_scsc = assertIncludes( &
                    example, &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_scss = assertIncludes( &
                    example, &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_sscc = assertIncludes( &
                    example, &
                    example, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_sscs = assertIncludes( &
                    example, &
                    example, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sssc = assertIncludes( &
                    example, &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ssss = assertIncludes( &
                    example, &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            result_ = &
                    assertThat( &
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
                            example_result_ss%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_ccc%passed(), &
                            example_result_ccc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_ccs%passed(), &
                            example_result_ccs%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_csc%passed(), &
                            example_result_csc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_css%passed(), &
                            example_result_css%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_scc%passed(), &
                            example_result_scc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_scs%passed(), &
                            example_result_scs%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_ssc%passed(), &
                            example_result_ssc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_sss%passed(), &
                            example_result_sss%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_cccc%passed(), &
                            example_result_cccc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_cccs%passed(), &
                            example_result_cccs%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_ccsc%passed(), &
                            example_result_ccsc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_ccss%passed(), &
                            example_result_ccss%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_cscc%passed(), &
                            example_result_cscc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_cscs%passed(), &
                            example_result_cscs%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_cssc%passed(), &
                            example_result_cssc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_csss%passed(), &
                            example_result_csss%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_sccc%passed(), &
                            example_result_sccc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_sccs%passed(), &
                            example_result_sccs%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_scsc%passed(), &
                            example_result_scsc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_scss%passed(), &
                            example_result_scss%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_sscc%passed(), &
                            example_result_sscc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_sscs%passed(), &
                            example_result_sscs%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_sssc%passed(), &
                            example_result_sssc%verboseDescription(.false.)) &
                    .and.assertThat( &
                            example_result_ssss%passed(), &
                            example_result_ssss%verboseDescription(.false.))
        class default
            result_ = fail("Expected a character string")
        end select
    end function checkPassForSameStrings

    pure function checkFailForDifferentStrings() result(result_)
        use iso_varying_string, only: var_str
        use Vegetables_m, only: Result_t, assertIncludes, assertNot

        type(Result_t) :: result_

        character(len=*), parameter :: ONE_STRNIG = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss
        type(Result_t) :: example_result_ccc
        type(Result_t) :: example_result_ccs
        type(Result_t) :: example_result_csc
        type(Result_t) :: example_result_css
        type(Result_t) :: example_result_scc
        type(Result_t) :: example_result_scs
        type(Result_t) :: example_result_ssc
        type(Result_t) :: example_result_sss
        type(Result_t) :: example_result_cccc
        type(Result_t) :: example_result_cccs
        type(Result_t) :: example_result_ccsc
        type(Result_t) :: example_result_ccss
        type(Result_t) :: example_result_cscc
        type(Result_t) :: example_result_cscs
        type(Result_t) :: example_result_cssc
        type(Result_t) :: example_result_csss
        type(Result_t) :: example_result_sccc
        type(Result_t) :: example_result_sccs
        type(Result_t) :: example_result_scsc
        type(Result_t) :: example_result_scss
        type(Result_t) :: example_result_sscc
        type(Result_t) :: example_result_sscs
        type(Result_t) :: example_result_sssc
        type(Result_t) :: example_result_ssss

        example_result_cc = assertIncludes(ONE_STRNIG, OTHER_STRING)
        example_result_cs = assertIncludes(ONE_STRNIG, var_str(OTHER_STRING))
        example_result_sc = assertIncludes(var_str(ONE_STRNIG), OTHER_STRING)
        example_result_ss = assertIncludes(var_str(ONE_STRNIG), var_str(OTHER_STRING))
        example_result_ccc = assertIncludes( &
                ONE_STRNIG, OTHER_STRING, BOTH_MESSAGE)
        example_result_ccs = assertIncludes( &
                ONE_STRNIG, OTHER_STRING, var_str(BOTH_MESSAGE))
        example_result_csc = assertIncludes( &
                ONE_STRNIG, var_str(OTHER_STRING), BOTH_MESSAGE)
        example_result_css = assertIncludes( &
                ONE_STRNIG, var_str(OTHER_STRING), var_str(BOTH_MESSAGE))
        example_result_scc = assertIncludes( &
                var_str(ONE_STRNIG), OTHER_STRING, BOTH_MESSAGE)
        example_result_scs = assertIncludes( &
                var_str(ONE_STRNIG), OTHER_STRING, var_str(BOTH_MESSAGE))
        example_result_ssc = assertIncludes( &
                var_str(ONE_STRNIG), var_str(OTHER_STRING), BOTH_MESSAGE)
        example_result_sss = assertIncludes( &
                var_str(ONE_STRNIG), var_str(OTHER_STRING), var_str(BOTH_MESSAGE))
        example_result_cccc = assertIncludes( &
                ONE_STRNIG, &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_cccs = assertIncludes( &
                ONE_STRNIG, &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_ccsc = assertIncludes( &
                ONE_STRNIG, &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_ccss = assertIncludes( &
                ONE_STRNIG, &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_cscc = assertIncludes( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_cscs = assertIncludes( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_cssc = assertIncludes( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_csss = assertIncludes( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_sccc = assertIncludes( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_sccs = assertIncludes( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_scsc = assertIncludes( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_scss = assertIncludes( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_sscc = assertIncludes( &
                var_str(ONE_STRNIG), &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_sscs = assertIncludes( &
                var_str(ONE_STRNIG), &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_sssc = assertIncludes( &
                var_str(ONE_STRNIG), &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_ssss = assertIncludes( &
                var_str(ONE_STRNIG), &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))

        result_ = &
                assertNot( &
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
                        example_result_ss%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_ccc%passed(), &
                        example_result_ccc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_ccs%passed(), &
                        example_result_ccs%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_csc%passed(), &
                        example_result_csc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_css%passed(), &
                        example_result_css%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_scc%passed(), &
                        example_result_scc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_scs%passed(), &
                        example_result_scs%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_ssc%passed(), &
                        example_result_ssc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_sss%passed(), &
                        example_result_sss%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_cccc%passed(), &
                        example_result_cccc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_cccs%passed(), &
                        example_result_cccs%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_ccsc%passed(), &
                        example_result_ccsc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_ccss%passed(), &
                        example_result_ccss%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_cscc%passed(), &
                        example_result_cscc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_cscs%passed(), &
                        example_result_cscs%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_cssc%passed(), &
                        example_result_cssc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_csss%passed(), &
                        example_result_csss%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_sccc%passed(), &
                        example_result_sccc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_sccs%passed(), &
                        example_result_sccs%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_scsc%passed(), &
                        example_result_scsc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_scss%passed(), &
                        example_result_scss%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_sscc%passed(), &
                        example_result_sscc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_sscs%passed(), &
                        example_result_sscs%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_sssc%passed(), &
                        example_result_sssc%verboseDescription(.false.)) &
                .and.assertNot( &
                        example_result_ssss%passed(), &
                        example_result_ssss%verboseDescription(.false.))
    end function checkFailForDifferentStrings
end module assert_includes_test
