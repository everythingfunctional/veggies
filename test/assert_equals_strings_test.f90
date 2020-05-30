module assert_equals_strings_test
    use iso_varying_string, only: VARYING_STRING, char, var_str
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
            StringInput_t, &
            TestItem_t, &
            assertEquals, &
            assertNot, &
            assertThat, &
            describe, &
            fail, &
            it, &
            ASCII_STRING_GENERATOR

    implicit none
    private

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"

    public :: test_assert_equals_strings
contains
    function test_assert_equals_strings() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with the same strings", ASCII_STRING_GENERATOR, checkPassForSameStrings)
        individual_tests(2) = it("fails with different strings", checkFailForDifferentStrings)
        tests = describe("assertEquals with strings", individual_tests)
    end function test_assert_equals_strings

    pure function checkPassForSameStrings(the_example) result(result_)
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
            example_result_cc = assertEquals(char(example), char(example))
            example_result_cs = assertEquals(char(example), example)
            example_result_sc = assertEquals(example, char(example))
            example_result_ss = assertEquals(example, example)
            example_result_ccc = assertEquals( &
                    char(example), char(example), BOTH_MESSAGE)
            example_result_ccs = assertEquals( &
                    char(example), char(example), var_str(BOTH_MESSAGE))
            example_result_csc = assertEquals( &
                    char(example), example, BOTH_MESSAGE)
            example_result_css = assertEquals( &
                    char(example), example, var_str(BOTH_MESSAGE))
            example_result_scc = assertEquals( &
                    example, char(example), BOTH_MESSAGE)
            example_result_scs = assertEquals( &
                    example, char(example), var_str(BOTH_MESSAGE))
            example_result_ssc = assertEquals( &
                    example, example, BOTH_MESSAGE)
            example_result_sss = assertEquals( &
                    example, example, var_str(BOTH_MESSAGE))
            example_result_cccc = assertEquals( &
                    char(example), &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cccs = assertEquals( &
                    char(example), &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_ccsc = assertEquals( &
                    char(example), &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ccss = assertEquals( &
                    char(example), &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_cscc = assertEquals( &
                    char(example), &
                    example, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cscs = assertEquals( &
                    char(example), &
                    example, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_cssc = assertEquals( &
                    char(example), &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_csss = assertEquals( &
                    char(example), &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_sccc = assertEquals( &
                    example, &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_sccs = assertEquals( &
                    example, &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_scsc = assertEquals( &
                    example, &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_scss = assertEquals( &
                    example, &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_sscc = assertEquals( &
                    example, &
                    example, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_sscs = assertEquals( &
                    example, &
                    example, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sssc = assertEquals( &
                    example, &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ssss = assertEquals( &
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

        example_result_cc = assertEquals(ONE_STRNIG, OTHER_STRING)
        example_result_cs = assertEquals(ONE_STRNIG, var_str(OTHER_STRING))
        example_result_sc = assertEquals(var_str(ONE_STRNIG), OTHER_STRING)
        example_result_ss = assertEquals(var_str(ONE_STRNIG), var_str(OTHER_STRING))
        example_result_ccc = assertEquals( &
                ONE_STRNIG, OTHER_STRING, BOTH_MESSAGE)
        example_result_ccs = assertEquals( &
                ONE_STRNIG, OTHER_STRING, var_str(BOTH_MESSAGE))
        example_result_csc = assertEquals( &
                ONE_STRNIG, var_str(OTHER_STRING), BOTH_MESSAGE)
        example_result_css = assertEquals( &
                ONE_STRNIG, var_str(OTHER_STRING), var_str(BOTH_MESSAGE))
        example_result_scc = assertEquals( &
                var_str(ONE_STRNIG), OTHER_STRING, BOTH_MESSAGE)
        example_result_scs = assertEquals( &
                var_str(ONE_STRNIG), OTHER_STRING, var_str(BOTH_MESSAGE))
        example_result_ssc = assertEquals( &
                var_str(ONE_STRNIG), var_str(OTHER_STRING), BOTH_MESSAGE)
        example_result_sss = assertEquals( &
                var_str(ONE_STRNIG), var_str(OTHER_STRING), var_str(BOTH_MESSAGE))
        example_result_cccc = assertEquals( &
                ONE_STRNIG, &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_cccs = assertEquals( &
                ONE_STRNIG, &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_ccsc = assertEquals( &
                ONE_STRNIG, &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_ccss = assertEquals( &
                ONE_STRNIG, &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_cscc = assertEquals( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_cscs = assertEquals( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_cssc = assertEquals( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_csss = assertEquals( &
                ONE_STRNIG, &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_sccc = assertEquals( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_sccs = assertEquals( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_scsc = assertEquals( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_scss = assertEquals( &
                var_str(ONE_STRNIG), &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_sscc = assertEquals( &
                var_str(ONE_STRNIG), &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_sscs = assertEquals( &
                var_str(ONE_STRNIG), &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_sssc = assertEquals( &
                var_str(ONE_STRNIG), &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_ssss = assertEquals( &
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
end module assert_equals_strings_test
