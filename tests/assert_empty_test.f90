module assert_empty_test
    use iso_varying_string, only: var_str
    use Vegetables_m, only: &
            Result_t, &
            TestItem_t, &
            assertEmpty, &
            assertNot, &
            assertThat, &
            describe, &
            it

    implicit none
    private

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"

    public :: test_assert_empty
contains
    function test_assert_empty() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with an empty character", checkPassForEmptyChars)
        individual_tests(2) = it("fails with a non empty character", checkFailsForNonemptyChars)
        tests = describe("assertEmpty", individual_tests)
    end function test_assert_empty

    pure function checkPassForEmptyChars() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: EMPTY = ""
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
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

        example_result_c = assertEmpty(EMPTY)
        example_result_s = assertEmpty(var_str(EMPTY))
        example_result_cc = assertEmpty(EMPTY, BOTH_MESSAGE)
        example_result_cs = assertEmpty(EMPTY, var_str(BOTH_MESSAGE))
        example_result_sc = assertEmpty(var_str(EMPTY), BOTH_MESSAGE)
        example_result_ss = assertEmpty(var_str(EMPTY), var_str(BOTH_MESSAGE))
        example_result_ccc = assertEmpty( &
                EMPTY, SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_ccs = assertEmpty( &
                EMPTY, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_csc = assertEmpty( &
                EMPTY, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_css = assertEmpty( &
                EMPTY, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
        example_result_scc = assertEmpty( &
                var_str(EMPTY), SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_scs = assertEmpty( &
                var_str(EMPTY), SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_ssc = assertEmpty( &
                var_str(EMPTY), var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_sss = assertEmpty( &
                var_str(EMPTY), var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))

        result_ = &
                assertThat( &
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
                        example_result_sss%verboseDescription(.false.))
    end function checkPassForEmptyChars

    pure function checkFailsForNonemptyChars() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: NOT_EMPTY = "Not Empty"
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
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

        example_result_c = assertEmpty(NOT_EMPTY)
        example_result_s = assertEmpty(var_str(NOT_EMPTY))
        example_result_cc = assertEmpty(NOT_EMPTY, BOTH_MESSAGE)
        example_result_cs = assertEmpty(NOT_EMPTY, var_str(BOTH_MESSAGE))
        example_result_sc = assertEmpty(var_str(NOT_EMPTY), BOTH_MESSAGE)
        example_result_ss = assertEmpty(var_str(NOT_EMPTY), var_str(BOTH_MESSAGE))
        example_result_ccc = assertEmpty( &
                NOT_EMPTY, SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_ccs = assertEmpty( &
                NOT_EMPTY, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_csc = assertEmpty( &
                NOT_EMPTY, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_css = assertEmpty( &
                NOT_EMPTY, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
        example_result_scc = assertEmpty( &
                var_str(NOT_EMPTY), SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_scs = assertEmpty( &
                var_str(NOT_EMPTY), SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_ssc = assertEmpty( &
                var_str(NOT_EMPTY), var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_sss = assertEmpty( &
                var_str(NOT_EMPTY), var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))

        result_ = &
                assertNot( &
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
                        example_result_sss%verboseDescription(.false.))
    end function checkFailsForNonemptyChars
end module assert_empty_test
