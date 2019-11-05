module assert_equals_integers_test
    implicit none
    private

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"

    public :: test_assert_equals_integers
contains
    function test_assert_equals_integers() result(tests)
        use iso_varying_string ! To make compiler happy
        use Vegetables_m, only: TestItem_t, describe, it, INTEGER_GENERATOR

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with the same integer", INTEGER_GENERATOR, checkPassForSameInteger)
        individual_tests(2) = it("fails with different integers", checkFailForDifferentIntegers)
        tests = describe("assertEquals with integers", individual_tests)
    end function test_assert_equals_integers

    pure function checkPassForSameInteger(the_input) result(result_)
        use iso_varying_string, only: var_str
        use Vegetables_m, only: &
                Input_t, &
                IntegerInput_t, &
                Result_t, &
                assertEquals, &
                assertThat, &
                fail

        class(Input_t), intent(in) :: the_input
        type(Result_t) :: result_

        type(Result_t) :: example_result
        type(Result_t) :: example_result_c
        type(Result_t) :: example_result_s
        type(Result_t) :: example_result_cc
        type(Result_t) :: example_result_cs
        type(Result_t) :: example_result_sc
        type(Result_t) :: example_result_ss
        integer :: input

        select type (the_input)
        type is (IntegerInput_t)
            input = the_input%value_
            example_result = assertEquals(input, input)
            example_result_c = assertEquals(input, input, BOTH_MESSAGE)
            example_result_s = assertEquals(input, input, var_str(BOTH_MESSAGE))
            example_result_cc = assertEquals( &
                    input, input, SUCCESS_MESSAGE, FAILURE_MESSAGE)
            example_result_cs = assertEquals( &
                    input, input, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
            example_result_sc = assertEquals( &
                    input, input, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
            example_result_ss = assertEquals( &
                    input, input, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
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
            result_ = fail("Expected to get an integer")
        end select
    end function checkPassForSameInteger

    pure function checkFailForDifferentIntegers() result(result_)
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

        example_result = assertEquals(1, 2)
        example_result_c = assertEquals(1, 2, BOTH_MESSAGE)
        example_result_s = assertEquals(1, 2, var_str(BOTH_MESSAGE))
        example_result_cc = assertEquals( &
                1, 2, SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_cs = assertEquals( &
                1, 2, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_sc = assertEquals( &
                1, 2, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_ss = assertEquals( &
                1, 2, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))

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
    end function checkFailForDifferentIntegers
end module assert_equals_integers_test
