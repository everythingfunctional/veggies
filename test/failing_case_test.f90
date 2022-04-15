module failing_case_test
    use example_asserts_m, only: &
            FAILURE_MESSAGE, &
            NUM_ASSERTS_IN_FAILING, &
            NUM_FAILING_ASSERTS_IN_FAILING, &
            SUCCESS_MESSAGE
    use example_cases_m, only: &
            example_failing_test_case, &
            EXAMPLE_DESCRIPTION
    use helpers_m, only: test_item_input_t, test_result_item_input_t, run_test
    use veggies, only: &
            input_t, &
            result_t, &
            test_item_t, &
            test_result_item_t, &
            assert_doesnt_include, &
            assert_equals, &
            assert_includes, &
            assert_not, &
            fail, &
            given, &
            then__, &
            when

    implicit none
    private

    public :: test_failing_case_behaviors
contains
    function test_failing_case_behaviors() result(test)
        type(test_item_t) :: test

        test = given( &
                "a failing test case", &
                test_item_input_t(example_failing_test_case()), &
                [ when( &
                        "it is run", &
                        run_test, &
                        [ then__("it knows it failed", check_case_fails) &
                        , then__("it has 1 test case", check_num_cases) &
                        , then__("it has 1 failing case", check_num_failing_cases) &
                        , then__( &
                                "it's verbose description includes the given description", &
                                check_verbose_for_given_description) &
                        , then__( &
                                "it's verbose description includes the success message", &
                                check_verbose_for_success_message) &
                        , then__( &
                                "it's verbose description includes the failure message", &
                                check_verbose_for_failure_message) &
                        , then__( &
                                "it's failure description includes the given description", &
                                check_failure_for_given_description) &
                        , then__( &
                                "it's failure description includes the failure message", &
                                check_failure_for_failure_message) &
                        , then__( &
                                "it's failure description doesn't include the success message", &
                                check_failure_no_success_message) &
                        , then__("it knows how many asserts there were", check_num_asserts) &
                        , then__("it knows how many asserts failed", check_num_failing_asserts) &
                        ]) &
                ])
    end function

    function check_case_fails(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_not(example_result%passed())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals(1, example_result%num_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals(1, example_result%num_failing_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_for_given_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_includes(EXAMPLE_DESCRIPTION, example_result%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_for_success_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_includes(SUCCESS_MESSAGE, example_result%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_for_failure_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_includes(FAILURE_MESSAGE, example_result%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_for_given_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_includes(EXAMPLE_DESCRIPTION, example_result%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_for_failure_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_includes(FAILURE_MESSAGE, example_result%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_no_success_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_doesnt_include(SUCCESS_MESSAGE, example_result%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals(NUM_ASSERTS_IN_FAILING, example_result%num_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals( &
                    NUM_FAILING_ASSERTS_IN_FAILING, example_result%num_failing_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function
end module
