module passing_case_test
    implicit none
    private

    public :: test_passing_case_behaviors
contains
    function test_passing_case_behaviors() result(test)
        use example_cases_m, only: example_passing_test_case
        use helpers_m, only: test_item_input_t, run_test
        use vegetables, only: test_item_t, given, then__, when

        type(test_item_t) :: test

        type(test_item_t) :: collection(1)
        type(test_item_input_t) :: the_case
        type(test_item_t) :: individual_tests(8)

        the_case%input = example_passing_test_Case()
        individual_tests(1) = then__("it knows it passed", check_case_passes)
        individual_tests(2) = then__("it has 1 test case", check_num_cases)
        individual_tests(3) = then__("it has no failing case", check_num_failing_cases)
        individual_tests(4) = then__("it's verbose description still includes the given description", check_verbose_description)
        individual_tests(5) = then__("it's verbose description includes the assertion message", check_verbose_description_assertion)
        individual_tests(6) = then__("it's failure description is empty", check_failure_description_empty)
        individual_tests(7) = then__("it knows how many asserts there were", check_num_asserts)
        individual_tests(8) = then__("it has no failing asserts", check_num_failing_asserts)
        collection(1) = when("it is run", run_test, individual_tests)
        test = given("a passing test case", the_case, collection)
    end function

    pure function check_case_passes(example_result) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_that(example_result%input%passed())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_cases(example_result) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_equals(1, example_result%input%num_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_failing_cases(example_result) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_equals(0, example_result%input%num_failing_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_verbose_description(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_includes, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_includes(EXAMPLE_DESCRIPTION, example_result%input%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_verbose_description_assertion(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_includes, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_includes(SUCCESS_MESSAGE, example_result%input%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_failure_description_empty(example_result) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_empty, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_empty(example_result%input%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_asserts(example_result) result(result_)
        use example_asserts_m, only: NUM_ASSERTS_IN_PASSING
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_equals(NUM_ASSERTS_IN_PASSING, example_result%input%num_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_failing_asserts(example_result) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_result
        type(result_t) :: result_

        select type (example_result)
        type is (test_result_item_input_t)
            result_ = assert_equals(0, example_result%input%num_failing_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function
end module
