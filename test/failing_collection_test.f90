module failing_collection_test
    use example_asserts_m, only: SUCCESS_MESSAGE
    use example_collections_m, only: &
            example_failing_collection, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_CASE_DESCRIPTION_2, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            EXAMPLE_FAILING_CASE_DESCRIPTION, &
            FAILURE_MESSAGE, &
            NUM_ASSERTS_IN_FAILING, &
            NUM_CASES_IN_FAILING, &
            NUM_FAILING_ASSERTS, &
            NUM_FAILING_CASES
    use helpers_m, only: test_item_input_t, test_result_item_input_t, run_test
    use strff, only: NEWLINE
    use vegetables, only: &
            input_t, &
            result_t, &
            test_item_t, &
            test_result_item_t, &
            assert_equals, &
            assert_doesnt_include, &
            assert_includes, &
            assert_not, &
            fail, &
            given, &
            then__, &
            when

    implicit none
    private

    public :: test_failing_collection_behaviors
contains
    function test_failing_collection_behaviors() result(tests)
        type(test_item_t) :: tests

        type(test_item_t) :: collection(1)
        type(test_item_input_t) :: the_collection
        type(test_item_t) :: individual_tests(15)

        the_collection = test_item_input_t(example_failing_collection())
        individual_tests(1) = then__("it knows it failed", check_collection_fails)
        individual_tests(2) = then__("it knows how many cases there were", check_num_cases)
        individual_tests(3) = then__("it knows how many cases failed", check_num_failing_cases)
        individual_tests(4) = then__("it's verbose description includes the given description", check_verbose_top_description)
        individual_tests(5) = then__( &
                "it's verbose description includes the individual case descriptions", &
                check_verbose_case_descriptions)
        individual_tests(6) = then__("it's verbose description includes the failure message", check_verbose_for_failure_message)
        individual_tests(7) = then__("it's verbose description includes the success message", check_verbose_for_success_message)
        individual_tests(8) = then__("it's failure description includes the given description", check_failure_for_top_description)
        individual_tests(9) = then__( &
                "it's failure description includes the failing case description", check_failure_case_description)
        individual_tests(10) = then__( &
                "it's failure description does not include the passing case descriptions", &
                check_failure_no_passing_descriptions)
        individual_tests(11) = then__("it's failure description includes the failure message", check_failure_for_message)
        individual_tests(12) = then__( &
                "it's failure description does not include the success message", &
                check_failure_no_success_message)
        individual_tests(13) = then__("it's failure description does not include blank lines", check_failure_no_blank_lines)
        individual_tests(14) = then__("it knows how many asserts there were", check_num_asserts)
        individual_tests(15) = then__("it knows how many asserts failed", check_num_failing_asserts)
        collection(1) = when("it is run", run_test, individual_tests)
        tests = given("a failing test collection", the_collection, collection)
    end function

    function check_collection_fails(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_not(example_results%passed())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(NUM_CASES_IN_FAILING, example_results%num_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(NUM_FAILING_CASES, example_results%num_failing_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_top_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_case_descriptions(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = &
                    assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%verbose_description(.false.)) &
                    .and.assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%verbose_description(.false.)) &
                    .and.assert_includes( &
                            EXAMPLE_FAILING_CASE_DESCRIPTION, &
                            example_results%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_for_failure_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    FAILURE_MESSAGE, &
                    example_results%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_for_success_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    SUCCESS_MESSAGE, &
                    example_results%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_for_top_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_case_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    EXAMPLE_FAILING_CASE_DESCRIPTION, &
                    example_results%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_no_passing_descriptions(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = &
                    assert_doesnt_include( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%failure_description(.false.)) &
                    .and.assert_doesnt_include( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_for_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    FAILURE_MESSAGE, &
                    example_results%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_no_success_message(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_doesnt_include( &
                    SUCCESS_MESSAGE, &
                    example_results%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_no_blank_lines(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_doesnt_include( &
                    NEWLINE // NEWLINE, &
                    example_results%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(NUM_ASSERTS_IN_FAILING, example_results%num_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(NUM_FAILING_ASSERTS, example_results%num_failing_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function
end module
