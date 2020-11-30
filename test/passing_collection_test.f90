module passing_collection_test
    implicit none
    private

    public :: test_passing_collection_behaviors
contains
    function test_passing_collection_behaviors() result(tests)
        use example_collections_m, only: example_passing_collection
        use helpers_m, only: test_item_input_t, run_test
        use vegetables, only: test_item_t, given, then__, when

        type(test_item_t) :: tests

        type(test_item_t) :: collection(1)
        type(test_item_input_t) :: the_collection
        type(test_item_t) :: individual_tests(9)

        the_collection%input = example_passing_collection()
        individual_tests(1) = then__("it knows it passed", check_collection_passes)
        individual_tests(2) = then__("it knows how many cases there were", check_num_cases)
        individual_tests(3) = then__("it has no failing cases", check_num_failing_cases)
        individual_tests(4) = then__("it's verbose description includes the given description", check_werbose_top_description)
        individual_tests(5) = then__( &
                "it's verbose description includes the individual case descriptions", &
                check_verbose_case_descriptions)
        individual_tests(6) = then__("it's verbose description includes the assertion message", check_verbose_description_assertion)
        individual_tests(7) = then__("it's failure description is empty", check_failure_description_empty)
        individual_tests(8) = then__("it knows how many asserts there were", check_num_asserts)
        individual_tests(9) = then__("it has no failing asserts", check_num_failing_asserts)
        collection(1) = when("it is run", run_test, individual_tests)
        tests = given("a passing test collection", the_collection, collection)
    end function

    pure function check_collection_passes(example_results) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_that, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_that(example_results%input%passed(), "It passed", "It didn't pass")
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_cases(example_results) result(result_)
        use example_collections_m, only: NUM_CASES_IN_PASSING
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_equals(NUM_CASES_IN_PASSING, example_results%input%num_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_failing_cases(example_results) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_equals(0, example_results%input%num_failing_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_werbose_top_description(example_results) result(result_)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_includes, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_includes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%input%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_verbose_case_descriptions(example_results) result(result_)
        use example_collections_m, only: &
                EXAMPLE_CASE_DESCRIPTION_1, EXAMPLE_CASE_DESCRIPTION_2
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_includes, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = &
                    assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%input%verbose_description(.false.)) &
                    .and.assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%input%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_verbose_description_assertion(example_results) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_includes, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_includes( &
                    SUCCESS_MESSAGE, &
                    example_results%input%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_failure_description_empty(example_results) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_empty, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_empty(example_results%input%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_asserts(example_results) result(result_)
        use example_collections_m, only: NUM_ASSERTS_IN_PASSING
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_equals(NUM_ASSERTS_IN_PASSING, example_results%input%num_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    pure function check_num_failing_asserts(example_results) result(result_)
        use helpers_m, only: test_result_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_results
        type(result_t) :: result_

        select type (example_results)
        type is (test_result_item_input_t)
            result_ = assert_equals(0, example_results%input%num_failing_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function
end module
