module vegetables
    use vegetables_assert_m, only: &
            assert_doesnt_include, &
            assert_empty, &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative, &
            assert_faster_than, &
            assert_includes, &
            assert_not, &
            assert_that
    use vegetables_ascii_string_generator_m, only: ASCII_STRING_GENERATOR
    use vegetables_double_precision_input_m, only: double_precision_input_t
    use vegetables_example_m, only: example_t, example
    use vegetables_generated_m, only: generated_t, generated
    use vegetables_generator_m, only: generator_t
    use vegetables_input_m, only: input_t
    use vegetables_input_test_case_m, only: input_test_case_t, input_test_case
    use vegetables_integer_generator_m, only: INTEGER_GENERATOR
    use vegetables_integer_input_m, only: integer_input_t
    use vegetables_messages_m, only: &
            delimit, &
            make_doesnt_include_failure_message, &
            make_doesnt_include_success_message, &
            make_empty_failure_message, &
            make_equals_failure_message, &
            make_equals_success_message, &
            make_faster_than_failure_message, &
            make_faster_than_success_message, &
            make_includes_failure_message, &
            make_includes_success_message, &
            make_within_failure_message, &
            make_within_success_message, &
            with_user_message, &
            EMPTY_SUCCESS_MESSAGE, &
            NOT_FAILURE_MESSAGE, &
            NOT_SUCCESS_MESSAGE, &
            THAT_FAILURE_MESSAGE, &
            THAT_SUCCESS_MESSAGE
    use vegetables_random_m, only: &
            get_random_ascii_character, &
            get_random_ascii_string, &
            get_random_ascii_string_with_max_length, &
            get_random_double_precision_with_magnitude, &
            get_random_double_precision_with_range, &
            get_random_integer, &
            get_random_integer_with_range, &
            get_random_logical
    use vegetables_result_m, only: result_t, fail, succeed
    use vegetables_run_tests_m, only: run_tests
    use vegetables_shrink_result_m, only: &
            shrink_result_t, shrunk_value, simplest_value
    use vegetables_simple_test_case_m, only: &
            simple_test_case_t, simple_test_case
    use vegetables_simple_test_collection_m, only: &
            simple_test_collection_t, simple_test_collection
    use vegetables_string_input_m, only: string_input_t
    use vegetables_test_m, only: filter_result_t, test_t
    use vegetables_test_case_result_m, only: &
            test_case_result_t, test_case_result
    use vegetables_test_case_with_examples_m, only: &
            test_case_with_examples_t, test_case_with_examples
    use vegetables_test_case_with_generator_m, only: &
            test_case_with_generator_t, test_case_with_generator
    use vegetables_test_collection_result_m, only: &
            test_collection_result_t, test_collection_result
    use vegetables_test_constructors_m, only: &
            describe, given, it, it_, test_that, then_, then__, when
    use vegetables_test_item_m, only: filter_item_result_t, test_item_t
    use vegetables_test_interfaces_m, only: &
            computation_i, input_test_i, simple_test_i, transformer_i
    use vegetables_test_result_m, only: test_result_t
    use vegetables_test_result_item_m, only: test_result_item_t
    use vegetables_transformation_failure_m, only: transformation_failure_t
    use vegetables_transformed_m, only: transformed_t, transformed
    use vegetables_transforming_test_collection_m, only: &
            transforming_test_collection_t, transforming_test_collection
end module
