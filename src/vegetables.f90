module vegetables
    use iso_varying_string, only: varying_string
    use vegetables_ascii_string_generator_m, only: ASCII_STRING_GENERATOR
    use vegetables_command_line_m, only: &
            options_t, get_options, NUM_GENERATOR_TESTS
    use vegetables_constants_m, only: INDENTATION
    use vegetables_double_precision_input_m, only: double_precision_input_t
    use vegetables_example_m, only: example_t, example
    use vegetables_generated_m, only: generated_t, generated
    use vegetables_generator_m, only: generator_t
    use vegetables_individual_result_m, only: &
            individual_result_t, individual_result
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
    use vegetables_test_collection_with_input_m, only: &
            test_collection_with_input_t, test_collection_with_input
    use vegetables_test_item_m, only: filter_item_result_t, test_item_t
    use vegetables_test_interfaces_m, only: &
            computation_i, input_test_i, simple_test_i, transformer_i
    use vegetables_test_result_m, only: test_result_t
    use vegetables_test_result_item_m, only: test_result_item_t
    use vegetables_transformation_failure_m, only: transformation_failure_t
    use vegetables_transformed_m, only: transformed_t, transformed
    use vegetables_transforming_test_collection_m, only: &
            transforming_test_collection_t, transforming_test_collection

    implicit none
    private
    public :: &
            double_precision_input_t, &
            example_t, &
            filter_item_result_t, &
            generated_t, &
            generator_t, &
            input_t, &
            input_test_case_t, &
            integer_input_t, &
            result_t, &
            shrink_result_t, &
            simple_test_case_t, &
            simple_test_collection_t, &
            string_input_t, &
            test_case_result_t, &
            test_case_with_examples_t, &
            test_case_with_generator_t, &
            test_collection_result_t, &
            test_collection_with_input_t, &
            test_item_t, &
            test_result_item_t, &
            transformation_failure_t, &
            transformed_t, &
            transforming_test_collection_t, &
            assert_doesnt_include, &
            assert_empty, &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative, &
            assert_faster_than, &
            assert_includes, &
            assert_not, &
            assert_that, &
            delimit, &
            describe, &
            example, &
            fail, &
            generated, &
            get_random_ascii_character, &
            get_random_ascii_string, &
            get_random_ascii_string_with_max_length, &
            get_random_double_precision_with_magnitude, &
            get_random_double_precision_with_range, &
            get_random_integer, &
            get_random_integer_with_range, &
            get_random_logical, &
            given, &
            it, &
            it_, &
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
            run_tests, &
            shrunk_value, &
            simplest_value, &
            succeed, &
            test_that, &
            then_, &
            then__, &
            transformed, &
            when, &
            with_user_message, &
            ASCII_STRING_GENERATOR, &
            INTEGER_GENERATOR

    interface assert_doesnt_include
        module procedure assert_doesnt_include_basic_cc
        module procedure assert_doesnt_include_basic_cs
        module procedure assert_doesnt_include_basic_sc
        module procedure assert_doesnt_include_basic_ss
        module procedure assert_doesnt_include_with_message_ccc
        module procedure assert_doesnt_include_with_message_ccs
        module procedure assert_doesnt_include_with_message_csc
        module procedure assert_doesnt_include_with_message_css
        module procedure assert_doesnt_include_with_message_scc
        module procedure assert_doesnt_include_with_message_scs
        module procedure assert_doesnt_include_with_message_ssc
        module procedure assert_doesnt_include_with_message_sss
        module procedure assert_doesnt_include_with_messages_cccc
        module procedure assert_doesnt_include_with_messages_cccs
        module procedure assert_doesnt_include_with_messages_ccsc
        module procedure assert_doesnt_include_with_messages_ccss
        module procedure assert_doesnt_include_with_messages_cscc
        module procedure assert_doesnt_include_with_messages_cscs
        module procedure assert_doesnt_include_with_messages_cssc
        module procedure assert_doesnt_include_with_messages_csss
        module procedure assert_doesnt_include_with_messages_sccc
        module procedure assert_doesnt_include_with_messages_sccs
        module procedure assert_doesnt_include_with_messages_scsc
        module procedure assert_doesnt_include_with_messages_scss
        module procedure assert_doesnt_include_with_messages_sscc
        module procedure assert_doesnt_include_with_messages_sscs
        module procedure assert_doesnt_include_with_messages_sssc
        module procedure assert_doesnt_include_with_messages_ssss
    end interface

    interface assert_empty
        module procedure assert_empty_basic_c
        module procedure assert_empty_basic_s
        module procedure assert_empty_with_message_cc
        module procedure assert_empty_with_message_cs
        module procedure assert_empty_with_message_sc
        module procedure assert_empty_with_message_ss
        module procedure assert_empty_with_messages_ccc
        module procedure assert_empty_with_messages_ccs
        module procedure assert_empty_with_messages_csc
        module procedure assert_empty_with_messages_css
        module procedure assert_empty_with_messages_scc
        module procedure assert_empty_with_messages_scs
        module procedure assert_empty_with_messages_ssc
        module procedure assert_empty_with_messages_sss
    end interface

    interface assert_equals
        module procedure assert_equals_double_precision
        module procedure assert_equals_double_precision_with_message_c
        module procedure assert_equals_double_precision_with_message_s
        module procedure assert_equals_double_precision_with_messages_cc
        module procedure assert_equals_double_precision_with_messages_cs
        module procedure assert_equals_double_precision_with_messages_sc
        module procedure assert_equals_double_precision_with_messages_ss
        module procedure assert_equals_integer_basic
        module procedure assert_equals_integer_with_message_c
        module procedure assert_equals_integer_with_message_s
        module procedure assert_equals_integer_with_messages_cc
        module procedure assert_equals_integer_with_messages_cs
        module procedure assert_equals_integer_with_messages_sc
        module procedure assert_equals_integer_with_messages_ss
        module procedure assert_equals_strings_cc
        module procedure assert_equals_strings_cs
        module procedure assert_equals_strings_sc
        module procedure assert_equals_strings_ss
        module procedure assert_equals_strings_with_message_ccc
        module procedure assert_equals_strings_with_message_ccs
        module procedure assert_equals_strings_with_message_csc
        module procedure assert_equals_strings_with_message_css
        module procedure assert_equals_strings_with_message_scc
        module procedure assert_equals_strings_with_message_scs
        module procedure assert_equals_strings_with_message_ssc
        module procedure assert_equals_strings_with_message_sss
        module procedure assert_equals_strings_with_messages_cccc
        module procedure assert_equals_strings_with_messages_cccs
        module procedure assert_equals_strings_with_messages_ccsc
        module procedure assert_equals_strings_with_messages_ccss
        module procedure assert_equals_strings_with_messages_cscc
        module procedure assert_equals_strings_with_messages_cscs
        module procedure assert_equals_strings_with_messages_cssc
        module procedure assert_equals_strings_with_messages_csss
        module procedure assert_equals_strings_with_messages_sccc
        module procedure assert_equals_strings_with_messages_sccs
        module procedure assert_equals_strings_with_messages_scsc
        module procedure assert_equals_strings_with_messages_scss
        module procedure assert_equals_strings_with_messages_sscc
        module procedure assert_equals_strings_with_messages_sscs
        module procedure assert_equals_strings_with_messages_sssc
        module procedure assert_equals_strings_with_messages_ssss
    end interface

    interface assert_equals_within_absolute
        module procedure assert_equals_within_absolute_basic
        module procedure assert_equals_within_absolute_with_message_c
        module procedure assert_equals_within_absolute_with_message_s
        module procedure assert_equals_within_absolute_with_messages_cc
        module procedure assert_equals_within_absolute_with_messages_cs
        module procedure assert_equals_within_absolute_with_messages_sc
        module procedure assert_equals_within_absolute_with_messages_ss
    end interface

    interface assert_equals_within_relative
        module procedure assert_equals_within_relative_basic
        module procedure assert_equals_within_relative_with_message_c
        module procedure assert_equals_within_relative_with_message_s
        module procedure assert_equals_within_relative_with_messages_cc
        module procedure assert_equals_within_relative_with_messages_cs
        module procedure assert_equals_within_relative_with_messages_sc
        module procedure assert_equals_within_relative_with_messages_ss
    end interface

    interface assert_faster_than
        module procedure assert_faster_than_absolute_bracketed
        module procedure assert_faster_than_absolute_bracketed_with_message_c
        module procedure assert_faster_than_absolute_bracketed_with_message_s
        module procedure assert_faster_than_absolute_bracketed_with_messages_cc
        module procedure assert_faster_than_absolute_bracketed_with_messages_cs
        module procedure assert_faster_than_absolute_bracketed_with_messages_sc
        module procedure assert_faster_than_absolute_bracketed_with_messages_ss
        module procedure assert_faster_than_absolute_simple
        module procedure assert_faster_than_absolute_simple_with_message_c
        module procedure assert_faster_than_absolute_simple_with_message_s
        module procedure assert_faster_than_absolute_simple_with_messages_cc
        module procedure assert_faster_than_absolute_simple_with_messages_cs
        module procedure assert_faster_than_absolute_simple_with_messages_sc
        module procedure assert_faster_than_absolute_simple_with_messages_ss
        module procedure assert_faster_than_relative_bracketed
        module procedure assert_faster_than_relative_bracketed_with_message_c
        module procedure assert_faster_than_relative_bracketed_with_message_s
        module procedure assert_faster_than_relative_bracketed_with_messages_cc
        module procedure assert_faster_than_relative_bracketed_with_messages_cs
        module procedure assert_faster_than_relative_bracketed_with_messages_sc
        module procedure assert_faster_than_relative_bracketed_with_messages_ss
        module procedure assert_faster_than_relative_simple
        module procedure assert_faster_than_relative_simple_with_message_c
        module procedure assert_faster_than_relative_simple_with_message_s
        module procedure assert_faster_than_relative_simple_with_messages_cc
        module procedure assert_faster_than_relative_simple_with_messages_cs
        module procedure assert_faster_than_relative_simple_with_messages_sc
        module procedure assert_faster_than_relative_simple_with_messages_ss
    end interface

    interface assert_includes
        module procedure assert_includes_cc
        module procedure assert_includes_cs
        module procedure assert_includes_sc
        module procedure assert_includes_ss
        module procedure assert_includes_with_message_ccc
        module procedure assert_includes_with_message_ccs
        module procedure assert_includes_with_message_csc
        module procedure assert_includes_with_message_css
        module procedure assert_includes_with_message_scc
        module procedure assert_includes_with_message_scs
        module procedure assert_includes_with_message_ssc
        module procedure assert_includes_with_message_sss
        module procedure assert_includes_with_messages_cccc
        module procedure assert_includes_with_messages_cccs
        module procedure assert_includes_with_messages_ccsc
        module procedure assert_includes_with_messages_ccss
        module procedure assert_includes_with_messages_cscc
        module procedure assert_includes_with_messages_cscs
        module procedure assert_includes_with_messages_cssc
        module procedure assert_includes_with_messages_csss
        module procedure assert_includes_with_messages_sccc
        module procedure assert_includes_with_messages_sccs
        module procedure assert_includes_with_messages_scsc
        module procedure assert_includes_with_messages_scss
        module procedure assert_includes_with_messages_sscc
        module procedure assert_includes_with_messages_sscs
        module procedure assert_includes_with_messages_sssc
        module procedure assert_includes_with_messages_ssss
    end interface

    interface assert_not
        module procedure assert_not_basic
        module procedure assert_not_with_message_c
        module procedure assert_not_with_message_s
        module procedure assert_not_with_messages_cc
        module procedure assert_not_with_messages_cs
        module procedure assert_not_with_messages_sc
        module procedure assert_not_with_messages_ss
    end interface

    interface assert_that
        module procedure assert_that_basic
        module procedure assert_that_with_message_c
        module procedure assert_that_with_message_s
        module procedure assert_that_with_messages_cc
        module procedure assert_that_with_messages_cs
        module procedure assert_that_with_messages_sc
        module procedure assert_that_with_messages_ss
    end interface

    interface describe
        module procedure describe_basic_c
        module procedure describe_basic_s
        module procedure describe_with_input_c
        module procedure describe_with_input_s
    end interface

    interface given
        module procedure given_basic_c
        module procedure given_basic_s
        module procedure given_with_input_c
        module procedure given_with_input_s
    end interface

    interface it
        module procedure it_basic_c
        module procedure it_basic_s
        module procedure it_with_examples_c
        module procedure it_with_examples_s
        module procedure it_with_generator_c
        module procedure it_with_generator_s
    end interface

    interface it_
        module procedure it_input_c
        module procedure it_input_s
    end interface

    interface then_
        module procedure then_basic_c
        module procedure then_basic_s
    end interface

    interface then__
        module procedure then_input_c
        module procedure then_input_s
    end interface

    interface when
        module procedure when_basic_c
        module procedure when_basic_s
        module procedure when_with_transformer_c
        module procedure when_with_transformer_s
    end interface

    double precision, parameter :: MACHINE_EPSILON = epsilon(0.0d0)
    double precision, parameter :: MACHINE_TINY = tiny(0.0d0)
contains
    pure function assert_doesnt_include_basic_cc(search_for, string) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_doesnt_include_basic_cs(search_for, string) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_doesnt_include_basic_sc(search_for, string) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_doesnt_include_basic_ss(search_for, string) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_doesnt_include_with_message_ccc( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_doesnt_include_with_message_ccs( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_doesnt_include_with_message_csc( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_doesnt_include_with_message_css( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                string, &
                message, &
                message)
    end function

    pure function assert_doesnt_include_with_message_scc( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_doesnt_include_with_message_scs( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_doesnt_include_with_message_ssc( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_doesnt_include_with_message_sss( &
            search_for, string, message) result(result__)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                string, &
                message, &
                message)
    end function

    pure function assert_doesnt_include_with_messages_cccc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_cccs( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_doesnt_include_with_messages_ccsc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_ccss( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_doesnt_include_with_messages_cscc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_cscs( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_doesnt_include_with_messages_cssc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_csss( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                var_str(search_for), &
                string, &
                success_message, &
                failure_message)
    end function

    pure function assert_doesnt_include_with_messages_sccc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_sccs( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_doesnt_include_with_messages_scsc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_scss( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_doesnt_include_with_messages_sscc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_sscs( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_doesnt_include_with_messages_sssc( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_doesnt_include( &
                search_for, &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_doesnt_include_with_messages_ssss( &
            search_for, string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (string.includes.search_for) then
            result__ = fail(with_user_message( &
                    make_doesnt_include_failure_message(search_for, string), &
                    failure_message))
        else
            result__ = succeed(with_user_message( &
                    make_doesnt_include_success_message(search_for, string), &
                    success_message))
        end if
    end function

    pure function assert_empty_basic_c(string) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_empty_basic_s(string) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_empty_with_message_cc(string, message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_empty_with_message_cs(string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_empty_with_message_sc(string, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_empty_with_message_ss(string, message) result(result__)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                message, &
                message)
    end function

    pure function assert_empty_with_messages_ccc( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_ccs( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_empty_with_messages_csc( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_css( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_empty_with_messages_scc( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_scs( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_empty_with_messages_ssc( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_sss( &
            string, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, operator(==)

        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (string == "") then
            result__ = succeed(with_user_message( &
                    EMPTY_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(with_user_message( &
                    make_empty_failure_message(string), failure_message))
        end if
    end function

    pure function assert_equals_double_precision( &
            expected, &
            actual) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_double_precision_with_message_c( &
            expected, &
            actual, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, var_str(message), var_str(message))
    end function

    pure function assert_equals_double_precision_with_message_s( &
            expected, &
            actual, &
            message) &
            result(result__)
        use iso_varying_string, only: varying_string

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals(expected, actual, message, message)
    end function

    pure function assert_equals_double_precision_with_messages_cc( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_double_precision_with_messages_cs( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, var_str(success_message), failure_message)
    end function

    pure function assert_equals_double_precision_with_messages_sc( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, success_message, var_str(failure_message))
    end function

    pure function assert_equals_double_precision_with_messages_ss( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                MACHINE_EPSILON, &
                success_message, &
                failure_message)
    end function

    pure function assert_equals_integer_basic(expected, actual) result(result__)
        use iso_varying_string, only: var_str

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_integer_with_message_c( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: var_str

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_integer_with_message_s( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                message, &
                message)
    end function

    pure function assert_equals_integer_with_messages_cc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_integer_with_messages_cs( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, var_str(success_message), failure_message)
    end function

    pure function assert_equals_integer_with_messages_sc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, success_message, var_str(failure_message))
    end function

    pure function assert_equals_integer_with_messages_ss( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: to_string

        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (expected == actual) then
            result__ = succeed(with_user_message( &
                    make_equals_success_message(to_string(expected)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_equals_failure_message( &
                            to_string(expected), to_string(actual)), &
                    failure_message))
        end if
    end function

    pure function assert_equals_strings_cc( &
            expected, actual) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_cs( &
            expected, actual) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_sc( &
            expected, actual) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_ss( &
            expected, actual) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_with_message_ccc( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_ccs( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_message_csc( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_css( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_message_scc( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_scs( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_message_ssc( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_sss( &
            expected, actual, message) result(result__)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_messages_cccc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_cccs( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_ccsc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_ccss( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                success_message, &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_cscc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_cscs( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_cssc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_csss( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                success_message, &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_sccc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_sccs( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_scsc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_scss( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                success_message, &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_sscc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_sscs( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_sssc( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_ssss( &
            expected, actual, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, operator(==)

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (expected == actual) then
            result__ = succeed(with_user_message( &
                    make_equals_success_message(expected), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_equals_failure_message(expected, actual), &
                    failure_message))
        end if
    end function

    pure function assert_equals_within_absolute_basic( &
            expected, &
            actual, &
            tolerance) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_within_absolute_with_message_c( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_within_absolute_with_message_s( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: varying_string

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                message, &
                message)
    end function

    pure function assert_equals_within_absolute_with_messages_cc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_within_absolute_with_messages_cs( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_within_absolute_with_messages_sc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_within_absolute_with_messages_ss( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: to_string

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (equals_within_absolute(expected, actual, tolerance)) then
            result__ = succeed(with_user_message( &
                    make_within_success_message( &
                            to_string(expected), &
                            to_string(actual), &
                            to_string(tolerance)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_within_failure_message( &
                            to_string(expected), &
                            to_string(actual), &
                            to_string(tolerance)), &
                    failure_message))
        end if
    end function

    pure function assert_equals_within_relative_basic( &
            expected, &
            actual, &
            tolerance) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(result_t) :: result__

        result__ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_within_relative_with_message_c( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_within_relative_with_message_s( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        use iso_varying_string, only: varying_string

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                message, &
                message)
    end function

    pure function assert_equals_within_relative_with_messages_cc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_within_relative_with_messages_cs( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_within_relative_with_messages_sc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_within_relative_with_messages_ss( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: to_string

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (equals_within_relative(expected, actual, tolerance)) then
            result__ = succeed(with_user_message( &
                    make_within_success_message( &
                            to_string(expected), &
                            to_string(actual), &
                            to_string(tolerance * 100.0d0) // "%"), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_within_failure_message( &
                            to_string(expected), &
                            to_string(actual), &
                            to_string(tolerance * 100.0d0) // "%"), &
                    failure_message))
        end if
    end function

    function assert_faster_than_absolute_bracketed( &
            reference, &
            before, &
            computation, &
            after, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_absolute_bracketed_with_message_c( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_absolute_bracketed_with_message_s( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: varying_string

        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_absolute_bracketed_with_messages_cc( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_bracketed_with_messages_cs( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_absolute_bracketed_with_messages_sc( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_bracketed_with_messages_ss( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: to_string

        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time

        total_time = 0.0d0
        do i = 1, iterations
            call before
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            call after
            total_time = total_time + (end_time - start_time)
        end do
        average_time = total_time / dble(iterations)
        if (average_time < reference) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function

    function assert_faster_than_absolute_simple( &
            reference, &
            computation, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_absolute_simple_with_message_c( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_absolute_simple_with_message_s( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: varying_string

        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_absolute_simple_with_messages_cc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_simple_with_messages_cs( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_absolute_simple_with_messages_sc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_simple_with_messages_ss( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: to_string

        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time

        total_time = 0.0d0
        do i = 1, iterations
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
        end do
        average_time = total_time / dble(iterations)
        if (average_time < reference) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function

    function assert_faster_than_relative_bracketed( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str

        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_relative_bracketed_with_message_c( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_relative_bracketed_with_message_s( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: varying_string

        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_relative_bracketed_with_messages_cc( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_bracketed_with_messages_cs( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_relative_bracketed_with_messages_sc( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_bracketed_with_messages_ss( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: to_string

        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time
        double precision :: reference_start_time
        double precision :: reference_end_time
        double precision :: reference_total_time
        double precision :: reference_average_time

        total_time = 0.0d0
        reference_total_time = 0.0d0
        do i = 1, iterations
            call reference_before
            call cpu_time(reference_start_time)
            call reference
            call cpu_time(reference_end_time)
            call reference_after
            reference_total_time = &
                    reference_total_time &
                    + (reference_end_time - reference_start_time)
            call before
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            call after
            total_time = total_time + (end_time - start_time)
        end do
        reference_average_time = reference_total_time / dble(iterations)
        average_time = total_time / dble(iterations)
        if (average_time < reference_average_time) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function

    function assert_faster_than_relative_simple( &
            reference, &
            computation, &
            iterations) &
            result(result__)
        use iso_varying_string, only: var_str

        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_relative_simple_with_message_c( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: var_str

        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_relative_simple_with_message_s( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        use iso_varying_string, only: varying_string

        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_relative_simple_with_messages_cc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: var_str

        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_simple_with_messages_cs( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_relative_simple_with_messages_sc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string, var_str

        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_simple_with_messages_ss( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: to_string

        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        double precision :: start_time
        double precision :: end_time
        double precision :: total_time
        double precision :: average_time
        double precision :: reference_start_time
        double precision :: reference_end_time
        double precision :: reference_total_time
        double precision :: reference_average_time

        total_time = 0.0d0
        reference_total_time = 0.0d0
        do i = 1, iterations
            call cpu_time(reference_start_time)
            call reference
            call cpu_time(reference_end_time)
            reference_total_time = &
                    reference_total_time &
                    + (reference_end_time - reference_start_time)
            call cpu_time(start_time)
            call computation
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
        end do
        reference_average_time = reference_total_time / dble(iterations)
        average_time = total_time / dble(iterations)
        if (average_time < reference_average_time) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function

    pure function assert_includes_cc( &
                search_for, &
                string) &
                result(result__)
            use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_cs( &
                search_for, &
                string) &
                result(result__)
            use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_sc( &
                search_for, &
                string) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_ss( &
                search_for, &
                string) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_with_message_ccc( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_ccs( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_includes_with_message_csc( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_css( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                message, &
                message)
    end function

    pure function assert_includes_with_message_scc( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_scs( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_includes_with_message_ssc( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_sss( &
                search_for, &
                string, &
                message) &
                result(result__)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                message, &
                message)
    end function

    pure function assert_includes_with_messages_cccc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_cccs( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_ccsc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_ccss( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_includes_with_messages_cscc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_cscs( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_cssc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_csss( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                success_message, &
                failure_message)
    end function

    pure function assert_includes_with_messages_sccc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_sccs( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_scsc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_scss( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_includes_with_messages_sscc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_sscs( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_sssc( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_ssss( &
                search_for, &
                string, &
                success_message, &
                failure_message) &
                result(result__)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (string.includes.search_for) then
            result__ = succeed(with_user_message( &
                    make_includes_success_message(search_for, string), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_includes_failure_message(search_for, string), &
                    failure_message))
        end if
    end function

    pure function assert_not_basic(condition) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        type(result_t) :: result__

        result__ = assert_not(condition, var_str(""), var_str(""))
    end function

    pure function assert_not_with_message_c(condition, message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_not(condition, var_str(message), var_str(message))
    end function

    pure function assert_not_with_message_s(condition, message) result(result__)
        use iso_varying_string, only: varying_string

        logical, intent(in) :: condition
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_not(condition, message, message)
    end function

    pure function assert_not_with_messages_cc( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_not( &
                condition, var_str(success_message), var_str(failure_message))
    end function

    pure function assert_not_with_messages_cs( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_not( &
                condition, var_str(success_message), failure_message)
    end function

    pure function assert_not_with_messages_sc( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        logical, intent(in) :: condition
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_not( &
                condition, success_message, var_str(failure_message))
    end function

    pure function assert_not_with_messages_ss( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string

        logical, intent(in) :: condition
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (condition) then
            result__ = fail(with_user_message( &
                    NOT_FAILURE_MESSAGE, failure_message))
        else
            result__ = succeed(with_user_message( &
                    NOT_SUCCESS_MESSAGE, success_message))
        end if
    end function

    pure function assert_that_basic(condition) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        type(result_t) :: result__

        result__ = assert_that(condition, var_str(""), var_str(""))
    end function

    pure function assert_that_with_message_c(condition, message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_that(condition, var_str(message), var_str(message))
    end function

    pure function assert_that_with_message_s(condition, message) result(result__)
        use iso_varying_string, only: varying_string

        logical, intent(in) :: condition
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_that(condition, message, message)
    end function

    pure function assert_that_with_messages_cc( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_that( &
                condition, var_str(success_message), var_str(failure_message))
    end function

    pure function assert_that_with_messages_cs( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_that( &
                condition, var_str(success_message), failure_message)
    end function

    pure function assert_that_with_messages_sc( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string, var_str

        logical, intent(in) :: condition
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_that( &
                condition, success_message, var_str(failure_message))
    end function

    pure function assert_that_with_messages_ss( &
            condition, success_message, failure_message) result(result__)
        use iso_varying_string, only: varying_string

        logical, intent(in) :: condition
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (condition) then
            result__ = succeed(with_user_message( &
                    THAT_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(with_user_message( &
                    THAT_FAILURE_MESSAGE, failure_message))
        end if
    end function

    function describe_basic_c(description, tests) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        allocate(item%test, source = simple_test_collection( &
                var_str(description), tests))
    end function

    function describe_basic_s(description, tests) result(item)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        allocate(item%test, source = simple_test_collection( &
                description, tests))
    end function

    function describe_with_input_c(description, input, tests) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        allocate(item%test, source = test_collection_with_input( &
                var_str(description), input, tests))
    end function

    function describe_with_input_s(description, input, tests) result(item)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        allocate(item%test, source = test_collection_with_input( &
                description, input, tests))
    end function

    pure function equals_within_absolute(expected, actual, tolerance)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        logical :: equals_within_absolute

        equals_within_absolute = abs(expected - actual) <= tolerance
    end function

    pure function equals_within_relative(expected, actual, tolerance)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        logical :: equals_within_relative

        equals_within_relative = &
                (abs(expected) <= MACHINE_TINY .and. abs(actual) <= MACHINE_TINY) &
                .or. (abs(expected - actual) / abs(expected) <= tolerance)
    end function

    function given_basic_c(description, tests) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, tests)
    end function

    function given_basic_s(description, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, tests)
    end function

    function given_with_input_c(description, input, tests) result(item)
        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests)
    end function

    function given_with_input_s(description, input, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests)
    end function

    function it_basic_c(description, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = simple_test_case(var_str(description), test))
    end function

    function it_basic_s(description, test) result(item)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = simple_test_case(description, test))
    end function

    function it_input_c(description, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = input_test_case(var_str(description), test))
    end function

    function it_input_s(description, test) result(item)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = input_test_case(description, test))
    end function

    function it_with_examples_c(description, examples, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = test_case_with_examples( &
                var_str(description), examples, test))
    end function

    function it_with_examples_s(description, examples, test) result(item)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = test_case_with_examples( &
                description, examples, test))
    end function

    function it_with_generator_c(description, generator, test) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = test_case_with_generator( &
                var_str(description), generator, test))
    end function

    function it_with_generator_s(description, generator, test) result(item)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        allocate(item%test, source = test_case_with_generator( &
                description, generator, test))
    end function

    subroutine run_tests(tests)
        use iso_fortran_env, only: error_unit, output_unit
        use iso_varying_string, only: operator(//), put_line
        use strff, only: to_string

        type(test_item_t), intent(in) :: tests

        double precision :: end_time
        type(filter_item_result_t) :: filtered_tests
        integer :: i
        type(options_t) :: options
        type(test_result_item_t) :: results
        double precision :: start_time
        logical, allocatable :: suite_failed[:]
        type(test_item_t) :: tests_to_run

        allocate(suite_failed[*])
        suite_failed = .false.

        options = get_options()

        if (options%filter_tests()) then
            filtered_tests = tests%filter(options%filter_string())
            if (filtered_tests%matched) then
                tests_to_run = filtered_tests%test
            else
                error stop "No matching tests found"
            end if
        else
            tests_to_run = tests
        end if

        if (this_image() == 1) then
            call put_line(output_unit, "Running Tests")
            call put_line(output_unit, "")

            if (.not.options%quiet()) then
                call put_line(output_unit, tests_to_run%description())
                call put_line(output_unit, "")
            end if

            call put_line( &
                    output_unit, &
                    "A total of " // to_string(tests_to_run%num_cases()) // " test cases")
            call put_line(output_unit, "")
        end if

        call cpu_time(start_time)
        results = tests_to_run%run()
        call cpu_time(end_time)

        critical ! report results one image at a time
            if (num_images() > 1) then
                call put_line(output_unit, "On image " // to_string(this_image()))
            end if
            if (results%passed()) then
                call put_line(output_unit, "All Passed")
                call put_line( &
                        output_unit, &
                        "Took " // to_string(end_time - start_time, 6) // " seconds")
                call put_line(output_unit, "")
                if (options%verbose()) then
                    call put_line( &
                            output_unit, &
                            results%verbose_description(options%colorize()))
                    call put_line(output_unit, "")
                end if
                call put_line( &
                        output_unit, &
                        "A total of " // to_string(results%num_cases()) &
                            // " test cases containing a total of " &
                            // to_string(results%num_asserts()) // " assertions")
                call put_line(output_unit, "")
            else
                call put_line(error_unit, "Failed")
                call put_line( &
                        error_unit, &
                        "Took " // to_string(end_time - start_time, 6) // " seconds")
                call put_line(error_unit, "")
                if (options%verbose()) then
                    call put_line( &
                            error_unit, &
                            results%verbose_description(options%colorize()))
                else
                    call put_line( &
                            error_unit, &
                            results%failure_description(options%colorize()))
                end if
                call put_line(error_unit, "")
                call put_line( &
                        error_unit, &
                        to_string(results%num_failing_cases()) // " of " &
                            // to_string(results%num_cases()) // " cases failed")
                call put_line( &
                        error_unit, &
                        to_string(results%num_failing_asserts()) // " of " &
                            // to_string(results%num_asserts()) // " assertions failed")
                call put_line(error_unit, "")
                suite_failed = .true.
            end if
        end critical
        sync all ! make sure all images have had a chance to record failure before checking for any
        if (this_image() == 1) then
            do i = 1, num_images()
                if (suite_failed[i]) error stop
            end do
        end if
    end subroutine

    function test_that(tests) result(item)
        type(test_item_t) :: tests(:)
        type(test_item_t) :: item

        item = describe("Test that", tests)
    end function

    function then_basic_c(description, test) result(item)
        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, test)
    end function

    function then_basic_s(description, test) result(item)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, test)
    end function

    function then_input_c(description, test) result(item)
        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it_("Then " // description, test)
    end function

    function then_input_s(description, test) result(item)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it_("Then " // description, test)
    end function

    function when_basic_c(description, tests) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, tests)
    end function

    function when_basic_s(description, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, tests)
    end function

    function when_with_transformer_c(description, transformer, tests) result(item)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        allocate(item%test, source = transforming_test_collection( &
                var_str("When " // description), transformer, tests))
    end function

    function when_with_transformer_s(description, transformer, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        allocate(item%test, source = transforming_test_collection( &
                "When " // description, transformer, tests))
    end function
end module
