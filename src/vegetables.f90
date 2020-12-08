module vegetables
    use iso_varying_string, only: varying_string
    use vegetables_ascii_string_generator_m, only: ASCII_STRING_GENERATOR
    use vegetables_command_line_m, only: &
            options_t, get_options, NUM_GENERATOR_TESTS
    use vegetables_double_precision_input_m, only: double_precision_input_t
    use vegetables_example_m, only: example_t, example
    use vegetables_generated_m, only: generated_t, generated
    use vegetables_generator_m, only: generator_t
    use vegetables_individual_result_m, only: &
            individual_result_t, individual_result
    use vegetables_input_m, only: input_t
    use vegetables_integer_generator_m, only: INTEGER_GENERATOR
    use vegetables_integer_input_m, only: integer_input_t
    use vegetables_random_m, only: &
            get_random_ascii_character, &
            get_random_ascii_string, &
            get_random_ascii_string_with_max_length, &
            get_random_double_precision_with_magnitude, &
            get_random_double_precision_with_range, &
            get_random_integer, &
            get_random_integer_with_range, &
            get_random_logical
    use vegetables_result_m, only: result_t
    use vegetables_shrink_result_m, only: &
            shrink_result_t, shrunk_value, simplest_value
    use vegetables_string_input_m, only: string_input_t
    use vegetables_transformed_m, only: transformed_t, transformed

    implicit none
    private
    public :: &
            double_precision_input_t, &
            example_t, &
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
            test_case_t, &
            test_case_result_t, &
            test_case_with_examples_t, &
            test_case_with_generator_t, &
            test_collection_t, &
            test_collection_result_t, &
            test_collection_with_input_t, &
            test_item_t, &
            test_result_item_t, &
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

    type, abstract :: test_t
        private
        type(varying_string) :: description_
    contains
        private
        procedure(test_description_i), public, deferred :: description
        procedure(filter_i), public, deferred :: filter
        procedure(test_count_i), public, deferred :: num_cases
        procedure(run_with_input_i), deferred :: run_with_input
        procedure(run_without_input_i), deferred :: run_without_input
        generic :: run => run_with_input, run_without_input
    end type

    type :: test_item_t
        private
        class(test_t), allocatable :: test
    contains
        private
        procedure, public :: description => test_item_description
        procedure, public :: filter => test_item_filter
        procedure, public :: num_cases => test_item_num_cases
        procedure :: run_with_input => test_item_run_with_input
        procedure :: run_without_input => test_item_run_without_input
        generic, public :: run => run_with_input, run_without_input
    end type

    type, abstract, extends(test_t) :: test_case_t
    contains
        private
        procedure, public :: description => test_case_description
        procedure, public :: filter => test_case_filter
        procedure, public :: num_cases => test_case_num_cases
    end type

    type, extends(test_case_t) :: simple_test_case_t
        private
        procedure(simple_test_i), nopass, pointer :: test
    contains
        private
        procedure :: run_with_input => simple_test_case_run_with_input
        procedure :: run_without_input => simple_test_case_run_without_input
    end type

    type, extends(test_case_t) :: input_test_case_t
        private
        procedure(input_test_i), nopass, pointer :: test
    contains
        private
        procedure :: run_with_input => input_test_case_run_with_input
        procedure :: run_without_input => input_test_case_run_without_input
    end type

    type, extends(test_case_t) :: test_case_with_examples_t
        private
        type(example_t), allocatable :: examples(:)
        procedure(input_test_i), nopass, pointer :: test
    contains
        private
        procedure :: run_with_input => test_case_with_examples_run_with_input
        procedure :: run_without_input => test_case_with_examples_run_without_input
    end type

    type, extends(test_case_t) :: test_case_with_generator_t
        private
        class(generator_t), allocatable :: generator
        procedure(input_test_i), nopass, pointer :: test
    contains
        private
        procedure :: run_with_input => test_case_with_generator_run_with_input
        procedure :: run_without_input => test_case_with_generator_run_without_input
    end type

    type, abstract, extends(test_t) :: test_collection_t
        private
        type(test_item_t), allocatable :: tests(:)
    contains
        private
        procedure, public :: description => test_collection_description
        procedure, public :: filter => test_collection_filter
        procedure, public :: num_cases => test_collection_num_cases
    end type

    type, extends(test_collection_t) :: simple_test_collection_t
    contains
        private
        procedure :: run_with_input => simple_test_collection_run_with_input
        procedure :: run_without_input => simple_test_collection_run_without_input
    end type

    type, extends(test_collection_t) :: test_collection_with_input_t
        private
        class(input_t), allocatable :: input
    contains
        private
        procedure :: run_with_input => test_collection_with_input_run_with_input
        procedure :: run_without_input => test_collection_with_input_run_without_input
    end type

    type, extends(test_collection_t) :: transforming_test_collection_t
        private
        procedure(transformer_i), nopass, pointer :: transformer
    contains
        private
        procedure :: run_with_input => transforming_test_collection_run_with_input
        procedure :: run_without_input => transforming_test_collection_run_without_input
    end type

    type, abstract :: test_result_t
        private
        type(varying_string) :: description
    contains
        private
        procedure(test_result_count_i), public, deferred :: num_asserts
        procedure(test_result_count_i), public, deferred :: num_cases
        procedure(test_result_count_i), public, deferred :: num_failing_asserts
        procedure(test_result_count_i), public, deferred :: num_failing_cases
        procedure(test_result_passed_i), public, deferred :: passed
        procedure(test_result_colorized_description_i), public, deferred :: &
                failure_description
        procedure(test_result_colorized_description_i), public, deferred :: &
                verbose_description
    end type

    type :: test_result_item_t
        private
        class(test_result_t), allocatable :: result_
    contains
        private
        procedure, public :: num_asserts => test_result_item_num_asserts
        procedure, public :: num_cases => test_result_item_num_cases
        procedure, public :: num_failing_asserts => test_result_item_num_failing_asserts
        procedure, public :: num_failing_cases => test_result_item_num_failing_cases
        procedure, public :: passed => test_result_item_passed
        procedure, public :: failure_description => test_result_item_failure_description
        procedure, public :: verbose_description => test_result_item_verbose_description
    end type

    type, extends(test_result_t) :: test_case_result_t
        private
        type(result_t) :: result_
    contains
        private
        procedure, public :: num_asserts => test_case_result_num_asserts
        procedure, public :: num_cases => test_case_result_num_cases
        procedure, public :: num_failing_asserts => test_case_result_num_failing_asserts
        procedure, public :: num_failing_cases => test_case_result_num_failing_cases
        procedure, public :: passed => test_case_result_passed
        procedure, public :: failure_description => &
                test_case_result_failure_description
        procedure, public :: verbose_description => &
                test_case_result_verbose_description
    end type

    type, extends(test_result_t) :: test_collection_result_t
        private
        type(test_result_item_t), allocatable :: results(:)
    contains
        private
        procedure, public :: num_asserts => test_collection_result_num_asserts
        procedure, public :: num_cases => test_collection_result_num_cases
        procedure, public :: num_failing_asserts => &
                test_collection_result_num_failing_asserts
        procedure, public :: num_failing_cases => &
                test_collection_result_num_failing_cases
        procedure, public :: passed => test_collection_result_passed
        procedure, public :: failure_description => &
                test_collection_result_failure_description
        procedure, public :: verbose_description => &
                test_collection_result_verbose_description
    end type

    type :: filter_result_t
        class(test_t), allocatable :: test
        logical :: matched
    end type

    type, public :: filter_item_result_t
        type(test_item_t) :: test
        logical :: matched
    end type

    type, public, extends(input_t) :: transformation_failure_t
        type(result_t) :: result_
    end type

    abstract interface
        subroutine computation_i
        end subroutine

        pure function filter_i(self, filter_string) result(filter_result)
            import test_t, filter_result_t, varying_string
            class(test_t), intent(in) :: self
            type(varying_string), intent(in) :: filter_string
            type(filter_result_t) :: filter_result
        end function

        function input_test_i(input) result(result_)
            import input_t, result_t
            class(input_t), intent(in) :: input
            type(result_t) :: result_
        end function

        function run_with_input_i(self, input) result(result_)
            import input_t, test_t, test_result_item_t
            class(test_t), intent(in) :: self
            class(input_t), intent(in) :: input
            type(test_result_item_t) :: result_
        end function

        function run_without_input_i(self) result(result_)
            import test_t, test_result_item_t
            class(test_t), intent(in) :: self
            type(test_result_item_t) :: result_
        end function

        function simple_test_i() result(result_)
            import result_t
            type(result_t) :: result_
        end function

        pure function test_count_i(self) result(num)
            import test_t
            class(test_t), intent(in) :: self
            integer :: num
        end function

        pure function test_description_i(self) result(description)
            import test_t, varying_string
            class(test_t), intent(in) :: self
            type(varying_string) :: description
        end function

        pure function test_result_colorized_description_i( &
                self, colorize) result(description)
            import test_result_t, varying_string
            class(test_result_t), intent(in) :: self
            logical, intent(in) :: colorize
            type(varying_string) :: description
        end function

        pure function test_result_count_i(self) result(num)
            import test_result_t
            class(test_result_t), intent(in) :: self
            integer :: num
        end function

        pure function test_result_passed_i(self) result(passed)
            import test_result_t
            class(test_result_t), intent(in) :: self
            logical :: passed
        end function

        function transformer_i(input) result(output)
            import input_t, transformed_t
            class(input_t), intent(in) :: input
            type(transformed_t) :: output
        end function
    end interface

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

    interface delimit
        module procedure delimit_c
        module procedure delimit_s
    end interface

    interface describe
        module procedure describe_basic_c
        module procedure describe_basic_s
        module procedure describe_with_input_c
        module procedure describe_with_input_s
    end interface

    interface fail
        module procedure fail_c
        module procedure fail_s
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

    interface make_doesnt_include_failure_message
        module procedure make_doesnt_include_failure_message_cc
        module procedure make_doesnt_include_failure_message_cs
        module procedure make_doesnt_include_failure_message_sc
        module procedure make_doesnt_include_failure_message_ss
    end interface

    interface make_doesnt_include_success_message
        module procedure make_doesnt_include_success_message_cc
        module procedure make_doesnt_include_success_message_cs
        module procedure make_doesnt_include_success_message_sc
        module procedure make_doesnt_include_success_message_ss
    end interface

    interface make_empty_failure_message
        module procedure make_empty_failure_message_c
        module procedure make_empty_failure_message_s
    end interface

    interface make_equals_failure_message
        module procedure make_equals_failure_message_cc
        module procedure make_equals_failure_message_cs
        module procedure make_equals_failure_message_sc
        module procedure make_equals_failure_message_ss
    end interface

    interface make_equals_success_message
        module procedure make_equals_success_message_c
        module procedure make_equals_success_message_s
    end interface

    interface make_faster_than_failure_message
        module procedure make_faster_than_failure_message_ccc
        module procedure make_faster_than_failure_message_ccs
        module procedure make_faster_than_failure_message_csc
        module procedure make_faster_than_failure_message_css
        module procedure make_faster_than_failure_message_scc
        module procedure make_faster_than_failure_message_scs
        module procedure make_faster_than_failure_message_ssc
        module procedure make_faster_than_failure_message_sss
    end interface

    interface make_faster_than_success_message
        module procedure make_faster_than_success_message_ccc
        module procedure make_faster_than_success_message_ccs
        module procedure make_faster_than_success_message_csc
        module procedure make_faster_than_success_message_css
        module procedure make_faster_than_success_message_scc
        module procedure make_faster_than_success_message_scs
        module procedure make_faster_than_success_message_ssc
        module procedure make_faster_than_success_message_sss
    end interface

    interface make_includes_failure_message
        module procedure make_includes_failure_message_cc
        module procedure make_includes_failure_message_cs
        module procedure make_includes_failure_message_sc
        module procedure make_includes_failure_message_ss
    end interface

    interface make_includes_success_message
        module procedure make_includes_success_message_cc
        module procedure make_includes_succes_message_cs
        module procedure make_includes_succes_message_sc
        module procedure make_includes_succes_message_ss
    end interface

    interface make_within_failure_message
        module procedure make_within_failure_message_ccc
        module procedure make_within_failure_message_ccs
        module procedure make_within_failure_message_csc
        module procedure make_within_failure_message_css
        module procedure make_within_failure_message_scc
        module procedure make_within_failure_message_scs
        module procedure make_within_failure_message_ssc
        module procedure make_within_failure_message_sss
    end interface

    interface make_within_success_message
        module procedure make_within_success_message_ccc
        module procedure make_within_success_message_ccs
        module procedure make_within_success_message_csc
        module procedure make_within_success_message_css
        module procedure make_within_success_message_scc
        module procedure make_within_success_message_scs
        module procedure make_within_success_message_ssc
        module procedure make_within_success_message_sss
    end interface

    interface succeed
        module procedure succeed_c
        module procedure succeed_s
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

    interface with_user_message
        module procedure with_user_message_cc
        module procedure with_user_message_cs
        module procedure with_user_message_sc
        module procedure with_user_message_ss
    end interface

    character(len=*), parameter, public :: EMPTY_SUCCESS_MESSAGE = "String was empty"
    integer, parameter :: INDENTATION = 4
    double precision, parameter :: MACHINE_EPSILON = epsilon(0.0d0)
    double precision, parameter :: MACHINE_TINY = tiny(0.0d0)
    character(len=*), parameter, public :: NOT_FAILURE_MESSAGE = "Expected to not be true"
    character(len=*), parameter, public :: NOT_SUCCESS_MESSAGE = "Was not true"
    character(len=*), parameter, public :: THAT_FAILURE_MESSAGE = "Expected to be true"
    character(len=*), parameter, public :: THAT_SUCCESS_MESSAGE = "Was true"
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

    pure function delimit_c(string) result(delimited)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: string
        type(varying_string) :: delimited

        delimited = delimit(var_str(string))
    end function

    pure function delimit_s(string) result(delimited)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: string
        type(varying_string) :: delimited

        delimited = "|" // string // "|"
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

    pure function fail_c(message) result(failure)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: message
        type(result_t) :: failure

        failure = fail(var_str(message))
    end function

    pure function fail_s(message) result(failure)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: message
        type(result_t) :: failure

        allocate(failure%results(1))
        failure%results(1) = individual_result(message, .false.)
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

    function input_test_case(description, test)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(input_test_case_t) :: input_test_case

        input_test_case%description_ = description
        input_test_case%test => test
    end function

    function input_test_case_run_with_input(self, input) result(result_)
        class(input_test_case_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        allocate(result_%result_, source = test_case_result( &
                self%description_, self%test(input)))
    end function

    function input_test_case_run_without_input(self) result(result_)
        class(input_test_case_t), intent(in) :: self
        type(test_result_item_t) :: result_

        allocate(result_%result_, source = test_case_result( &
                self%description_, fail("No input provided")))
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

    pure function make_doesnt_include_failure_message_cc( &
            search_for, string) result(message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_failure_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_doesnt_include_failure_message_cs( &
            search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_failure_message( &
                var_str(search_for), string)
    end function

    pure function make_doesnt_include_failure_message_sc( &
            search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_failure_message( &
                search_for, var_str(string))
    end function

    pure function make_doesnt_include_failure_message_ss( &
            search_for, string) result(message)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = hanging_indent( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "to not include" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_doesnt_include_success_message_cc( &
            search_for, string) result(message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_success_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_doesnt_include_success_message_cs( &
            search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_success_message( &
                var_str(search_for), string)
    end function

    pure function make_doesnt_include_success_message_sc( &
            search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_success_message( &
                search_for, var_str(string))
    end function

    pure function make_doesnt_include_success_message_ss( &
            search_for, string) result(message)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = hanging_indent( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "did not include" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_empty_failure_message_c(string) result(message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_empty_failure_message(var_str(string))
    end function

    pure function make_empty_failure_message_s(string) result(message)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = hanging_indent( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "wasn't empty", &
                INDENTATION)
    end function

    pure function make_equals_failure_message_cc(expected, actual) result(message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string) :: message

        message = make_equals_failure_message(var_str(expected), var_str(actual))
    end function

    pure function make_equals_failure_message_cs(expected, actual) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string) :: message

        message = make_equals_failure_message(var_str(expected), actual)
    end function

    pure function make_equals_failure_message_sc(expected, actual) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string) :: message

        message = make_equals_failure_message(expected, var_str(actual))
    end function

    pure function make_equals_failure_message_ss(expected, actual) result(message)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string) :: message

        message = hanging_indent( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(expected, 1)), &
                            INDENTATION) // NEWLINE &
                    // "but got" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(actual, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_equals_success_message_c(expected) result(message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: expected
        type(varying_string) :: message

        message = make_equals_success_message(var_str(expected))
    end function

    pure function make_equals_success_message_s(expected) result(message)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: expected
        type(varying_string) :: message

        message = hanging_indent( &
                "Expected and got" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(expected, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_faster_than_failure_message_ccc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_failure_message_ccs( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), var_str(actual), iterations)
    end function

    pure function make_faster_than_failure_message_csc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), actual, var_str(iterations))
    end function

    pure function make_faster_than_failure_message_css( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), actual, iterations)
    end function

    pure function make_faster_than_failure_message_scc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                reference, var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_failure_message_scs( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                reference, var_str(actual), iterations)
    end function

    pure function make_faster_than_failure_message_ssc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                reference, actual, var_str(iterations))
    end function

    pure function make_faster_than_failure_message_sss( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = &
                "Computation took " // actual &
                // ", which was slower than the reference time of " &
                // reference // ", averaged over " // iterations // " iterations."
    end function

    pure function make_faster_than_success_message_ccc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_success_message_ccs( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), var_str(actual), iterations)
    end function

    pure function make_faster_than_success_message_csc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), actual, var_str(iterations))
    end function

    pure function make_faster_than_success_message_css( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), actual, iterations)
    end function

    pure function make_faster_than_success_message_scc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                reference, var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_success_message_scs( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                reference, var_str(actual), iterations)
    end function

    pure function make_faster_than_success_message_ssc( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                reference, actual, var_str(iterations))
    end function

    pure function make_faster_than_success_message_sss( &
            reference, actual, iterations) result(message)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = &
                "Computation took " // actual &
                // ", which was faster than the reference time of " &
                // reference // ", averaged over " // iterations // " iterations."
    end function

    pure function make_includes_failure_message_cc(search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_failure_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_includes_failure_message_cs(search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_failure_message(var_str(search_for), string)
    end function

    pure function make_includes_failure_message_sc(search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_failure_message(search_for, var_str(string))
    end function

    pure function make_includes_failure_message_ss(search_for, string) result(message)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = hanging_indent( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "to include" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_includes_success_message_cc(search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_success_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_includes_succes_message_cs(search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_success_message(var_str(search_for), string)
    end function

    pure function make_includes_succes_message_sc(search_for, string) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_success_message(search_for, var_str(string))
    end function

    pure function make_includes_succes_message_ss(search_for, string) result(message)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = hanging_indent( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "included" // NEWLINE &
                    // indent( &
                            delimit(hanging_indent(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_within_failure_message_ccc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), var_str(actual), var_str(tolerance))
    end function

    pure function make_within_failure_message_ccs( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), var_str(actual), tolerance)
    end function

    pure function make_within_failure_message_csc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), actual, var_str(tolerance))
    end function

    pure function make_within_failure_message_css( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), actual, tolerance)
    end function

    pure function make_within_failure_message_scc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                expected, var_str(actual), var_str(tolerance))
    end function

    pure function make_within_failure_message_scs( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                expected, var_str(actual), tolerance)
    end function

    pure function make_within_failure_message_ssc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                expected, actual, var_str(tolerance))
    end function

    pure function make_within_failure_message_sss( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = &
                "Expected " // delimit(actual) // " to be within " &
                // delimit("±" // tolerance) // " of " // delimit(expected)
    end function

    pure function make_within_success_message_ccc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), var_str(actual), var_str(tolerance))
    end function

    pure function make_within_success_message_ccs( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), var_str(actual), tolerance)
    end function

    pure function make_within_success_message_csc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), actual, var_str(tolerance))
    end function

    pure function make_within_success_message_css( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), actual, tolerance)
    end function

    pure function make_within_success_message_scc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                expected, var_str(actual), var_str(tolerance))
    end function

    pure function make_within_success_message_scs( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                expected, var_str(actual), tolerance)
    end function

    pure function make_within_success_message_ssc( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                expected, actual, var_str(tolerance))
    end function

    pure function make_within_success_message_sss( &
            expected, actual, tolerance) result(message)
        use iso_varying_string, only: varying_string, operator(//)

        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = &
                delimit(actual) // " was within " // delimit("±" // tolerance) &
                // " of " // delimit(expected)
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

    function simple_test_case(description, test)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(simple_test_case_t) :: simple_test_case

        simple_test_case%description_ = description
        simple_test_case%test => test
    end function

    function simple_test_case_run_with_input(self, input) result(result_)
        class(simple_test_case_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function

    function simple_test_case_run_without_input(self) result(result_)
        class(simple_test_case_t), intent(in) :: self
        type(test_result_item_t) :: result_

        allocate(result_%result_, source = test_case_result( &
                self%description_, self%test()))
    end function

    function simple_test_collection(description, tests)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(simple_test_collection_t) :: simple_test_collection

        simple_test_collection%description_ = description
        allocate(simple_test_collection%tests, source = tests)
    end function

    recursive function simple_test_collection_run_with_input(self, input) result(result_)
        class(simple_test_collection_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function

    recursive function simple_test_collection_run_without_input(self) result(result_)
        class(simple_test_collection_t), intent(in) :: self
        type(test_result_item_t) :: result_

        integer :: i
        type(test_result_item_t) :: results(size(self%tests))

        do i = 1, size(self%tests)
            results(i) = self%tests(i)%run()
        end do
        allocate(result_%result_, source = test_collection_result( &
                self%description_, results))
    end function

    pure function succeed_c(message) result(success)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: message
        type(result_t) :: success

        success = succeed(var_str(message))
    end function

    pure function succeed_s(message) result(success)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: message
        type(result_t) :: success

        allocate(success%results(1))
        success%results(1) = individual_result(message, .true.)
    end function

    pure function test_case_description(self) result(description)
        use iso_varying_string, only: varying_string

        class(test_case_t), intent(in) :: self
        type(varying_string) :: description

        description = self%description_
    end function

    pure function test_case_filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)

        class(test_case_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        if (self%description_.includes.filter_string) then
            filter_result%matched = .true.
            allocate(filter_result%test, source = self)
        else
            filter_result%matched = .false.
        end if
    end function

    pure function test_case_num_cases(self) result(num_cases)
        class(test_case_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate

        num_cases = 1
    end function

    pure function test_case_result(description, result_)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        type(result_t), intent(in) :: result_
        type(test_case_result_t) :: test_case_result

        test_case_result%description = description
        test_case_result%result_ = result_
    end function

    pure function test_case_result_failure_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string, assignment(=), operator(//)
        use strff, only: hanging_indent, NEWLINE

        class(test_case_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        if (self%passed()) then
            description = ""
        else
            description = hanging_indent( &
                    self%description // NEWLINE &
                        // self%result_%failure_description(colorize), &
                    INDENTATION)
        end if
    end function

    pure function test_case_result_num_asserts(self) result(num_asserts)
        class(test_case_result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_asserts()
    end function

    pure function test_case_result_num_cases(self) result(num_cases)
        class(test_case_result_t), intent(in) :: self
        integer :: num_cases

        associate(a => self)
        end associate

        num_cases = 1
    end function

    pure function test_case_result_num_failing_asserts(self) result(num_asserts)
        class(test_case_result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_failing_asserts()
    end function

    pure function test_case_result_num_failing_cases(self) result(num_cases)
        class(test_case_result_t), intent(in) :: self
        integer :: num_cases

        if (self%passed()) then
            num_cases = 0
        else
            num_cases = 1
        end if
    end function

    pure function test_case_result_passed(self) result(passed)
        class(test_case_result_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function

    pure function test_case_result_verbose_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, NEWLINE

        class(test_case_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = hanging_indent( &
                self%description // NEWLINE &
                    // self%result_%verbose_description(colorize), &
                INDENTATION)
    end function

    function test_case_with_examples(description, examples, test)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_case_with_examples_t) :: test_case_with_examples

        test_case_with_examples%description_ = description
        allocate(test_case_with_examples%examples, source = examples)
        test_case_with_examples%test => test
    end function

    function test_case_with_examples_run_with_input(self, input) result(result_)
        class(test_case_with_examples_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function

    function test_case_with_examples_run_without_input(self) result(result_)
        class(test_case_with_examples_t), intent(in) :: self
        type(test_result_item_t) :: result_

        integer :: i
        type(result_t) :: results

        do i = 1, size(self%examples)
            results = results.and.self%test(self%examples(i)%input)
        end do
        allocate(result_%result_, source = test_case_result( &
                self%description_, results))
    end function

    function test_case_with_generator(description, generator, test)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_case_with_generator_t) :: test_case_with_generator

        test_case_with_generator%description_ = description
        allocate(test_case_with_generator%generator, source = generator)
        test_case_with_generator%test => test
    end function

    function test_case_with_generator_run_with_input(self, input) result(result_)
        class(test_case_with_generator_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function

    function test_case_with_generator_run_without_input(self) result(result_)
        use iso_varying_string, only: operator(//)
        use strff, only: to_string

        class(test_case_with_generator_t), intent(in) :: self
        type(test_result_item_t) :: result_

        type(generated_t) :: generated_value
        integer :: i
        type(result_t) :: new_result
        type(result_t) :: previous_result
        type(shrink_result_t) :: simpler_value

        do i = 1, NUM_GENERATOR_TESTS
            generated_value = self%generator%generate()
            previous_result = self%test(generated_value%input)
            if (.not.previous_result%passed()) exit
        end do
        if (i > NUM_GENERATOR_TESTS) then
            allocate(result_%result_, source = test_case_result( &
                    self%description_, &
                    succeed("Passed after " // to_string(NUM_GENERATOR_TESTS) // " examples")))
        else
            do
                simpler_value = self%generator%shrink(generated_value%input)
                if (simpler_value%simplest) then
                    new_result = self%test(simpler_value%input)
                    if (new_result%passed()) then
                        allocate(result_%result_, source = test_case_result( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result))
                        return
                    else
                        allocate(result_%result_, source = test_case_result( &
                                self%description_, &
                                fail('Fails with the simplest possible example').and.new_result))
                        return
                    end if
                else
                    new_result = self%test(simpler_value%input)
                    if (new_result%passed()) then
                        allocate(result_%result_, source = test_case_result( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result))
                        return
                    else
                        previous_result = new_result
                        generated_value = generated(simpler_value%input)
                    end if
                end if
            end do
        end if
    end function

    pure recursive function test_collection_description(self) result(description)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, join, NEWLINE

        class(test_collection_t), intent(in) :: self
        type(varying_string) :: description

        integer :: i

        description = hanging_indent( &
                self%description_ // NEWLINE // join( &
                        [(self%tests(i)%description(), i = 1, size(self%tests))], &
                        NEWLINE), &
                INDENTATION)
    end function

    pure recursive function test_collection_filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)

        class(test_collection_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        class(test_collection_t), allocatable :: new_collection
        type(filter_item_result_t) :: filter_results(size(self%tests))
        integer :: i
        logical :: matches(size(self%tests))
        type(test_item_t) :: maybe_tests(size(self%tests))

        if (self%description_.includes.filter_string) then
            filter_result%matched = .true.
            allocate(filter_result%test, source = self)
        else
            filter_results = [(self%tests(i)%filter(filter_string), i = 1, size(self%tests))]
            if (any(filter_results%matched)) then
                matches = filter_results%matched
                maybe_tests = filter_results%test
                allocate(new_collection, source = self)
                deallocate(new_collection%tests)
                allocate(new_collection%tests, source = &
                        pack(maybe_tests, mask=matches))
                filter_result%matched = .true.
                allocate(filter_result%test, source = new_collection)
            else
                filter_result%matched = .false.
            end if
        end if
    end function

    pure recursive function test_collection_num_cases(self) result(num_cases)
        class(test_collection_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%tests(i)%num_cases(), i = 1, size(self%tests))])
    end function

    pure function test_collection_result(description, results)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        type(test_result_item_t), intent(in) :: results(:)
        type(test_collection_result_t) :: test_collection_result

        test_collection_result%description = description
        allocate(test_collection_result%results, source = results)
    end function

    pure recursive function test_collection_result_failure_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string, assignment(=), operator(//)
        use strff, only: hanging_indent, join, NEWLINE

        class(test_collection_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        integer :: i

        if (self%passed()) then
            description = ""
        else
            description = hanging_indent( &
                    self%description // NEWLINE // join( &
                            [(self%results(i)%failure_description(colorize), i = 1, size(self%results))], &
                            NEWLINE), &
                    INDENTATION)
        end if
    end function

    pure recursive function test_collection_result_num_asserts(self) result(num_asserts)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_asserts

        integer :: i

        num_asserts = sum([(self%results(i)%num_asserts(), i = 1, size(self%results))])
    end function

    pure recursive function test_collection_result_num_cases(self) result(num_cases)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%results(i)%num_cases(), i = 1, size(self%results))])
    end function

    pure recursive function test_collection_result_num_failing_asserts(self) result(num_asserts)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_asserts

        integer :: i

        num_asserts = sum([(self%results(i)%num_failing_asserts(), i = 1, size(self%results))])
    end function

    pure recursive function test_collection_result_num_failing_cases(self) result(num_cases)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%results(i)%num_failing_cases(), i = 1, size(self%results))])
    end function

    pure recursive function test_collection_result_passed(self) result(passed)
        class(test_collection_result_t), intent(in) :: self
        logical :: passed

        integer :: i

        passed = all([(self%results(i)%passed(), i = 1, size(self%results))])
    end function

    pure recursive function test_collection_result_verbose_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, join, NEWLINE

        class(test_collection_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        integer :: i

        description = hanging_indent( &
                self%description // NEWLINE // join( &
                        [(self%results(i)%verbose_description(colorize), i = 1, size(self%results))], &
                        NEWLINE), &
                INDENTATION)
    end function

    function test_collection_with_input(description, input, tests)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_collection_with_input_t) :: test_collection_with_input

        test_collection_with_input%description_ = description
        allocate(test_collection_with_input%input, source = input)
        allocate(test_collection_with_input%tests, source = tests)
    end function

    recursive function test_collection_with_input_run_with_input(self, input) result(result_)
        class(test_collection_with_input_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(a => input)
        end associate

        result_ = self%run()
    end function

    recursive function test_collection_with_input_run_without_input(self) result(result_)
        class(test_collection_with_input_t), intent(in) :: self
        type(test_result_item_t) :: result_

        integer :: i
        type(test_result_item_t) :: results(size(self%tests))

        do i = 1, size(self%tests)
            results(i) = self%tests(i)%run(self%input)
        end do
        allocate(result_%result_, source = test_collection_result( &
                self%description_, results))
    end function

    pure recursive function test_item_description(self) result(description)
        use iso_varying_string, only: varying_string

        class(test_item_t), intent(in) :: self
        type(varying_string) :: description

        description = self%test%description()
    end function

    pure recursive function test_item_filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string

        class(test_item_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_item_result_t) :: filter_result

        type(filter_result_t) :: test_filter_result

        test_filter_result = self%test%filter(filter_string)
        if (test_filter_result%matched) then
            filter_result%matched = .true.
            allocate(filter_result%test%test, source = test_filter_result%test)
        else
            filter_result%matched = .false.
        end if
    end function

    pure recursive function test_item_num_cases(self) result(num_cases)
        class(test_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%test%num_cases()
    end function

    recursive function test_item_run_with_input(self, input) result(result_)
        class(test_item_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        result_ = self%test%run(input)
    end function

    recursive function test_item_run_without_input(self) result(result_)
        class(test_item_t), intent(in) :: self
        type(test_result_item_t) :: result_

        result_ = self%test%run()
    end function

    pure recursive function test_result_item_failure_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string

        class(test_result_item_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = self%result_%failure_description(colorize)
    end function

    pure recursive function test_result_item_num_asserts(self) result(num_asserts)
        class(test_result_item_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_asserts()
    end function

    pure recursive function test_result_item_num_cases(self) result(num_cases)
        class(test_result_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%num_cases()
    end function

    pure recursive function test_result_item_num_failing_asserts(self) result(num_asserts)
        class(test_result_item_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_failing_asserts()
    end function

    pure recursive function test_result_item_num_failing_cases(self) result(num_cases)
        class(test_result_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%num_failing_cases()
    end function

    pure recursive function test_result_item_passed(self) result(passed)
        class(test_result_item_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function

    pure recursive function test_result_item_verbose_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string

        class(test_result_item_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = self%result_%verbose_description(colorize)
    end function

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

    function transforming_test_collection(description, transformer, tests)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(transforming_test_collection_t) :: transforming_test_collection

        transforming_test_collection%description_ = description
        transforming_test_collection%transformer => transformer
        allocate(transforming_test_collection%tests, source = tests)
    end function

    recursive function transforming_test_collection_run_with_input(self, input) result(result_)
        class(transforming_test_collection_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        integer :: i
        type(test_result_item_t) :: results(size(self%tests))
        type(transformed_t) :: transformed_

        transformed_ = self%transformer(input)
        select type (transformed_input => transformed_%input)
        type is (transformation_failure_t)
            allocate(result_%result_, source = test_case_result( &
                    self%description_, transformed_input%result_))
        class default
            do i = 1, size(self%tests)
                results(i) = self%tests(i)%run(transformed_input)
            end do
            allocate(result_%result_, source = test_collection_result( &
                    self%description_, results))
        end select
    end function

    function transforming_test_collection_run_without_input(self) result(result_)
        class(transforming_test_collection_t), intent(in) :: self
        type(test_result_item_t) :: result_

        allocate(result_%result_, source = test_case_result( &
                self%description_, fail("No input provided")))
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

    pure function with_user_message_cc(message, user_message) result(whole_message)
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(varying_string) :: whole_message

        whole_message = with_user_message( &
                var_str(message), var_str(user_message))
    end function

    pure function with_user_message_cs(message, user_message) result(whole_message)
        use iso_varying_string, only: varying_string, var_str

        character(len=*), intent(in) :: message
        type(varying_string), intent(in) :: user_message
        type(varying_string) :: whole_message

        whole_message = with_user_message(var_str(message), user_message)
    end function

    pure function with_user_message_sc(message, user_message) result(whole_message)
        use iso_varying_string, only: varying_string, var_str

        type(varying_string), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(varying_string) :: whole_message

        whole_message = with_user_message( &
                message, var_str(user_message))
    end function

    pure function with_user_message_ss(message, user_message) result(whole_message)
        use iso_varying_string, only: varying_string, operator(//), operator(==)
        use strff, only: hanging_indent, indent, NEWLINE

        type(varying_string), intent(in) :: message
        type(varying_string), intent(in) :: user_message
        type(varying_string) :: whole_message

        if (user_message == "") then
            whole_message = message
        else
            whole_message = &
                    message // NEWLINE &
                    // indent( &
                            hanging_indent( &
                                    "User Message:" // NEWLINE &
                                        // delimit(hanging_indent( &
                                                user_message, 1)), &
                                    INDENTATION), &
                            INDENTATION)
        end if
    end function
end module
