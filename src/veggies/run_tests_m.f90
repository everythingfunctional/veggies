module veggies_run_tests_m
    use iso_fortran_env, only: error_unit, int64, output_unit
    use iso_varying_string, only: operator(//), put_line, var_str
    use strff, only: to_string
    use veggies_command_line_m, only: options_t, get_options, DEBUG
    use veggies_test_item_m, only: filter_item_result_t, test_item_t
    use veggies_test_result_item_m, only: test_result_item_t

    implicit none
    private
    public :: run_tests
contains
    subroutine run_tests(tests)
        type(test_item_t), intent(in) :: tests

        integer(int64) :: clock_rate
        real :: elapsed_time
        integer(int64) :: end_time
        type(filter_item_result_t) :: filtered_tests
        type(options_t) :: options
        type(test_result_item_t) :: results
        integer(int64) :: start_time
        logical :: suite_failed
        type(test_item_t) :: tests_to_run

        options = get_options()

        if (options%filter_tests()) then
            filtered_tests = tests%filter(options%filter_string())
            if (filtered_tests%matched()) then
                tests_to_run = filtered_tests%test()
            else
                call put_line(error_unit, "No matching tests found")
                stop 1
            end if
        else
            tests_to_run = tests
        end if

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

        if (DEBUG) call put_line("Beginning execution of test suite")
        call system_clock(start_time, clock_rate)
        results = tests_to_run%run()
        call system_clock(end_time)
        if (DEBUG) call put_line("Completed execution of test suite.")
        elapsed_time = real(end_time - start_time) / real(clock_rate)

        if (results%passed()) then
            call put_line(output_unit, "All Passed")
            call put_line( &
                    output_unit, &
                    "Took " // to_string(elapsed_time, 6) // " seconds")
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
                    "Took " // to_string(elapsed_time, 6) // " seconds")
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
            stop 1
        end if
    end subroutine
end module
