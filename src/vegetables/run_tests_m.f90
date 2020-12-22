module vegetables_run_tests_m
    implicit none
    private
    public :: run_tests
contains
    subroutine run_tests(tests)
        use iso_fortran_env, only: error_unit, output_unit
        use iso_varying_string, only: operator(//), put_line
        use strff, only: to_string
        use vegetables_command_line_m, only: options_t, get_options
        use vegetables_test_item_m, only: filter_item_result_t, test_item_t
        use vegetables_test_result_item_m, only: test_result_item_t

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
            if (filtered_tests%matched()) then
                tests_to_run = filtered_tests%test()
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
end module
