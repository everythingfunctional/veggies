program make_driver
    use iso_varying_string, only: VARYING_STRING, assignment(=)
    use make_driver_m, only: makeDriver

    implicit none

    character(len=1000) :: argument
    type(VARYING_STRING) :: driver_file
    integer :: i
    integer :: num_arguments
    character(len=100) :: program_name
    type(VARYING_STRING), allocatable :: test_files(:)

    num_arguments = command_argument_count()
    if (num_arguments < 2) then
        call get_command_argument(0, program_name)
        print *, "Usage: " // trim(program_name) // " driver_name test_file [more [test [files [...]]]]"
        call exit(1)
    else
        allocate(test_files(num_arguments - 1))
        call get_command_argument(1, argument)
        driver_file = trim(argument)
        do i = 1, num_arguments - 1
            call get_command_argument(i+1, argument)
            test_files(i) = trim(argument)
        end do
    end if

    call makeDriver(driver_file, test_files)
end program make_driver
