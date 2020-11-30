program make_vegetable_driver
    use iso_varying_string, only: varying_string, assignment(=)
    use make_driver_m, only: make_driver

    implicit none

    character(len=1000) :: argument
    type(varying_string) :: driver_file
    integer :: i
    integer :: num_arguments
    character(len=100) :: program_name
    type(varying_string), allocatable :: test_files(:)

    num_arguments = command_argument_count()
    if (num_arguments < 2) then
        call get_command_argument(0, program_name)
        print *, "Usage: " // trim(program_name) // " driver_name test_file [more [test [files [...]]]]"
        error stop
    else
        allocate(test_files(num_arguments - 1))
        call get_command_argument(1, argument)
        driver_file = trim(argument)
        do i = 1, num_arguments - 1
            call get_command_argument(i+1, argument)
            test_files(i) = trim(argument)
        end do
    end if

    call make_driver(driver_file, test_files)
end program
