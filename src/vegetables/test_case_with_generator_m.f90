module vegetables_test_case_with_generator_m
    use iso_varying_string, only: varying_string, operator(//), put_line, var_str
    use strff, only: operator(.includes.), to_string
    use vegetables_command_line_m, only: &
            DEBUG, MAX_SHRINK_ATTEMPTS, NUM_GENERATOR_TESTS
    use vegetables_generated_m, only: generated_t
    use vegetables_generator_m, only: generator_t
    use vegetables_input_m, only: input_t
    use vegetables_result_m, only: result_t, fail, succeed
    use vegetables_shrink_result_m, only: shrink_result_t
    use vegetables_test_m, only: &
            filter_result_t, test_t, filter_failed, filter_matched
    use vegetables_test_case_result_m, only: test_case_result_t
    use vegetables_test_interfaces_m, only: input_test_i
    use vegetables_test_result_item_m, only: test_result_item_t

    implicit none
    private
    public :: test_case_with_generator_t

    type, extends(test_t) :: test_case_with_generator_t
        private
        type(varying_string) :: description_
        class(generator_t), allocatable :: generator
        procedure(input_test_i), nopass, pointer :: test
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure, public :: run_with_input
        procedure, public :: run_without_input
    end type

    interface test_case_with_generator_t
        module procedure constructor
    end interface
contains
    function constructor(description, generator, test) result(test_case_with_generator)
        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_case_with_generator_t) :: test_case_with_generator

        test_case_with_generator%description_ = description
        allocate(test_case_with_generator%generator, source = generator)
        test_case_with_generator%test => test
    end function

    pure function description(self)
        class(test_case_with_generator_t), intent(in) :: self
        type(varying_string) :: description

        description = self%description_
    end function

    function filter(self, filter_string) result(filter_result)
        class(test_case_with_generator_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        if (self%description_.includes.filter_string) then
            filter_result = filter_matched(self)
        else
            filter_result = filter_failed()
        end if
    end function

    pure function num_cases(self)
        class(test_case_with_generator_t), intent(in) :: self
        integer :: num_cases

        associate(unused => self)
        end associate

        num_cases = 1
    end function

    function run_with_input(self, input) result(result_)
        class(test_case_with_generator_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(unused => input)
        end associate

        result_ = self%run()
    end function

    function run_without_input(self) result(result_)
        class(test_case_with_generator_t), intent(in) :: self
        type(test_result_item_t) :: result_

        type(generated_t) :: generated_value
        integer :: i
        type(result_t) :: new_result
        type(result_t) :: previous_result
        type(shrink_result_t) :: simpler_value

        if (DEBUG) call put_line( &
                "Beginning execution of: " // self%description_&
                // merge(" on image " // to_string(this_image()), var_str(""), num_images() > 1))
        do i = 1, NUM_GENERATOR_TESTS
            generated_value = self%generator%generate()
            previous_result = self%test(generated_value%input())
            if (.not.previous_result%passed()) exit
        end do
        if (i > NUM_GENERATOR_TESTS) then
            result_ = test_result_item_t(test_case_result_t( &
                    self%description_, &
                    succeed("Passed after " // to_string(NUM_GENERATOR_TESTS) // " examples")))
        else
            do i = 1, MAX_SHRINK_ATTEMPTS
                simpler_value = self%generator%shrink(generated_value%input())
                new_result = self%test(simpler_value%input())
                if (simpler_value%simplest()) then
                    if (new_result%passed()) then
                        result_ = test_result_item_t(test_case_result_t( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result))
                        return
                    else
                        result_ = test_result_item_t(test_case_result_t( &
                                self%description_, &
                                fail('Fails with the simplest possible example').and.new_result))
                        return
                    end if
                else
                    if (new_result%passed()) then
                        result_ = test_result_item_t(test_case_result_t( &
                                self%description_, &
                                fail('Found simplest example causing failure').and.previous_result))
                        return
                    else
                        previous_result = new_result
                        generated_value = generated_t(simpler_value%input())
                    end if
                end if
            end do
            result_ = test_result_item_t(test_case_result_t( &
                    self%description_, &
                    fail("Exhausted shrink attempts looking for simplest value causing failure").and.previous_result))
        end if
        if (DEBUG) call put_line( &
                "Completed execution of: " // self%description_&
                // merge(" on image " // to_string(this_image()), var_str(""), num_images() > 1))
    end function
end module
