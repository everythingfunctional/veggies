module vegetables_test_item_m
    use vegetables_test_m, only: test_t

    implicit none
    private
    public :: filter_item_result_t, test_item_t

    type :: test_item_t
        private
        class(test_t), allocatable :: test
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure :: run_with_input
        procedure :: run_without_input
        generic, public :: run => run_with_input, run_without_input
    end type

    type :: filter_item_result_t
        type(test_item_t) :: test
        logical :: matched
    end type

    interface test_item_t
        module procedure constructor
    end interface
contains
    function constructor(test) result(test_item)
        class(test_t), intent(in) :: test
        type(test_item_t) :: test_item

        allocate(test_item%test, source = test)
    end function

    pure recursive function description(self)
        use iso_varying_string, only: varying_string

        class(test_item_t), intent(in) :: self
        type(varying_string) :: description

        description = self%test%description()
    end function

    recursive function filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string
        use vegetables_test_m, only: filter_result_t

        class(test_item_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_item_result_t) :: filter_result

        type(filter_result_t) :: result_

        result_ = self%test%filter(filter_string)
        if (result_%matched()) then
            filter_result%matched = .true.
            allocate(filter_result%test%test, source = result_%test())
        else
            filter_result%matched = .false.
        end if
    end function

    pure recursive function num_cases(self)
        class(test_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%test%num_cases()
    end function

    recursive function run_with_input(self, input) result(result_)
        use vegetables_input_m, only: input_t
        use vegetables_test_result_item_m, only: test_result_item_t

        class(test_item_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        result_ = self%test%run(input)
    end function

    recursive function run_without_input(self) result(result_)
        use vegetables_test_result_item_m, only: test_result_item_t

        class(test_item_t), intent(in) :: self
        type(test_result_item_t) :: result_

        result_ = self%test%run()
    end function
end module
