module vegetables_input_test_case_m
    use iso_varying_string, only: varying_string
    use vegetables_test_m, only: test_t
    use vegetables_test_interfaces_m, only: input_test_i

    implicit none
    private
    public :: input_test_case_t

    type, extends(test_t) :: input_test_case_t
        private
        type(varying_string) :: description_
        procedure(input_test_i), nopass, pointer :: test
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure, public :: run_with_input
        procedure, public :: run_without_input
    end type

    interface input_test_case_t
        module procedure constructor
    end interface
contains
    function constructor(description, test) result(input_test_case)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(input_test_case_t) :: input_test_case

        input_test_case%description_ = description
        input_test_case%test => test
    end function

    pure function description(self)
        use iso_varying_string, only: varying_string

        class(input_test_case_t), intent(in) :: self
        type(varying_string) :: description

        description = self%description_
    end function

    function filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)
        use vegetables_test_m, only: &
                filter_result_t, filter_failed, filter_matched

        class(input_test_case_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        if (self%description_.includes.filter_string) then
            filter_result = filter_matched(self)
        else
            filter_result = filter_failed()
        end if
    end function

    pure function num_cases(self)
        class(input_test_case_t), intent(in) :: self
        integer :: num_cases

        associate(unused => self)
        end associate

        num_cases = 1
    end function

    function run_with_input(self, input) result(result_)
        use vegetables_input_m, only: input_t
        use vegetables_test_case_result_m, only: test_case_result_t
        use vegetables_test_result_item_m, only: test_result_item_t

        class(input_test_case_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        result_ = test_result_item_t(test_case_result_t( &
                self%description_, self%test(input)))
    end function

    function run_without_input(self) result(result_)
        use vegetables_result_m, only: fail
        use vegetables_test_case_result_m, only: test_case_result_t
        use vegetables_test_result_item_m, only: test_result_item_t

        class(input_test_case_t), intent(in) :: self
        type(test_result_item_t) :: result_

        result_ = test_result_item_t(test_case_result_t( &
                self%description_, fail("No input provided")))
    end function
end module