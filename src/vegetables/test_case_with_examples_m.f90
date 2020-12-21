module vegetables_test_case_with_examples_m
    use iso_varying_string, only: varying_string
    use vegetables_example_m, only: example_t
    use vegetables_test_m, only: test_t
    use vegetables_test_interfaces_m, only: input_test_i

    implicit none
    private
    public :: test_case_with_examples_t, test_case_with_examples

    type, extends(test_t) :: test_case_with_examples_t
        private
        type(varying_string) :: description_
        type(example_t), allocatable :: examples(:)
        procedure(input_test_i), nopass, pointer :: test
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure, public :: run_with_input
        procedure, public :: run_without_input
    end type
contains
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

    pure function description(self)
        use iso_varying_string, only: varying_string

        class(test_case_with_examples_t), intent(in) :: self
        type(varying_string) :: description

        description = self%description_
    end function

    function filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)
        use vegetables_test_m, only: filter_result_t

        class(test_case_with_examples_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        if (self%description_.includes.filter_string) then
            filter_result%matched = .true.
            allocate(filter_result%test, source = self)
        else
            filter_result%matched = .false.
        end if
    end function

    pure function num_cases(self)
        class(test_case_with_examples_t), intent(in) :: self
        integer :: num_cases

        associate(unused => self)
        end associate

        num_cases = 1
    end function

    function run_with_input(self, input) result(result_)
        use vegetables_input_m, only: input_t
        use vegetables_test_result_item_m, only: test_result_item_t

        class(test_case_with_examples_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(unused => input)
        end associate

        result_ = self%run()
    end function

    function run_without_input(self) result(result_)
        use vegetables_result_m, only: result_t
        use vegetables_test_case_result_m, only: test_case_result
        use vegetables_test_result_item_m, only: test_result_item_t

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
end module
