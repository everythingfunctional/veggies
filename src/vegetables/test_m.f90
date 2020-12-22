module vegetables_test_m
    implicit none
    private
    public :: filter_result_t, test_t, filter_failed, filter_matched

    type, abstract :: test_t
    contains
        private
        procedure(test_description_i), public, deferred :: description
        procedure(filter_i), public, deferred :: filter
        procedure(test_count_i), public, deferred :: num_cases
        procedure(run_with_input_i), public, deferred :: run_with_input
        procedure(run_without_input_i), public, deferred :: run_without_input
        generic, public :: run => run_with_input, run_without_input
    end type

    type :: filter_result_t
        private
        class(test_t), allocatable :: test_
        logical :: matched_
    contains
        private
        procedure, public :: matched
        procedure, public :: test
    end type

    abstract interface
        function filter_i(self, filter_string) result(filter_result)
            use iso_varying_string, only: varying_string
            import test_t, filter_result_t

            implicit none

            class(test_t), intent(in) :: self
            type(varying_string), intent(in) :: filter_string
            type(filter_result_t) :: filter_result
        end function

        function run_with_input_i(self, input) result(result_)
            use vegetables_input_m, only: input_t
            use vegetables_test_result_item_m, only: test_result_item_t
            import test_t

            implicit none

            class(test_t), intent(in) :: self
            class(input_t), intent(in) :: input
            type(test_result_item_t) :: result_
        end function

        function run_without_input_i(self) result(result_)
            use vegetables_test_result_item_m, only: test_result_item_t
            import test_t

            implicit none

            class(test_t), intent(in) :: self
            type(test_result_item_t) :: result_
        end function

        pure function test_count_i(self) result(num)
            import test_t
            class(test_t), intent(in) :: self
            integer :: num
        end function

        pure function test_description_i(self) result(description)
            use iso_varying_string, only: varying_string
            import test_t

            implicit none

            class(test_t), intent(in) :: self
            type(varying_string) :: description
        end function
    end interface
contains
    function filter_failed()
        type(filter_result_t) :: filter_failed

        filter_failed%matched_ = .false.
    end function

    function filter_matched(test)
        class(test_t), intent(in) :: test
        type(filter_result_t) :: filter_matched

        allocate(filter_matched%test_, source = test)
        filter_matched%matched_ = .true.
    end function

    pure function matched(self)
        class(filter_result_t), intent(in) :: self
        logical :: matched

        matched = self%matched_
    end function

    function test(self)
        class(filter_result_t), intent(in) :: self
        class(test_t), allocatable :: test

        allocate(test, source = self%test_)
    end function
end module
