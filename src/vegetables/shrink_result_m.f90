module vegetables_shrink_result_m
    use vegetables_input_m, only: input_t

    implicit none
    private
    public :: shrink_result_t, shrunk_value, simplest_value

    type :: shrink_result_t
        class(input_t), allocatable :: input
        logical :: simplest
    end type
contains
    pure function shrunk_value(value_)
        class(input_t), intent(in) :: value_
        type(shrink_result_t) :: shrunk_value

        allocate(shrunk_value%input, source = value_)
        shrunk_value%simplest = .false.
    end function

    pure function simplest_value(value_)
        class(input_t), intent(in) :: value_
        type(shrink_result_t) :: simplest_value

        allocate(simplest_value%input, source = value_)
        simplest_value%simplest = .true.
    end function
end module
