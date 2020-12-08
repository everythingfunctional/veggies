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
    pure function shrink_result(value_, simplest)
        class(input_t), intent(in) :: value_
        logical, intent(in) :: simplest
        type(shrink_result_t) :: shrink_result

        allocate(shrink_result%input, source = value_)
        shrink_result%simplest = simplest
    end function

    pure function shrunk_value(value_)
        class(input_t), intent(in) :: value_
        type(shrink_result_t) :: shrunk_value

        shrunk_value = shrink_result(value_, .false.)
    end function

    pure function simplest_value(value_)
        class(input_t), intent(in) :: value_
        type(shrink_result_t) :: simplest_value

        simplest_value = shrink_result(value_, .true.)
    end function
end module
