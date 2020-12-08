module vegetables_transformed_m
    use vegetables_input_m, only: input_t

    implicit none
    private
    public :: transformed_t, transformed

    type :: transformed_t
        class(input_t), allocatable :: input
    end type
contains
    pure function transformed(input)
        class(input_t), intent(in) :: input
        type(transformed_t) :: transformed

        allocate(transformed%input, source = input)
    end function
end module
