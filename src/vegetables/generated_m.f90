module vegetables_generated_m
    use vegetables_input_m, only: input_t

    implicit none
    private
    public :: generated_t, generated

    type :: generated_t
        class(input_t), allocatable :: input
    end type
contains
    pure function generated(value_)
        class(input_t), intent(in) :: value_
        type(generated_t) :: generated

        allocate(generated%input, source = value_)
    end function
end module
