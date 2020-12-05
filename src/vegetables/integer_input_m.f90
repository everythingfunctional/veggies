module vegetables_integer_input_m
    use vegetables_input_m, only: input_t

    implicit none
    private
    public :: integer_input_t

    type, extends(input_t) :: integer_input_t
        integer :: value_
    end type
end module
