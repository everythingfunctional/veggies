module vegetables_double_precision_input_m
    use vegetables_input_m, only: input_t

    implicit none
    private
    public :: double_precision_input_t

    type, extends(input_t) :: double_precision_input_t
        double precision :: value_
    end type
end module
