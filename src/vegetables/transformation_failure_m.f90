module vegetables_transformation_failure_m
    use vegetables_input_m, only: input_t
    use vegetables_result_m, only: result_t

    implicit none
    private
    public :: transformation_failure_t

    type, extends(input_t) :: transformation_failure_t
        type(result_t) :: result_
    end type
end module
