module vegetables_result_m
    use vegetables_individual_result_m, only: individual_result_t

    implicit none
    private
    public :: result_t

    type :: result_t
        type(individual_result_t), allocatable :: results(:)
    contains
        private
        procedure :: combine_results
        generic, public :: operator(.and.) => combine_results
        procedure, public :: num_asserts
        procedure, public :: num_failing_asserts
        procedure, public :: passed
        procedure, public :: failure_description
        procedure, public :: verbose_description
    end type
contains
    pure function combine_results(lhs, rhs) result(combined)
        class(result_t), intent(in) :: lhs
        type(result_t), intent(in) :: rhs
        type(result_t) :: combined

        integer :: num_lhs
        integer :: num_rhs

        if (allocated(lhs%results) .and. allocated(rhs%results)) then
            num_lhs = size(lhs%results)
            num_rhs = size(rhs%results)
            allocate(combined%results(num_lhs + num_rhs))
            combined%results(1:num_lhs) = lhs%results(:)
            combined%results(num_lhs+1:) = rhs%results(:)
        else if (allocated(lhs%results)) then
            combined = lhs
        else if (allocated(rhs%results)) then
            combined = rhs
        end if
    end function

    pure function failure_description(self, colorize) result(description)
        use iso_varying_string, only: varying_string
        use strff, only: join, NEWLINE

        class(result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        type(varying_string) :: individual_descriptions(size(self%results))

        individual_descriptions = self%results%failure_description(colorize)
        description = join(individual_descriptions, NEWLINE)
    end function

    pure function num_asserts(self)
        class(result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = size(self%results)
    end function

    pure function num_failing_asserts(self) result(num_asserts)
        class(result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = count(.not.self%results%passed_)
    end function

    pure function passed(self)
        class(result_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed_)
    end function

    pure function verbose_description(self, colorize) result(description)
        use iso_varying_string, only: varying_string
        use strff, only: join, NEWLINE

        class(result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        type(varying_string) :: individual_descriptions(size(self%results))

        individual_descriptions = self%results%verbose_description(colorize)
        description = join(individual_descriptions, NEWLINE)
    end function
end module
