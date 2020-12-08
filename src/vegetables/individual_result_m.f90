module vegetables_individual_result_m
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: individual_result_t, individual_result

    type :: individual_result_t
        type(varying_string) :: message
        logical :: passed_
    contains
        private
        procedure, public :: failure_description
        procedure, public :: verbose_description
    end type
contains
    pure function individual_result(message, passed)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: message
        logical, intent(in) :: passed
        type(individual_result_t) :: individual_result

        individual_result%message = message
        individual_result%passed_ = passed
    end function

    elemental function failure_description(self, colorize) result(description)
        use iso_varying_string, only: varying_string, assignment(=), operator(//)

        class(individual_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        if (self%passed_) then
            description = ""
        else
            if (colorize) then
                description = char(27) // "[31m" // self%message // char(27) // "[0m"
            else
                description = self%message
            end if
        end if
    end function

    elemental function verbose_description(self, colorize) result(description)
        use iso_varying_string, only: varying_string, operator(//)

        class(individual_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        if (colorize) then
            if (self%passed_) then
                description = char(27) // "[32m" // self%message // char(27) // "[0m"
            else
                description = char(27) // "[31m" // self%message // char(27) // "[0m"
            end if
        else
            description = self%message
        end if
    end function
end module
