module example_asserts_m
    implicit none
    private

    character(len=*), parameter, public :: FAILURE_MESSAGE = "Failure Message"
    integer, parameter, public :: NUM_FAILING_ASSERTS_IN_FAILING = 1
    integer, parameter, public :: NUM_PASSING_ASSERTS_IN_FAILING = 1
    integer, parameter, public :: NUM_ASSERTS_IN_FAILING = &
            NUM_FAILING_ASSERTS_IN_FAILING + NUM_PASSING_ASSERTS_IN_FAILING
    integer, parameter, public :: NUM_ASSERTS_IN_PASSING = 2
    character(len=*), parameter, public :: SUCCESS_MESSAGE = "Success Message"

    public :: exampleMultipleAsserts, exampleMultipleAssertsWithFail
contains
    function exampleMultipleAsserts() result(result_)
        use Vegetables_m, only: Result_t, succeed

        type(Result_t) :: result_

        result_ = succeed(SUCCESS_MESSAGE).and.succeed(SUCCESS_MESSAGE)
    end function exampleMultipleAsserts

    function exampleMultipleAssertsWithFail() result(result_)
        use Vegetables_m, only: Result_t, fail, succeed

        type(Result_t) :: result_

        result_ = succeed(SUCCESS_MESSAGE).and.fail(FAILURE_MESSAGE)
    end function exampleMultipleAssertsWithFail
end module example_asserts_m
