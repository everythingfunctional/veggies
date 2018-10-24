module Vegetables_m
    implicit none
    private

    type, public :: Result_t
    end type Result_t

    type, public :: TestCase_t
    contains
        procedure, public :: description => testCaseDescription
    end type TestCase_t

    type, public :: TestCollection_t
    end type TestCollection_t

    interface
        function test_() result(result_)
            import Result_t

            type(Result_t) :: result_
        end function test_
    end interface


    public :: assertIncludes, Describe, It, succeed
contains
    function assertIncludes(search_for, string) result(result_)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result_

        associate(a => search_for, b => string)
        end associate
        result_ = Result_t()
    end function assertIncludes

    function Describe(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestCase_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        associate(a => description, b => tests)
        end associate
        test_collection = TestCollection_t()
    end function Describe

    function It(description, test) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: test
        type(TestCase_t) :: test_case

        associate(a => description, b => test)
        end associate
        test_case = TestCase_t()
    end function It

    function succeed() result(result_)
        type(Result_t) :: result_

        result_ = Result_t()
    end function succeed

    function testCaseDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        character(len=:), allocatable :: description

        associate(a => self)
        end associate
        description = ""
    end function testCaseDescription
end module Vegetables_m
