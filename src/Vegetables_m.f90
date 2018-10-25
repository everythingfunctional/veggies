module Vegetables_m
!   use cVegetables
    use iso_c_binding, only: c_ptr

    implicit none
    private

    type, public :: Result_t
    end type Result_t

    type, public :: TestCase_t
        private
        type(c_ptr) :: contents
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


    public :: assertIncludes, Describe, It, runTests, succeed, testThat
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

    function fStringToC(f_string) result(c_string)
        character(len=*), intent(in) :: f_string
        character(len=:), allocatable :: c_string

        c_string = f_string // char(0)
    end function fStringToC

    function It(description, test) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: test
        type(TestCase_t) :: test_case

        interface
            function cTestCase(description) result(test_case) bind(C, name="cTestCase")
                use iso_c_binding, only: c_char, c_ptr

                character(len=1, kind=C_CHAR), dimension(*), intent(in) :: description
                type(c_ptr) :: test_case
            end function cTestCase
        end interface

        associate(a => description, b => test)
        end associate
        test_case%contents = cTestCase(fStringToC(description))
    end function It

    subroutine runTests(tests)
        type(TestCollection_t) :: tests

        associate(a => tests)
        end associate
        print *, "Running Tests"
    end subroutine runTests

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

    function testThat(tests) result(test_collection)
        type(TestCollection_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        associate(a => tests)
        end associate
        test_collection = TestCollection_t()
    end function testThat
end module Vegetables_m
