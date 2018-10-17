module Vegetables_m
    implicit none
    private

    type :: VegetableString_t
        private
        character(len=:), allocatable :: string
    contains
        private
        generic, public :: WRITE(FORMATTED) => stringWrite
        procedure :: stringWrite
    end type VegetableString_t

    type, abstract, public :: Test_t
        private
        type(VegetableString_t) :: description_
    contains
        private
        procedure(description_), pass(self), public, deferred :: description
        procedure(run_), pass(self), public, deferred :: run
    end type Test_t

    type, abstract, public :: TestResult_t
    end type TestResult_t

    abstract interface
        pure function description_(self)
            import Test_t, VegetableString_t

            class(Test_t), intent(in) :: self
            type(VegetableString_t) :: description_
        end function description_

        pure function run_(self) result(test_result)
            import Test_t, TestResult_t

            class(Test_t), intent(in) :: self
            class(TestResult_t), allocatable :: test_result
        end function run_
    end interface

    type, public, extends(Test_t) :: TestCase_t
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: run => runTestCase
    end type TestCase_t

    public :: runTests, SUCCESSFUL, TODO
contains
    subroutine runTests(tests)
        use iso_fortran_env, only: output_unit
        class(Test_t) :: tests

        write(output_unit, *) "Running Tests"
        write(output_unit, *) tests%description()
    end subroutine

    pure function SUCCESSFUL() result(test_case)
        type(TestCase_t) :: test_case

        test_case = SUCCEEDS()
    end function SUCCESSFUL

    pure function SUCCEEDS()
        type(TestCase_t) :: SUCCEEDS

        SUCCEEDS = TestCase_t(description_ = toString("SUCCEEDS"))
    end function SUCCEEDS

    pure function TODO() result(test_case)
        type(TestCase_t) :: test_case

        test_case = TestCase_t(description_ = toString("TODO"))
    end function TODO

    pure function toString(string_in) result(string_out)
        character(len=*), intent(in) :: string_in
        type(VegetableString_t) :: string_out

        string_out = VegetableString_t(string_in)
    end function toString

    subroutine stringWrite(string, unit, iotype, v_list, iostat, iomsg)
        class(VegetableString_t), intent(in) :: string
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        associate(a => iotype, b => v_list); end associate

        write(unit=unit, iostat=iostat, iomsg=iomsg, fmt='(A)') string%string
    end subroutine stringWrite

    pure function testCaseDescription(self) result(description)
        class(TestCase_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = self%description_
    end function testCaseDescription

    pure function runTestCase(self) result(test_result)
        class(TestCase_t), intent(in) :: self
        class(TestResult_t), allocatable :: test_result

        associate(a => self, b => test_result); end associate
    end function runTestCase
end module Vegetables_m
