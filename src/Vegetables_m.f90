module Vegetables_m
!   use cVegetables
    use iso_c_binding, only: c_bool, c_ptr

    implicit none
    private

    type, bind(c), public :: Result_t
        private
        type(c_ptr) :: contents
    end type Result_t

    type, public :: TestCase_t
        private
        type(c_ptr) :: contents
    contains
        procedure, public :: description => testCaseDescription
    end type TestCase_t

    type, public :: TestCollection_t
        private
        type(c_ptr) :: contents
    contains
        private
        procedure, public :: run => runTestCollection
    end type TestCollection_t

    type, public :: TestResultCollection_t
        private
        type(c_ptr) :: contents
    end type TestResultCollection_t

    interface operator(.includes.)
        module procedure includes
    end interface

    interface
        subroutine cAddTest(collection, test) bind(C, name="cAddTest")
            use iso_c_binding, only: c_ptr

            type(c_ptr), value, intent(in) :: collection
            type(c_ptr), value, intent(in) :: test
        end subroutine cAddTest
        function cResult(passed) result(result_) bind(C, name="cResult")
            use iso_c_binding, only: c_bool, c_ptr

            logical(kind=c_bool), value, intent(in) :: passed
            type(c_ptr) :: result_
        end function cResult

        function cTestCollection( &
                description) &
                result(test_collection) &
                bind(C, name="cTestCollection")
            use iso_c_binding, only: c_char, c_int, c_ptr

            character(len=1, kind=c_char), dimension(*), intent(in) :: description
            type(c_ptr) :: test_collection
        end function cTestCollection

        function test_() result(result_) bind(c)
            import Result_t

            type(Result_t) :: result_
        end function test_
    end interface

    logical(kind=c_bool), parameter :: CTRUE = .true.
    logical(kind=c_bool), parameter :: CFALSE = .false.

    public :: assertIncludes, Describe, It, runATest, runTests, succeed, testThat
contains
    function assertIncludes(search_for, string) result(result_)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(Result_t) :: result_

        if (string.includes.search_for) then
            result_ = succeed()
        else
            result_ = fail()
        end if
    end function assertIncludes

    function cStringToF(c_string) result(f_string)
        character(len=*), intent(in) :: c_string
        character(len=:), allocatable :: f_string

        integer :: terminator_position

        terminator_position = index(c_string, char(0))
        if (terminator_position == 0) then
            f_string = c_string
        else
            f_string = c_string(1:terminator_position-1)
        end if
    end function cStringToF

    function Describe(description, tests) result(test_collection)
        character(len=*), intent(in) :: description
        type(TestCase_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        integer :: i

        test_collection%contents = cTestCollection(fStringToC(description))
        do i = 1, size(tests)
            call cAddTest(test_collection%contents, tests(i)%contents)
        end do
    end function Describe

    function fail() result(result_)
        type(Result_t) :: result_

        result_%contents = cResult(CFALSE)
    end function fail

    function fStringToC(f_string) result(c_string)
        character(len=*), intent(in) :: f_string
        character(len=:), allocatable :: c_string

        c_string = f_string // char(0)
    end function fStringToC

    pure function includes(string, search_for)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: search_for
        logical :: includes

        includes = index(string, search_for) > 0
    end function includes

    function It(description, test) result(test_case)
        character(len=*), intent(in) :: description
        procedure(test_) :: test
        type(TestCase_t) :: test_case

        interface
            function cTestCase(description, test) result(test_case) bind(C, name="cTestCase")
                use iso_c_binding, only: c_char, c_ptr

                character(len=1, kind=c_char), dimension(*), intent(in) :: description
                procedure(test_) :: test
                type(c_ptr) :: test_case
            end function cTestCase
        end interface

        test_case%contents = cTestCase(fStringToC(description), test)
    end function It

    function runATest(test) result(result_) bind(C, name="runATest")
        procedure(test_) :: test
        type(c_ptr) :: result_

        type(Result_t) :: test_result

        test_result = test()
        result_ = test_result%contents
    end function runATest

    function runTestCollection(self) result(test_result)
        class(TestCollection_t), intent(in) :: self
        type(TestResultCollection_t) :: test_result

        interface
            function cRunTestCollection( &
                    collection) &
                    result(results) &
                    bind(C, name="cRunTestCollection")
                use iso_c_binding, only: c_ptr

                type(c_ptr), value, intent(in) :: collection
                type(c_ptr) :: results
            end function cRunTestCollection
        end interface

        test_result%contents = cRunTestCollection(self%contents)
    end function runTestCollection

    subroutine runTests(tests)
        type(TestCollection_t), intent(in) :: tests

        type(TestResultCollection_t) :: test_results

        print *, "Running Tests"
        test_results = tests%run()
    end subroutine runTests

    function succeed() result(result_)
        type(Result_t) :: result_

        result_%contents = cResult(CTRUE)
    end function succeed

    function testCaseDescription(self) result(description)
        use iso_c_binding, only: c_char

        class(TestCase_t), intent(in) :: self
        character(len=:), allocatable :: description

        interface
            subroutine cTestCaseDescription( &
                    test_case, &
                    description, &
                    max_length) &
                    bind(C, name="cTestCaseDescription")
                use iso_c_binding, only: c_char, c_int, c_ptr

                type(c_ptr), value, intent(in) :: test_case
                character(kind=c_char), dimension(*) :: description
                integer(kind=c_int), value, intent(in) :: max_length
            end subroutine cTestCaseDescription
        end interface

        integer, parameter :: MAX_STRING_LENGTH = 10000
        character(len=MAX_STRING_LENGTH, kind=c_char) :: description_

        call cTestCaseDescription(self%contents, description_, MAX_STRING_LENGTH)
        description = cStringToF(description_)
    end function testCaseDescription

    function testThat(tests) result(test_collection)
        type(TestCollection_t), intent(in) :: tests(:)
        type(TestCollection_t) :: test_collection

        integer :: i

        test_collection%contents = cTestCollection(fStringToC("Test that"))
        do i = 1, size(tests)
            call cAddTest(test_collection%contents, tests(i)%contents)
        end do
    end function testThat
end module Vegetables_m
