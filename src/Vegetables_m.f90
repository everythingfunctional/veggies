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
        private
        type(VegetableString_t) :: description_
    contains
        private
        procedure(statNum), pass(self), public, deferred :: numCases
        procedure(passed_), pass(self), public, deferred :: passed
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

        pure function passed_(self)
            import TestResult_t

            class(TestResult_t), intent(in) :: self
            logical :: passed_
        end function passed_

        pure function statNum(self) result(num)
            import TestResult_t

            class(TestResult_t), intent(in) :: self
            integer :: num
        end function statNum
    end interface

    type :: Result_t
        private
        integer :: num_asserts
        logical :: passed
        type(VegetableString_t) :: message
    end type Result_t

    type, public, extends(Test_t) :: TestCase_t
        private
        procedure(test_), nopass, pointer :: test
    contains
        private
        procedure, public :: description => testCaseDescription
        procedure, public :: run => runTestCase
    end type TestCase_t

    type :: TestItem_t
        private
        class(Test_t), allocatable :: test
    contains
        procedure, public :: run => runTestItem
    end type TestItem_t

    type, public, extends(Test_t) :: TestCollection_t
        private
        type(TestItem_t), allocatable :: tests(:)
    contains
        procedure, public :: description => testCollectionDescription
        procedure, public :: run => runTestCollection
    end type TestCollection_t

    type, public, extends(TestResult_t) :: TestCaseResult_t
        private
        type(Result_t) :: result_
    contains
        private
        procedure, public :: numCases => testCaseNumCases
        procedure, public :: passed => testCasePassed
    end type TestCaseResult_t

    type :: TestResultItem_t
        private
        class(TestResult_t), allocatable :: test_result
    end type TestResultItem_t

    type, public, extends(TestResult_t) :: TestCollectionResult_t
        private
        type(TestResultItem_t), allocatable :: results(:)
    contains
        private
        procedure, public :: numCases => testCollectionNumCases
        procedure, public :: passed => testCollectionPassed
    end type TestCollectionResult_t

    interface
        pure function test_() result(result)
            import Result_t

            type(Result_t) :: result
        end function test_
    end interface

    interface operator(.and.)
        module procedure testCollectionAndTest
    end interface

    public :: operator(.and.), testThat, runTests, SUCCESSFUL, TODO
contains
    pure function testThat(test_case) result(test_collection)
        class(Test_t), intent(in) :: test_case
        type(TestCollection_t) :: test_collection

        test_collection = TestCollection_t( &
                description_ = toString("Test That"), &
                tests = [TestItem_t(test_case)])
    end function testThat

    pure function testCollectionAndTest(test_collection, test) result(new_collection)
        type(TestCollection_t), intent(in) :: test_collection
        class(Test_t), intent(in) :: test
        type(TestCollection_t) :: new_collection

        integer :: new_num_tests
        integer :: prev_num_tests

        prev_num_tests = size(test_collection%tests)
        new_num_tests = prev_num_tests + 1

        new_collection%description_ = test_collection%description_
        allocate(new_collection%tests(new_num_tests))
        new_collection%tests(1:prev_num_tests) = test_collection%tests(1:prev_num_tests)
        new_collection%tests(new_num_tests) = TestItem_t(test)
    end function testCollectionAndTest

    subroutine runTests(tests)
        use iso_fortran_env, only: error_unit, output_unit

        class(Test_t) :: tests

        class(TestResult_t), allocatable :: test_result

        write(output_unit, *) "Running Tests"
        write(output_unit, *) tests%description()
        test_result = tests%run()
        if (test_result%passed()) then
            write(output_unit, *) "Passed"
        else
            write(error_unit, *) "Failed"
            stop 1
        end if
    end subroutine

    pure function SUCCESSFUL() result(test_case)
        type(TestCase_t) :: test_case

        test_case = SUCCEEDS()
    end function SUCCESSFUL

    pure function SUCCEEDS()
        type(TestCase_t) :: SUCCEEDS

        SUCCEEDS = TestCase_t(description_ = toString("SUCCEEDS"), test = alwaysSucceed)
    end function SUCCEEDS

    pure function alwaysSucceed() result(test_result)
        type(Result_t) :: test_result

        test_result = succeed()
    end function alwaysSucceed

    pure function succeed() result(success)
        type(Result_t) :: success

        success = Result_t(num_asserts = 1, passed = .true., message = toString(""))
    end function succeed

    pure function TODO() result(test_case)
        type(TestCase_t) :: test_case

        test_case = TestCase_t(description_ = toString("TODO"), test = alwaysFail)
    end function TODO

    pure function alwaysFail() result(test_result)
        type(Result_t) :: test_result

        test_result = fail(toString("Intentional Failure"))
    end function alwaysFail

    pure function fail(message) result(failure)
        type(VegetableString_t), intent(in) :: message
        type(Result_t) :: failure

        failure = Result_t(num_asserts = 1, passed = .false., message = message)
    end function fail

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

        test_result = TestCaseResult_t( &
                description_ = self%description_, &
                result_ = self%test())
    end function runTestCase

    pure function testCaseNumCases(self) result(num_cases)
        class(TestCaseResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self); end associate
        num_cases = 1
    end function testCaseNumCases

    pure function testCasePassed(self) result(passed)
        class(TestCaseResult_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed
    end function testCasePassed

    pure function testCollectionDescription(self) result(description)
        class(TestCollection_t), intent(in) :: self
        type(VegetableString_t) :: description

        description = self%description_
    end function testCollectionDescription

    pure function runTestCollection(self) result(test_result)
        class(TestCollection_t), intent(in) :: self
        class(TestResult_t), allocatable :: test_result

        test_result = TestCollectionResult_t( &
                description_ = self%description_, &
                results = self%tests%run())
    end function runTestCollection

    pure function testCollectionNumCases(self) result(num_cases)
        class(TestCollectionResult_t), intent(in) :: self
        integer :: num_cases

        associate(a => self); end associate
        num_cases = 1
    end function testCollectionNumCases

    pure function testCollectionPassed(self) result(passed)
        class(TestCollectionResult_t), intent(in) :: self
        logical :: passed

        associate(a => self); end associate
        passed = .true.
    end function testCollectionPassed

    elemental function runTestItem(self) result(test_result)
        class(TestItem_t), intent(in) :: self
        type(TestResultItem_t) :: test_result

        test_result = TestResultItem_t(self%test%run())
    end function runTestItem
end module Vegetables_m
