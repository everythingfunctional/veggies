module test_case_test
    implicit none
    private

    character(len=*), parameter :: EXAMPLE_DESCRIPTION = "Example Description"

    public :: test_case_properties, test_passing_case_behaviors
contains
    function test_case_properties() result(test)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test

        test = describe("A test case", &
                [it("includes the given description", caseDescriptionCheck), &
                it("Only has 1 test case", checkNumCases)])
    end function test_case_properties

    function test_passing_case_behaviors() result(test)
        use Vegetables_m, only: TestCollection_t, given, then, when

        type(TestCollection_t) :: test

        test = given("a passing test case", &
                [when("it is run", &
                        [then("it knows it passed", checkCasePasses)])])
    end function test_passing_case_behaviors

    function caseDescriptionCheck() result(result_)
        use Vegetables_m, only: Result_t, TestCase_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case

        test_case = exampleTestCase()
        result_ = assertIncludes(EXAMPLE_DESCRIPTION, test_case%description())
    end function caseDescriptionCheck

    function checkNumCases() result(result_)
        use Vegetables_m, only: Result_t, TestCase_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case

        test_case = exampleTestCase()
        result_ = assertEquals(1, test_case%numCases())
    end function checkNumCases

    function exampleTestCase() result(test_case)
        use Vegetables_m, only: TestCase_t, it, succeed

        type(TestCase_t) :: test_case

        test_case = it(EXAMPLE_DESCRIPTION, succeed)
    end function exampleTestCase

    function checkCasePasses() result(result_)
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertThat

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleTestCase()
        test_result = test_case%run()
        result_ = assertThat(test_result%passed())
    end function checkCasePasses
end module test_case_test
