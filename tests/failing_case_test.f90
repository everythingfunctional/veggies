module failing_case_test
    implicit none
    private

    public :: test_failing_case_behaviors
contains
    function test_failing_case_behaviors() result(test)
        use Vegetables_m, only: TestCollection_t, given, then, when

        type(TestCollection_t) :: test

        test = given("a failing test case", &
                [when("it is run", &
                        [then("it knows it failed", checkCaseFails), &
                        then("it still has 1 test case", checkNumCases), &
                        then("it's verbose description includes the given description", checkVerboseForGivenDescription), &
                        then("it's verbose description includes the failure message", checkVerboseForFailureMessage)])])
    end function test_failing_case_behaviors

    function checkCaseFails() result(result_)
        use example_cases_m, only: exampleFailingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertNot

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertNot(test_result%passed())
    end function checkCaseFails

    function checkNumCases() result(result_)
        use example_cases_m, only: exampleFailingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertEquals(1, test_result%numCases())
    end function checkNumCases

    function checkVerboseForGivenDescription() result(result_)
        use example_cases_m, only: exampleFailingTestCase, EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertIncludes(EXAMPLE_DESCRIPTION, test_result%verboseDescription())
    end function checkVerboseForGivenDescription

    function checkVerboseForFailureMessage() result(result_)
        use example_cases_m, only: exampleFailingTestCase, FAILURE_MESSAGE
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertIncludes(FAILURE_MESSAGE, test_result%verboseDescription())
    end function checkVerboseForFailureMessage
end module failing_case_test
