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
                        then("it has 1 test case", checkNumCases), &
                        then("it has no passing case", checkNumPassingCases), &
                        then("it has 1 failing case", checkNumFailingCases), &
                        then("it's verbose description includes the given description", checkVerboseForGivenDescription), &
                        then("it's verbose description includes the failure message", checkVerboseForFailureMessage), &
                        then("it's failure description includes the given description", checkFailureForGivenDescription), &
                        then("it's failure description includes the failure message", checkFailureForFailureMessage), &
                        then("it knows how many asserts there were", checkNumAsserts), &
                        then("it knows how many asserts failed", checkNumFailingAsserts), &
                        then("it knows how many asserts passed", checkNumPassingAsserts)])])
    end function test_failing_case_behaviors

    function checkCaseFails() result(result_)
        use example_cases_m, only: exampleFailingTestCase
        use Vegetables_m, only: &
                Result_t, &
                TestCase_t, &
                TestCaseResult_t, &
                operator(.and.), &
                assertNot, &
                assertThat

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertThat(test_result%failed()).and.assertNot(test_result%passed())
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

    function checkNumPassingCases() result(result_)
        use example_cases_m, only: exampleFailingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertEquals(0, test_result%numPassingCases())
    end function checkNumPassingCases

    function checkNumFailingCases() result(result_)
        use example_cases_m, only: exampleFailingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertEquals(1, test_result%numFailingCases())
    end function checkNumFailingCases

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

    function checkFailureForGivenDescription() result(result_)
        use example_cases_m, only: exampleFailingTestCase, EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertIncludes(EXAMPLE_DESCRIPTION, test_result%failureDescription())
    end function checkFailureForGivenDescription

    function checkFailureForFailureMessage() result(result_)
        use example_cases_m, only: exampleFailingTestCase, FAILURE_MESSAGE
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertIncludes(FAILURE_MESSAGE, test_result%failureDescription())
    end function checkFailureForFailureMessage

    function checkNumAsserts() result(result_)
        use example_cases_m, only: exampleFailingTestCase, NUM_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertEquals(NUM_ASSERTS_IN_FAILING, test_result%numAsserts())
    end function checkNumAsserts

    function checkNumFailingAsserts() result(result_)
        use example_cases_m, only: &
                exampleFailingTestCase, NUM_FAILING_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertEquals( &
                NUM_FAILING_ASSERTS_IN_FAILING, test_result%numFailingAsserts())
    end function checkNumFailingAsserts

    function checkNumPassingAsserts() result(result_)
        use example_cases_m, only: &
                exampleFailingTestCase, NUM_PASSING_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = exampleFailingTestCase()
        test_result = test_case%run()
        result_ = assertEquals( &
                NUM_PASSING_ASSERTS_IN_FAILING, test_result%numPassingAsserts())
    end function checkNumPassingAsserts
end module failing_case_test
