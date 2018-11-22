module passing_case_test
    implicit none
    private

    public :: test_passing_case_behaviors
contains
    function test_passing_case_behaviors() result(test)
        use Vegetables_m, only: TestItem_t, given, then, when

        type(TestItem_t) :: test

        test = given("a passing test case", &
                [when("it is run", &
                        [then("it knows it passed", checkCasePasses), &
                        then("it has 1 test case", checkNumCases), &
                        then("it has 1 passing case", checkNumPassingCases), &
                        then("it has no failing case", checkNumFailingCases), &
                        then("it's verbose description still includes the given description", checkVerboseDescription), &
                        then("it's failure description is empty", checkFailureDescriptionEmpty)])])
    end function test_passing_case_behaviors

    function checkCasePasses() result(result_)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: &
                Result_t, &
                TestCase_t, &
                TestCaseResult_t, &
                assertNot, &
                assertThat

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = examplePassingTestCase()
        test_result = test_case%run()
        result_ = assertThat(test_result%passed()).and.assertNot(test_result%failed())
    end function checkCasePasses

    function checkNumCases() result(result_)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = examplePassingTestCase()
        test_result = test_case%run()
        result_ = assertEquals(1, test_result%numCases())
    end function checkNumCases

    function checkNumPassingCases() result(result_)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = examplePassingTestCase()
        test_result = test_case%run()
        result_ = assertEquals(1, test_result%numPassingCases())
    end function checkNumPassingCases

    function checkNumFailingCases() result(result_)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = examplePassingTestCase()
        test_result = test_case%run()
        result_ = assertEquals(0, test_result%numFailingCases())
    end function checkNumFailingCases

    function checkVerboseDescription() result(result_)
        use example_cases_m, only: examplePassingTestCase, EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = examplePassingTestCase()
        test_result = test_case%run()
        result_ = assertIncludes(EXAMPLE_DESCRIPTION, test_result%verboseDescription())
    end function checkVerboseDescription

    function checkFailureDescriptionEmpty() result(result_)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, TestCaseResult_t, assertEmpty

        type(Result_t) :: result_

        type(TestCase_t) :: test_case
        type(TestCaseResult_t) :: test_result

        test_case = examplePassingTestCase()
        test_result = test_case%run()
        result_ = assertEmpty(test_result%failureDescription())
    end function checkFailureDescriptionEmpty
end module passing_case_test
