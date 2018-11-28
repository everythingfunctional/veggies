module passing_case_test
    implicit none
    private

    public :: test_passing_case_behaviors
contains
    function test_passing_case_behaviors() result(test)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: &
                TestCase_t, TestCaseResult_t, TestItem_t, given, then_, when

        type(TestItem_t) :: test

        type(TestCase_t) :: example_case
        type(TestCaseResult_t) :: example_result

        example_case = examplePassingTestCase()
        example_result = example_case%run()
        test = given("a passing test case", &
                [when("it is run", example_result, &
                        [then_("it knows it passed", checkCasePasses), &
                        then_("it has 1 test case", checkNumCases), &
                        then_("it has 1 passing case", checkNumPassingCases), &
                        then_("it has no failing case", checkNumFailingCases), &
                        then_("it's verbose description still includes the given description", checkVerboseDescription), &
                        then_("it's verbose description includes the assertion message", checkVerboseDescriptionAssertion), &
                        then_("it's failure description is empty", checkFailureDescriptionEmpty), &
                        then_("it knows how many asserts there were", checkNumAsserts), &
                        then_("it has no failing asserts", checkNumFailingAsserts), &
                        then_("it knows how many asserts passed", checkNumPassingAsserts)])])
    end function test_passing_case_behaviors

    function checkCasePasses(example_result) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCaseResult_t, assertNot, assertThat, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertThat(example_result%passed()).and.assertNot(example_result%failed())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkCasePasses

    function checkNumCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(1, example_result%numCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumCases

    function checkNumPassingCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(1, example_result%numPassingCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumPassingCases

    function checkNumFailingCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(0, example_result%numFailingCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingCases

    function checkVerboseDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseDescription

    function checkVerboseDescriptionAssertion(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(SUCCESS_MESSAGE, example_result%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseDescriptionAssertion

    function checkFailureDescriptionEmpty(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEmpty, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEmpty(example_result%failureDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureDescriptionEmpty

    function checkNumAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_ASSERTS_IN_PASSING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(NUM_ASSERTS_IN_PASSING, example_result%numAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(0, example_result%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingAsserts

    function checkNumPassingAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_ASSERTS_IN_PASSING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals( &
                    NUM_ASSERTS_IN_PASSING, example_result%numPassingAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumPassingAsserts
end module passing_case_test
