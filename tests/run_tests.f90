program run_tests
    use failing_case_test, only: test_failing_case_behaviors
    use passing_case_test, only: test_passing_case_behaviors
    use single_case_properties_test, only: test_case_properties
    use test_collection_test, only: test_collection_properties
    use Vegetables_m, only: TestCollection_t, testThat, runTests

    implicit none

    type(TestCollection_t) :: tests

    tests = testThat([test_failing_case_behaviors(), &
            test_passing_case_behaviors(), &
            test_case_properties(), &
            test_collection_properties()])
    call runTests(tests)
end program run_tests
