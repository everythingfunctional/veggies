program run_tests
    use test_case_test, only: test_can_be_single_case
    use test_collection_test, only: test_collection_can_tell_failure
    use Vegetables_m, only: TestCollection_t, operator(.and.), testThat, runTests, SUCCESSFUL

    implicit none

    type(TestCollection_t) :: tests

    tests = testThat(test_can_be_single_case())
    tests = tests.and.test_collection_can_tell_failure()
    call runTests(tests)
end program run_tests
