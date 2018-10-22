program run_tests
    use test_case_test, only: test_case_properties
    use test_collection_test, only: &
            test_collection_can_tell_failure, test_collection_properties
    use Vegetables_m, only: TestCollection_t, testThat, runTests

    implicit none

    type(TestCollection_t) :: tests

    tests = testThat([test_case_properties(), &
            test_collection_can_tell_failure(), &
            test_collection_properties()])
    call runTests(tests)
end program run_tests
