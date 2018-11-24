program run_tests
    use collection_properties_test, only: test_collection_properties
    use failing_case_test, only: test_failing_case_behaviors
    use failing_collection_test, only: test_failing_collection_behaviors
    use passing_case_test, only: test_passing_case_behaviors
    use passing_collection_test, only: test_passing_collection_behaviors
    use single_case_properties_test, only: test_case_properties
    use Vegetables_m, only: TestItem_t, testThat, runTests

    implicit none

    type(TestItem_t) :: tests

    tests = testThat( &
            [test_collection_properties(), &
            test_case_properties(), &
            test_failing_case_behaviors(), &
            test_failing_collection_behaviors(), &
            test_passing_case_behaviors(), &
            test_passing_collection_behaviors()])
    call runTests(tests)
end program run_tests
