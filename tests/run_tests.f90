program run_tests
    use test_case_test, only: test_can_be_single_case
    use Vegetables_m, only: TestCollection_t, operator(.and.), testThat, runTests, SUCCESSFUL

    implicit none

    type(TestCollection_t) :: tests

    tests = testThat(test_can_be_single_case())
    tests = tests.and.SUCCESSFUL()
    call runTests(tests)
end program run_tests
