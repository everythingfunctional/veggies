program test_runner
    use fizzbuzz_test, only: &
            fizzbuzzTestGenerator => testGenerator
    use Vegetables_m,  only: TestSuite_t, TestSuiteResult_t

    implicit none

    integer, parameter :: NUM_TEST_SUITES = 1
    type(TestSuite_t) :: test_suites(NUM_TEST_SUITES)
    type(TestSuiteResult_t) :: test_suite_results(NUM_TEST_SUITES)

    test_suites(1) = fizzbuzzTestGenerator()

    test_suite_results = test_suites%runSuite()
end program test_runner
