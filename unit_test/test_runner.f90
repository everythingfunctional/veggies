program test_runner
    use Vegetables_m,  only: TestCollection_t, TestCollectionResult_t
    use test_collection_test, only: test_collection_testGenerator => testGenerator

    implicit none

    integer, parameter :: NUM_test_collectionS = 1
    type(TestCollection_t) :: test_collections(NUM_test_collectionS)
    type(TestCollectionResult_t) :: test_collection_results(NUM_test_collectionS)

    test_collections(1) = test_collection_testGenerator()

    test_collection_results = test_collections%runSuite()
end program test_runner
