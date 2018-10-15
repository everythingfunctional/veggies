program test_runner
    use test_collection_test, only: test_collection_testGenerator => testGenerator
    use Vegetables_m,  only: TestCollection_t, executeEverything

    implicit none

    integer, parameter :: NUM_test_collectionS = 1
    type(TestCollection_t) :: test_collections(NUM_test_collectionS)

    test_collections(1) = test_collection_testGenerator()
    call executeEverything(test_collections)

end program test_runner
