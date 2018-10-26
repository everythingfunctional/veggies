/*
MODULE cVegetables
*/

class Result {
private:
    bool passed;

public:
    Result(bool passed);
};

class TestCase {
private:
  char *description;
  void *test;

public:
  TestCase(char *description, void *test);
};

Result::Result(bool passed) : passed(passed) {}

extern "C" Result *cResult(bool passed) {
    Result *result = new Result(passed);
    return result;
}

TestCase::TestCase(char *description, void *test) : description(description), test(test) {}

extern "C" TestCase *cTestCase(char *description, void *test) {
  TestCase *test_case = new TestCase(description, test);
  return test_case;
}
