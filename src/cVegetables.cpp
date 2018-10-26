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

public:
  TestCase(char *description);
};

Result::Result(bool passed) : passed(passed) {}

extern "C" Result *cResult(bool passed) {
    Result *result = new Result(passed);
    return result;
}

TestCase::TestCase(char *description) : description(description) {}

extern "C" TestCase *cTestCase(char *description) {
  TestCase *test_case = new TestCase(description);
  return test_case;
}
