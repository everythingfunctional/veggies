/*
MODULE cVegetables
*/

#include <cstring>

class Result {
private:
  bool passed;

public:
  Result(bool passed);
};

class TestCase {
private:
  char *_description;
  void *_test;

public:
  TestCase(char *description, void *test);
  char *description();
};

class TestCollection {
private:
  char *_description;

public:
  TestCollection(char *description);
};

Result::Result(bool passed) : passed(passed) {}

extern "C" Result *cResult(bool passed) {
  Result *result = new Result(passed);
  return result;
}

TestCase::TestCase(char *description, void *test)
    : _description(description), _test(test) {}

char *TestCase::description() { return this->_description; }

extern "C" TestCase *cTestCase(char *description, void *test) {
  TestCase *test_case = new TestCase(description, test);
  return test_case;
}

extern "C" void cTestCaseDescription(TestCase *test_case, char *description,
                                     int maxlen) {
  strncpy(description, test_case->description(), maxlen);
}

TestCollection::TestCollection(char *description) : _description(description) {}

extern "C" TestCollection *cTestCollection(char *description) {
  TestCollection *test_collection = new TestCollection(description);
  return test_collection;
}
