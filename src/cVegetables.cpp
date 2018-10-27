/*
MODULE cVegetables
*/

#include <cstring>
#include <vector>

class Result {
private:
  bool passed;

public:
  Result(bool passed);
};

class Test {};

class TestCase : public Test {
private:
  char *_description;
  void *_test;

public:
  TestCase(char *description, void *test);
  char *description();
};

class TestCollection : public Test {
private:
  char *_description;
  std::vector<Test *> _tests;

public:
  TestCollection(char *description, std::vector<Test *> tests);
};

class TestResultCollection {
private:
public:
  TestResultCollection();
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

TestCollection::TestCollection(char *description, std::vector<Test *> tests)
    : _description(description), _tests(tests) {}

extern "C" TestCollection *cTestCollection(char *description, Test *tests[],
                                           int num_tests) {
  std::vector<Test *> tests_;
  for (int i = 0; i < num_tests; i++) {
    tests_.push_back(tests[i]);
  }
  TestCollection *test_collection = new TestCollection(description, tests_);
  return test_collection;
}

TestResultCollection::TestResultCollection() {}

extern "C" TestResultCollection *cRunTestCollection(TestCollection *tests) {
  (void)tests;
  TestResultCollection *results = new TestResultCollection();
  return results;
}
