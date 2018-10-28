/*
MODULE cVegetables
*/

#include <algorithm>
#include <cstring>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

class Result {
private:
  bool _passed;

public:
  Result(bool passed);
  Result *combineWith(Result *addition);
  bool passed();
};

class TestResult {
protected:
  std::string _description;

public:
  TestResult(std::string description);
  virtual bool passed() = 0;
};

class TestCaseResult : public TestResult {
private:
  Result *_result;

public:
  TestCaseResult(std::string _description, Result *result);
  bool passed();
};

class TestCollectionResult : public TestResult {
private:
  std::vector<TestResult *> _results;

public:
  TestCollectionResult(std::string _description,
                       std::vector<TestResult *> results);
  bool passed();
};

class Test {
protected:
  std::string _description;

public:
  Test(std::string description);
  virtual std::string description() = 0;
  virtual int numCases() = 0;
  virtual TestResult *run() = 0;
};

class TestCase : public Test {
private:
  void *_test;

public:
  TestCase(std::string description, void *test);
  std::string description();
  int numCases();
  TestCaseResult *run();
};

class TestCollection : public Test {
private:
  std::vector<Test *> _tests;

public:
  TestCollection(std::string description);
  void addTest(Test *test);
  std::string description();
  int numCases();
  TestCollectionResult *run();
};

extern "C" Result *runATest(void *test);

std::string join(std::vector<std::string> strings, std::string separator) {
  std::string joined = strings[0];
  for (size_t i = 1; i < strings.size(); i++) {
    joined += separator + strings[i];
  }
  return joined;
}

Result::Result(bool passed) : _passed(passed) {}

bool Result::passed() { return this->_passed; }

Result *Result::combineWith(Result *addition) {
  Result *combined = new Result(this->_passed && addition->_passed);
  return combined;
}

extern "C" Result *cCombineResults(Result *lhs, Result *rhs) {
  return lhs->combineWith(rhs);
}

extern "C" Result *cResult(bool passed) {
  Result *result = new Result(passed);
  return result;
}

Test::Test(std::string description) : _description(description) {}

TestCase::TestCase(std::string description, void *test)
    : Test(description), _test(test) {}

std::string TestCase::description() { return this->_description; }

int TestCase::numCases() { return 1; }

TestCaseResult *TestCase::run() {
  Result *test_result = runATest(this->_test);
  TestCaseResult *result = new TestCaseResult(this->_description, test_result);
  return result;
}

extern "C" TestCase *cTestCase(char *description, void *test) {
  TestCase *test_case = new TestCase(std::string(description), test);
  return test_case;
}

extern "C" void cTestCaseDescription(TestCase *test_case, char *description,
                                     int maxlen) {
  std::string the_description = test_case->description();
  strncpy(description, the_description.c_str(), maxlen);
}

TestCollection::TestCollection(std::string description) : Test(description) {}

void TestCollection::addTest(Test *test) { this->_tests.push_back(test); }

std::string TestCollection::description() {
  std::vector<std::string> individual_descriptions;
  individual_descriptions.resize(this->_tests.size());
  std::transform(this->_tests.begin(), this->_tests.end(),
                 individual_descriptions.begin(),
                 [](Test *test) { return test->description(); });
  return this->_description + "\n" + join(individual_descriptions, "\n");
}

int TestCollection::numCases() {
  std::vector<int> individual_nums;
  individual_nums.resize(this->_tests.size());
  std::transform(this->_tests.begin(), this->_tests.end(),
                 individual_nums.begin(),
                 [](Test *test) { return test->numCases(); });
  return std::accumulate(individual_nums.begin(), individual_nums.end(), 0);
}

TestCollectionResult *TestCollection::run() {
  std::vector<TestResult *> results;
  for (auto const &test : this->_tests) {
    results.push_back(test->run());
  }
  TestCollectionResult *result =
      new TestCollectionResult(this->_description, results);
  return result;
}

extern "C" TestCollection *cTestCollection(char *description) {
  TestCollection *test_collection =
      new TestCollection(std::string(description));
  return test_collection;
}

extern "C" void cAddTest(TestCollection *collection, Test *test) {
  collection->addTest(test);
}

extern "C" TestCollectionResult *cRunTestCollection(TestCollection *tests) {
  return tests->run();
}

extern "C" void cTestCollectionDescription(TestCollection *test_collection,
                                           char *description, int maxlen) {
  std::string the_description = test_collection->description();
  strncpy(description, the_description.c_str(), maxlen);
}

extern "C" int cTestCollectionNumCases(TestCollection *tests) {
  return tests->numCases();
}

TestResult::TestResult(std::string description) : _description(description) {}

TestCaseResult::TestCaseResult(std::string description, Result *result)
    : TestResult(description), _result(result) {}

bool TestCaseResult::passed() { return this->_result->passed(); }

TestCollectionResult::TestCollectionResult(std::string description,
                                           std::vector<TestResult *> results)
    : TestResult(description), _results(results) {}

bool TestCollectionResult::passed() {
  return std::all_of(this->_results.begin(), this->_results.end(),
                     [](TestResult *result) { return result->passed(); });
}

extern "C" bool cTestCollectionPassed(TestCollectionResult *collection) {
  return collection->passed();
}
