/*
MODULE cVegetables
*/

class TestCase {
private:
  char *description;

public:
  TestCase(char *description);
};

TestCase::TestCase(char *description) : description(description) {}

extern "C" TestCase *cTestCase(char *description) {
  TestCase *test_case = new TestCase(description);
  return test_case;
}
