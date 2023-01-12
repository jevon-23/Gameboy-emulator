#include <gtest/gtest.h>
#include <string.h>
extern "C" {
#include "../include/utils.h"
#include <unistd.h>
}

namespace {
TEST(readFileTest, readFile) {
  char fp[200];
  getcwd(fp, sizeof(fp));
  strcat(fp, "/build/testing/utils_help.txt");
  aFile theFile = readFile(fp);
  EXPECT_EQ(**theFile.contents, 'g'); // broken down in terms of u_char
  EXPECT_EQ(*(*theFile.contents+8), '1'); // broken down in terms of u_char
}

TEST(testStack, stack) {
  stack *s = new_stack();
  EXPECT_EQ(s->data[0], 0); 
  EXPECT_EQ(s->len, 0); 
  EXPECT_EQ(s->max_len, MAX_STACK_LEN); 
  EXPECT_EQ(stack_is_empty(s), true); 
}

TEST(testStackOps, stackOps) {
  stack *s = new_stack();
  EXPECT_EQ(s->data[0], 0); 
  EXPECT_EQ(s->len, 0); 
  EXPECT_EQ(s->max_len, MAX_STACK_LEN); 
  EXPECT_EQ(stack_is_empty(s), true); 

  printf("passed 1\n");

  uint16_t one = 0x01;
  uint16_t two = 0x02;

  EXPECT_EQ(stack_push(s, one), true);
  EXPECT_EQ(stack_peak(s), one);
  EXPECT_EQ(s->len, 1);
  EXPECT_EQ(stack_is_empty(s), false); 

  printf("passewd 2\n");

  EXPECT_EQ(stack_push(s, two), true);
  EXPECT_EQ(stack_peak(s), two);
  EXPECT_EQ(s->len, 2);
  EXPECT_EQ(stack_is_empty(s), false); 

  printf("passed 3\n");

  EXPECT_EQ(stack_pop(s), two);
  EXPECT_EQ(s->len, 1);
  EXPECT_EQ(stack_peak(s), one);
  printf("passed 4\n");

  EXPECT_EQ(stack_pop(s), one);
  printf("pass pop\n");
  EXPECT_EQ(stack_peak(s), 0x00);
  printf("pass peak\n");
  EXPECT_EQ(stack_is_empty(s), true); 
  printf("passed all \n");

  
}

} // namespace
