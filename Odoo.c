// Write a program that outputs sequentially the integers from 1 to 99 but on some conditions prints a string instead:
//   when the integer is a multiple of 3 print “Open” instead of the number,
//   when it is a multiple of 7 print “Source” instead of the number,
//   when it is a multiple of both 3 and 7 print “OpenSource” instead of the number.
#include <iostream>

main ( int argc, char **argv ){
  int i = 0;
  for(int i = 0; i < 99; i++)
    switch(i){
    case i % 3 == 0: cout << "Open";
    }
}
