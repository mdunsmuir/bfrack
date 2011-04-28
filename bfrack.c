#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define TAPELEN 30000
#define PROGLEN 30000

int main(int argc, char **argv)
{
  char tape[TAPELEN];
  char program[PROGLEN];
  unsigned int data_ptr = 0, prog_ptr = 0, bcount = 0;

  /*
    read script from file
  */
  FILE *file = fopen(argv[1], "r");
  if(file == NULL){
    printf("error -- failed to open file %s\n", argv[1]);
    return 0;
  }
  
  while(fgets(&program[prog_ptr], PROGLEN - prog_ptr, file)){
    if(prog_ptr >= PROGLEN - 1){
      puts("error -- file too long!");
      return 0;
    }
    prog_ptr = (unsigned int)strlen(program);
  }

  for(data_ptr = 0; data_ptr < TAPELEN; data_ptr++){
    tape[data_ptr] = 0;
  }; data_ptr = 0;
  
  //  puts("done reading script, executing...");

  /*
    execute program
  */

  prog_ptr = 0;
  while(prog_ptr < strlen(program)){
    switch(program[prog_ptr]){
    case '>':
      data_ptr++;
      if(data_ptr >= TAPELEN){
	puts("error -- off left end of tape!");
	return 0;
      }
      break;
    case '<':
      data_ptr--;
      if(data_ptr >= TAPELEN){
	puts("error -- off right end of tape!");
	return 0;
      }
      break;
    case '+':
      tape[data_ptr]++;
      break;
    case '-':
      tape[data_ptr]--;
      break;
    case '.':
      printf("%c", tape[data_ptr]);
      break;
    case ',':
      tape[data_ptr] = getchar();
      break;
    case '[':
      if(tape[data_ptr] == 0){
	bcount = 1;
	while(bcount > 0){
	  prog_ptr++;
	  if(prog_ptr >= strlen(program)){
	    puts("error -- mismatched brackets!");
	    return 0;
	  }
	  switch(program[prog_ptr]){
	  case '[':
	    bcount++;
	    break;
	  case ']':
	    bcount--;
	    break;
	  }
	}
      }
      break;
    case ']':
      if(tape[data_ptr] > 0){
	bcount = 1;
	while(bcount > 0){
	  prog_ptr--;
	  if(prog_ptr < 0){
	    puts("error -- mismatched brackets!");
	    return 0;
	  }
	  switch(program[prog_ptr]){
	  case '[':
	    bcount--;
	    break;
	  case ']':
	    bcount++;
	    break;
	  }
	}
      }
      break;
    }
    prog_ptr++;
  }

  return 0;
}
