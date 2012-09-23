
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>

/* Holds the name of this program */
const char* program_name;

/**
 * Processes command line arguments
 * @param argc Command line parameter count
 * @param argv Command line parameter data
 */
void process_options(int argc, char* argv[]);

/**
 * Prints the usage statement then quits
 * @param stream Where to print the usage statement to
 * @param exit_code What code to terminate with
 */
void print_usage(FILE* stream, int exit_code);

/*
 * Main program entry
 */
int main(int argc, char* argv[]) {

   /* get the program's name */
   program_name = argv[0];

   /* process the command line */
   process_options(argc, argv);

   return 0;
}

/**
 * Processes command line arguments
 * @param argc Command line parameter count
 * @param argv Command line parameter data
 */
void process_options(int argc, char* argv[]) {
   int next_option;

   /* Short listing of arguments */
   const char* const short_options = "h";

   /* Command line arguments */
   const struct option long_options[] = {
      /*
      { long, takes_argument, NULL, short }
      */
      { "help", 0, NULL, 'h' },
      { NULL  , 0, NULL, 0   }      /* required to end the array */
   };

   do {
      /* process the next item */
      next_option = getopt_long(
            argc, argv,
            short_options, long_options,
            NULL
      );

      /* interpret the next option */
      switch (next_option) {
      case 'h':
         /* print the usage meessage and terminate */
         print_usage(stdout, 0);

      /* -- switch that expects an argument
      case 'o':
         some_var = optarg;
      */

      case '?': /* invalid user specified option */
         /* print the usage message and terminate */
         print_usage(stdout, 1);

      case -1: /* done with options */
         break;

      default: /* something else happened */
         abort();
      }
   } while (next_option != -1);
}

/**
 * Prints the usage statement then quits
 * @param stream Where to print the usage statement to
 * @param exit_code What code to terminate with
 */
void print_usage(FILE* stream, int exit_code) {

   /* show the usage message */
   fprintf(stream, "Usage: %s options [ inputfile ... ]\n", program_name);
   fprintf(stream,
         "  -h  --help         Display this usage information\n"
   );

   /* terminate */
   exit(exit_code);
}


