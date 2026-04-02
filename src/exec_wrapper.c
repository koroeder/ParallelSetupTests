#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

/**
    This is a wrapper for a call to exec made from Fortran.

    @param cmd The complete command line to execute, which must be null
           terminated.
    @return -1 if the exec call fails, otherwise will not return.
 */
int exec_wrapper(char* cmd)
{
    // The command to execute. Can be a complete path or just a file name.
    char* filename;
    // Store for the arguments. The first will be the file name as well. There
    // is an extra one at the end that becomes NULL.
    char** argv;
    // For loop counter.
    char* current;
    size_t i;
    // First pass through the cmd line. Here, we are just counting the number
    // of arguments. no becomes the number of arguments.
    size_t no = 0;
    int in_word = 0;


    // Check for very long command strings
    if (strlen(cmd) > 65536) { // Arbitrary large limit
       fprintf(stderr, "Command too long\n");
       return -1;
    }

    // Count arguments
    for (current = cmd; *current != '\0'; ++current) {
        // If we weren't in a word, but are now, set in_word and increase
        // argument number.
        if (!in_word && !isspace((unsigned char)*current)) {
            in_word = 1;
            ++no;
        }
        // If we were in a word, but aren't now, reset in_word.
        else if (in_word && isspace((unsigned char)*current)) {
            in_word = 0;
        }
    }

    // Now we know the number of arguments, we can allocate the array to hold
    // them. We add one extra, as the last element must be NULL.
    argv = (char**)malloc((no + 1) * sizeof(char*));
    if (!argv) {
        perror("argv: malloc failed");
        return -1;
    }

    // Initialise all to NULL.
    memset(argv, 0, (no + 1) * sizeof(char*));

    // Split command into argv    
    // Pass through the cmd line.
    // We need to split the command into the file name and different arguments.
    // We split the input string at each space.
    size_t arg_index = 0;
    char* current_start = NULL;
    for (current = cmd, i = 0; *current != '\0' && i != no; ++current) {
        if (current_start == NULL && !isspace((unsigned char)*current)) {
            current_start = current;

        // If we were in a word, but aren't now, replace the space with a null
        // and set not being in a word.
        } else if (current_start != NULL && isspace((unsigned char)*current)) {
            *current = '\0';
            if (i == 0) {
                filename = current_start;
            } else {
                argv[i] = current_start;
            }
            ++i;
            current_start = NULL;
        }
    }

    // We may have ended a word with '\0', so we might have a final word to add.
    if (current_start != NULL && i < no) {
        if (i == 0) {
            filename = current_start;
        } else {
            argv[i] = current_start;
        }
    }

    // Extract filename (basename)
    // Now we need to fiddle the file name. The filename pointer contains the
    // path as specified (which might just be the name itself). argv[0] is
    // supposed to contain only the name. So, we just track back from the end
    // of filename until we get to the start or a '/'.
    for (current_start = filename + strlen(filename) - 1;
         current_start != filename && *current_start != '/';
         --current_start);
    if (*current_start == '/') {
        // We found a /. Take the section following it.
        argv[0] = current_start + 1;
    } else {
        argv[0] = filename;
    }

    // Everything is ready. Call exec.
    execvp(filename, argv);
    perror("execvp failed");

    // Cleanup and return
    free(argv);
    return -1;
}
