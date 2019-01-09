/*========================================================================================*/
/*                                                                                        */
/*                rmem executable model                                                   */
/*                =====================                                                   */
/*                                                                                        */
/*  This file is:                                                                         */
/*                                                                                        */
/*  Copyright Luc Maranget, INRIA Paris                                       2011-2012   */
/*  Copyright Peter Sewell, University of Cambridge                          2011, 2014   */
/*  Copyright Ohad Kammar, University of Cambridge (when this work was done)       2013   */
/*  Copyright Susmit Sarkar, University of St Andrews                              2014   */
/*  Copyright Shaked Flur, University of Cambridge                                 2017   */
/*                                                                                        */
/*  All rights reserved.                                                                  */
/*                                                                                        */
/*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.           */
/*  For author information see README.md.                                                 */
/*                                                                                        */
/*========================================================================================*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pwd.h>
#include <unistd.h>

#define MEGA (1 << 20) 

int showrtime = 1 ;
int showutime = 1 ;
int showstime = 1 ;


unsigned long timeout = 0 ;
unsigned long memory = 0 ;
unsigned long unlimitstack = 0 ;
int limitfile = 0 ;
unsigned long filemax = 0 ;
int testmode = 0 ;

int verbose = 0 ;
char *procstdout = NULL ;
char *procstderr = NULL ;
char *user = NULL ;

void changestd(int fd, char *name) {
  int temp_fd ;

  temp_fd = open(name, O_WRONLY|O_CREAT|O_TRUNC, 0666) ;
  if (temp_fd < 0) {
    perror("open") ;
    exit(2) ;
  }
  if (chmod(name, 0666) < 0) {
    if (!testmode) perror("Warning: chmod") ;
  }
  if (dup2(temp_fd, fd) < 0) {
    perror("dup2") ;
    exit(2) ;
  }
  if (close(temp_fd) < 0) {
    perror("close") ;
    exit(2) ;
  }
}

void dump_tval(FILE *fp, char *s, struct timeval *tv) {
  int secs = tv->tv_sec ;
  int csecs = (tv->tv_usec + 5000)/10000 ;
  secs += csecs / 100 ;
  csecs %= 100 ;
  if (testmode) {
    fprintf(fp, "%s%i.%02i\n", s, secs, csecs) ;
  } else {
    fprintf(fp, "%s%2i.%02i\n", s, secs, csecs) ;
  }
}

static pid_t  child_pid ;
static int killed = -1 ;

void killchild(int signum) {
  if (child_pid > 0) {
    if (kill(child_pid,signum)) {
      perror("kill") ;
      exit(2);
    }
  }
  killed = signum ;
  /*  if (signum != 14) exit(1) ; */
}

void install_sighandler(int signum, struct sigaction *p) {
  if (sigaction(signum,p,0)) {
    perror("sigaction") ;
    exit(2) ;
  }
}

int safeexec(const char *pgm, char *const argv[]) {
  pid_t pid ;

  fflush(stdout) ; fflush(stderr) ;

  {
    struct sigaction sa ;
    sa.sa_handler = killchild ;
    if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset") ;
      exit(2) ;
    }
    sa.sa_flags = 0 ;
    install_sighandler(1,&sa) ;
    install_sighandler(2,&sa) ;
    install_sighandler(3,&sa) ;
    install_sighandler(15,&sa) ;
  }
  pid = fork() ;

  if (pid > 0) {
    struct timeval start ;

    child_pid  = pid;
    if (gettimeofday(&start, NULL) < 0) {
      perror("gettimeofday") ;
      exit(2) ;
    }
    for ( ; ; ) {
      int status ;
      pid_t r = waitpid(pid, &status, 0) ;

      if (r < 0 && errno != EINTR) {
        perror("waitpid") ;
        exit(2) ;
      } else {
        struct rusage rusage ;
        struct timeval end ;
        struct timeval elapsed ;

        if (gettimeofday(&end, NULL) < 0) {
          perror("gettimeofday") ;
          exit(2) ;
        }
        if (end.tv_usec < start.tv_usec) {
          end.tv_usec += 1000000 ;
          end.tv_sec -= 1 ;
        }
        elapsed.tv_usec = end.tv_usec - start.tv_usec ;
        elapsed.tv_sec = end.tv_sec - start.tv_sec ;

        if (!testmode) {
          if (killed >= 0) {
            printf("signal: %i\n", killed) ;
          } else if (WIFEXITED(status)) {
            printf("status: %i\n", WEXITSTATUS(status)) ;
          } else if (WIFSIGNALED(status)) {
            printf("signal: %i\n", WTERMSIG(status)) ;
          }
        }

        if (getrusage(RUSAGE_CHILDREN, &rusage)) {
          perror("getrusage") ;
        } else {
          if (testmode) {
            dump_tval(stdout, "", &rusage.ru_utime) ;
          } else {
            if (showrtime) dump_tval(stdout, "real: ", &elapsed) ;
            if (showutime) dump_tval(stdout, "user: ", &rusage.ru_utime) ;
            if (showstime) dump_tval(stdout, " sys: ", &rusage.ru_stime) ;
          }
        }
        return 0 ;
      }
    }
  } else if (pid == 0) {
    struct rlimit limit ;

    if (user) {
      struct passwd *p = getpwnam(user) ;
      if (!p) {
        perror("getpwnam") ;
        exit(2) ;
        
      }
      if (setuid(p->pw_uid) < 0) {
        perror("setuid") ;
        exit(2) ;
      }
    } else {
      if (setuid(getuid()) < 0) {
        perror("setuid") ;
        exit(2) ;
      }
    }
    if (procstdout)
      changestd(1, procstdout) ;

    if (procstderr)
      changestd(2, procstderr) ;

    if (timeout) {
      if (getrlimit(RLIMIT_CPU, &limit) < 0) {
        perror("getrlimit") ;
        exit(2) ;
      }
      limit.rlim_cur = timeout ;
      if (setrlimit(RLIMIT_CPU, &limit) < 0) {
        perror("setrlimit") ;
        exit(2) ;
      }
    }

    if (memory) {
      if (getrlimit(RLIMIT_AS, &limit) < 0) {
        perror("getrlimit") ;
        exit(2) ;
      }
      limit.rlim_cur = memory * MEGA ;
      if (setrlimit(RLIMIT_AS, &limit) < 0) {
        perror("setrlimit") ;
        exit(2) ;
      }
    }

    if (limitfile) {
      if (getrlimit(RLIMIT_FSIZE, &limit) < 0) {
        perror("getrlimit") ;
        exit(2) ;
      }
      limit.rlim_cur = filemax*MEGA ;
      if (setrlimit(RLIMIT_FSIZE, &limit) < 0) {
        perror("setrlimit") ;
        exit(2) ;
      }
    }
    if (unlimitstack) {
      if (getrlimit(RLIMIT_STACK, &limit) < 0) {
        perror("getrlimit") ;
        exit(2) ;
      }
      limit.rlim_cur = limit.rlim_max ;
      if (setrlimit(RLIMIT_STACK, &limit) < 0) {
        perror("setrlimit") ;
        exit(2) ;
      }
    }
    /*
      if (getrlimit(RLIMIT_NPROC, &limit) < 0) {
      perror("getrlimit") ;
      exit(2) ;
      }
      limit.rlim_cur = 64 ;
      if (setrlimit(RLIMIT_NPROC, &limit) < 0) {
      perror("setrlimit") ;
      exit(2) ;
      }
    */
    if (execvp(pgm, argv)) {
      perror("execvp") ;
      exit(2) ;
    }
  } else {
    perror("fork") ;
    exit(2) ;
  }
  return 0 ;
}

void usage(const char *pgm) {
  fprintf(stderr, "usage: %s [options]* pgm [arg]*\n", pgm) ;
  fprintf(stderr, "  -v,      be verbose\n") ;
  fprintf(stderr, "  -V,      be very verbose\n") ;
  fprintf(stderr, "  -show s  show running times, default RUS\n") ;
  fprintf(stderr, "  -t n,    set cpu timeout in seconds, default unlimited\n") ;
  fprintf(stderr, "  -m n,    set memory limit (RSS), default unlimited\n") ;
  fprintf(stderr, "  -f n,    set file size limit in Mb, default unlimited\n") ;
  fprintf(stderr, "  -ustack, unlimit stack usage\n") ;
  fprintf(stderr, "  -o name, redirect stdout\n") ;
  fprintf(stderr, "  -e name, redirect stderr\n") ;
  fprintf(stderr, "  -u user, lauch pgm with identity 'user'\n") ;
  exit(2) ;
}

int main(int argc, char * const argv[]) {
  const char *pgm = argv[0] ;

  while (argc > 1 && argv[1][0] == '-') {
    if (strcmp(argv[1], "-t") == 0) {
      argc-- ;
      argv++ ;
      if (argc <= 1 || (timeout = strtoul(argv[1], NULL, 10)) == ULONG_MAX) {
        usage(pgm) ;
      }
    } else if (strcmp(argv[1], "-m") == 0) {
      argc-- ;
      argv++ ;
      if (argc <= 1 || (memory = strtoul(argv[1], NULL, 10)) == ULONG_MAX) {
        usage(pgm) ;
      }
    } else if (strcmp(argv[1], "-show") == 0) {
      argc-- ;
      argv++ ;
      if (argc <= 1) {
        usage(pgm) ;
      } else {
        char *arg = argv[1] ;
        while (*arg) {
          char c = *arg++ ;
          if (c == 'r') showrtime = 0 ;
          else if (c == 'R')  showrtime = 1 ;
          else if (c == 'u') showutime = 0 ;
          else if (c == 'U')  showutime = 1 ;
          else if (c == 's') showstime = 0 ;
          else if (c == 'S')  showstime = 1 ;
          else usage(pgm) ;
        }
      }
    } else if (strcmp(argv[1], "-f") == 0) {
      argc-- ;
      argv++ ;
      limitfile++ ;
      if (argc <= 1 || (filemax = strtoul(argv[1], NULL, 10)) == ULONG_MAX) {
        usage(pgm) ;
      }
    } else if (strcmp(argv[1], "-o") == 0) {
      argc-- ;
      argv++ ;
      if (argc <= 1) usage(pgm) ;
      procstdout = argv[1] ;
    } else if (strcmp(argv[1], "-e") == 0) {
      argc-- ;
      argv++ ;
      if (argc <= 1) usage(pgm) ;
      procstderr = argv[1] ;
    } else if (strcmp(argv[1], "-u") == 0) {
      argc-- ;
      argv++ ;
      if (argc <= 1) usage(pgm) ;
      user = argv[1] ;
    } else if (strcmp(argv[1], "-test") == 0) {
      testmode++ ;
    } else if (strcmp(argv[1], "-v") == 0) {
      verbose++ ;
    } else if (strcmp(argv[1], "-V") == 0) {
      verbose += 2 ;
    } else if (strcmp(argv[1], "-ustack") == 0) {
      unlimitstack = 1 ;
    } else {
      usage(pgm) ;
    }
    argc-- ;
    argv++ ;
  }

  if (argc <= 1) usage(pgm) ;

  return safeexec(argv[1], argv+1) ;
}
