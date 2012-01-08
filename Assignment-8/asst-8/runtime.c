#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <ucontext.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#define stack_size 100000
#define heap_size 100000

#ifdef __APPLE__
#define SCHEME_ENTRY scheme_entry
#endif
#ifdef __linux__
#define SCHEME_ENTRY _scheme_entry
#endif 

extern long SCHEME_ENTRY(char *, char *); 

/* locally defined functions */
static char *guarded_area(long n);
#ifdef __APPLE__
static void segv_handler(int signo, siginfo_t *info, void *ignore);
#endif
#ifdef __linux__
static void segv_handler(int signo, struct sigcontext sc);
#endif
static void bus_handler(int signo);
static void usage_error(char *who);
static void print(long x);

/* local stack/heap management variables */
static long pagesize;
static char *heap;
static char *stack;
static long heapsize;
static long stacksize;

int main(int argc, char *argv[]) {
  struct sigaction action;
  sigset_t s_set;
  int n;

  pagesize = sysconf(_SC_PAGESIZE);

  stacksize = stack_size * sizeof(void *);
  heapsize = heap_size * sizeof(void *);

  for (n = 1; n < argc; n++)
    if ((*argv[n] == '-') && (*(argv[n]+2) == 0))
      switch (*(argv[n]+1)) {
        case 'h': /* heap size option */
          argv[n] = (char *)NULL;
          if (++n == argc) usage_error(argv[0]);
          heapsize = atoi(argv[n]);
          if (heapsize <= 0) usage_error(argv[0]);
          break;
        case 's': /* stack size option */
          argv[n] = (char *)NULL;
          if (++n == argc) usage_error(argv[0]);
          stacksize = atoi(argv[n]);
          if (stacksize <= 0) usage_error(argv[0]);
          break;
        default:
          usage_error(argv[0]);
      }
    else
      usage_error(argv[0]);
   
 /* round stack and heap sizes to even pages */
  stacksize = ((stacksize + pagesize - 1) / pagesize) * pagesize;
  heapsize = ((heapsize + pagesize - 1) / pagesize) * pagesize;

  stack = guarded_area(stacksize);
  heap = guarded_area(heapsize);

 /* Set up segmentation fault signal handler to catch stack and heap
  * overflow and some memory faults */
  sigemptyset(&s_set);
#ifdef __linux__
  action.sa_handler = (void *)segv_handler;
  action.sa_flags = SA_RESETHAND;
#else
  action.sa_sigaction = segv_handler;
  action.sa_flags = SA_SIGINFO | SA_RESETHAND;
#endif
  action.sa_mask = s_set;
  if (sigaction(SIGSEGV, &action, NULL)) {
    fprintf(stderr, "sigaction failed: %s\n", strerror(errno));
    fprintf(stderr, "  overflow checking may not work\n");
  }

 /* Set up bus error signal handler to catch remaining memory faults */
  sigemptyset(&s_set);
  action.sa_handler = bus_handler;
  action.sa_mask = s_set;
  action.sa_flags = SA_RESETHAND;
  if (sigaction(SIGBUS, &action, NULL)) {
      fprintf(stderr, "sigaction failed: %s\n", strerror(errno));
  }

 /* run the Scheme program and print the result */
  print(SCHEME_ENTRY(stack, heap));
  printf("\n");

  return 0;
}

/* allocate a chunk of memory with a guard page on either end */
static char *guarded_area(long n) {  /* n must be page aligned */
  char *addr;

 /* allocate, leaving room for guard pages */
  addr = (char *)mmap(NULL,
                      (size_t)(n + 2 * pagesize),
                      PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON,
                      -1, 0);
  if (addr == (char *)-1) {
    fprintf(stderr, "mmap failed: %s\n", strerror(errno));
    exit(2);
  }

 /* remove access rights from the guard pages */
  if (mprotect(addr, (size_t)pagesize, PROT_NONE) ||
    mprotect(addr + pagesize + n, (size_t)pagesize, PROT_NONE)) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
    exit(3);
  }
   
  return addr + pagesize;
}

/* Signal handler that traps SIGSEGV and checks if the violation
 * might have been caused by stack or heap overflow */
#ifdef __APPLE__
static void segv_handler(int signo, siginfo_t *info, void *ingore) {
#endif
#ifdef __linux__
static void segv_handler(int signo, struct sigcontext sc) {
#endif
  char *addr;

#ifdef __APPLE__
  addr = (char *)info->si_addr;
#endif
#ifdef __linux__
  addr = (char *)(sc.cr2);
#endif

  if (heap-pagesize <= addr && addr < heap) {
    fprintf(stderr,"invalid access just below the heap\n");
  } else if (heap+heapsize <= addr && addr <= heap+heapsize+pagesize) {
    fprintf(stderr,"invalid access just above the heap\n");
  } else if (stack-pagesize <= addr && addr < stack) {
    fprintf(stderr,"invalid access just below the stack\n");
  } else if (stack+stacksize <= addr && addr < stack+stacksize+pagesize) {
    fprintf(stderr,"invalid access just above the stack\n");
  } else {
    fprintf(stderr, "Segmentation violation\n");
  }

  exit(-1);
}

/* Signal handler for bus errors */
static void bus_handler(int signo) {
  fprintf(stderr, "Bus error\n");
  exit(-1);
}

static void usage_error(char *who) {
  fprintf(stderr, "usage: %s [-h <heap size>] [-s <stack size>]\n", who);
  fprintf(stderr, "   specify sizes in pages (base 10)\n");
  fprintf(stderr, "   page size is %ld bytes\n",pagesize);
  exit(1);
}

static void print(long x) {
    printf("%ld", x);
} 
