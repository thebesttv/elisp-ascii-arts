#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>

#define XSIZ 60
#define YSIZ 20
#define MAXTABLE (256*5)

static unsigned char *bitmap;
static unsigned int table[MAXTABLE];

static void
gentable (void)
{
  unsigned int i, p2;
  int minus = 800 / YSIZ;
  if (minus == 0)
    minus = 1;
  for (i = 0; i < MAXTABLE; i++)
    {
      if (i > minus)
	{
	  p2 = (i - minus) / 5;
	  table[i] = p2;
	}
      else
	table[i] = 0;
      printf("%u ", table[i]);
    }
  getchar();
}


static void firemain (void)
{
  unsigned char *p;
#define END (bitmap + XSIZ * YSIZ)
  for (p = bitmap;
       p <= (unsigned char *) (END); p += 1)
    {
      *p = table[
		  (*(p + XSIZ - 1) + *(p + XSIZ + 1) + *(p + XSIZ)) +
		  (*(p + 2 * XSIZ - 1) + *(p + 2 * XSIZ + 1))];
    }
}

unsigned char get(int i, int j){
  unsigned char *p = bitmap + i * XSIZ + j;
  return *p;
}

void print_bitmap(){
  static int max = 0;
  for(int i = 0; i < YSIZ; i+=1){
    for(int j = 0; j < XSIZ; j+=1){
      int sum = get(i, j);
      if (sum >= max) max = sum;
      printf("%2d ", sum);
    }
    putchar('\n');
  }
  printf("=== %d ===\n",max);
}


#define min(x,y) ((x)<(y)?(x):(y))
static void
drawfire (void)
{
  unsigned int i, last1, i1, i2;
  static int loop = 0, sloop = 0, height = 0;
  register unsigned char *p;
  height++;
  loop--;
  if (loop < 0)
    loop = rand () % 3, sloop++;;
  i1 = 1;
  i2 = 4 * XSIZ + 1;
  int max = 0;
  for (p = (char *) bitmap + XSIZ * (YSIZ + 0);
       p < ((unsigned char *) bitmap + XSIZ * (YSIZ + 1));
       p++, i1 += 4, i2 -= 4)
    {
      last1 = rand () % min (i1, min (i2, height));
      i = rand () % 6;
      for (;
           p < (unsigned char *) bitmap + XSIZ * (YSIZ + 1) && i != 0;
           p++, i--, i1 += 4, i2 -= 4){
	*p = last1;
        last1 += rand () % 6 - 2;
        *(p + XSIZ) = last1;
        last1 += rand () % 6 - 2;
      }
      *(p + 2 * XSIZ) = last1;
      last1 += rand () % 6 - 2;
    }
  i = 0;
  printf("!!! %d !!!\n",max);
  firemain ();

  print_bitmap();
  for(int i = 0; i < (1<<26); i++);
}

int main(void){
  srand(0);
  bitmap = (unsigned char*) malloc(XSIZ * (YSIZ + 3));

  gentable();

  while(1){
    drawfire();
  }
}
