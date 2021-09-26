#include <float.h>
#include <stdio.h>
main()
{
   double x,y,diff;
   x= 5.46040095964857E-303;
   y= 5.46040095964852E-303;
   diff= x-y;
   diff= diff / x;
   diff= abs(diff);
   printf("The result is %f \n",diff);
}

