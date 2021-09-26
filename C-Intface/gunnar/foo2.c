/* simple C-routine to test interface C / SIMULA; */
#include <simula.h>
short_integer foo2 (i,j) short_integer i,j;
{
	integer k;
	boolean b;
	text th;
	real rt;
	long_real lr;
	name na;
	character ch;
	return (4711 * i) - j;
}
