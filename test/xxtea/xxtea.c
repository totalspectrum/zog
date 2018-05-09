#include <stdio.h>

#include "xxtea.h"


void btea(uint32_t *v, int n, uint32_t const k[4])
{
    uint32_t y, z, sum;
    unsigned p, rounds, e;
    if (n > 1)
    {                            /* Coding Part */
        rounds = 6 + 52/n;
        sum = 0;
        z = v[n-1];
        do
        {
            sum += DELTA;
            e = (sum >> 2) & 3;
            for (p=0; p<n-1; p++)
                y = v[p+1], z = v[p] += MX;
            y = v[0];
            z = v[n-1] += MX;
        } while (--rounds);
    }                            /* Decoding Part */
    else if (n < -1)
    {
        n = -n;
        rounds = 6 + 52/n;
        sum = rounds*DELTA;
        y = v[0];
        do
        {
            e = (sum >> 2) & 3;
            for (p=n-1; p>0; p--)
                z = v[p-1], y = v[p] -= MX;
            z = v[n-1];
            y = v[0] -= MX;
        } while ((sum -= DELTA) != 0);
    }
}


