#include "stdio.h"
#include "sys/types.h"
#include "sys/stat.h"
/* #include "/import/franz/lib/misc/lisp.h" */
/* #include "sparse-vector.h" */

#define bitref(a,i) (((char *)a)[(i) >> 3] >> ((i)&7) & 1)

FILE* open_byte8_stream(name,type)
    char *name, *type;
    {
    return fopen(name,type);
    }       

long set_byte8_stream_position(stream, position)
    FILE *stream;
    long position;
    {
    return fseek(stream,position,0);
    }


long get_byte8_stream_position(stream)
    FILE *stream;
    {
    return ftell(stream);
    }
    
long byte8_stream_length(stream)
    FILE *stream;
    {
    struct stat buf;
    int cur_end;
    
    fstat(fileno(stream),&buf);
    cur_end = ftell(stream)+stream->_cnt;
    return cur_end > buf.st_size ? cur_end : buf.st_size;
    }
    
void pad_byte8_stream (n,stream)
    int n;
    FILE *stream;
    {
    while(n--)
	putc('\0',stream);
    }

unsigned char byte8_read(stream)
    FILE *stream;
    {
    return getc(stream);
    }

unsigned char byte8_write(c,stream)
    char c;
    FILE *stream;
    {
    return putc(c,stream);
    }

unsigned int byte16_read(stream)
    FILE *stream;
    {
    return (getc(stream) << 8) | getc(stream);
    }

unsigned int byte16_write(i,stream)
    unsigned int i;
    FILE *stream;
    {
    putc((i>>8)&0xff,stream);
    putc(i&0xff,stream);
    return i;
    }
    
unsigned long read_byte32(stream)
    FILE *stream;
    {
    return ((getc(stream) << 24) | (getc(stream) << 16)
	    | (getc(stream) << 8)| getc(stream));
    }
    
unsigned long write_byte32(l,stream)
    FILE *stream;
    {
    putc(l>>24,stream);
    putc((l>>16) & 0xff,stream);
    putc((l>>8) & 0xff,stream);
    putc(l & 0xff,stream);
    }

    
int int29_read(stream)
    FILE *stream;
    {
    int shift,ans;
    int c;

    /*what to do if read EOF?*/
    
    shift = ans = 0;
    do	{
	if ((c = getc(stream))==EOF)
	    return -1;
	ans |= (c & 0x7F) << shift;
	shift += 7;
	} while (c & 0x80);
    return c ? ans : -ans;
    }

int int29_write(i_in,stream)
    int i_in;
    FILE *stream;
    {
    int negative = (i_in>>24)&0x80; /*sign bit to byte8 high bit*/
    int i = negative? -i_in:i_in;

    while (i & ~0x7f) /*more stuff*/
	{
	putc( (i&0x7f) | 0x80, stream);
	i >>= 7;
	}
    putc((i|negative),stream);
    if (negative)
	putc(0,stream);
    return i_in;
    }

#ifdef FUTURE
    
int countfile_read_into(lspfixv, stream)
    FILE *stream;
    LispVal lspfixv;
    {
    int length,feature;
    int *pfeatures, *pvalues;
    SPFIXV *spfixv;

    feature = 0;
    spfixv = (SPFIXV*)true_ptr(lspfixv);
    length = spfixv->length-1;  /*ignore sentinel*/
    pfeatures = V_fixnum(spfixv->features);
    pvalues = V_fixnum(spfixv->values);

    while (length--)
	{
	*pfeatures++ = (feature += int29_read(stream));
	*pvalues++ = int29_read(stream);
	}
    return length;
    }
 
 
int countfile_read_into_include_terms(spfixv, stream, include_terms)
    FILE *stream;
    LispVal lspfixv;
    char *include_terms;
    {
    int length, used, feature, value;
    int *pfeatures, *pvalues;
    SPFIXV *spfixv;
    
    spfixv = (SPFIXV*)true_ptr(lspfixv);
    include_terms = (char *)V_bit((LispVal *)include_terms);
    
    used = length = spfixv->length-1;  /*ignore sentinel*/
    pfeatures = V_fixnum(spfixv->features);
    pvalues = V_fixnum(spfixv->values);
    feature = 0;
     
    while (length--)
	{
	feature += int29_read(stream);
	value = int29_read(stream);
	if (bitref(include_terms, feature))
	    {
	    *pfeatures++=feature;
	    *pvalues++ = value;
	    }
	else
	    {
	    used--;
	    }
	}
    spfixv->length = 1+IntToFixnum(used);
    return lspfixv;
    }
    
int sfv_read_into(lsfv,stream,length)
    FILE *stream;
    int length;
    LispVal lsfv;
    {
    char *sfv;
    
    length <<=2; /*4 byte floats*/
    sfv = (char*)V_float(sfv);
    while(length--)
	*sfv++ = getc(stream);
    return lsfv;
    }

int fixv_read_into(fixv,stream,length)
    FILE *stream;
    int length;
    LispVal lfixv;
    {
    int *fixv;
    
    fixv = (char*)V_fixnum(fixv);
    while(length--)
	*fixv++ = int29_read(stream);
    return lfixv;
    }

int byte32_read_into(byte32v,stream,length)
    FILE *stream;
    int length;
    LispVal lbyte32v;
    {
    int *byte32v;
    
    byte32v = (int*)V_fixnum(byte32v);
    while(length--)
	*byte32v++ = getw(stream);
    return lbyte32v;
    }
    
     
#endif

