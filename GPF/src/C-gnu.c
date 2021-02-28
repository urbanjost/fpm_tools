/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/gnu.c - M_DRAW driver for GNU plotutil 'plot' format"
#ident "@(#)M_DRAW:author : John S. Urban"
#ident "@(#)M_DRAW:version : 1.0"

/*

   Need a very simple ASCII meta-file for long-term (40-year) retention.
   Instead of reinventing the wheel, using a subset of the GNU Plotutils
   plot(1) metafile format. The very simple very low-level ASCII file
   produced is

      o easily parsed at a later time to recreate plot images in the future

      o allows access to a preexisting metafile reader (plot(1)) that
        supports formats currently not directly supported by M_DRAW such as
        animated pseudo-gif files and encapsulated PostScript and WebCGM.
        If you already have ghostview(1) and display(1) or xv(1) or
        netpbm(1) and ralcmg(1) or gplot(1) then you can already convert
        to PNG and encapsulated PostScript and various CGM formats from
        existing M_DRAW drivers; but the GNU Plotutils package supplies
        support of all these vector formats (plus PNG/PPM pixmaps)
        from one package.

   Should create a M_DRAW-based reader too (that was the original idea)
   but vog driver is readable by USH already and plot(1) gives
   good access to other devices with no effort other than to build GNU
   plotutils and make some simple scripts to get "nice" page layouts.

   Note that use of GNU plot(1) was already possible with unixplot driver
   but color, fill, hardware text and linethickness were not supported
   and the unixplot file is binary and Endian-dependent.

   NEXT TIME IN:

   A variant where instead of each page being <o ... x> only one o and one x for
   entire document, so can display animations and not pop up huge numbers of
   X windows using the GNU plot(1) command if many frames in a file.

*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "draw.h"

#define XSPACE_SIZE      65536
#define YSPACE_SIZE      65536

#define POINT(x, y)     (0x10000 * (y) + (x))
#define MAXCOLOR        256
static int MAXCOLOR_used;

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))


extern FILE     *_draw_outfile();

static int      gnu_first_time = 1;
static int      drawn = 0;
static int      curcol = 0;          /* black */
static int      gnulstx = -1, gnulsty = -1; /* last (x, y) drawn */

extern float hardwidth[128];

struct rgb_color{
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
};

static struct rgb_color carray[MAXCOLOR];

extern  FILE     *draw_fp;

/* How to convert degrees to radians */
#ifndef PI
#define PI      3.14159265358979323846264338327950288419716939937510
#endif
#define d2r(x)     ((x) * PI / 180.0)
#define r2d(x)     ((x) * 180.0 / PI)

static int gnu_string(char *s);

static int      curwid = 1; /* Current pen width */
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value */
static int gnu_mapcolor(int i, int r, int g, int b) {
   if(i >= MAXCOLOR ){
      return(-1);
   }
   carray[i].red = (unsigned short)(r);
   carray[i].green = (unsigned short)(g);
   carray[i].blue = (unsigned short)(b);
   MAXCOLOR_used = MAX(MAXCOLOR_used,i+1);
   /* HAS TO BE PUT OUT IN A CERTAIN POSITION AT TOP OF FILE */
   /* fprintf(draw_fp,"- %d %d %d %d\n",i+1,(r+1)*256-1,(g+1)*256-1,(b+1)*256-1); */
   return(0);
}
/******************************************************************************/
/*
 * gnu_init
 *
 *      Set up the environment. Returns 1 on success.
 */
static int gnu_init(void) {
        int i;
        int prefx, prefy, prefxs, prefys;

        draw_fp = _draw_outfile();

        if (!gnu_first_time)
                return(1);

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        } else{
           vdevice.sizeSy = YSPACE_SIZE;  /* size in resolution rasters */
           vdevice.sizeSx = XSPACE_SIZE;  /* size in resolution rasters */
           vdevice.sizeX = vdevice.sizeY = MIN(XSPACE_SIZE,YSPACE_SIZE); /* current viewport to use */
        }

        fprintf(draw_fp,"#PLOT 2\n");
        fprintf(draw_fp,"# version 2.4.1 GNU plotutils portable(ASCII) metatfile subset generated by M_DRAW\n");
        fprintf(draw_fp,"# meanings:\n");
        fprintf(draw_fp,"# one command per line, values are space delimited.\n");
        fprintf(draw_fp,"#  $ xval yval           # move to <float,float>\n");
        fprintf(draw_fp,"#  ) xval yval           # draw or continue to <float,float>\n");
        fprintf(draw_fp,"#\n");
        fprintf(draw_fp,"# Set colors: integer values in range 0 to 65536-1 using RGB color model\n");
        fprintf(draw_fp,"#  ~ red green blue      # background color\n");
        fprintf(draw_fp,"#  D red green blue      # fill color\n");
        fprintf(draw_fp,"#  - red green blue      # pen color\n");
        fprintf(draw_fp,"#\n");
        fprintf(draw_fp,"#  0 width               # line thickness in user coordinates\n");
        fprintf(draw_fp,"#  s xmin ymin xmax ymax # coordinate space\n");
        fprintf(draw_fp,"#  e                     # erase\n");
        fprintf(draw_fp,"#  o                     # open plot\n");
        fprintf(draw_fp,"#  x                     # close plot\n");
        fprintf(draw_fp,"#  NOTE: when using plot(1) if append metafiles attributes are carried on thru o/x\n");
        fprintf(draw_fp,"#  O                     # restore state\n");
        fprintf(draw_fp,"#  U                     # save state \n");
        fprintf(draw_fp,"#  E                     # end path\n");
        fprintf(draw_fp,"#  L saturation          # fill mode (0=off, 1->65535 is color saturation level\n");
        fprintf(draw_fp,"#  Tjklabel              # j=lcr (left, center, right) k=tcbx (top, center, bottom, baseline)\n");
        fprintf(draw_fp,"#  Ffontname             #\n");
        fprintf(draw_fp,"#  7 fontsize            # font size \n");
        fprintf(draw_fp,"#  ( textangle           # text path angle in degrees; ccw+, start angle 0-x\n");
        fprintf(draw_fp,"#  Jjoinmode             # miter round bevel\n");
        fprintf(draw_fp,"#  flinemode             # \n");
        fprintf(draw_fp,"#GNU documentation is not clear on some issues, so experimented with plot(1)\n");
        fprintf(draw_fp,"#and took conservative interpretations of what gets reset on a page clear or page end and so on\n");
        fprintf(draw_fp,"#to minimize issues with appended meta files and plotting of single pages and such.\n");
        fprintf(draw_fp,"#\n");
        fprintf(draw_fp,"o\nU\ns 0 0 %d %d\n",vdevice.sizeX,vdevice.sizeY);
        fprintf(draw_fp,"Jround\n");  /* set miter join method */
        fprintf(draw_fp,"FHelvetica\n");

        vdevice.depth = 8;

        for (i=0; i < MAXCOLOR; i++){
           carray[i].red=0;
           carray[i].green=0;
           carray[i].blue=0;
        }

        /* initial color table */
        gnu_mapcolor(0, 255, 255, 255);
        gnu_mapcolor(1, 255,   0,   0);
        gnu_mapcolor(2,   0, 255,   0);
        gnu_mapcolor(3, 255, 255,   0);
        gnu_mapcolor(4,   0,   0, 255);
        gnu_mapcolor(5, 255,   0, 255);
        gnu_mapcolor(6,   0, 255, 255);
        gnu_mapcolor(7,   0,   0,   0);
        gnu_mapcolor(8, 155,   0,   0);
        gnu_mapcolor(9,   0, 155,   0);
        gnu_mapcolor(10, 155, 255, 255);
        gnu_mapcolor(11, 155, 155,   0);
        gnu_mapcolor(12,   0,   0, 155);
        gnu_mapcolor(13, 155,   0, 155);
        gnu_mapcolor(14,   0, 155, 155);
        gnu_mapcolor(15, 100, 100, 100);

        MAXCOLOR_used = 16;

        return(1);
}
/******************************************************************************/
static int gnu_exit(void) { /* Flush remaining data and close output file if necessary.  */
    putc('O',draw_fp);   /* restore state */
    putc('\n',draw_fp);
    putc('x',draw_fp);   /* end page */
    putc('\n',draw_fp);
   fflush(draw_fp);
   if (draw_fp != stdout && draw_fp != stderr ) {
                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
   }
   return(0);
}
/******************************************************************************/
static int gnu_draw(int x, int y) {                /* draw to an x, y point.  */
   if (gnulstx != vdevice.cpVx || gnulsty != vdevice.cpVy) {
      fprintf(draw_fp,"$ %d %d\n", vdevice.cpVx, vdevice.cpVy );  /* move */
   }
   fprintf(draw_fp,") %d %d\n", x, y );
   gnulstx = x;
   gnulsty = y;
   drawn = 1;
   return(0);
}
/******************************************************************************/
/* load in small or large - could be improved. Radically KLUDGED; made SoftText extern  */
static int gnu_font(char *fontname) {
   int i;
   float rat;
   /* assuming vdevice.hwidth is the desired width of the reference character,
    * this is a list of percentages of the other character widths.
    */
   static int helvetica_w[128] = {
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,
      27,33,43,55,55,88,72,28,33,33,39,58,27,33,28,28,55,55,55,55,55,55,55,55,55,55,
      34,34,58,58,58,61,97,
      73,73,73,73,66,61,78,72,28,55,73,61,83,72,78,66,78,72,67,61,72,66,94,66,66,61,
      33,28,33,58,55,28,
      55,61,55,61,55,33,61,61,28,28,55,28,88,61,61,61,61,39,55,33,61,55,78,55,55,50,
      39,28,39,58,0
   };

   vdevice.attr->a.softtext = SOFTHARDWARE;
   /* textsize will be obeyed after the font is set
    * maybe should read current software size and convert virtual
    * to device instead of resetting each time */

      if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
         vdevice.hwidth=11.0*300.0/72.0;
         vdevice.hheight=11.0*300.0/72.0;
      }

   if (strcmp(fontname, "small") == 0) {
      rat=0.80; /* Kludge Factor */
      fprintf(draw_fp,"FTimes-Bold\n");
      for (i = 0; i < 128; i++){
         hardwidth[i]=1.00 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
         }
   } else if (strcmp(fontname, "large") == 0) {
      fprintf(draw_fp,"FHelvetica\n");
      rat=1.26; /* Kludge Factor */
      rat=1.00; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=((float)helvetica_w[i])/100.0 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
      }
   } else{
      return(0);
   }

   return(1);
}
/******************************************************************************/
static int gnu_clear(void) {                    /* gnu_clear - Erase the plot */
   if (drawn) {
      putc('x',  draw_fp);  /* end old page */
      putc('\n', draw_fp);

      putc('o',  draw_fp);  /* open new page */
      putc('\n', draw_fp);


      fprintf(draw_fp,"Jround\n");  /* set miter join method */

      fprintf(draw_fp,"~ %d %d %d\n",
         (carray[curcol].red+1)*256-1,
         (carray[curcol].green+1)*256-1,
         (carray[curcol].blue+1)*256-1);  /* BGCOLOR */

       /* size in resolution rasters */
       fprintf(draw_fp,"U\ns 0 0 %d %d\n",vdevice.sizeX,vdevice.sizeY);

      putc('e',  draw_fp);  /* clear new page */
      putc('\n', draw_fp);
      fprintf(draw_fp,"0 %d\n",curwid*vdevice.sizeX/10000); /* testing with plot(1) shows linewidth not retained after clear */
   }
   drawn = 0;
   return(0);
}
/******************************************************************************/
static int gnu_setlw(int rasters) {      /* Change the thickness of the lines */
   curwid=ABS(rasters);
   fprintf(draw_fp,"0 %d\n",curwid*vdevice.sizeX/10000);
   return(0);
}
/******************************************************************************/
/*
 * Line style map
 */
static char     *stylemap[7 + 1] = {
        "solid",
        "dotted",
        "shortdashed",
        "longdashed",
        "dotdashed",
        "solid",
        "solid",
        "solid"
};
static int gnu_color(int col) {              /* Change the color of the lines */
   if (col > MAXCOLOR){
      return(0);
   }else if(col < 0 ){
      gnu_setlw(ABS(col));
      return(0);
   }
   fprintf(draw_fp,"D %d %d %d\n",
      (carray[col].red+1)*256-1,
      (carray[col].green+1)*256-1,
      (carray[col].blue+1)*256-1
      );  /* FILLCOLOR */
   fprintf(draw_fp,"- %d %d %d\n",
      (carray[col].red+1)*256-1,
      (carray[col].green+1)*256-1,
      (carray[col].blue+1)*256-1
      );  /* PENCOLOR */
   curcol = col;
   fprintf(draw_fp, "f%s\n", stylemap[(int)(col/MAXCOLOR)]);
   return(0);
}
/******************************************************************************/
static int gnu_char(char c) {                           /* Output a character */
  char  s[2];
  s[0] = c; s[1]='\0';
  gnu_string(s);
  return(0);
}
/******************************************************************************/
/* output a character string using current character size and rotation angle. */
static int gnu_string(char *s) {
        float rot;

        if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
           fprintf(stderr,"*gnu_string* ERROR: ZERO SIZE CHARACTERS\n");
           vdevice.hwidth=11.0*300.0/72.0;
           vdevice.hheight=11.0*300.0/72.0;
        }

        rot=r2d(atan2((double)vdevice.attr->a.textsin,(double)vdevice.attr->a.textcos));
        fprintf(draw_fp,"(%f\n",rot);                                    /* text angle */
        fprintf(draw_fp,"7 %f\n",vdevice.hwidth);       /* text size  */
        fprintf(draw_fp,"#7 %f\n",vdevice.hheight);     /* text size  */
        if (gnulstx != vdevice.cpVx || gnulsty != vdevice.cpVy) {
           fprintf(draw_fp,"$ %d %d\n",vdevice.cpVx, vdevice.cpVy);      /* current position */
        }
        fprintf(draw_fp,"Tbl%s\n", s);                                   /* bottom left justified string */
        drawn = 1;
        gnulstx = gnulsty = -1;
        return(0);
}
/******************************************************************************/
static int gnu_fill(int n, int x[], int y[]) {   /* fill and draw the polygon */
   int     i;
   fprintf(draw_fp,"# POLYGON\nL 1\n$ %d %d\n", x[0], y[0]);
   for (i = 1; i < n; i++)
   {
      fprintf(draw_fp,") %d %d\n", x[i], y[i]);
   }
   fprintf(draw_fp,") %d %d\nE\nL 0\n# END POLYGON\n", x[0], y[0]);
   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];
   gnulstx = gnulsty = -1;
   drawn = 1;
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry gnudev = {
        "gnu",          /* name of device */
        "large",        /* name of large font */
        "small",        /* name of small font */
        noop,           /* set drawing into back buffer */
        gnu_char,       /* draw a hardware character */
        noop,           /* check if a key was hit */
        gnu_clear,      /* clear screen to current color */
        gnu_color,      /* set current color */
        gnu_draw,       /* draw a line */
        gnu_exit,       /* exit graphics */
        gnu_fill,       /* fill a polygon */
        gnu_font,       /* set hardware font */
        noop,           /* set drawing into front buffer */
        noop,           /* wait for and get the next key hit */
        gnu_init,       /* initialize the device */
        noop2,          /* get mouse/crosshair position */
        gnu_mapcolor,   /* set color indices */
        gnu_setlw,      /* Set line width */
        gnu_string,     /* draw a hardware string */
        noop,           /* swap front and back buffers */
        noop            /* syncronize the display */
};
/******************************************************************************/
int _GNU_draw_devcpy(void) {        /* copy the gnu plot device into vdevice.dev.  */
        vdevice.dev = gnudev;
        return(0);
}
/******************************************************************************/
