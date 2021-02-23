# fexpand - a filter to expand tabs in a file

As documented in the [fexpand](https://urbanjost.github.io/fexpand/fexpand.1.html)
man-page (re-formatted as HTML) the `fexpand program expands tabs in files.
It is very similar to simple versions of the ULS command expand(1).

## BUILDING
To build it requires `git`, `fpm`(Fortran Package Manager), a modern
Fortran compiler and WWW access. It was tested with

   + GNU Fortran (GCC) 8.3.1 20191121 
   + ifort (IFORT) 19.1.3.304 20200925

```bash
   # ACCESSING

   # go to where you want to create the `fexpand` directory
   mkdir github
   cd github
   # get a clone of the repository
   git clone https://github.com/urbanjost/fexpand.git
   # enter the repository directory
   cd fexpand

   # BUILDING AND INSTALLING

   # install (in the default location)
   fpm install 

   # TRY IT

   # if you placed the program in a directory in your command path you are ready to go!
   ls|fexpand 
```

