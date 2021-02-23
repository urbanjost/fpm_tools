#!/bin/bash
cd $(basename $0)/..
cd docs
fpm help manual >manual.man
txt2man manual.man >manual.1
man2html manual.1 >manual.html
exit
