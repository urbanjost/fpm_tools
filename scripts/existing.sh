#!/bin/bash

# push an existing repository from the command line

git remote add origin https://github.com/urbanjost/fpm_tools.git
git branch -M main
git push -u origin main

exit

# create a new repository on the command line

echo "# fpm_tools" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M main
git remote add origin https://github.com/urbanjost/fpm_tools.git
git push -u origin main
