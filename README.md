# TransformersStepByStep

This is a tutorial on monad transformers.

# The PDF

The PDF version of the tutorial is in subdirectory `pdf`.

# Playing with the code

You can load the code into ghci and play with it like this:

```
ghci Transformers.lhs
:t eval2b
```

# How to build

On Debian 10, the following should work and create Transformers.pdf:

```
sudo apt install texlive

lhs2TeX Transformers.lhs > Transformers.tex
pdflatex Transformers.tex
bibtex Transformers.aux
pdflatex Transformers.tex
```

