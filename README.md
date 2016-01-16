# Various Fortran code

These Fortran routines were created in 2006--2007 while re-implementing my PhD thesis work. The original implementation was done in Matlab, finished in 2004. Fortran got the job done; it's very fast. However, I do not expect to actively write Fortran code again.

This folder contains two types of Fortran routines:

(1) very basic Fortran routines of general interest.
The functionalities are on the level that,
in some high level languages such as Matlab,
they are built-in functions.
Each of these functions carries out a well defined, simple, general
task that does not usually fall into a narrow category.

(2) Fortran procedures that are not so "basic" or "general" to be on
the language level, but rather involve some
level of algorithms. Examples:

  numerical integration
  evaluation
  root finding
  special functions
  minimization and maximization (optimization)
  quantiles
  sort


Together the procedures do not form a closely related,
cooperative group of utilities for a common target problem.
