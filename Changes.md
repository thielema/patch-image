# Change log for the `patch-image` package

## 0.3.3.2:

 * Move from package `fft` to `comfort-fftw`.

## 0.3.2:

 * Use package `dsp` instead of `hmatrix`
   for the solution of the linear least squares problem
   for determining absolute coordinates from image pair displacements.
   This removes dependency from LAPACK and GSL
   and makes the code a bit simpler.

## 0.3.1:

 * Speed up computation by moving more stuff to Knead/LLVM.

## 0.3:

 * Allow to save program state, modify it manually
   and re-run with adapted parameters using CSV files.

## 0.2:

 * Add new executable that is based on LLVM and `knead`.

 * Add new algorithm for assembling the image from its parts.
   The algorithm finds exactly matching part shapes,
   such that the border of the shapes is where it hurts least visually.

## 0.1.0.2:

 * Switch from `accelerate-fft` to `accelerate-cufft`.

## 0.1:

 * Implement the patching algorithm using `accelerate-cuda`.

## 0.0:

 * Tests for a weighting algorithm using `GeomAlgLib`.
   The goal is to find a reasonable weighting
   for mixing arbitrary overlapping polygons.
