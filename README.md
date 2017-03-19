This is the workflow:
Scan parts of an image that considerably overlap.
They must all be approximately oriented correctly.
The program uses the overlapping areas for reconstruction
of the layout of the parts.
If all parts are in the directory `part`
then in the best case you can simply run:

    patch-image --output=collage.jpeg part/*.jpeg

If you get blurred areas,
you might enable an additional rotation correction:

    patch-image --finetune-rotate --output=collage.jpeg part/*.jpeg

It follows an overview of how the program works.
It implies some things you should care about when using the program.

The program runs three phases:

* Orientate each image part individually

* Find overlapping areas in the parts
  and reconstruct the part positions within the big image

* Blend the parts in order to get the big image

The first phase orientates each part
such that horizontal structures become perfectly aligned.
Only the brightness channel of the image is analysed.
Horizontal structures can be text or the border of the image.
This also means that you should orientate the parts
horizontally, not vertically.
I also recommend not to mix horizontal and vertical scanned parts
since the horizontal and vertical resolution of your scanner
might differ slightly.
However, it should be fine to rotate the image source by 180°
and rotate it back digitally,
before feeding it to the patch-image program.

## 1st Phase

Options for the first phase:

* `--maximum-absolute-angle`:
  Maximum angle to test for.

* `--number-angles`:
  Number of angles minus one
  to test between negative and positive maximum angle.

* `--hint-angle`:
  If the search for horizontal structures
  does not yield satisfying results for an image part,
  you may prepend the `--hint-angle` option with the wanted angle
  to the image path.

## 2nd Phase

In the second phase the program looks
for overlapping parts between all pairs of images.
For every pair it computes a convolution via a Fourier transform.
Only the brightness channel of the image is analysed.

* `--pad-size`:
  Computing a convolution of two big images may exceed your graphics memory.
  To this end, images are shrunk before convolution.
  The pad size is the size in pixels after shrinking
  that holds 2x2 shrunken image parts.
  After determination of the distance between the shrunken parts
  the matching is repeated on a non-reduced clip of the original image part,
  in order to get precise coordinates.

* `--minimum-overlap`:
  There must be a minimum of overlap
  in order to accept that the images actually overlap.
  The overlap is measured as a portion of the image part size.

* `--maximum-difference`:
  The maximum allowed mean difference
  within an overlapping area of two overlapping images.
  If the difference is larger,
  then the program assumes that the parts do not overlap.

* `--smooth`:
  It is important to eliminate a brightness offset,
  that is, big black and big white areas should be handled equally.
  To this end the image is smoothed
  and the smoothed image is subtracted from the original one.
  This option allows to specify the degree (radius) of the smoothing.
  I don't think you ever need to touch this parameter.

* `--output-overlap`:
  Writes images for all pairs of image parts.
  These images allow you to diagnose
  where the matching algorithm failed.
  It may help you to adjust the matching parameters.
  In the future we might add an option to ignore problematic pairs.

Since in the first phase every image part is oriented individually
it may happen that the part orientations don't match.
This would result in blurred areas in the final collage.
In order to correct this,
you can run phase two in an extended mode,
that also re-evaluates the part orientations.
The orientation of the composed image is then determined
by the estimated orientation of the first image.

Options:

* `--finetune-rotate`:
  Enables the extended overlapping mode.
  The option `--output-overlap` will then be ignored.

* `--number-stamps`:
  The extended mode selects many small clips in the overlapping area
  and tries to match them.
  We call these clips /stamps/.
  This option controls the number of stamps per overlapping area minus one.

* `--stamp-size`:
  Size of a square stamp in pixels.

## 3rd Phase

The third phase composes a big image from the parts.
The parts are weighted such that the part boundaries cannot be seen anymore
and differences in brightness are faded into another.
The downside is that the superposition may lead to blur.

Options:

* `--output`:
  Path of the output JPEG image with the weighted collage.

* `--output-hard`:
  Alternative output of a JPEG collage
  where the image parts are simply averaged.
  You will certainly see bumps in brightness
  at the borders of the image parts.
  This output may be mostly useful to promote the great weighting algorithm
  employed by `--output`.

* `--output-distance-map`:
  The weight for every pixel is chosen according to the distance
  to an image part boundary that lies within other parts.
  The rationale is that the weight shall become zero
  when the pixel is close to a position
  that will be affected by a disruption otherwise.
  This option allows to emit the distance map for every image part.

* `--distance-gamma`:
  If the distances are used for weighting as they are,
  the program fades evenly between the overlapping image parts
  over the entire overapping area.
  This may mean that the overlapping area is blurred.
  Raising the distance to a power greater than one reduces the area of blur.
  The downside is that it also reduces the area for adaption
  of differing brightness.

The LLVM implementation provides an additional way to assemble the image parts.
The weighting approach tries to blend across all the overlapping area.
This can equalize differences in brightness.
The downside is that imperfectly matching image parts
lead to blurred content in the overlapping area.
An alternative algorithm tries to make the overlapping as small as possible
and additionally performs blending where it hurts least.
More precisely, parts are blended where they differ least.
However, if the brightness of the image parts differ
then the blending boundaries may become visible.

Options:

* `--output-shaped`:
  Path of the output JPEG image with smoothly blended image parts
  along curves of low image difference.

* `--output-shaped-hard`:
  Like before but the image parts are not smoothly faded.
  Instead, every pixel belongs to exactly one original image part.
  This is more for debugging purposes than of practical use.

* `--output-shape`:
  Emit the smoothed mask of each image part used for blending.

* `--output-shape-hard`:
  Emit the non-smoothed masks.

* `--shape-smooth`:
  Smooth radius of the image masks.
  The higher, the smoother is the blending between parts.

General options:

* `--quality`:
  JPEG quality percentage for writing the images.
