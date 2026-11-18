# fortimg

PNG and JPEG images in Fortran, without C libraries. It builds with gfortran
and LFortran, so the same code runs natively and in WebAssembly.

```fortran
use fortimg
type(image_t) :: img
integer :: status
call image_decode(bytes, img, status, upright=.true.)  ! PNG or JPEG; EXIF applied
call image_convert(img, 3)                             ! 1 grey .. 4 RGBA channels
call png_encode(img, png_bytes)
call jpeg_encode(img, jpeg_bytes, quality=90, dpi=300)
```

- PNG: every colour type and bit depth, Adam7 interlacing, palettes and tRNS
  (as an alpha channel), chunk CRCs checked; 16-bit samples are rounded to 8
  bits. Encoding of 8-bit images with a filter chosen per row.
- JPEG: baseline, extended and progressive Huffman-coded 8-bit images, any
  sampling factors, restart intervals, greyscale, YCbCr, RGB and Adobe
  CMYK/YCCK, EXIF orientation. Chroma is upsampled and converted with
  libjpeg's formulas. Arithmetic coding, 12-bit, lossless and hierarchical JPEG
  are not supported. Encoding writes baseline JFIF with the standard tables
  scaled by quality as libjpeg does, 4:4:4 chroma from quality 90 and 4:2:0
  below, and the resolution in pixels per inch.

Compression goes through [fortzip](https://github.com/lazy-fortran/fortzip).

## Tests

```sh
fo test        # or: fpm test
```

The tests contain no image files; they generate everything they check.

- PNG: a writer in the test, independent of fortimg's encoder, produces every
  colour type, bit depth, interlacing, row filter and tRNS variant from chosen
  samples, so the expected pixels follow from the specification; damaged files
  must be refused, and fortimg's own PNGs must round-trip.
- JPEG: libjpeg-turbo (`libturbojpeg.so.0`, opened at run time, test only)
  encodes generated pictures in every supported encoding and decodes them as
  the reference; fortimg must agree within 3 levels per sample and 0.5 on
  average (its inverse DCT is exact, libjpeg-turbo's fixed point). fortimg's
  JPEGs decode in libjpeg-turbo with an error within 5 % of libjpeg-turbo's own
  at equal quality and a size within 10 %.
