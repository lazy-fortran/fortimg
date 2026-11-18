module fortimg
    !! PNG and JPEG images in Fortran: decoding both, encoding both.
    use iso_fortran_env, only: int8
    use fortimg_image, only: image_t, image_convert, image_orient, IMG_OK, IMG_ERR_FORMAT, &
        IMG_ERR_CORRUPT, IMG_ERR_UNSUPPORTED
    use fortimg_png, only: png_decode, png_encode, is_png
    use fortimg_jpeg, only: jpeg_decode, jpeg_exif_orientation, is_jpeg
    use fortimg_jpeg_encode, only: jpeg_encode
    implicit none
    private
    public :: image_t, image_convert, image_orient, image_decode
    public :: png_decode, png_encode, is_png, jpeg_decode, jpeg_encode, jpeg_exif_orientation, is_jpeg
    public :: IMG_OK, IMG_ERR_FORMAT, IMG_ERR_CORRUPT, IMG_ERR_UNSUPPORTED

contains

    subroutine image_decode(bytes, img, status, upright)
        !! A PNG or JPEG file by its signature. With `upright`, a JPEG's EXIF
        !! orientation is applied, so the image is as a viewer shows it.
        integer(int8), intent(in) :: bytes(:)
        type(image_t), intent(out) :: img
        integer, intent(out) :: status
        logical, intent(in), optional :: upright
        if (is_png(bytes)) then
            call png_decode(bytes, img, status)
        else if (is_jpeg(bytes)) then
            call jpeg_decode(bytes, img, status)
            if (status /= IMG_OK) return
            if (present(upright)) then
                if (upright) call image_orient(img, jpeg_exif_orientation(bytes))
            end if
        else
            status = IMG_ERR_FORMAT
        end if
    end subroutine image_decode

end module fortimg
