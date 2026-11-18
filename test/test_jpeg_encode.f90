program test_jpeg_encode
    !! JPEG files from jpeg_encode decode in libjpeg-turbo, close to the
    !! source pixels, to the same pixels as fortimg's decoder, and with sizes
    !! close to libjpeg-turbo's own at equal quality.
    use iso_fortran_env, only: int8
    use fortimg
    use test_pictures, only: picture, u8
    use turbojpeg_reference
    implicit none
    integer :: failures, state
    failures = 0
    state = 20261119
    if (.not. turbojpeg_open()) error stop "the reference libturbojpeg (libturbojpeg.so.0) is missing"
    call colour("4:4:4 q95", 99, 67, 95, .true.)
    call colour("4:2:0 q75", 99, 67, 75, .false.)
    call colour("4:2:0 q30", 160, 90, 30, .false.)
    call colour("1x1", 1, 1, 90, .false.)
    call colour("17x9 odd edges", 17, 9, 90, .false.)
    call grey_case()
    call density()
    if (failures > 0) error stop "jpeg_encode tests failed"
    print '(a)', "jpeg_encode: all checks passed"
contains
    subroutine colour(name, w, h, quality, full)
        !! Our error against the source stays within 5 % of libjpeg-turbo's
        !! at the same quality and chroma, and the file within 10 % of its size.
        character(*), intent(in) :: name
        integer, intent(in) :: w, h, quality
        logical, intent(in) :: full
        type(image_t) :: img, back
        integer(int8), allocatable :: bytes(:), ref(:, :, :), theirs(:)
        integer :: status
        real :: ours_err, theirs_err
        img%width = w
        img%height = h
        img%channels = 3
        img%pixels = picture(w, h, state)
        call jpeg_encode(img, bytes, quality=quality, full_chroma=full)
        ref = tj_decode(bytes, TJPF_RGB, 3)
        call check(size(ref, 2) == w .and. size(ref, 3) == h, name // ": libjpeg-turbo reads it")
        if (size(ref, 2) /= w .or. size(ref, 3) /= h) return
        ours_err = sum(abs(u8(ref) - u8(img%pixels))) / real(size(ref))
        call jpeg_decode(bytes, back, status)
        call check(status == IMG_OK, name // ": fortimg reads it")
        if (status == IMG_OK) call check(maxval(abs(u8(back%pixels(1:3, :, :)) - u8(ref))) <= 3, &
            name // ": both decoders agree")
        theirs = tj_encode(img%pixels, w, h, TJPF_RGB, merge(TJSAMP_444, TJSAMP_420, full), &
            quality, .false., 0, 0, -1)
        ref = tj_decode(theirs, TJPF_RGB, 3)
        theirs_err = sum(abs(u8(ref) - u8(img%pixels))) / real(size(ref))
        call check(ours_err <= 1.05 * theirs_err + 0.05, name // ": error as libjpeg-turbo's")
        call check(abs(size(bytes) - size(theirs)) <= 0.1 * size(theirs) + 64, &
            name // ": size as libjpeg-turbo's")
    end subroutine colour

    subroutine grey_case()
        type(image_t) :: img
        integer(int8), allocatable :: bytes(:), ref(:, :, :), rgb(:, :, :)
        rgb = picture(64, 48, state)
        img%width = 64
        img%height = 48
        img%channels = 1
        img%pixels = rgb(2:2, :, :)
        call jpeg_encode(img, bytes, quality=85)
        ref = tj_decode(bytes, TJPF_GRAY, 1)
        call check(size(ref, 1) == 1 .and. sum(abs(u8(ref) - u8(img%pixels))) / real(size(ref)) < 4.0, &
            "greyscale")
    end subroutine grey_case

    subroutine density()
        type(image_t) :: img
        integer(int8), allocatable :: bytes(:)
        img%width = 8
        img%height = 8
        img%channels = 3
        allocate(img%pixels(3, 8, 8))
        img%pixels = 0_int8
        call jpeg_encode(img, bytes, dpi=300)
        call check(u8(bytes(14)) == 1 .and. 256 * u8(bytes(15)) + u8(bytes(16)) == 300, &
            "resolution in pixels per inch")
    end subroutine density

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        if (.not. ok) then
            print '(2a)', "FAIL: ", what
            failures = failures + 1
        end if
    end subroutine check
end program test_jpeg_encode
