program test_jpeg
    !! libjpeg-turbo encodes generated pictures in every encoding this decoder
    !! claims; each decodes to libjpeg-turbo's own pixels within a small
    !! tolerance (the inverse DCT here is exact in floating point, libjpeg-turbo's
    !! is fixed point). EXIF orientation is read from inserted APP1 segments.
    use iso_fortran_env, only: int8
    use fortimg
    use test_pictures, only: picture, u8, s8
    use turbojpeg_reference
    implicit none
    ! Largest and mean absolute difference per sample allowed.
    integer, parameter :: MAX_DIFF = 3
    real, parameter :: MEAN_DIFF = 0.5
    integer :: failures, checked, state

    failures = 0
    checked = 0
    state = 20261115
    if (.not. turbojpeg_open()) error stop "the reference libturbojpeg (libturbojpeg.so.0) is missing"
    call colour("baseline 4:4:4 q90", 99, 67, TJSAMP_444, 90, .false., 0, 0)
    call colour("baseline 4:2:2 q90", 99, 67, TJSAMP_422, 90, .false., 0, 0)
    call colour("baseline 4:2:0 q90", 99, 67, TJSAMP_420, 90, .false., 0, 0)
    call colour("baseline 4:4:0 q90", 99, 67, TJSAMP_440, 90, .false., 0, 0)
    call colour("baseline 4:1:1 q90", 99, 67, TJSAMP_411, 90, .false., 0, 0)
    call colour("baseline 4:2:0 q30", 99, 67, TJSAMP_420, 30, .false., 0, 0)
    call colour("baseline 4:4:4 q100", 99, 67, TJSAMP_444, 100, .false., 0, 0)
    call colour("progressive 4:2:0 q85", 99, 67, TJSAMP_420, 85, .true., 0, 0)
    call colour("progressive 4:4:4 q95", 99, 67, TJSAMP_444, 95, .true., 0, 0)
    call colour("progressive 4:2:2 q60", 99, 67, TJSAMP_422, 60, .true., 0, 0)
    call colour("restart every 3 MCUs", 99, 67, TJSAMP_420, 80, .false., 3, 0)
    call colour("progressive, restart every row", 99, 67, TJSAMP_420, 80, .true., 0, 1)
    call colour("1x1", 1, 1, TJSAMP_420, 90, .false., 0, 0)
    call colour("7x5 4:2:0", 7, 5, TJSAMP_420, 90, .false., 0, 0)
    call colour("17x9 progressive", 17, 9, TJSAMP_420, 90, .true., 0, 0)
    call colour("wide 256x40", 256, 40, TJSAMP_420, 75, .true., 0, 0)
    call grey("greyscale", .false.)
    call grey("greyscale progressive", .true.)
    call cmyk("CMYK (Adobe, no transform)", TJCS_CMYK)
    call cmyk("YCCK (Adobe)", TJCS_YCCK)
    call exif()
    write (*, '(a, i0, a, i0, a)') "jpeg: ", checked, " checks, ", failures, " failures"
    if (failures > 0) error stop 1

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        checked = checked + 1
        if (ok) return
        failures = failures + 1
        write (*, '(a)') "FAIL: " // what
    end subroutine check

    subroutine colour(name, w, h, subsamp, quality, progressive, blocks, rows)
        character(*), intent(in) :: name
        integer, intent(in) :: w, h, subsamp, quality, blocks, rows
        logical, intent(in) :: progressive
        integer(int8), allocatable :: jpeg(:), ref(:, :, :)
        jpeg = tj_encode(picture(w, h, state), w, h, TJPF_RGB, subsamp, quality, progressive, &
            blocks, rows, -1)
        ref = tj_decode(jpeg, TJPF_RGB, 3)
        call compare(name, jpeg, ref)
    end subroutine colour

    subroutine grey(name, progressive)
        character(*), intent(in) :: name
        logical, intent(in) :: progressive
        integer(int8), allocatable :: jpeg(:), rgb(:, :, :), g(:, :, :), ref(:, :, :)
        rgb = picture(99, 67, state)
        g = rgb(2:2, :, :)
        jpeg = tj_encode(g, 99, 67, TJPF_GRAY, TJSAMP_GRAY, 85, progressive, 0, 0, -1)
        ref = tj_decode(jpeg, TJPF_GRAY, 1)
        call compare(name, jpeg, ref)
    end subroutine grey

    subroutine cmyk(name, colorspace)
        !! RGB of libjpeg-turbo's CMYK, read as Adobe's inverted convention:
        !! R = c k / 255, rounded.
        character(*), intent(in) :: name
        integer, intent(in) :: colorspace
        integer(int8), allocatable :: jpeg(:), rgb(:, :, :), src(:, :, :), ref4(:, :, :), &
            ref(:, :, :)
        integer :: x, y, k
        rgb = picture(99, 67, state)
        allocate (src(4, 99, 67))
        src(1:3, :, :) = rgb
        src(4, :, :) = rgb(2, :, :)
        jpeg = tj_encode(src, 99, 67, TJPF_CMYK, TJSAMP_444, 90, .false., 0, 0, colorspace)
        ref4 = tj_decode(jpeg, TJPF_CMYK, 4)
        allocate (ref(3, 99, 67))
        do y = 1, 67
            do x = 1, 99
                do k = 1, 3
                    ref(k, x, y) = s8((u8(ref4(k, x, y)) * u8(ref4(4, x, y)) + 127) / 255)
                end do
            end do
        end do
        call compare(name, jpeg, ref)
    end subroutine cmyk

    subroutine compare(name, jpeg, ref)
        character(*), intent(in) :: name
        integer(int8), intent(in) :: jpeg(:), ref(:, :, :)
        type(image_t) :: img
        integer :: status, worst
        real :: mean
        call jpeg_decode(jpeg, img, status)
        call check(status == IMG_OK, name // ": decodes")
        if (status /= IMG_OK) return
        call check(img%channels == size(ref, 1) .and. img%width == size(ref, 2) .and. &
            img%height == size(ref, 3), name // ": size and channels")
        if (img%channels /= size(ref, 1) .or. img%width /= size(ref, 2) .or. &
            img%height /= size(ref, 3)) return
        worst = maxval(abs(u8(img%pixels) - u8(ref)))
        mean = real(sum(abs(u8(img%pixels) - u8(ref)))) / size(ref)
        write (*, '(a, t34, a, i3, a, f6.3)') name, "max diff", worst, "  mean", mean
        call check(worst <= MAX_DIFF .and. mean <= MEAN_DIFF, name // ": pixels")
        call check(jpeg_exif_orientation(jpeg) == 1, name // ": no EXIF orientation")
    end subroutine compare

    subroutine exif()
        !! An APP1 Exif segment with orientation 6 (little-endian TIFF) or 8
        !! (big-endian) right after SOI; the image turns upright accordingly.
        integer(int8), allocatable :: jpeg(:), with(:)
        type(image_t) :: img
        integer :: status
        jpeg = tj_encode(picture(40, 24, state), 40, 24, TJPF_RGB, TJSAMP_420, 90, .false., &
            0, 0, -1)
        with = [jpeg(1:2), app1(.true., 6), jpeg(3:)]
        call check(jpeg_exif_orientation(with) == 6, "EXIF orientation 6, little-endian")
        call image_decode(with, img, status, upright=.true.)
        call check(status == IMG_OK .and. img%width == 24 .and. img%height == 40, &
            "EXIF orientation 6 turns the image upright")
        with = [jpeg(1:2), app1(.false., 8), jpeg(3:)]
        call check(jpeg_exif_orientation(with) == 8, "EXIF orientation 8, big-endian")
    end subroutine exif

    function app1(little, orientation) result(seg)
        logical, intent(in) :: little
        integer, intent(in) :: orientation
        integer(int8), allocatable :: seg(:)
        integer(int8), allocatable :: tiff(:)
        if (little) then
            tiff = s8([73, 73, 42, 0, 8, 0, 0, 0, 1, 0, 18, 1, 3, 0, 1, 0, 0, 0, &
                orientation, 0, 0, 0, 0, 0, 0, 0])
        else
            tiff = s8([77, 77, 0, 42, 0, 0, 0, 8, 0, 1, 1, 18, 0, 3, 0, 0, 0, 1, &
                0, orientation, 0, 0, 0, 0, 0, 0])
        end if
        seg = [s8([255, 225, 0, 2 + 6 + size(tiff), 69, 120, 105, 102, 0, 0]), tiff]
    end function app1

end program test_jpeg
