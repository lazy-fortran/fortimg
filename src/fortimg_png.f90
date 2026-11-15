module fortimg_png
    !! PNG (ISO/IEC 15948) decoding of every colour type, bit depth and Adam7
    !! interlacing, with chunk CRCs checked, and encoding of 8-bit images.
    !! 16-bit samples are rounded to 8 bits; tRNS becomes an alpha channel.
    use iso_fortran_env, only: int8, int32
    use fortzip, only: zlib_compress, zlib_decompress, crc32, ZIP_OK
    use fortimg_image, only: image_t, IMG_OK, IMG_ERR_FORMAT, IMG_ERR_CORRUPT, &
        IMG_ERR_UNSUPPORTED
    implicit none
    private
    public :: png_decode, png_encode, is_png

    integer(int8), parameter :: SIGNATURE(8) = int([-119, 80, 78, 71, 13, 10, 26, 10], int8)
    ! Adam7 passes: first column and row, column and row step.
    integer, parameter :: PASS_X0(7) = [0, 4, 0, 2, 0, 1, 0], PASS_Y0(7) = [0, 0, 4, 0, 2, &
        0, 1], PASS_DX(7) = [8, 8, 4, 4, 2, 2, 1], PASS_DY(7) = [8, 8, 8, 4, 4, 2, 2]

contains

    logical function is_png(bytes)
        integer(int8), intent(in) :: bytes(:)
        is_png = .false.
        if (size(bytes) < 8) return
        is_png = all(bytes(1:8) == SIGNATURE)
    end function is_png

    subroutine png_decode(bytes, img, status)
        integer(int8), intent(in) :: bytes(:)
        type(image_t), intent(out) :: img
        integer, intent(out) :: status
        integer(int8), allocatable :: idat(:), raw(:), grown(:)
        integer :: pos, length, nidat, width, height, depth, ctype, interlace, k
        integer :: npal, samples, palette(4, 0:255), trns(3)
        logical :: have_ihdr, have_trns, have_plte, have_idat, idat_done
        character(4) :: tag

        status = IMG_ERR_FORMAT
        if (.not. is_png(bytes)) return
        status = IMG_ERR_CORRUPT
        have_ihdr = .false.
        have_trns = .false.
        have_plte = .false.
        have_idat = .false.
        idat_done = .false.
        npal = 0
        palette = 255
        trns = -1
        nidat = 0
        allocate (idat(max(1024, size(bytes))))
        width = 0
        height = 0
        depth = 0
        ctype = 0
        interlace = 0
        pos = 9
        do
            if (pos + 11 > size(bytes)) return
            length = be32(bytes, pos)
            if (length < 0 .or. length > size(bytes) - pos - 11) return
            tag = transfer(bytes(pos + 4:pos + 7), tag)
            if (crc32(bytes(pos + 4:pos + 7 + length)) /= be32(bytes, pos + 8 + length)) return
            if (.not. have_ihdr .and. tag /= "IHDR") return
            if (have_idat .and. tag /= "IDAT") idat_done = .true.
            select case (tag)
            case ("IHDR")
                if (have_ihdr .or. length /= 13) return
                have_ihdr = .true.
                width = be32(bytes, pos + 8)
                height = be32(bytes, pos + 12)
                depth = u8(bytes(pos + 16))
                ctype = u8(bytes(pos + 17))
                interlace = u8(bytes(pos + 20))
                if (width <= 0 .or. height <= 0) return
                if (u8(bytes(pos + 18)) /= 0 .or. u8(bytes(pos + 19)) /= 0) return
                if (interlace > 1) return
                if (.not. valid_depth(ctype, depth)) return
            case ("PLTE")
                if (mod(length, 3) /= 0 .or. length > 768 .or. length == 0) return
                npal = length / 3
                do k = 0, npal - 1
                    palette(1:3, k) = [u8(bytes(pos + 8 + 3 * k)), u8(bytes(pos + 9 + 3 * k)), &
                        u8(bytes(pos + 10 + 3 * k))]
                end do
                have_plte = .true.
            case ("tRNS")
                have_trns = .true.
                select case (ctype)
                case (0)
                    trns(1) = be16(bytes, pos + 8)
                case (2)
                    trns = [be16(bytes, pos + 8), be16(bytes, pos + 10), be16(bytes, pos + 12)]
                case (3)
                    do k = 0, min(255, length - 1)
                        palette(4, k) = u8(bytes(pos + 8 + k))
                    end do
                end select
            case ("IDAT")
                if (idat_done) return
                have_idat = .true.
                if (nidat + length > size(idat)) then
                    allocate (grown(2 * (nidat + length)))
                    grown(1:nidat) = idat(1:nidat)
                    call move_alloc(grown, idat)
                end if
                idat(nidat + 1:nidat + length) = bytes(pos + 8:pos + 7 + length)
                nidat = nidat + length
            case ("IEND")
                exit
            case default
                ! An unknown critical chunk (upper-case first letter) cannot be skipped.
                if (iachar(tag(1:1)) >= 65 .and. iachar(tag(1:1)) <= 90) then
                    status = IMG_ERR_UNSUPPORTED
                    return
                end if
            end select
            pos = pos + 12 + length
        end do
        if (.not. have_idat) return
        if (ctype == 3 .and. .not. have_plte) return

        call zlib_decompress(idat(1:nidat), raw, k)
        if (k /= ZIP_OK) return

        samples = channels_of(ctype)
        img%width = width
        img%height = height
        img%channels = samples
        if (ctype == 3) img%channels = 3
        if (have_trns .and. (ctype == 0 .or. ctype == 2 .or. ctype == 3)) &
            img%channels = img%channels + 1
        allocate (img%pixels(img%channels, width, height))
        if (.not. unfilter_all(raw, img, ctype, depth, samples, interlace, palette, npal, &
            have_trns, trns)) return
        status = IMG_OK
    end subroutine png_decode

    logical function unfilter_all(raw, img, ctype, depth, samples, interlace, palette, npal, &
            have_trns, trns) result(ok)
        !! Undo the row filters of every pass and store the pixels.
        integer(int8), intent(inout) :: raw(:)
        type(image_t), intent(inout) :: img
        integer, intent(in) :: ctype, depth, samples, interlace, palette(4, 0:255), npal
        logical, intent(in) :: have_trns
        integer, intent(in) :: trns(3)
        integer :: pass, x0, y0, dx, dy, pw, ph, stride, bpp, pos, y, last, npass
        ok = .false.
        bpp = max(1, samples * depth / 8)
        pos = 1
        npass = 1
        if (interlace == 1) npass = 7
        do pass = 1, npass
            if (interlace == 1) then
                x0 = PASS_X0(pass)
                y0 = PASS_Y0(pass)
                dx = PASS_DX(pass)
                dy = PASS_DY(pass)
            else
                x0 = 0
                y0 = 0
                dx = 1
                dy = 1
            end if
            pw = (img%width - x0 + dx - 1) / dx
            ph = (img%height - y0 + dy - 1) / dy
            if (pw <= 0 .or. ph <= 0) cycle
            stride = (pw * samples * depth + 7) / 8
            if (pos - 1 + ph * (stride + 1) > size(raw)) return
            last = -1
            do y = 0, ph - 1
                if (.not. unfilter_row(raw, pos, stride, bpp, last)) return
                call store_row(raw(pos + 1:pos + stride), img, y0 + y * dy + 1, x0, dx, pw, &
                    ctype, depth, samples, palette, npal, have_trns, trns)
                last = pos + 1
                pos = pos + stride + 1
            end do
        end do
        ok = .true.
    end function unfilter_all

    logical function unfilter_row(raw, pos, stride, bpp, prev) result(ok)
        !! raw(pos) is the filter type, raw(pos+1:pos+stride) the row; prev is
        !! where the previous row of this pass starts, or -1 for the first row.
        integer(int8), intent(inout) :: raw(:)
        integer, intent(in) :: pos, stride, bpp, prev
        integer :: i, a, b, c, p, pa, pb, pc, v
        ok = .true.
        select case (u8(raw(pos)))
        case (0)
            return
        case (1)
            do i = bpp + 1, stride
                raw(pos + i) = s8(u8(raw(pos + i)) + u8(raw(pos + i - bpp)))
            end do
        case (2)
            if (prev < 0) return
            do i = 1, stride
                raw(pos + i) = s8(u8(raw(pos + i)) + u8(raw(prev + i - 1)))
            end do
        case (3, 4)
            do i = 1, stride
                a = 0
                b = 0
                c = 0
                if (i > bpp) a = u8(raw(pos + i - bpp))
                if (prev >= 0) b = u8(raw(prev + i - 1))
                if (prev >= 0 .and. i > bpp) c = u8(raw(prev + i - 1 - bpp))
                if (u8(raw(pos)) == 3) then
                    v = (a + b) / 2
                else
                    p = a + b - c
                    pa = abs(p - a)
                    pb = abs(p - b)
                    pc = abs(p - c)
                    if (pa <= pb .and. pa <= pc) then
                        v = a
                    else if (pb <= pc) then
                        v = b
                    else
                        v = c
                    end if
                end if
                raw(pos + i) = s8(u8(raw(pos + i)) + v)
            end do
        case default
            ok = .false.
        end select
    end function unfilter_row

    subroutine store_row(row, img, y, x0, dx, pw, ctype, depth, samples, palette, npal, &
            have_trns, trns)
        integer(int8), intent(in) :: row(:)
        type(image_t), intent(inout) :: img
        integer, intent(in) :: y, x0, dx, pw, ctype, depth, samples, palette(4, 0:255), npal
        logical, intent(in) :: have_trns
        integer, intent(in) :: trns(3)
        integer :: i, x, s, v(4), raw16(3), idx
        do i = 0, pw - 1
            x = x0 + i * dx + 1
            do s = 1, samples
                v(s) = sample(row, i * samples + s - 1, depth)
                if (s <= 3) raw16(s) = v(s)
            end do
            select case (ctype)
            case (0)
                img%pixels(1, x, y) = s8(scale8(v(1), depth))
                if (have_trns) img%pixels(2, x, y) = s8(merge(0, 255, v(1) == trns(1)))
            case (2)
                do s = 1, 3
                    img%pixels(s, x, y) = s8(scale8(v(s), depth))
                end do
                if (have_trns) img%pixels(4, x, y) = s8(merge(0, 255, all(raw16 == trns)))
            case (3)
                idx = v(1)
                if (idx >= npal) idx = 0
                img%pixels(1:3, x, y) = [s8(palette(1, idx)), s8(palette(2, idx)), &
                    s8(palette(3, idx))]
                if (have_trns) img%pixels(4, x, y) = s8(palette(4, idx))
            case default
                do s = 1, samples
                    img%pixels(s, x, y) = s8(scale8(v(s), depth))
                end do
            end select
        end do
    end subroutine store_row

    pure integer function sample(row, index, depth) result(v)
        !! The index-th sample (from 0) of a row packed at `depth` bits.
        integer(int8), intent(in) :: row(:)
        integer, intent(in) :: index, depth
        integer :: bit
        select case (depth)
        case (16)
            v = 256 * u8(row(2 * index + 1)) + u8(row(2 * index + 2))
        case (8)
            v = u8(row(index + 1))
        case default
            bit = index * depth
            v = iand(ishft(u8(row(bit / 8 + 1)), -(8 - depth - mod(bit, 8))), 2**depth - 1)
        end select
    end function sample

    pure integer function scale8(v, depth)
        !! A sample at `depth` bits as 8 bits: exact for 1..8, rounded for 16.
        integer, intent(in) :: v, depth
        select case (depth)
        case (16)
            scale8 = (v * 255 + 32767) / 65535
        case (8)
            scale8 = v
        case default
            scale8 = v * 255 / (2**depth - 1)
        end select
    end function scale8

    subroutine png_encode(img, bytes, level)
        !! An 8-bit PNG of img (grey, grey+alpha, RGB or RGBA), not interlaced;
        !! each row gets the filter with the smallest sum of absolute values.
        type(image_t), intent(in) :: img
        integer(int8), allocatable, intent(out) :: bytes(:)
        integer, intent(in), optional :: level
        integer(int8), allocatable :: raw(:), z(:), cur(:), best(:), prev(:)
        integer :: ctype, stride, y, f, lvl, cost, best_cost, n, pos, best_filter
        integer, parameter :: CTYPE_OF(4) = [0, 4, 2, 6]
        integer(int8) :: ihdr(13)
        lvl = 6
        if (present(level)) lvl = level
        ctype = CTYPE_OF(img%channels)
        stride = img%width * img%channels
        allocate (raw(img%height * (stride + 1)), cur(stride), best(stride), prev(stride))
        prev = 0
        do y = 1, img%height
            best_cost = huge(1)
            do f = 0, 4
                call filter_row(reshape(img%pixels(:, :, y), [stride]), prev, img%channels, &
                    f, cur, y == 1)
                cost = sum(abs(int(cur)))
                if (cost < best_cost) then
                    best_cost = cost
                    best = cur
                    best_filter = f
                end if
            end do
            raw((y - 1) * (stride + 1) + 1) = int(best_filter, int8)
            raw((y - 1) * (stride + 1) + 2:y * (stride + 1)) = best
            prev = reshape(img%pixels(:, :, y), [stride])
        end do
        call zlib_compress(raw, z, lvl)
        ihdr = [be32_bytes(img%width), be32_bytes(img%height), 8_int8, int(ctype, int8), &
            0_int8, 0_int8, 0_int8]
        n = 8 + 25 + 12 + size(z) + 12
        allocate (bytes(n))
        bytes(1:8) = SIGNATURE
        pos = 9
        call put_chunk("IHDR", ihdr)
        call put_chunk("IDAT", z)
        call put_chunk("IEND", [integer(int8) ::])
    contains
        subroutine put_chunk(tag, data)
            character(4), intent(in) :: tag
            integer(int8), intent(in) :: data(:)
            integer :: k
            bytes(pos:pos + 3) = be32_bytes(size(data))
            bytes(pos + 4:pos + 7) = [(int(iachar(tag(k:k)), int8), k = 1, 4)]
            bytes(pos + 8:pos + 7 + size(data)) = data
            bytes(pos + 8 + size(data):pos + 11 + size(data)) = &
                be32_bytes(int(crc32(bytes(pos + 4:pos + 7 + size(data)))))
            pos = pos + 12 + size(data)
        end subroutine put_chunk
    end subroutine png_encode

    pure subroutine filter_row(row, prev, bpp, f, out, first)
        integer(int8), intent(in) :: row(:), prev(:)
        integer, intent(in) :: bpp, f
        integer(int8), intent(out) :: out(:)
        logical, intent(in) :: first
        integer :: i, a, b, c, p, pa, pb, pc, v
        do i = 1, size(row)
            a = 0
            b = 0
            c = 0
            if (i > bpp) a = u8(row(i - bpp))
            if (.not. first) b = u8(prev(i))
            if (.not. first .and. i > bpp) c = u8(prev(i - bpp))
            select case (f)
            case (0)
                v = 0
            case (1)
                v = a
            case (2)
                v = b
            case (3)
                v = (a + b) / 2
            case default
                p = a + b - c
                pa = abs(p - a)
                pb = abs(p - b)
                pc = abs(p - c)
                if (pa <= pb .and. pa <= pc) then
                    v = a
                else if (pb <= pc) then
                    v = b
                else
                    v = c
                end if
            end select
            out(i) = s8(iand(u8(row(i)) - v, 255))
        end do
    end subroutine filter_row

    pure logical function valid_depth(ctype, depth)
        integer, intent(in) :: ctype, depth
        select case (ctype)
        case (0)
            valid_depth = any(depth == [1, 2, 4, 8, 16])
        case (3)
            valid_depth = any(depth == [1, 2, 4, 8])
        case (2, 4, 6)
            valid_depth = any(depth == [8, 16])
        case default
            valid_depth = .false.
        end select
    end function valid_depth

    pure integer function channels_of(ctype)
        integer, intent(in) :: ctype
        select case (ctype)
        case (0, 3)
            channels_of = 1
        case (4)
            channels_of = 2
        case (2)
            channels_of = 3
        case default
            channels_of = 4
        end select
    end function channels_of

    pure integer function be32(b, pos)
        integer(int8), intent(in) :: b(:)
        integer, intent(in) :: pos
        be32 = ior(ior(ishft(u8(b(pos)), 24), ishft(u8(b(pos + 1)), 16)), &
            ior(ishft(u8(b(pos + 2)), 8), u8(b(pos + 3))))
    end function be32

    pure integer function be16(b, pos)
        integer(int8), intent(in) :: b(:)
        integer, intent(in) :: pos
        be16 = 256 * u8(b(pos)) + u8(b(pos + 1))
    end function be16

    pure function be32_bytes(v) result(b)
        integer, intent(in) :: v
        integer(int8) :: b(4)
        b = [s8(ibits(v, 24, 8)), s8(ibits(v, 16, 8)), s8(ibits(v, 8, 8)), s8(ibits(v, 0, 8))]
    end function be32_bytes

    pure integer function u8(b)
        integer(int8), intent(in) :: b
        u8 = iand(int(b), 255)
    end function u8

    pure integer(int8) function s8(v)
        integer, intent(in) :: v
        s8 = int(iand(v, 255) - 256 * (iand(v, 255) / 128), int8)
    end function s8

end module fortimg_png
