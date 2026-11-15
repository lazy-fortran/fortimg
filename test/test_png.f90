program test_png
    !! PNG files written here from chosen samples, independently of fortimg's
    !! encoder: every colour type and bit depth, plain and Adam7-interlaced,
    !! each row with a random filter, with and without tRNS, split IDAT chunks
    !! and ancillary chunks. Each decodes to the RGBA the PNG specification
    !! gives for its samples. Damaged files are refused, and fortimg's own
    !! PNGs of generated pictures decode to the same pixels.
    use iso_fortran_env, only: int8, int32
    use fortzip, only: zlib_compress, crc32
    use fortimg
    use test_pictures, only: random_int, picture, u8, s8
    implicit none
    integer, parameter :: PASS_X0(7) = [0, 4, 0, 2, 0, 1, 0], PASS_Y0(7) = [0, 0, 4, 0, 2, &
        0, 1], PASS_DX(7) = [8, 8, 4, 4, 2, 2, 1], PASS_DY(7) = [8, 8, 8, 4, 4, 2, 2]
    integer, parameter :: SIZES(2, 5) = reshape([1, 1, 3, 2, 7, 9, 33, 17, 64, 5], [2, 5])
    integer :: failures, checked, state, ctype, d, depth, interlace, s, trns
    integer, parameter :: DEPTHS(5) = [1, 2, 4, 8, 16]
    ! Samples per pixel of colour types 0..6 (1 and 5 do not exist).
    integer, parameter :: SAMPLES_OF(0:6) = [1, 0, 3, 1, 2, 0, 4]

    failures = 0
    checked = 0
    state = 20261115
    do ctype = 0, 6
        if (ctype == 1 .or. ctype == 5) cycle
        do d = 1, 5
            depth = DEPTHS(d)
            if (.not. valid(ctype, depth)) cycle
            do interlace = 0, 1
                do s = 1, size(SIZES, 2)
                    do trns = 0, merge(1, 0, ctype == 0 .or. ctype == 2 .or. ctype == 3)
                        call check_generated(ctype, depth, interlace, SIZES(1, s), &
                            SIZES(2, s), trns == 1)
                    end do
                end do
            end do
        end do
    end do
    call check_damage()
    call check_round_trips()
    write (*, '(a, i0, a, i0, a)') "png: ", checked, " checks, ", failures, " failures"
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

    pure logical function valid(ctype, depth)
        integer, intent(in) :: ctype, depth
        select case (ctype)
        case (0)
            valid = .true.
        case (3)
            valid = depth <= 8
        case default
            valid = depth >= 8
        end select
    end function valid

    subroutine check_generated(ctype, depth, interlace, w, h, with_trns)
        integer, intent(in) :: ctype, depth, interlace, w, h
        logical, intent(in) :: with_trns
        integer(int8), allocatable :: png(:)
        integer :: expected(4, w, h), status
        type(image_t) :: img
        character(64) :: name
        write (name, '(a, i0, a, i0, a, i0, a, i0, a, i0, a, l1)') "type ", ctype, " depth ", &
            depth, " interlace ", interlace, " ", w, "x", h, " tRNS ", with_trns
        call generate(ctype, depth, interlace, w, h, with_trns, png, expected)
        call png_decode(png, img, status)
        call check(status == IMG_OK, trim(name) // ": decodes")
        if (status /= IMG_OK) return
        call image_convert(img, 4)
        call check(img%width == w .and. img%height == h, trim(name) // ": size")
        if (img%width /= w .or. img%height /= h) return
        call check(all(u8(img%pixels) == expected), trim(name) // ": pixels")
    end subroutine check_generated

    subroutine generate(ctype, depth, interlace, w, h, with_trns, png, expected)
        !! Random samples, the expected RGBA (8 bits, 16-bit samples rounded),
        !! and the PNG file.
        integer, intent(in) :: ctype, depth, interlace, w, h
        logical, intent(in) :: with_trns
        integer(int8), allocatable, intent(out) :: png(:)
        integer, intent(out) :: expected(4, w, h)
        integer :: samples(4, w, h), ns, maxv, x, y, k, npal, palette(4, 0:255), key(3)
        integer(int8), allocatable :: raw(:), z(:)
        ns = SAMPLES_OF(ctype)
        maxv = 2**depth - 1
        npal = 0
        if (ctype == 3) then
            npal = 1 + random_int(state, 2**depth)
            do k = 0, npal - 1
                palette(:, k) = [random_int(state, 256), random_int(state, 256), &
                    random_int(state, 256), 255]
                if (with_trns .and. random_int(state, 2) == 0) palette(4, k) = random_int(state, 256)
            end do
            maxv = npal - 1
        end if
        do y = 1, h
            do x = 1, w
                do k = 1, ns
                    samples(k, x, y) = random_int(state, maxv + 1)
                end do
            end do
        end do
        ! The transparent key is a colour that occurs, so both outcomes are tested.
        key = samples(1:3, 1 + w / 2, 1 + h / 2)
        do y = 1, h
            do x = 1, w
                select case (ctype)
                case (0)
                    expected(1:3, x, y) = scale(samples(1, x, y), depth)
                    expected(4, x, y) = 255
                    if (with_trns .and. samples(1, x, y) == key(1)) expected(4, x, y) = 0
                case (2)
                    expected(1:3, x, y) = [(scale(samples(k, x, y), depth), k = 1, 3)]
                    expected(4, x, y) = 255
                    if (with_trns .and. all(samples(1:3, x, y) == key)) expected(4, x, y) = 0
                case (3)
                    expected(:, x, y) = palette(:, samples(1, x, y))
                case (4)
                    expected(1:3, x, y) = scale(samples(1, x, y), depth)
                    expected(4, x, y) = scale(samples(2, x, y), depth)
                case default
                    expected(:, x, y) = [(scale(samples(k, x, y), depth), k = 1, 4)]
                end select
            end do
        end do
        raw = scanlines(samples(1:ns, :, :), depth, interlace)
        call zlib_compress(raw, z, random_int(state, 10))
        png = s8([137, 80, 78, 71, 13, 10, 26, 10])
        png = [png, chunk("IHDR", [be32(w), be32(h), s8([depth, ctype, 0, 0, interlace])])]
        png = [png, chunk("gAMA", be32(45455))]
        if (ctype == 3) png = [png, chunk("PLTE", [(s8(palette(1:3, k)), k = 0, npal - 1)])]
        if (with_trns) then
            select case (ctype)
            case (0)
                png = [png, chunk("tRNS", be16(key(1)))]
            case (2)
                png = [png, chunk("tRNS", [be16(key(1)), be16(key(2)), be16(key(3))])]
            case (3)
                png = [png, chunk("tRNS", [(s8(palette(4, k)), k = 0, npal - 1)])]
            end select
        end if
        ! IDAT in up to three pieces, with an ancillary chunk after the data.
        k = max(1, size(z) / 3)
        png = [png, chunk("IDAT", z(1:k))]
        if (size(z) > k) png = [png, chunk("IDAT", z(k + 1:))]
        png = [png, chunk("tEXt", s8([67, 0, 120])), chunk("IEND", [integer(int8) ::])]
    end subroutine generate

    function scanlines(samples, depth, interlace) result(raw)
        !! Rows of every pass, packed at `depth` bits, each behind a random
        !! filter type with the filter applied.
        integer, intent(in) :: samples(:, :, :), depth, interlace
        integer(int8), allocatable :: raw(:)
        integer(int8), allocatable :: row(:), prev(:), filtered(:)
        integer :: ns, w, h, pass, npass, x0, y0, dx, dy, pw, ph, stride, bpp, py, px, k, f
        ns = size(samples, 1)
        w = size(samples, 2)
        h = size(samples, 3)
        bpp = max(1, ns * depth / 8)
        allocate (raw(0))
        npass = merge(7, 1, interlace == 1)
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
            pw = (w - x0 + dx - 1) / dx
            ph = (h - y0 + dy - 1) / dy
            if (pw <= 0 .or. ph <= 0) cycle
            stride = (pw * ns * depth + 7) / 8
            allocate (prev(stride))
            prev = 0
            do py = 0, ph - 1
                allocate (row(stride))
                row = 0
                do px = 0, pw - 1
                    do k = 1, ns
                        call put_sample(row, px * ns + k - 1, depth, &
                            samples(k, x0 + px * dx + 1, y0 + py * dy + 1))
                    end do
                end do
                f = random_int(state, 5)
                filtered = apply_filter(row, prev, bpp, f, py == 0)
                raw = [raw, int(f, int8), filtered]
                prev = row
                deallocate (row)
            end do
            deallocate (prev)
        end do
    end function scanlines

    subroutine put_sample(row, index, depth, v)
        integer(int8), intent(inout) :: row(:)
        integer, intent(in) :: index, depth, v
        integer :: bit, byte
        select case (depth)
        case (16)
            row(2 * index + 1) = s8(v / 256)
            row(2 * index + 2) = s8(mod(v, 256))
        case (8)
            row(index + 1) = s8(v)
        case default
            bit = index * depth
            byte = bit / 8 + 1
            row(byte) = s8(ior(u8(row(byte)), ishft(v, 8 - depth - mod(bit, 8))))
        end select
    end subroutine put_sample

    function apply_filter(row, prev, bpp, f, first) result(out)
        !! The PNG filters (ISO/IEC 15948, clause 9) applied to one row.
        integer(int8), intent(in) :: row(:), prev(:)
        integer, intent(in) :: bpp, f
        logical, intent(in) :: first
        integer(int8) :: out(size(row))
        integer :: i, a, b, c, p, pred
        do i = 1, size(row)
            a = 0
            b = 0
            c = 0
            if (i > bpp) a = u8(row(i - bpp))
            if (.not. first) b = u8(prev(i))
            if (i > bpp .and. .not. first) c = u8(prev(i - bpp))
            select case (f)
            case (0)
                pred = 0
            case (1)
                pred = a
            case (2)
                pred = b
            case (3)
                pred = (a + b) / 2
            case default
                p = a + b - c
                if (abs(p - a) <= abs(p - b) .and. abs(p - a) <= abs(p - c)) then
                    pred = a
                else if (abs(p - b) <= abs(p - c)) then
                    pred = b
                else
                    pred = c
                end if
            end select
            out(i) = s8(u8(row(i)) - pred)
        end do
    end function apply_filter

    pure integer function scale(v, depth)
        integer, intent(in) :: v, depth
        if (depth == 16) then
            scale = nint(real(v, 8) * 255 / 65535)
        else
            scale = v * 255 / (2**depth - 1)
        end if
    end function scale

    function chunk(tag, data) result(c)
        character(4), intent(in) :: tag
        integer(int8), intent(in) :: data(:)
        integer(int8), allocatable :: c(:), typed(:)
        integer :: k
        typed = [(int(iachar(tag(k:k)), int8), k = 1, 4), data]
        c = [be32(size(data)), typed, be32(int(crc32(typed)))]
    end function chunk

    pure function be32(v) result(b)
        integer, intent(in) :: v
        integer(int8) :: b(4)
        b = s8([ibits(v, 24, 8), ibits(v, 16, 8), ibits(v, 8, 8), ibits(v, 0, 8)])
    end function be32

    pure function be16(v) result(b)
        integer, intent(in) :: v
        integer(int8) :: b(2)
        b = s8([v / 256, mod(v, 256)])
    end function be16

    subroutine check_damage()
        !! Each defect in an otherwise valid file is refused.
        integer(int8), allocatable :: good(:), bad(:)
        integer :: expected(4, 7, 9), status, idat
        type(image_t) :: img
        call generate(2, 8, 0, 7, 9, .false., good, expected)
        bad = good
        bad(2) = s8(0)
        call expect_error(bad, IMG_ERR_FORMAT, "wrong signature")
        bad = good
        bad(30) = s8(ieor(u8(bad(30)), 1))
        call expect_error(bad, IMG_ERR_CORRUPT, "wrong IHDR CRC")
        idat = find(good, "IDAT")
        bad = good
        bad(idat + 6) = s8(ieor(u8(bad(idat + 6)), 1))
        call expect_error(bad, IMG_ERR_CORRUPT, "damaged IDAT data")
        call expect_error(good(:size(good) / 2), IMG_ERR_CORRUPT, "truncated file")
        call expect_error(with_ihdr(good, 1, 8), IMG_ERR_CORRUPT, "colour type 1")
        call expect_error(with_ihdr(good, 9, 8), IMG_ERR_CORRUPT, "colour type 9")
        call expect_error(with_ihdr(good, 2, 3), IMG_ERR_CORRUPT, "bit depth 3")
        call expect_error(with_ihdr(good, 3, 16), IMG_ERR_CORRUPT, "16-bit palette")
        call expect_error(with_ihdr(good, 3, 8), IMG_ERR_CORRUPT, "palette image without PLTE")
        call expect_error(without(good, "IDAT"), IMG_ERR_CORRUPT, "no IDAT")
        bad = [good(1:33), chunk("ABCD", s8([1, 2, 3])), good(34:)]
        call expect_error(bad, IMG_ERR_UNSUPPORTED, "unknown critical chunk")
        bad = [good(1:33), chunk("abCD", s8([1, 2, 3])), good(34:)]
        call png_decode(bad, img, status)
        call check(status == IMG_OK, "unknown ancillary chunk skipped")
    end subroutine check_damage

    subroutine expect_error(bytes, code, what)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: code
        character(*), intent(in) :: what
        type(image_t) :: img
        integer :: status
        call png_decode(bytes, img, status)
        call check(status == code, what // " refused")
    end subroutine expect_error

    integer function find(png, tag)
        !! Start of the first chunk with this tag (its length field).
        integer(int8), intent(in) :: png(:)
        character(4), intent(in) :: tag
        integer :: pos, k
        pos = 9
        find = 0
        do while (pos + 7 <= size(png))
            if (all([(u8(png(pos + 3 + k)), k = 1, 4)] == [(iachar(tag(k:k)), k = 1, 4)])) then
                find = pos
                return
            end if
            pos = pos + 12 + length_at(png, pos)
        end do
    end function find

    pure integer function length_at(png, pos)
        integer(int8), intent(in) :: png(:)
        integer, intent(in) :: pos
        length_at = ior(ior(ishft(u8(png(pos)), 24), ishft(u8(png(pos + 1)), 16)), &
            ior(ishft(u8(png(pos + 2)), 8), u8(png(pos + 3))))
    end function length_at

    function without(png, tag) result(out)
        integer(int8), intent(in) :: png(:)
        character(4), intent(in) :: tag
        integer(int8), allocatable :: out(:)
        integer :: at, n
        out = png
        do
            at = find(out, tag)
            if (at == 0) exit
            n = 12 + length_at(out, at)
            out = [out(1:at - 1), out(at + n:)]
        end do
    end function without

    function with_ihdr(png, ctype, depth) result(out)
        !! The file with another colour type and bit depth and a valid IHDR CRC.
        integer(int8), intent(in) :: png(:)
        integer, intent(in) :: ctype, depth
        integer(int8), allocatable :: out(:)
        out = png
        out(25) = s8(depth)
        out(26) = s8(ctype)
        out(30:33) = be32(int(crc32(out(13:29))))
    end function with_ihdr

    subroutine check_round_trips()
        !! fortimg's encoder at every channel count, on pictures of odd sizes.
        type(image_t) :: img, back
        integer(int8), allocatable :: png(:)
        integer :: ch, status, s
        integer, parameter :: DIMS(2, 3) = reshape([1, 1, 37, 23, 120, 7], [2, 3])
        do s = 1, size(DIMS, 2)
            do ch = 1, 4
                img%width = DIMS(1, s)
                img%height = DIMS(2, s)
                img%channels = 3
                img%pixels = picture(img%width, img%height, state)
                call image_convert(img, ch)
                if (ch == 2 .or. ch == 4) img%pixels(ch, :, :) = img%pixels(1, :, :)
                call png_encode(img, png, random_int(state, 10))
                call png_decode(png, back, status)
                call check(status == IMG_OK .and. back%channels == ch .and. &
                    all(back%pixels == img%pixels), "own PNG round trip")
            end do
        end do
    end subroutine check_round_trips

end program test_png
