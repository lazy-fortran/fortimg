module fortimg_jpeg
    !! JPEG (ISO/IEC 10918-1) decoding: baseline, extended and progressive
    !! Huffman-coded 8-bit images, any sampling factors, restart intervals,
    !! greyscale, YCbCr, RGB and Adobe CMYK/YCCK. Chroma is upsampled and
    !! converted with libjpeg's formulas; the inverse DCT is exact in floating
    !! point. Arithmetic coding, 12-bit and lossless JPEG are not supported.
    use iso_fortran_env, only: int8, int16, int64
    use fortimg_image, only: image_t, IMG_OK, IMG_ERR_FORMAT, IMG_ERR_CORRUPT, &
        IMG_ERR_UNSUPPORTED
    implicit none
    private
    public :: jpeg_decode, jpeg_exif_orientation, is_jpeg

    ! Natural (row-major) index of the k-th coefficient in zigzag order.
    integer, parameter :: ZIGZAG(0:63) = [0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, &
        4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35, 42, 49, 56, &
        57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45, 38, 31, 39, 46, 53, &
        60, 61, 54, 47, 55, 62, 63]

    type :: huffman_t
        !! fast(i): 256 * length + symbol for the 9-bit prefix i, 0 for longer codes.
        integer :: fast(0:511) = 0
        integer :: maxcode(1:17) = -1, mincode(1:16) = 0, valptr(1:16) = 0
        integer :: vals(0:255) = 0
        logical :: defined = .false.
    end type huffman_t

    type :: component_t
        integer :: id = 0, h = 1, v = 1, tq = 0
        integer :: bw = 0, bh = 0 ! blocks per row and column, MCU-padded
        integer :: width = 0, height = 0 ! samples actually covered by the image
        integer :: pred = 0, dc_table = 0, ac_table = 0
        integer(int16), allocatable :: coef(:, :, :) ! (0:63 natural, bw, bh)
        integer(int8), allocatable :: plane(:, :) ! (8 bw, 8 bh) samples
    end type component_t

    type :: decoder_t
        integer :: pos = 1, marker = -1
        integer(int64) :: acc = 0
        integer :: nbits = 0
        logical :: bad = .false.
        integer :: quant(0:63, 0:3) = 0
        type(huffman_t) :: dc(0:3), ac(0:3)
        type(component_t), allocatable :: comp(:)
        integer :: width = 0, height = 0, hmax = 1, vmax = 1, mcux = 0, mcuy = 0
        logical :: progressive = .false.
        integer :: restart = 0, eobrun = 0, adobe = -1
        logical :: jfif = .false.
    end type decoder_t

contains

    logical function is_jpeg(bytes)
        integer(int8), intent(in) :: bytes(:)
        is_jpeg = .false.
        if (size(bytes) < 3) return
        is_jpeg = u8(bytes(1)) == 255 .and. u8(bytes(2)) == 216 .and. u8(bytes(3)) == 255
    end function is_jpeg

    subroutine jpeg_decode(bytes, img, status)
        !! A JPEG file into an 8-bit image: 1 channel for greyscale, else RGB.
        integer(int8), intent(in) :: bytes(:)
        type(image_t), intent(out) :: img
        integer, intent(out) :: status
        type(decoder_t) :: d
        integer :: m, length, frames
        status = IMG_ERR_FORMAT
        if (.not. is_jpeg(bytes)) return
        status = IMG_ERR_CORRUPT
        d%pos = 3
        frames = 0
        do
            m = next_marker(d, bytes)
            if (m < 0) return
            if (m == 217) exit ! EOI
            if (m == 216 .or. (m >= 208 .and. m <= 215)) cycle
            if (d%pos + 1 > size(bytes)) return
            length = 256 * u8(bytes(d%pos)) + u8(bytes(d%pos + 1))
            if (length < 2 .or. d%pos + length - 1 > size(bytes)) return
            select case (m)
            case (192, 193, 194) ! SOF0 baseline, SOF1 extended, SOF2 progressive
                if (frames > 0) return
                frames = 1
                d%progressive = m == 194
                if (.not. read_frame(d, bytes(d%pos + 2:d%pos + length - 1), status)) return
                status = IMG_ERR_CORRUPT
            case (195, 197:199, 201:203, 205:207)
                status = IMG_ERR_UNSUPPORTED ! lossless, hierarchical or arithmetic
                return
            case (196)
                if (.not. read_huffman(d, bytes(d%pos + 2:d%pos + length - 1))) return
            case (219)
                if (.not. read_quant(d, bytes(d%pos + 2:d%pos + length - 1))) return
            case (221)
                if (length /= 4) return
                d%restart = 256 * u8(bytes(d%pos + 2)) + u8(bytes(d%pos + 3))
            case (224) ! APP0: JFIF
                if (length >= 7) d%jfif = all(bytes(d%pos + 2:d%pos + 6) == &
                    int([74, 70, 73, 70, 0], int8))
            case (238) ! APP14: Adobe colour transform
                if (length >= 14) then
                    if (all(bytes(d%pos + 2:d%pos + 6) == int([65, 100, 111, 98, 101], int8))) &
                        d%adobe = u8(bytes(d%pos + 13))
                end if
            case (218) ! SOS
                if (frames == 0) return
                if (.not. read_scan(d, bytes, length)) return
                cycle
            end select
            d%pos = d%pos + length
        end do
        if (frames == 0) return
        call finish(d, img)
        status = IMG_OK
    end subroutine jpeg_decode

    integer function next_marker(d, bytes) result(m)
        !! The next marker code, with d%pos after it; -1 at the end of the data.
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        m = -1
        do while (d%pos + 1 <= size(bytes))
            if (u8(bytes(d%pos)) == 255) then
                m = u8(bytes(d%pos + 1))
                if (m /= 0 .and. m /= 255) then
                    d%pos = d%pos + 2
                    return
                end if
                if (m == 255) then
                    d%pos = d%pos + 1
                    cycle
                end if
            end if
            d%pos = d%pos + 1
        end do
        m = -1
    end function next_marker

    logical function read_frame(d, seg, status) result(ok)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: seg(:)
        integer, intent(inout) :: status
        integer :: n, k, mcuw, mcuh
        ok = .false.
        if (size(seg) < 6) return
        if (u8(seg(1)) /= 8) then
            status = IMG_ERR_UNSUPPORTED
            return
        end if
        d%height = 256 * u8(seg(2)) + u8(seg(3))
        d%width = 256 * u8(seg(4)) + u8(seg(5))
        n = u8(seg(6))
        if (d%width == 0 .or. d%height == 0) return ! a DNL height is not supported
        if (n /= 1 .and. n /= 3 .and. n /= 4) then
            status = IMG_ERR_UNSUPPORTED
            return
        end if
        if (size(seg) < 6 + 3 * n) return
        allocate (d%comp(n))
        do k = 1, n
            d%comp(k)%id = u8(seg(4 + 3 * k))
            d%comp(k)%h = ishft(u8(seg(5 + 3 * k)), -4)
            d%comp(k)%v = iand(u8(seg(5 + 3 * k)), 15)
            d%comp(k)%tq = u8(seg(6 + 3 * k))
            if (d%comp(k)%h < 1 .or. d%comp(k)%h > 4 .or. d%comp(k)%v < 1 .or. &
                d%comp(k)%v > 4 .or. d%comp(k)%tq > 3) return
        end do
        ! A single component is never interleaved: its MCU is one block.
        if (n == 1) then
            d%comp(1)%h = 1
            d%comp(1)%v = 1
        end if
        d%hmax = maxval(d%comp%h)
        d%vmax = maxval(d%comp%v)
        mcuw = 8 * d%hmax
        mcuh = 8 * d%vmax
        d%mcux = (d%width + mcuw - 1) / mcuw
        d%mcuy = (d%height + mcuh - 1) / mcuh
        do k = 1, n
            associate (c => d%comp(k))
                c%bw = d%mcux * c%h
                c%bh = d%mcuy * c%v
                c%width = (d%width * c%h + d%hmax - 1) / d%hmax
                c%height = (d%height * c%v + d%vmax - 1) / d%vmax
                allocate (c%coef(0:63, c%bw, c%bh))
                c%coef = 0
            end associate
        end do
        ok = .true.
    end function read_frame

    logical function read_quant(d, seg) result(ok)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: seg(:)
        integer :: p, pq, tq, k
        ok = .false.
        p = 1
        do while (p <= size(seg))
            pq = ishft(u8(seg(p)), -4)
            tq = iand(u8(seg(p)), 15)
            if (tq > 3 .or. pq > 1) return
            if (p + 64 * (pq + 1) > size(seg)) return
            do k = 0, 63
                if (pq == 0) then
                    d%quant(ZIGZAG(k), tq) = u8(seg(p + 1 + k))
                else
                    d%quant(ZIGZAG(k), tq) = 256 * u8(seg(p + 1 + 2 * k)) + &
                        u8(seg(p + 2 + 2 * k))
                end if
            end do
            p = p + 1 + 64 * (pq + 1)
        end do
        ok = .true.
    end function read_quant

    logical function read_huffman(d, seg) result(ok)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: seg(:)
        integer :: p, tc, th, counts(16), total, k
        ok = .false.
        p = 1
        do while (p <= size(seg))
            if (p + 16 > size(seg)) return
            tc = ishft(u8(seg(p)), -4)
            th = iand(u8(seg(p)), 15)
            if (tc > 1 .or. th > 3) return
            counts = [(u8(seg(p + k)), k = 1, 16)]
            total = sum(counts)
            if (total > 256 .or. p + 16 + total > size(seg)) return
            if (tc == 0) then
                call build(d%dc(th), counts, seg(p + 17:p + 16 + total))
            else
                call build(d%ac(th), counts, seg(p + 17:p + 16 + total))
            end if
            p = p + 17 + total
        end do
        ok = .true.
    end function read_huffman

    subroutine build(t, counts, symbols)
        !! Canonical codes of Annex C, with a 9-bit lookup table.
        type(huffman_t), intent(out) :: t
        integer, intent(in) :: counts(16)
        integer(int8), intent(in) :: symbols(:)
        integer :: code, k, len, i, j, first, fill
        k = 0
        code = 0
        do len = 1, 16
            t%valptr(len) = k
            t%mincode(len) = code
            do i = 1, counts(len)
                t%vals(k) = u8(symbols(k + 1))
                if (len <= 9) then
                    first = ishft(code, 9 - len)
                    fill = ishft(1, 9 - len)
                    do j = first, first + fill - 1
                        t%fast(j) = 256 * len + t%vals(k)
                    end do
                end if
                k = k + 1
                code = code + 1
            end do
            t%maxcode(len) = code - 1
            if (counts(len) == 0) t%maxcode(len) = -1
            code = ishft(code, 1)
        end do
        t%maxcode(17) = huge(1)
        t%defined = .true.
    end subroutine build

    ! --- entropy-coded data -------------------------------------------------

    subroutine fill(d, bytes)
        !! At least 32 bits in the accumulator; zeros once a marker is reached.
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        integer :: b, nb
        do while (d%nbits <= 56)
            b = 0
            if (d%marker < 0 .and. d%pos <= size(bytes)) then
                b = u8(bytes(d%pos))
                if (b == 255) then
                    nb = 0
                    if (d%pos + 1 <= size(bytes)) nb = u8(bytes(d%pos + 1))
                    if (nb == 0) then
                        d%pos = d%pos + 2
                    else
                        d%marker = nb
                        b = 0
                    end if
                else
                    d%pos = d%pos + 1
                end if
            end if
            d%acc = ior(ishft(d%acc, 8), int(b, int64))
            d%nbits = d%nbits + 8
            if (d%nbits > 32 .and. d%marker >= 0) exit
        end do
    end subroutine fill

    integer function getbits(d, bytes, n) result(v)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: n
        if (n == 0) then
            v = 0
            return
        end if
        if (d%nbits < n) call fill(d, bytes)
        v = int(iand(ishft(d%acc, -(d%nbits - n)), ishft(1_int64, n) - 1))
        d%nbits = d%nbits - n
    end function getbits

    integer function extend(v, s)
        !! The signed value of an s-bit magnitude category (F.2.2.1).
        integer, intent(in) :: v, s
        extend = v
        if (s > 0) then
            if (v < ishft(1, s - 1)) extend = v - ishft(1, s) + 1
        end if
    end function extend

    integer function decode(d, bytes, t) result(sym)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        type(huffman_t), intent(in) :: t
        integer :: e, len, code
        if (d%nbits < 16) call fill(d, bytes)
        e = t%fast(int(iand(ishft(d%acc, -(d%nbits - 9)), 511_int64)))
        if (e > 0) then
            d%nbits = d%nbits - e / 256
            sym = iand(e, 255)
            return
        end if
        do len = 10, 16
            code = int(iand(ishft(d%acc, -(d%nbits - len)), ishft(1_int64, len) - 1))
            if (code <= t%maxcode(len)) then
                d%nbits = d%nbits - len
                sym = t%vals(t%valptr(len) + code - t%mincode(len))
                return
            end if
        end do
        d%bad = .true.
        sym = 0
    end function decode

    logical function read_scan(d, bytes, length) result(ok)
        !! An SOS header and its entropy-coded data, with d%pos left at the
        !! marker that ends it.
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: length
        integer :: ns, k, j, id, ss, se, ah, al, which(4)
        integer :: mx, my, nx, ny, c, bx, by, hh, vv, count, total
        ok = .false.
        ns = u8(bytes(d%pos + 2))
        if (ns < 1 .or. ns > 4 .or. length /= 6 + 2 * ns) return
        do k = 1, ns
            id = u8(bytes(d%pos + 1 + 2 * k))
            which(k) = 0
            do j = 1, size(d%comp)
                if (d%comp(j)%id == id) which(k) = j
            end do
            if (which(k) == 0) return
            d%comp(which(k))%dc_table = ishft(u8(bytes(d%pos + 2 + 2 * k)), -4)
            d%comp(which(k))%ac_table = iand(u8(bytes(d%pos + 2 + 2 * k)), 15)
            if (d%comp(which(k))%dc_table > 3 .or. d%comp(which(k))%ac_table > 3) return
        end do
        ss = u8(bytes(d%pos + 3 + 2 * ns))
        se = u8(bytes(d%pos + 4 + 2 * ns))
        ah = ishft(u8(bytes(d%pos + 5 + 2 * ns)), -4)
        al = iand(u8(bytes(d%pos + 5 + 2 * ns)), 15)
        if (.not. d%progressive) then
            ss = 0
            se = 63
            ah = 0
            al = 0
        else
            if (ss > se .or. se > 63 .or. (ss == 0 .and. se /= 0) .or. (ss > 0 .and. ns /= 1)) &
                return
        end if
        d%pos = d%pos + length
        call reset(d)
        count = 0
        if (ns == 1) then
            ! Non-interleaved: the blocks the component actually covers.
            c = which(1)
            nx = (d%comp(c)%width + 7) / 8
            ny = (d%comp(c)%height + 7) / 8
            total = nx * ny
            do by = 1, ny
                do bx = 1, nx
                    call restart_check()
                    call block(c, bx, by)
                    if (d%bad) return
                end do
            end do
        else
            do my = 0, d%mcuy - 1
                do mx = 0, d%mcux - 1
                    call restart_check()
                    do k = 1, ns
                        c = which(k)
                        do vv = 1, d%comp(c)%v
                            do hh = 1, d%comp(c)%h
                                call block(c, mx * d%comp(c)%h + hh, my * d%comp(c)%v + vv)
                            end do
                        end do
                    end do
                    if (d%bad) return
                end do
            end do
        end if
        ! Resume marker parsing at the marker that ended the entropy-coded data.
        if (d%marker >= 0) then
            d%marker = -1
        end if
        d%nbits = 0
        d%acc = 0
        ok = .true.
    contains
        subroutine restart_check()
            if (d%restart > 0) then
                if (count > 0 .and. mod(count, d%restart) == 0) then
                    ! Drop the padding bits, skip to the RSTn marker and past it.
                    d%nbits = 0
                    d%acc = 0
                    if (d%marker < 0) then
                        k = next_marker(d, bytes)
                    else
                        d%pos = d%pos + 2
                    end if
                    d%marker = -1
                    call reset(d)
                end if
            end if
            count = count + 1
        end subroutine restart_check

        subroutine block(c, bx, by)
            integer, intent(in) :: c, bx, by
            if (bx > d%comp(c)%bw .or. by > d%comp(c)%bh) return
            if (.not. d%progressive) then
                call decode_baseline(d, bytes, d%comp(c), d%comp(c)%coef(:, bx, by))
            else if (ss == 0) then
                call decode_dc_progressive(d, bytes, d%comp(c), d%comp(c)%coef(:, bx, by), &
                    ah, al)
            else if (ah == 0) then
                call decode_ac_first(d, bytes, d%ac(d%comp(c)%ac_table), &
                    d%comp(c)%coef(:, bx, by), ss, se, al)
            else
                call decode_ac_refine(d, bytes, d%ac(d%comp(c)%ac_table), &
                    d%comp(c)%coef(:, bx, by), ss, se, al)
            end if
        end subroutine block
    end function read_scan

    subroutine reset(d)
        type(decoder_t), intent(inout) :: d
        integer :: k
        do k = 1, size(d%comp)
            d%comp(k)%pred = 0
        end do
        d%eobrun = 0
    end subroutine reset

    subroutine decode_baseline(d, bytes, c, coef)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        type(component_t), intent(inout) :: c
        integer(int16), intent(inout) :: coef(0:63)
        integer :: t, k, rs, r, s
        t = decode(d, bytes, d%dc(c%dc_table))
        if (t > 11) then
            d%bad = .true.
            return
        end if
        c%pred = c%pred + extend(getbits(d, bytes, t), t)
        coef = 0
        coef(0) = int(c%pred, int16)
        k = 1
        do while (k <= 63)
            rs = decode(d, bytes, d%ac(c%ac_table))
            r = ishft(rs, -4)
            s = iand(rs, 15)
            if (s == 0) then
                if (r /= 15) exit
                k = k + 16
                cycle
            end if
            k = k + r
            if (k > 63) then
                d%bad = .true.
                return
            end if
            coef(ZIGZAG(k)) = int(extend(getbits(d, bytes, s), s), int16)
            k = k + 1
        end do
    end subroutine decode_baseline

    subroutine decode_dc_progressive(d, bytes, c, coef, ah, al)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        type(component_t), intent(inout) :: c
        integer(int16), intent(inout) :: coef(0:63)
        integer, intent(in) :: ah, al
        integer :: t
        if (ah == 0) then
            t = decode(d, bytes, d%dc(c%dc_table))
            if (t > 11) then
                d%bad = .true.
                return
            end if
            c%pred = c%pred + extend(getbits(d, bytes, t), t)
            coef(0) = int(c%pred * 2**al, int16)
        else
            if (getbits(d, bytes, 1) == 1) coef(0) = int(ior(int(coef(0)), 2**al), int16)
        end if
    end subroutine decode_dc_progressive

    subroutine decode_ac_first(d, bytes, t, coef, ss, se, al)
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        type(huffman_t), intent(in) :: t
        integer(int16), intent(inout) :: coef(0:63)
        integer, intent(in) :: ss, se, al
        integer :: k, rs, r, s
        if (d%eobrun > 0) then
            d%eobrun = d%eobrun - 1
            return
        end if
        k = ss
        do while (k <= se)
            rs = decode(d, bytes, t)
            r = ishft(rs, -4)
            s = iand(rs, 15)
            if (s == 0) then
                if (r < 15) then
                    d%eobrun = ishft(1, r) - 1
                    if (r > 0) d%eobrun = d%eobrun + getbits(d, bytes, r)
                    exit
                end if
                k = k + 16
                cycle
            end if
            k = k + r
            if (k > 63) then
                d%bad = .true.
                return
            end if
            coef(ZIGZAG(k)) = int(extend(getbits(d, bytes, s), s) * 2**al, int16)
            k = k + 1
        end do
    end subroutine decode_ac_first

    subroutine decode_ac_refine(d, bytes, t, coef, ss, se, al)
        !! Successive approximation of AC coefficients (G.1.2.3), as in stb_image.
        type(decoder_t), intent(inout) :: d
        integer(int8), intent(in) :: bytes(:)
        type(huffman_t), intent(in) :: t
        integer(int16), intent(inout) :: coef(0:63)
        integer, intent(in) :: ss, se, al
        integer :: k, rs, r, s, bit, p
        bit = 2**al
        if (d%eobrun > 0) then
            d%eobrun = d%eobrun - 1
            do k = ss, se
                call refine(ZIGZAG(k))
            end do
            return
        end if
        k = ss
        do while (k <= se)
            rs = decode(d, bytes, t)
            if (d%bad) return
            r = ishft(rs, -4)
            s = iand(rs, 15)
            if (s == 0) then
                if (r < 15) then
                    d%eobrun = ishft(1, r) - 1
                    if (r > 0) d%eobrun = d%eobrun + getbits(d, bytes, r)
                    r = 64
                end if
            else
                if (s /= 1) then
                    d%bad = .true.
                    return
                end if
                if (getbits(d, bytes, 1) == 1) then
                    s = bit
                else
                    s = -bit
                end if
            end if
            do while (k <= se)
                p = ZIGZAG(k)
                k = k + 1
                if (coef(p) /= 0) then
                    call refine(p)
                else
                    if (r == 0) then
                        coef(p) = int(s, int16)
                        exit
                    end if
                    r = r - 1
                end if
            end do
        end do
    contains
        subroutine refine(p)
            integer, intent(in) :: p
            if (coef(p) == 0) return
            if (getbits(d, bytes, 1) == 0) return
            if (iand(int(coef(p)), bit) /= 0) return
            if (coef(p) > 0) then
                coef(p) = int(coef(p) + bit, int16)
            else
                coef(p) = int(coef(p) - bit, int16)
            end if
        end subroutine refine
    end subroutine decode_ac_refine

    ! --- reconstruction -----------------------------------------------------

    subroutine finish(d, img)
        type(decoder_t), intent(inout) :: d
        type(image_t), intent(inout) :: img
        integer :: k
        do k = 1, size(d%comp)
            call inverse_dct(d%comp(k), d%quant(:, d%comp(k)%tq))
        end do
        img%width = d%width
        img%height = d%height
        if (size(d%comp) == 1) then
            img%channels = 1
            allocate (img%pixels(1, d%width, d%height))
            img%pixels(1, :, :) = d%comp(1)%plane(1:d%width, 1:d%height)
            return
        end if
        img%channels = 3
        allocate (img%pixels(3, d%width, d%height))
        call colour(d, img)
    end subroutine finish

    subroutine inverse_dct(c, q)
        !! Dequantised coefficients to samples: f = C^T F C with the exact
        !! orthonormal DCT-II basis, rounded and clamped to 0..255.
        type(component_t), intent(inout) :: c
        integer, intent(in) :: q(0:63)
        real :: basis(0:7, 0:7), f(0:7, 0:7), t(0:7, 0:7), qr(0:63)
        integer :: bx, by, x, y, u
        real, parameter :: PI = acos(-1.0)
        do x = 0, 7
            do u = 0, 7
                basis(u, x) = 0.5 * cos((2 * x + 1) * u * PI / 16)
                if (u == 0) basis(u, x) = basis(u, x) / sqrt(2.0)
            end do
        end do
        qr = real(q)
        allocate (c%plane(8 * c%bw, 8 * c%bh))
        do by = 1, c%bh
            do bx = 1, c%bw
                ! f(v, u): row v, column u of the block's coefficients.
                f = transpose(reshape(real(c%coef(:, bx, by)) * qr, [8, 8]))
                t = matmul(transpose(basis), f)
                t = matmul(t, basis)
                do y = 0, 7
                    do x = 0, 7
                        c%plane(8 * (bx - 1) + x + 1, 8 * (by - 1) + y + 1) = &
                            s8(max(0, min(255, nint(t(y, x)) + 128)))
                    end do
                end do
            end do
        end do
        deallocate (c%coef)
    end subroutine inverse_dct

    subroutine colour(d, img)
        !! Upsample every component to full size and convert to RGB.
        type(decoder_t), intent(inout) :: d
        type(image_t), intent(inout) :: img
        integer, allocatable :: full(:, :, :)
        integer :: k, x, y, n, transform
        integer :: c1, c2, c3, kk
        n = size(d%comp)
        allocate (full(d%width, d%height, n))
        do k = 1, n
            call upsample(d%comp(k), d%hmax / d%comp(k)%h, d%vmax / d%comp(k)%v, d%width, &
                d%height, full(:, :, k))
        end do
        ! 0: no transform (RGB or CMYK), 1: YCbCr, 2: YCCK.
        if (d%adobe >= 0) then
            transform = d%adobe
        else if (n == 3 .and. d%comp(1)%id == 82 .and. d%comp(2)%id == 71 .and. &
                d%comp(3)%id == 66) then
            transform = 0
        else if (n == 3) then
            transform = 1
        else
            transform = 0
        end if
        do y = 1, d%height
            do x = 1, d%width
                c1 = full(x, y, 1)
                c2 = full(x, y, 2)
                c3 = full(x, y, 3)
                if (transform >= 1) call ycc_to_rgb(c1, c2, c3)
                if (transform == 2) then
                    ! YCCK: libjpeg's CMYK is 255 minus the YCbCr conversion.
                    c1 = 255 - c1
                    c2 = 255 - c2
                    c3 = 255 - c3
                end if
                if (n == 4) then
                    ! Adobe CMYK is stored inverted: R = (255 - C)(255 - K)/255 = c k/255.
                    kk = full(x, y, 4)
                    c1 = (c1 * kk + 127) / 255
                    c2 = (c2 * kk + 127) / 255
                    c3 = (c3 * kk + 127) / 255
                end if
                img%pixels(:, x, y) = [s8(c1), s8(c2), s8(c3)]
            end do
        end do
    end subroutine colour

    subroutine ycc_to_rgb(y, cb, cr)
        !! libjpeg's fixed-point YCbCr to RGB (jdcolor.c), in place.
        integer, intent(inout) :: y, cb, cr
        integer(int64), parameter :: HALF = 32768
        integer(int64), parameter :: F_R = 91881, F_GB = 22554, F_GR = 46802, F_B = 116130
        integer :: r, g, b
        r = y + int(floor_shift(F_R * (cr - 128) + HALF))
        g = y + int(floor_shift(-F_GB * (cb - 128) - F_GR * (cr - 128) + HALF))
        b = y + int(floor_shift(F_B * (cb - 128) + HALF))
        y = max(0, min(255, r))
        cb = max(0, min(255, g))
        cr = max(0, min(255, b))
    end subroutine ycc_to_rgb

    pure integer(int64) function floor_shift(v)
        !! v / 65536 rounded towards minus infinity (an arithmetic right shift).
        integer(int64), intent(in) :: v
        floor_shift = (v - modulo(v, 65536_int64)) / 65536
    end function floor_shift

    subroutine upsample(c, fx, fy, width, height, out)
        !! A component at full resolution. Factors 2x1, 1x2 and 2x2 use
        !! libjpeg-turbo's triangle ("fancy") filters; other factors replicate.
        type(component_t), intent(in) :: c
        integer, intent(in) :: fx, fy, width, height
        integer, intent(out) :: out(:, :)
        integer :: x, y, n, m, sx, sy, cur, near, i
        integer, allocatable :: row(:), colsum(:)
        n = c%width
        m = c%height
        if (fx == 1 .and. fy == 1) then
            do y = 1, height
                do x = 1, width
                    out(x, y) = u8(c%plane(x, y))
                end do
            end do
        else if (fx == 2 .and. fy == 1) then
            allocate (row(n))
            do y = 1, height
                row = [(u8(c%plane(i, y)), i = 1, n)]
                call fancy_h2(row, n, out(:, y), width)
            end do
        else if (fx == 1 .and. fy == 2) then
            do y = 1, height
                sy = (y + 1) / 2
                do x = 1, width
                    cur = u8(c%plane(x, sy))
                    if (mod(y, 2) == 1) then
                        near = u8(c%plane(x, max(1, sy - 1)))
                        out(x, y) = (3 * cur + near + 1) / 4
                    else
                        near = u8(c%plane(x, min(m, sy + 1)))
                        out(x, y) = (3 * cur + near + 2) / 4
                    end if
                end do
            end do
        else if (fx == 2 .and. fy == 2) then
            allocate (colsum(n))
            do y = 1, height
                sy = (y + 1) / 2
                do i = 1, n
                    if (mod(y, 2) == 1) then
                        near = u8(c%plane(i, max(1, sy - 1)))
                    else
                        near = u8(c%plane(i, min(m, sy + 1)))
                    end if
                    colsum(i) = 3 * u8(c%plane(i, sy)) + near
                end do
                call fancy_h2v2(colsum, n, out(:, y), width)
            end do
        else
            do y = 1, height
                sy = min(m, (y - 1) / fy + 1)
                do x = 1, width
                    sx = min(n, (x - 1) / fx + 1)
                    out(x, y) = u8(c%plane(sx, sy))
                end do
            end do
        end if
    end subroutine upsample

    subroutine fancy_h2(row, n, out, width)
        !! jdsample.c h2v1_fancy_upsample: each output 3/4 nearer, 1/4 farther.
        integer, intent(in) :: n, width
        integer, intent(in) :: row(n)
        integer, intent(inout) :: out(:)
        integer :: i, tmp(2 * n)
        if (n == 1) then
            tmp(1:2) = row(1)
        else
            tmp(1) = row(1)
            tmp(2) = (3 * row(1) + row(2) + 2) / 4
            do i = 2, n - 1
                tmp(2 * i - 1) = (3 * row(i) + row(i - 1) + 1) / 4
                tmp(2 * i) = (3 * row(i) + row(i + 1) + 2) / 4
            end do
            tmp(2 * n - 1) = (3 * row(n) + row(n - 1) + 1) / 4
            tmp(2 * n) = row(n)
        end if
        out(1:width) = tmp(1:width)
    end subroutine fancy_h2

    subroutine fancy_h2v2(colsum, n, out, width)
        !! jdsample.c h2v2_fancy_upsample on precomputed column sums (3 near + far).
        integer, intent(in) :: n, width
        integer, intent(in) :: colsum(n)
        integer, intent(inout) :: out(:)
        integer :: i, tmp(2 * n)
        if (n == 1) then
            tmp(1) = (colsum(1) * 4 + 8) / 16
            tmp(2) = (colsum(1) * 4 + 7) / 16
        else
            tmp(1) = (colsum(1) * 4 + 8) / 16
            tmp(2) = (colsum(1) * 3 + colsum(2) + 7) / 16
            do i = 2, n - 1
                tmp(2 * i - 1) = (colsum(i) * 3 + colsum(i - 1) + 8) / 16
                tmp(2 * i) = (colsum(i) * 3 + colsum(i + 1) + 7) / 16
            end do
            tmp(2 * n - 1) = (colsum(n) * 3 + colsum(n - 1) + 8) / 16
            tmp(2 * n) = (colsum(n) * 4 + 7) / 16
        end if
        out(1:width) = tmp(1:width)
    end subroutine fancy_h2v2

    ! --- EXIF ---------------------------------------------------------------

    pure integer function jpeg_exif_orientation(b) result(o)
        !! EXIF orientation (1..8) of a JPEG, 1 when absent. Walks the markers to
        !! the APP1 "Exif" segment and reads tag 0x0112 of the first TIFF IFD.
        integer(int8), intent(in) :: b(:)
        integer :: p, seg, t, ifd, entries, k, e
        logical :: le
        o = 1
        p = 3
        do while (p + 3 <= size(b))
            if (u8(b(p)) /= 255) return
            if (u8(b(p + 1)) == 218 .or. u8(b(p + 1)) == 217) return
            seg = 256 * u8(b(p + 2)) + u8(b(p + 3))
            if (u8(b(p + 1)) == 225 .and. p + 3 + seg <= size(b) .and. seg > 16) then
                if (all([(u8(b(p + 3 + k)), k = 1, 6)] == [69, 120, 105, 102, 0, 0])) then
                    t = p + 10
                    le = u8(b(t)) == 73
                    ifd = t + get32(t + 4)
                    if (ifd < t .or. ifd + 1 > size(b)) return
                    entries = get16(ifd)
                    do k = 0, entries - 1
                        e = ifd + 2 + 12 * k
                        if (e + 11 > size(b)) return
                        if (get16(e) == 274) then
                            o = get16(e + 8)
                            if (o < 1 .or. o > 8) o = 1
                            return
                        end if
                    end do
                    return
                end if
            end if
            p = p + 2 + seg
        end do
    contains
        pure integer function get16(at)
            integer, intent(in) :: at
            if (le) then
                get16 = u8(b(at)) + 256 * u8(b(at + 1))
            else
                get16 = 256 * u8(b(at)) + u8(b(at + 1))
            end if
        end function get16
        pure integer function get32(at)
            integer, intent(in) :: at
            if (le) then
                get32 = get16(at) + 65536 * get16(at + 2)
            else
                get32 = 65536 * get16(at) + get16(at + 2)
            end if
        end function get32
    end function jpeg_exif_orientation

    pure integer function u8(b)
        integer(int8), intent(in) :: b
        u8 = iand(int(b), 255)
    end function u8

    pure integer(int8) function s8(v)
        integer, intent(in) :: v
        s8 = int(iand(v, 255) - 256 * (iand(v, 255) / 128), int8)
    end function s8

end module fortimg_jpeg
