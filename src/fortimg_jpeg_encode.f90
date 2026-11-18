module fortimg_jpeg_encode
    !! Baseline JPEG (JFIF) encoder: YCbCr with 4:2:0 or 4:4:4 chroma, or
    !! greyscale; the quantization tables of the JPEG standard (Annex K) scaled
    !! by quality as the IJG library does; the standard Huffman tables.
    use iso_fortran_env, only: int8, int32
    use fortimg_image, only: image_t
    implicit none
    private
    public :: jpeg_encode

    integer, parameter :: ZIGZAG(0:63) = [0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, &
        4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35, 42, 49, 56, &
        57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45, 38, 31, 39, 46, 53, &
        60, 61, 54, 47, 55, 62, 63]
    integer, parameter :: Q_LUMA(0:63) = [16, 11, 10, 16, 24, 40, 51, 61, 12, 12, 14, 19, &
        26, 58, 60, 55, 14, 13, 16, 24, 40, 57, 69, 56, 14, 17, 22, 29, 51, 87, 80, 62, 18, &
        22, 37, 56, 68, 109, 103, 77, 24, 35, 55, 64, 81, 104, 113, 92, 49, 64, 78, 87, 103, &
        121, 120, 101, 72, 92, 95, 98, 112, 100, 103, 99]
    integer, parameter :: Q_CHROMA(0:63) = [17, 18, 24, 47, 99, 99, 99, 99, 18, 21, 26, 66, &
        99, 99, 99, 99, 24, 26, 56, 99, 99, 99, 99, 99, 47, 66, 99, 99, 99, 99, 99, 99, &
        spread(99, 1, 32)]
    integer, parameter :: DC_LUMA_BITS(16) = [0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0]
    integer, parameter :: DC_CHROMA_BITS(16) = [0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
    integer, parameter :: DC_VALUES(12) = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    integer, parameter :: AC_LUMA_BITS(16) = [0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 125]
    integer, parameter :: AC_LUMA_VALUES(162) = [ &
        int(z'01'), int(z'02'), int(z'03'), int(z'00'), int(z'04'), int(z'11'), int(z'05'), &
        int(z'12'), int(z'21'), int(z'31'), int(z'41'), int(z'06'), int(z'13'), int(z'51'), &
        int(z'61'), int(z'07'), int(z'22'), int(z'71'), int(z'14'), int(z'32'), int(z'81'), &
        int(z'91'), int(z'a1'), int(z'08'), int(z'23'), int(z'42'), int(z'b1'), int(z'c1'), &
        int(z'15'), int(z'52'), int(z'd1'), int(z'f0'), int(z'24'), int(z'33'), int(z'62'), &
        int(z'72'), int(z'82'), int(z'09'), int(z'0a'), int(z'16'), int(z'17'), int(z'18'), &
        int(z'19'), int(z'1a'), int(z'25'), int(z'26'), int(z'27'), int(z'28'), int(z'29'), &
        int(z'2a'), int(z'34'), int(z'35'), int(z'36'), int(z'37'), int(z'38'), int(z'39'), &
        int(z'3a'), int(z'43'), int(z'44'), int(z'45'), int(z'46'), int(z'47'), int(z'48'), &
        int(z'49'), int(z'4a'), int(z'53'), int(z'54'), int(z'55'), int(z'56'), int(z'57'), &
        int(z'58'), int(z'59'), int(z'5a'), int(z'63'), int(z'64'), int(z'65'), int(z'66'), &
        int(z'67'), int(z'68'), int(z'69'), int(z'6a'), int(z'73'), int(z'74'), int(z'75'), &
        int(z'76'), int(z'77'), int(z'78'), int(z'79'), int(z'7a'), int(z'83'), int(z'84'), &
        int(z'85'), int(z'86'), int(z'87'), int(z'88'), int(z'89'), int(z'8a'), int(z'92'), &
        int(z'93'), int(z'94'), int(z'95'), int(z'96'), int(z'97'), int(z'98'), int(z'99'), &
        int(z'9a'), int(z'a2'), int(z'a3'), int(z'a4'), int(z'a5'), int(z'a6'), int(z'a7'), &
        int(z'a8'), int(z'a9'), int(z'aa'), int(z'b2'), int(z'b3'), int(z'b4'), int(z'b5'), &
        int(z'b6'), int(z'b7'), int(z'b8'), int(z'b9'), int(z'ba'), int(z'c2'), int(z'c3'), &
        int(z'c4'), int(z'c5'), int(z'c6'), int(z'c7'), int(z'c8'), int(z'c9'), int(z'ca'), &
        int(z'd2'), int(z'd3'), int(z'd4'), int(z'd5'), int(z'd6'), int(z'd7'), int(z'd8'), &
        int(z'd9'), int(z'da'), int(z'e1'), int(z'e2'), int(z'e3'), int(z'e4'), int(z'e5'), &
        int(z'e6'), int(z'e7'), int(z'e8'), int(z'e9'), int(z'ea'), int(z'f1'), int(z'f2'), &
        int(z'f3'), int(z'f4'), int(z'f5'), int(z'f6'), int(z'f7'), int(z'f8'), int(z'f9'), &
        int(z'fa')]
    integer, parameter :: AC_CHROMA_BITS(16) = [0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 119]
    integer, parameter :: AC_CHROMA_VALUES(162) = [ &
        int(z'00'), int(z'01'), int(z'02'), int(z'03'), int(z'11'), int(z'04'), int(z'05'), &
        int(z'21'), int(z'31'), int(z'06'), int(z'12'), int(z'41'), int(z'51'), int(z'07'), &
        int(z'61'), int(z'71'), int(z'13'), int(z'22'), int(z'32'), int(z'81'), int(z'08'), &
        int(z'14'), int(z'42'), int(z'91'), int(z'a1'), int(z'b1'), int(z'c1'), int(z'09'), &
        int(z'23'), int(z'33'), int(z'52'), int(z'f0'), int(z'15'), int(z'62'), int(z'72'), &
        int(z'd1'), int(z'0a'), int(z'16'), int(z'24'), int(z'34'), int(z'e1'), int(z'25'), &
        int(z'f1'), int(z'17'), int(z'18'), int(z'19'), int(z'1a'), int(z'26'), int(z'27'), &
        int(z'28'), int(z'29'), int(z'2a'), int(z'35'), int(z'36'), int(z'37'), int(z'38'), &
        int(z'39'), int(z'3a'), int(z'43'), int(z'44'), int(z'45'), int(z'46'), int(z'47'), &
        int(z'48'), int(z'49'), int(z'4a'), int(z'53'), int(z'54'), int(z'55'), int(z'56'), &
        int(z'57'), int(z'58'), int(z'59'), int(z'5a'), int(z'63'), int(z'64'), int(z'65'), &
        int(z'66'), int(z'67'), int(z'68'), int(z'69'), int(z'6a'), int(z'73'), int(z'74'), &
        int(z'75'), int(z'76'), int(z'77'), int(z'78'), int(z'79'), int(z'7a'), int(z'82'), &
        int(z'83'), int(z'84'), int(z'85'), int(z'86'), int(z'87'), int(z'88'), int(z'89'), &
        int(z'8a'), int(z'92'), int(z'93'), int(z'94'), int(z'95'), int(z'96'), int(z'97'), &
        int(z'98'), int(z'99'), int(z'9a'), int(z'a2'), int(z'a3'), int(z'a4'), int(z'a5'), &
        int(z'a6'), int(z'a7'), int(z'a8'), int(z'a9'), int(z'aa'), int(z'b2'), int(z'b3'), &
        int(z'b4'), int(z'b5'), int(z'b6'), int(z'b7'), int(z'b8'), int(z'b9'), int(z'ba'), &
        int(z'c2'), int(z'c3'), int(z'c4'), int(z'c5'), int(z'c6'), int(z'c7'), int(z'c8'), &
        int(z'c9'), int(z'ca'), int(z'd2'), int(z'd3'), int(z'd4'), int(z'd5'), int(z'd6'), &
        int(z'd7'), int(z'd8'), int(z'd9'), int(z'da'), int(z'e2'), int(z'e3'), int(z'e4'), &
        int(z'e5'), int(z'e6'), int(z'e7'), int(z'e8'), int(z'e9'), int(z'ea'), int(z'f2'), &
        int(z'f3'), int(z'f4'), int(z'f5'), int(z'f6'), int(z'f7'), int(z'f8'), int(z'f9'), &
        int(z'fa')]

    type :: huff_t
        integer :: code(0:255) = 0, length(0:255) = 0
    end type huff_t

    type :: writer_t
        integer(int8), allocatable :: buf(:)
        integer :: n = 0
        integer :: acc = 0, bits = 0
    end type writer_t

contains

    subroutine jpeg_encode(img, bytes, quality, dpi, full_chroma)
        !! Encodes img (grey, grey+alpha, RGB or RGBA; alpha is dropped).
        !! quality 1..100 (default 90); dpi is stored in the JFIF header
        !! (default 72); full_chroma keeps colour at full resolution (4:4:4,
        !! default for quality 90 and above), otherwise 4:2:0.
        type(image_t), intent(in) :: img
        integer(int8), allocatable, intent(out) :: bytes(:)
        integer, intent(in), optional :: quality, dpi
        logical, intent(in), optional :: full_chroma
        type(writer_t) :: w
        type(huff_t) :: dcl, acl, dcc, acc
        integer :: q, density, qt(0:63, 2), h, k, mx, my, bx, by, ncomp, dc(3)
        logical :: colour, full
        real, allocatable :: plane(:, :, :)
        real :: block(0:7, 0:7)

        q = 90
        if (present(quality)) q = max(1, min(100, quality))
        density = 72
        if (present(dpi)) density = max(1, min(65535, dpi))
        full = q >= 90
        if (present(full_chroma)) full = full_chroma
        colour = img%channels >= 3
        ncomp = merge(3, 1, colour)
        h = merge(1, 2, full .or. .not. colour)
        call scaled_table(Q_LUMA, q, qt(:, 1))
        call scaled_table(Q_CHROMA, q, qt(:, 2))
        call build(DC_LUMA_BITS, DC_VALUES, dcl)
        call build(AC_LUMA_BITS, AC_LUMA_VALUES, acl)
        call build(DC_CHROMA_BITS, DC_VALUES, dcc)
        call build(AC_CHROMA_BITS, AC_CHROMA_VALUES, acc)
        call planes(img, colour, plane)

        allocate(w%buf(4096 + img%width * img%height))
        call put(w, [255, 216])
        call put(w, [255, 224, 0, 16, 74, 70, 73, 70, 0, 1, 1, 1, density / 256, mod(density, 256), &
            density / 256, mod(density, 256), 0, 0])
        call put(w, [255, 219, 0, 2 + 65 * merge(2, 1, colour)])
        do k = 1, merge(2, 1, colour)
            call put(w, [k - 1])
            call put(w, qt(ZIGZAG, k))
        end do
        call put(w, [255, 192, 0, 8 + 3 * ncomp, 8, img%height / 256, mod(img%height, 256), &
            img%width / 256, mod(img%width, 256), ncomp])
        call put(w, [1, 17 * h, 0])
        if (colour) call put(w, [2, 17, 1, 3, 17, 1])
        call huffman_segment(w, 0, DC_LUMA_BITS, DC_VALUES)
        call huffman_segment(w, 16, AC_LUMA_BITS, AC_LUMA_VALUES)
        if (colour) then
            call huffman_segment(w, 1, DC_CHROMA_BITS, DC_VALUES)
            call huffman_segment(w, 17, AC_CHROMA_BITS, AC_CHROMA_VALUES)
        end if
        call put(w, [255, 218, 0, 6 + 2 * ncomp, ncomp, 1, 0])
        if (colour) call put(w, [2, 17, 3, 17])
        call put(w, [0, 63, 0])

        dc = 0
        mx = (img%width + 8 * h - 1) / (8 * h)
        my = (img%height + 8 * h - 1) / (8 * h)
        do by = 0, my - 1
            do bx = 0, mx - 1
                do k = 0, h * h - 1
                    call fetch(plane(:, :, 1), 8 * (h * bx + mod(k, h)), 8 * (h * by + k / h), 1, block)
                    call encode_block(w, block, qt(:, 1), dc(1), dcl, acl)
                end do
                if (colour) then
                    call fetch(plane(:, :, 2), 8 * bx * h, 8 * by * h, h, block)
                    call encode_block(w, block, qt(:, 2), dc(2), dcc, acc)
                    call fetch(plane(:, :, 3), 8 * bx * h, 8 * by * h, h, block)
                    call encode_block(w, block, qt(:, 2), dc(3), dcc, acc)
                end if
            end do
        end do
        call flush_bits(w)
        call put(w, [255, 217])
        bytes = w%buf(1:w%n)
    end subroutine jpeg_encode

    subroutine scaled_table(base, q, out)
        integer, intent(in) :: base(0:63), q
        integer, intent(out) :: out(0:63)
        integer :: s
        if (q < 50) then
            s = 5000 / q
        else
            s = 200 - 2 * q
        end if
        out = max(1, min(255, (base * s + 50) / 100))
    end subroutine scaled_table

    subroutine build(bits, values, t)
        !! Codes of a Huffman table from its code-length counts (Annex C).
        integer, intent(in) :: bits(16), values(:)
        type(huff_t), intent(out) :: t
        integer :: code, k, len, n
        code = 0
        k = 0
        do len = 1, 16
            do n = 1, bits(len)
                k = k + 1
                t%code(values(k)) = code
                t%length(values(k)) = len
                code = code + 1
            end do
            code = 2 * code
        end do
    end subroutine build

    subroutine huffman_segment(w, class_id, bits, values)
        type(writer_t), intent(inout) :: w
        integer, intent(in) :: class_id, bits(16), values(:)
        call put(w, [255, 196, 0, 19 + size(values), class_id])
        call put(w, bits)
        call put(w, values)
    end subroutine huffman_segment

    subroutine planes(img, colour, plane)
        !! Level-shifted Y, Cb and Cr (JFIF) as real planes.
        type(image_t), intent(in) :: img
        logical, intent(in) :: colour
        real, allocatable, intent(out) :: plane(:, :, :)
        integer :: x, y
        real :: r, g, b
        allocate(plane(img%width, img%height, merge(3, 1, colour)))
        do y = 1, img%height
            do x = 1, img%width
                r = real(iand(int(img%pixels(1, x, y)), 255))
                if (colour) then
                    g = real(iand(int(img%pixels(2, x, y)), 255))
                    b = real(iand(int(img%pixels(3, x, y)), 255))
                    plane(x, y, 1) = 0.299 * r + 0.587 * g + 0.114 * b - 128.0
                    plane(x, y, 2) = -0.168736 * r - 0.331264 * g + 0.5 * b
                    plane(x, y, 3) = 0.5 * r - 0.418688 * g - 0.081312 * b
                else
                    plane(x, y, 1) = r - 128.0
                end if
            end do
        end do
    end subroutine planes

    subroutine fetch(p, x0, y0, step, block)
        !! 8x8 samples from (x0, y0) (0-based), each the mean of step x step
        !! pixels, edges repeated beyond the image.
        real, intent(in) :: p(:, :)
        integer, intent(in) :: x0, y0, step
        real, intent(out) :: block(0:7, 0:7)
        integer :: i, j, a, b
        real :: s
        do j = 0, 7
            do i = 0, 7
                s = 0.0
                do b = 0, step - 1
                    do a = 0, step - 1
                        s = s + p(min(size(p, 1), x0 + step * i + a + 1), &
                            min(size(p, 2), y0 + step * j + b + 1))
                    end do
                end do
                block(i, j) = s / real(step * step)
            end do
        end do
    end subroutine fetch

    subroutine encode_block(w, block, qt, dc, hdc, hac)
        type(writer_t), intent(inout) :: w
        real, intent(in) :: block(0:7, 0:7)
        integer, intent(in) :: qt(0:63)
        integer, intent(inout) :: dc
        type(huff_t), intent(in) :: hdc, hac
        real :: f(0:7, 0:7), cosine(0:7, 0:7), cu(0:7), tmp(0:7, 0:7)
        integer :: coef(0:63), u, v, x, k, run, s, diff
        real, parameter :: PI = 3.14159265358979
        do u = 0, 7
            cu(u) = merge(sqrt(0.125), 0.5, u == 0)
            do x = 0, 7
                cosine(x, u) = cos((2 * x + 1) * u * PI / 16.0)
            end do
        end do
        ! Separable forward DCT: columns of x, then rows of y.
        do v = 0, 7
            do u = 0, 7
                tmp(u, v) = cu(u) * sum(block(:, v) * cosine(:, u))
            end do
        end do
        do v = 0, 7
            do u = 0, 7
                f(u, v) = cu(v) * sum(tmp(u, :) * cosine(:, v))
            end do
        end do
        do k = 0, 63
            ! Natural index n = 8 v + u.
            coef(k) = nint(f(mod(ZIGZAG(k), 8), ZIGZAG(k) / 8) / qt(ZIGZAG(k)))
        end do
        diff = coef(0) - dc
        dc = coef(0)
        s = category(diff)
        call emit(w, hdc%code(s), hdc%length(s))
        if (s > 0) call emit(w, magnitude(diff, s), s)
        run = 0
        do k = 1, 63
            if (coef(k) == 0) then
                run = run + 1
                cycle
            end if
            do while (run > 15)
                call emit(w, hac%code(240), hac%length(240))
                run = run - 16
            end do
            s = category(coef(k))
            call emit(w, hac%code(16 * run + s), hac%length(16 * run + s))
            call emit(w, magnitude(coef(k), s), s)
            run = 0
        end do
        if (run > 0) call emit(w, hac%code(0), hac%length(0))
    end subroutine encode_block

    pure integer function category(v)
        integer, intent(in) :: v
        integer :: a
        a = abs(v)
        category = 0
        do while (a > 0)
            category = category + 1
            a = a / 2
        end do
    end function category

    pure integer function magnitude(v, s)
        !! Negative values are sent as v - 1 in s bits (ones' complement).
        integer, intent(in) :: v, s
        if (v >= 0) then
            magnitude = v
        else
            magnitude = v + 2**s - 1
        end if
    end function magnitude

    subroutine emit(w, code, length)
        !! Appends `length` bits, with a zero byte stuffed after each 0xFF.
        type(writer_t), intent(inout) :: w
        integer, intent(in) :: code, length
        integer :: byte
        if (length == 0) return
        w%acc = ior(ishft(w%acc, length), iand(code, 2**length - 1))
        w%bits = w%bits + length
        do while (w%bits >= 8)
            byte = iand(ishft(w%acc, 8 - w%bits), 255)
            w%bits = w%bits - 8
            w%acc = iand(w%acc, 2**w%bits - 1)
            call put(w, [byte])
            if (byte == 255) call put(w, [0])
        end do
    end subroutine emit

    subroutine flush_bits(w)
        type(writer_t), intent(inout) :: w
        if (w%bits > 0) call emit(w, 127, 8 - w%bits)
    end subroutine flush_bits

    subroutine put(w, values)
        type(writer_t), intent(inout) :: w
        integer, intent(in) :: values(:)
        integer(int8), allocatable :: grown(:)
        integer :: k, v
        if (w%n + size(values) > size(w%buf)) then
            allocate(grown(2 * size(w%buf) + size(values)))
            grown(1:w%n) = w%buf(1:w%n)
            call move_alloc(grown, w%buf)
        end if
        do k = 1, size(values)
            v = iand(values(k), 255)
            if (v > 127) v = v - 256
            w%n = w%n + 1
            w%buf(w%n) = int(v, int8)
        end do
    end subroutine put
end module fortimg_jpeg_encode
