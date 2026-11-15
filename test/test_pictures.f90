module test_pictures
    !! Deterministic synthetic pictures and random numbers for the tests.
    use iso_fortran_env, only: int8
    implicit none
    private
    public :: random_int, picture, u8, s8

contains

    integer function random_int(state, n)
        !! 0..n-1 from a linear congruential generator (Numerical Recipes).
        integer, intent(inout) :: state
        integer, intent(in) :: n
        state = int(modulo(1664525_8 * state + 1013904223_8, 2_8**31))
        random_int = int(modulo(int(ishft(state, -7), 8), int(n, 8)))
    end function random_int

    function picture(width, height, state) result(rgb)
        !! Gradients, a sine band, discs, lines and noise: smooth areas and
        !! hard edges, as photographs and plots have them.
        integer, intent(in) :: width, height
        integer, intent(inout) :: state
        integer(int8) :: rgb(3, width, height)
        integer :: x, y, k, cx, cy, r, c(3), x0, y0, x1, y1, t, n
        do y = 1, height
            do x = 1, width
                rgb(1, x, y) = s8(255 * (x - 1) / max(1, width - 1))
                rgb(2, x, y) = s8(nint(128 + 100 * sin((y - 1) / 5.0)))
                rgb(3, x, y) = s8(255 * (x + y - 2) / max(1, width + height - 2))
            end do
        end do
        do k = 1, max(1, width * height / 900)
            cx = random_int(state, width) + 1
            cy = random_int(state, height) + 1
            r = 2 + random_int(state, max(1, min(width, height) / 3))
            c = [(random_int(state, 256), n = 1, 3)]
            do y = max(1, cy - r), min(height, cy + r)
                do x = max(1, cx - r), min(width, cx + r)
                    if ((x - cx)**2 + (y - cy)**2 <= r * r) rgb(:, x, y) = s8(c)
                end do
            end do
            x0 = random_int(state, width) + 1
            y0 = random_int(state, height) + 1
            x1 = random_int(state, width) + 1
            y1 = random_int(state, height) + 1
            c = [(random_int(state, 256), n = 1, 3)]
            do t = 0, 100
                rgb(:, x0 + (x1 - x0) * t / 100, y0 + (y1 - y0) * t / 100) = s8(c)
            end do
        end do
        do k = 1, width * height / 20
            x = random_int(state, width) + 1
            y = random_int(state, height) + 1
            do n = 1, 3
                rgb(n, x, y) = s8(max(0, min(255, u8(rgb(n, x, y)) + random_int(state, 81) - 40)))
            end do
        end do
    end function picture

    elemental integer function u8(b)
        integer(int8), intent(in) :: b
        u8 = iand(int(b), 255)
    end function u8

    elemental integer(int8) function s8(v)
        integer, intent(in) :: v
        s8 = int(iand(v, 255) - 256 * (iand(v, 255) / 128), int8)
    end function s8

end module test_pictures
