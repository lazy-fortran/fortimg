module fortimg_image
    !! An 8-bit image with 1 (grey), 2 (grey and alpha), 3 (RGB) or 4 (RGBA)
    !! channels, row 1 at the top: pixels(channel, x, y).
    use iso_fortran_env, only: int8
    implicit none
    private
    public :: image_t, image_convert, image_orient

    integer, parameter, public :: IMG_OK = 0, IMG_ERR_FORMAT = 1, IMG_ERR_CORRUPT = 2, &
        IMG_ERR_UNSUPPORTED = 3

    type :: image_t
        integer :: width = 0, height = 0, channels = 0
        integer(int8), allocatable :: pixels(:, :, :)
    end type image_t

contains

    subroutine image_convert(img, channels)
        !! The same picture with `channels` channels: grey is (R+G+B)/3 rounded
        !! (ITU-R BT.601 luma is a caller's choice), missing alpha is opaque.
        type(image_t), intent(inout) :: img
        integer, intent(in) :: channels
        integer(int8), allocatable :: out(:, :, :)
        integer :: x, y, r, g, b, a
        if (channels == img%channels) return
        allocate (out(channels, img%width, img%height))
        do y = 1, img%height
            do x = 1, img%width
                select case (img%channels)
                case (1, 2)
                    r = u8(img%pixels(1, x, y))
                    g = r
                    b = r
                case default
                    r = u8(img%pixels(1, x, y))
                    g = u8(img%pixels(2, x, y))
                    b = u8(img%pixels(3, x, y))
                end select
                a = 255
                if (img%channels == 2) a = u8(img%pixels(2, x, y))
                if (img%channels == 4) a = u8(img%pixels(4, x, y))
                select case (channels)
                case (1)
                    out(1, x, y) = s8((r + g + b + 1) / 3)
                case (2)
                    out(:, x, y) = [s8((r + g + b + 1) / 3), s8(a)]
                case (3)
                    out(:, x, y) = [s8(r), s8(g), s8(b)]
                case default
                    out(:, x, y) = [s8(r), s8(g), s8(b), s8(a)]
                end select
            end do
        end do
        call move_alloc(out, img%pixels)
        img%channels = channels
    end subroutine image_convert

    subroutine image_orient(img, orientation)
        !! Turns an image stored with EXIF orientation 1..8 upright.
        type(image_t), intent(inout) :: img
        integer, intent(in) :: orientation
        integer(int8), allocatable :: out(:, :, :)
        integer :: x, y, w, h
        if (orientation < 2 .or. orientation > 8) return
        w = img%width
        h = img%height
        if (orientation >= 5) then
            allocate (out(img%channels, h, w))
        else
            allocate (out(img%channels, w, h))
        end if
        do y = 1, h
            do x = 1, w
                select case (orientation)
                case (2)
                    out(:, w + 1 - x, y) = img%pixels(:, x, y)
                case (3)
                    out(:, w + 1 - x, h + 1 - y) = img%pixels(:, x, y)
                case (4)
                    out(:, x, h + 1 - y) = img%pixels(:, x, y)
                case (5)
                    out(:, y, x) = img%pixels(:, x, y)
                case (6)
                    out(:, h + 1 - y, x) = img%pixels(:, x, y)
                case (7)
                    out(:, h + 1 - y, w + 1 - x) = img%pixels(:, x, y)
                case (8)
                    out(:, y, w + 1 - x) = img%pixels(:, x, y)
                end select
            end do
        end do
        call move_alloc(out, img%pixels)
        if (orientation >= 5) then
            img%width = h
            img%height = w
        end if
    end subroutine image_orient

    pure integer function u8(b)
        integer(int8), intent(in) :: b
        u8 = iand(int(b), 255)
    end function u8

    pure integer(int8) function s8(v)
        integer, intent(in) :: v
        s8 = int(v - 256 * (v / 128), int8)
    end function s8

end module fortimg_image
