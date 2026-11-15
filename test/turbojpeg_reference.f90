module turbojpeg_reference
    !! libjpeg-turbo's TurboJPEG 3 API, opened at run time, as the reference
    !! encoder and decoder of the JPEG tests. The library is not linked.
    use iso_c_binding
    use iso_fortran_env, only: int8
    implicit none
    private
    public :: turbojpeg_open, tj_encode, tj_decode

    ! Values of turbojpeg.h (libjpeg-turbo 3).
    integer(c_int), parameter, public :: TJSAMP_444 = 0, TJSAMP_422 = 1, TJSAMP_420 = 2, &
        TJSAMP_GRAY = 3, TJSAMP_440 = 4, TJSAMP_411 = 5
    integer(c_int), parameter, public :: TJPF_RGB = 0, TJPF_GRAY = 6, TJPF_CMYK = 11
    integer(c_int), parameter, public :: TJCS_CMYK = 3, TJCS_YCCK = 4
    integer(c_int), parameter :: TJINIT_COMPRESS = 0, TJINIT_DECOMPRESS = 1
    integer(c_int), parameter :: TJPARAM_QUALITY = 3, TJPARAM_SUBSAMP = 4, &
        TJPARAM_JPEGWIDTH = 5, TJPARAM_JPEGHEIGHT = 6, TJPARAM_COLORSPACE = 8, &
        TJPARAM_PROGRESSIVE = 12, TJPARAM_RESTARTBLOCKS = 18, TJPARAM_RESTARTROWS = 19

    abstract interface
        type(c_ptr) function fn_init(kind) bind(C)
            import :: c_ptr, c_int
            integer(c_int), value :: kind
        end function
        subroutine fn_handle(h) bind(C)
            import :: c_ptr
            type(c_ptr), value :: h
        end subroutine
        integer(c_int) function fn_set(h, param, value) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: h
            integer(c_int), value :: param, value
        end function
        integer(c_int) function fn_get(h, param) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: h
            integer(c_int), value :: param
        end function
        integer(c_int) function fn_compress(h, src, width, pitch, height, pf, jpeg, &
                jpeg_size) bind(C)
            import :: c_ptr, c_int, c_size_t
            type(c_ptr), value :: h, src
            integer(c_int), value :: width, pitch, height, pf
            type(c_ptr), intent(inout) :: jpeg
            integer(c_size_t), intent(inout) :: jpeg_size
        end function
        integer(c_int) function fn_header(h, jpeg, jpeg_size) bind(C)
            import :: c_ptr, c_int, c_size_t
            type(c_ptr), value :: h, jpeg
            integer(c_size_t), value :: jpeg_size
        end function
        integer(c_int) function fn_decompress(h, jpeg, jpeg_size, dst, pitch, pf) bind(C)
            import :: c_ptr, c_int, c_size_t
            type(c_ptr), value :: h, jpeg, dst
            integer(c_size_t), value :: jpeg_size
            integer(c_int), value :: pitch, pf
        end function
    end interface

    interface
        type(c_ptr) function dlopen(name, flags) bind(C)
            import :: c_ptr, c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), value :: flags
        end function
        type(c_funptr) function dlsym(handle, name) bind(C)
            import :: c_ptr, c_funptr, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: name(*)
        end function
    end interface

    procedure(fn_init), pointer :: tj3Init => null()
    procedure(fn_handle), pointer :: tj3Destroy => null(), tj3Free => null()
    procedure(fn_set), pointer :: tj3Set => null()
    procedure(fn_get), pointer :: tj3Get => null()
    procedure(fn_compress), pointer :: tj3Compress8 => null()
    procedure(fn_header), pointer :: tj3DecompressHeader => null()
    procedure(fn_decompress), pointer :: tj3Decompress8 => null()

contains

    logical function turbojpeg_open()
        type(c_ptr) :: lib
        lib = dlopen("libturbojpeg.so.0" // c_null_char, 2_c_int)
        if (.not. c_associated(lib)) lib = dlopen("libturbojpeg.dylib" // c_null_char, 2_c_int)
        turbojpeg_open = c_associated(lib)
        if (.not. turbojpeg_open) return
        call c_f_procpointer(dlsym(lib, "tj3Init" // c_null_char), tj3Init)
        call c_f_procpointer(dlsym(lib, "tj3Destroy" // c_null_char), tj3Destroy)
        call c_f_procpointer(dlsym(lib, "tj3Free" // c_null_char), tj3Free)
        call c_f_procpointer(dlsym(lib, "tj3Set" // c_null_char), tj3Set)
        call c_f_procpointer(dlsym(lib, "tj3Get" // c_null_char), tj3Get)
        call c_f_procpointer(dlsym(lib, "tj3Compress8" // c_null_char), tj3Compress8)
        call c_f_procpointer(dlsym(lib, "tj3DecompressHeader" // c_null_char), &
            tj3DecompressHeader)
        call c_f_procpointer(dlsym(lib, "tj3Decompress8" // c_null_char), tj3Decompress8)
    end function turbojpeg_open

    function tj_encode(pixels, width, height, pf, subsamp, quality, progressive, &
            restart_blocks, restart_rows, colorspace) result(jpeg)
        !! pixels (channels, width, height) as a JPEG file.
        integer(int8), intent(in), target :: pixels(:, :, :)
        integer, intent(in) :: width, height, pf, subsamp, quality
        logical, intent(in) :: progressive
        integer, intent(in) :: restart_blocks, restart_rows, colorspace
        integer(int8), allocatable :: jpeg(:)
        integer(int8), pointer :: view(:)
        type(c_ptr) :: h, buf
        integer(c_size_t) :: n
        h = tj3Init(TJINIT_COMPRESS)
        call set(TJPARAM_QUALITY, quality)
        call set(TJPARAM_SUBSAMP, subsamp)
        call set(TJPARAM_PROGRESSIVE, merge(1, 0, progressive))
        if (restart_blocks > 0) call set(TJPARAM_RESTARTBLOCKS, restart_blocks)
        if (restart_rows > 0) call set(TJPARAM_RESTARTROWS, restart_rows)
        if (colorspace >= 0) call set(TJPARAM_COLORSPACE, colorspace)
        buf = c_null_ptr
        n = 0
        if (tj3Compress8(h, c_loc(pixels), int(width, c_int), 0_c_int, int(height, c_int), &
            int(pf, c_int), buf, n) /= 0) error stop "tj3Compress8 failed"
        call c_f_pointer(buf, view, [n])
        jpeg = view
        call tj3Free(buf)
        call tj3Destroy(h)
    contains
        subroutine set(param, value)
            integer(c_int), intent(in) :: param
            integer, intent(in) :: value
            if (tj3Set(h, param, int(value, c_int)) /= 0) error stop "tj3Set failed"
        end subroutine set
    end function tj_encode

    function tj_decode(jpeg, pf, channels) result(pixels)
        !! A JPEG file decoded by libjpeg-turbo into (channels, width, height).
        integer(int8), intent(in), target :: jpeg(:)
        integer, intent(in) :: pf, channels
        integer(int8), allocatable, target :: pixels(:, :, :)
        type(c_ptr) :: h
        integer :: w, ht
        h = tj3Init(TJINIT_DECOMPRESS)
        if (tj3DecompressHeader(h, c_loc(jpeg), size(jpeg, kind=c_size_t)) /= 0) &
            error stop "tj3DecompressHeader failed"
        w = tj3Get(h, TJPARAM_JPEGWIDTH)
        ht = tj3Get(h, TJPARAM_JPEGHEIGHT)
        allocate (pixels(channels, w, ht))
        if (tj3Decompress8(h, c_loc(jpeg), size(jpeg, kind=c_size_t), c_loc(pixels), &
            0_c_int, int(pf, c_int)) /= 0) error stop "tj3Decompress8 failed"
        call tj3Destroy(h)
    end function tj_decode

end module turbojpeg_reference
