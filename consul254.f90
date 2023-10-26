!-------------------------------------------------------------------------------
! Эмулятор электрической пишущей машинки/терминала Consul-254.
!
! (c) 2023 Станислав Масловский <stanislav.maslovski@gmail.com>
!
! Исходный текст на ЯП GNU Fortran 2008.
! Для реализации многопоточности использована библиотека OpenMP.
!-------------------------------------------------------------------------------

module Consul254

  implicit none

  type :: CharSeq
    character(:), allocatable :: seq
  end type

  interface CharSeq
    module procedure charseq_cons
  end interface

  interface assignment (=)
    module procedure c2cs
  end interface

  character, parameter :: TAB = achar(9), LF = achar(10), CR = achar(13), SO = achar(14), SI = achar(15), ESC = achar(27)

  character(*), parameter :: data_rd_fmt = "(3i1,1a1,4i1)"
  character(*), parameter :: data_wr_fmt = "(3i1,'.',4i1)"

  type(CharSeq) :: symbol(0:127) ! таблица печатных символов
  integer :: code(0:255) ! таблица перекодировки

contains

  subroutine Consul_254_printer(iunit, stdout)

    integer, intent(in) :: iunit, stdout
    integer :: stat, ba(0:6)
    character :: dummy
    character(128) :: errmsg

    write (stdout, iostat=stat, iomsg=errmsg) "Consul-254 TTY EMU. Copyright (c) 2023 Stanislav Maslovski", CR, LF, CR, LF
    flush (stdout)
    if (stat /= 0) goto 10

    stat = 0
l1: do while (stat == 0)
      read (iunit, data_rd_fmt, iostat=stat, iomsg=errmsg) ba(0:2), dummy, ba(3:6)
      if (stat /= 0) exit l1
      write (stdout, iostat=stat, iomsg=errmsg) symbol(ba2i(ba))%seq
      flush (stdout)
    enddo l1

10  write (0,*) "Printer: " // trim(errmsg)

  end subroutine

  subroutine Consul_254_keyboard(stdin, ounit)

    integer, intent(in) :: stdin, ounit
    integer :: i, c_cnt, stat, parser_state
    integer, parameter :: FIRST_CHAR=0, ESCAPE=1, LESS_EQ=2, NOT_EQ=3, GREAT_EQ=4, POW_10=5
    type(CharSeq) :: out_seq
    character(128) :: errmsg
    character(4) :: c

    ! Парсер многобайтных символов в кодировке UTF-8 и ESC-последовательностей

    stat = 0
    parser_state = FIRST_CHAR

l1: do while (stat == 0)
      read (stdin, iostat=stat, iomsg=errmsg) c(1:1)
      if (stat /= 0) exit l1
      i = ichar(c(1:1))
      select case (parser_state)
        case (FIRST_CHAR)
          c_cnt = 1
          if (i < 128) then      ! однобайтовый символ UTF-8
            if (c(1:1) == ESC) then
              parser_state = ESCAPE
              cycle l1
            elseif (code(i) == 128) then
              cycle l1
            end if
          elseif (i < 224) then ! двухбайтовый
            c_cnt = 2
          elseif (i < 240) then ! трехбайтовый
            c_cnt = 3
          else                  ! четырехбайтовый
            c_cnt = 4
          endif
          if (c_cnt > 1) read (stdin, iostat=stat, iomsg=errmsg) c(2:c_cnt)
          if (stat /= 0) exit l1
          out_seq = c(1:c_cnt)
        case (ESCAPE)
          select case(c(1:1))
            case ("*")
              out_seq = "×"
            case (" ")
              out_seq = "␣"
            case ("=")
              out_seq = "≡"
            case ("7")
              out_seq = "↴"
            case ("}")
              out_seq = "⊃"
            case ("v")
              out_seq = "∨"
            case (":")
              out_seq = "÷"
            case ("^")
              out_seq = "⋀"
            case ("#")
              out_seq = "◊"
            case ("<")
              parser_state = LESS_EQ; cycle l1
            case ("/")
              parser_state = NOT_EQ; cycle l1
            case (">")
              parser_state = GREAT_EQ; cycle l1
            case ("1")
              parser_state = POW_10; cycle l1
            case default
              parser_state = FIRST_CHAR; cycle l1
          end select
        case (LESS_EQ, NOT_EQ, GREAT_EQ)
          if (c(1:1) == "=") then
            select case (parser_state)
              case (LESS_EQ)
                out_seq = "≤"
              case (NOT_EQ)
                out_seq = "≠"
              case (GREAT_EQ)
                out_seq = "≥"
            end select
          elseif (c(1:1) == ESC) then
            parser_state = ESCAPE; cycle l1
          else
            parser_state = FIRST_CHAR; cycle l1
          endif
        case (POW_10)
          if (c(1:1) == "0") then
            out_seq = "⏨"
          elseif (c(1:1) == ESC) then
            parser_state = ESCAPE; cycle l1
          else
            parser_state = FIRST_CHAR; cycle l1
          endif
      end select
      write (ounit, data_wr_fmt, iostat=stat, iomsg=errmsg) i2ba(code(hash(out_seq%seq)))
      flush (ounit)
      parser_state = FIRST_CHAR
    enddo l1

    write (0,*) "Keyboard: " // trim(errmsg)

  end subroutine

  subroutine init_tables

    ! Равные по начертанию русские и латинские буквы

    character(2), parameter :: rus(*) = [ "А", "В", "Е", "К", "М", "Н", "О", "Р", "С", "Т", "У", "Х" ] ! русские буквы
    character(1), parameter :: lat(*) = [ "A", "B", "E", "K", "M", "H", "O", "P", "C", "T", "Y", "X" ] ! латинские буквы

    integer :: i, k

    symbol(:) = CharSeq()

    ! Таблица печатных символов в кодировке UTF-8

    symbol(0:12)    = [ ("0123456789+-/"(i:i), i=1,13) ]
    symbol(16:17)   = [ CR, LF ]
    symbol(18)      = ESC//"[1;31m"
    symbol(19)      = ESC//"[0m"
    symbol(20:23)   = [ SO, TAB, SI, " " ]
    symbol(24)      = ESC//"[9m"
    symbol(25)      = "*"
    symbol(32:63)   = [ ("АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЬЭЮЯ"(i:i+1), i=1,63,2) ] ! двухбайтные символы
    symbol(65:67)   = [ "]", ",", "." ]
    symbol(68)      = "⏨"
    symbol(69:71)   = [ "^", "(", ")" ]
    symbol(72)      = "×"
    symbol(73:74)   = [ ";", "[" ]
    symbol(75)      = "␣"
    symbol(76:77)   = [ "=", ">" ]
    symbol(88)      = ESC//"[4m"
    symbol(89)      = "|"
    symbol(96:105)  = [ ("_%WGD:VZIJ"(i:i), i=1,10) ]
    symbol(106)     = "≤"
    symbol(107)     = "L"
    symbol(108)     = "≡"
    symbol(109)     = "N"
    symbol(110)     = "↴"
    symbol(111)     = "⊃"
    symbol(112:113) = [ "R", "S" ]
    symbol(114)     = "∨"
    symbol(115:117) = [ "U", "F", "<" ]
    symbol(118)     = "≠"
    symbol(119)     = "÷"
    symbol(120:121) = [ "‘", "’" ]
    symbol(122)     = "!"
    symbol(123:125) = [ "⋀", "≥", "◊" ]
    symbol(126)     = "Q"

    ! Xeш-таблица для перекодировки в символьные коды Сетунь-70
    code(:) = 128

    ! блок печатных символов
    do i = 0, 127
      k = hash(symbol(i)%seq)
      code(k) = i
    enddo

    ! блок равнозначной латиницы
    do i = 1, size(lat)
      code(ichar(lat(i))) = code(hash(rus(i)))
    enddo
    code(97:122) = code(65:90)

    ! Кавычки
    code(ichar("`")) = code(hash("‘"))
    code(ichar("'")) = code(hash("’"))

  end subroutine

  function hash(s) result(h)
    character(*) s
    integer :: h, l, i, k
    h = 0
    l = len(s)
    if (l > 0) then
      if (s(1:1) /= ESC) then
        do i = 1, l
          k = ichar(s(i:i))
          if (i == 2) then
            h = ibclr(h, 0)
            if (h == 80) then
              k = modulo(k - 16, 32)
            else
              k = ishftc(k, 3, 6)
            endif
          endif
          h = h + ishft(h, -3)
          h = modulo(h + k, 128)
        enddo
      endif
    endif
    if (l /= 1) h = h + 128
  end function

  function ba2i(ba) result (n)
    integer, intent(in) :: ba(0:6)
    integer :: i, n
    n = 0
    do i = 0, 6
      if (ba(i) == 1) n = ibset(n,i)
    enddo
  end function

  function i2ba(n) result (ba)
    integer, intent(in) :: n
    integer :: i, ba(0:6)
    do i = 0, 6
      ba(i) = ibits(n,i,1)
    enddo
  end function

  elemental function charseq_cons(c) result (cs)
    character(*), intent(in), optional :: c
    type(CharSeq) :: cs
    if (present(c)) then
      cs%seq = c
    else
      cs%seq = ""
    endif
  end function

  elemental subroutine c2cs(cs, c)
    type(CharSeq), intent(out) :: cs
    character(*), intent(in) :: c
    cs = CharSeq(c)
  end subroutine

end module

program Consul_254

  use Consul254

  implicit none

  integer :: stat, iunit, ounit, stdin, stdout
  character(64) :: input_fifo, output_fifo

  ! Разбор командной строки

  if (command_argument_count() /= 2) stop "Usage: consul254 <input_fifo> <output_fifo>"
  call get_command_argument(1, input_fifo)
  call get_command_argument(2, output_fifo)

  ! Бесформатный потоковый режим ввода/вывода на stdin/stdout (поддерживается на линукс и андроид)

  open (newunit=stdin, file="/dev/stdin", status="old", access="stream", action="read", iostat=stat)
  if (stat /= 0) stop "Error opening /dev/stdin"

  open (newunit=stdout, file="/dev/stdout", status="old", access="stream", action="write", iostat=stat)
  if (stat /= 0) stop "Error opening /dev/stdout"

  call init_tables

  !$omp parallel sections num_threads(2)

  open (newunit=ounit, file=output_fifo, status="old", action="write", iostat=stat)
  if (stat /= 0) stop "Error opening output fifo"

  call Consul_254_keyboard(stdin, ounit)

  !$omp section

  open (newunit=iunit, file=input_fifo, status="old", action="read", iostat=stat)
  if (stat /= 0) stop "Error opening input fifo"

  call Consul_254_printer(iunit, stdout)

  !$omp end parallel sections

end program
