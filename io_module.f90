!-------------------------------------------------------------------------------
! Компилируемое алгоритмическое описание троичной машины Сетунь-70.
!
! (c) 2023 Станислав Масловский <stanislav.maslovski@gmail.com>
!
! Модуль поддержки ввода-вывода.
! Исходный текст на ЯП GNU Fortran 2008.
!-------------------------------------------------------------------------------

module IO_module

  implicit none

  type :: IO_Port
    logical :: flg = .false.          ! флаг доступности
    character(:), allocatable :: name ! имя ассоциированного файла
    integer :: unum = 0               ! ссылочный номер файла
  end type

  type :: IO_Dev
    logical :: on = .false.           ! флаг включения/выключения устройства
    type(IO_Port) :: rd, wr           ! Порты чтения/записи устройства
  end type

  type(IO_Dev) :: io_unit(-1:1, -4:4) ! индексы => (номер_группы, номер_устройства)

  logical :: ansi_terminal   = .true.
  logical :: register_dump   = .true.
  logical :: verbose         = .true.
  logical :: timer_interrupt = .true.
  logical :: single_step     = .true.

  integer :: cycle_cnt = 0
  integer, parameter :: SIGINT = 2, SIGTSTP = 20, SIG_ERR = -1
  character, parameter :: ESC = achar(27)

contains

  elemental function to_nonary(num) result(res)

    integer, intent(in) :: num
    character(len=3) :: res
    integer :: i, s, d, r
    character, parameter :: digit(-4:4) = [ "W", "X", "Y", "Z", "0", "1", "2", "3", "4" ]

    s = 1; d = num
    if (d < 0) then
      s = -s; d = -d
    endif
    do i = 3, 1, -1
      r = mod(d, 9)
      if (r > 4) r = r - 9
      d = (d - r) / 9
      res(i:i) = digit(s*r)
    enddo

  end function

  elemental function from_nonary(kod) result(res)

    character(len=*), intent(in) :: kod
    integer :: res, i, c

    res = 0
    do i = 1, len(kod)
      res = 9*res
      c = iachar(kod(i:i))
      select case (c)
        case (iachar("w"):iachar("z"))
          res = res + (c - iachar("z") - 1)
        case (iachar("W"):iachar("Z"))
          res = res + (c - iachar("Z") - 1)
        case (iachar("0"):iachar("4"))
          res = res + (c - iachar("0"))
      end select
    enddo

  end function

end module IO_module
