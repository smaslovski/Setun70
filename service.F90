!-------------------------------------------------------------------------------
! Компилируемое алгоритмическое описание троичной машины Сетунь-70.
!
! (c) 2023 Станислав Масловский <stanislav.maslovski@gmail.com>
!
! Сервисные процедуры.
! Исходный текст на ЯП GNU Fortran 2008 с использованием директив препроцессора.
!-------------------------------------------------------------------------------

#include "service.h"

subroutine INIT_EMU

  use Setun70
  use IO_module

  implicit none

  ! Процедура INIT_EMU инициализирует структуры данных
  ! для эмуляции внешней аппаратуры ЭВМ Сетунь-70.

  integer, parameter :: max_arg_num = 25, max_arg_len = 128
  character(len=max_arg_len) :: cmd_arg, filename, msg
  character(len=:), allocatable :: rom_file
  integer :: i, j, gnum, unum, argc, stat, dfu
  logical :: rd_flg, wr_flg

  ! Подключаем устройства по-умолчанию

  ! Регистр пульта
  io_unit(-1,-1) = IO_Dev(.false.,IO_Port(.true.,"register"),IO_Port())

  ! Фото-ввод
  io_unit(0,-1) = IO_Dev(.false.,IO_Port(.true.,"tape_reader"),IO_Port())

  ! Перфоратор
  io_unit(0,1) = IO_Dev(.false.,IO_Port(),IO_Port(.true.,"tape_punch"))

  ! Консул
  io_unit(1,3) = IO_Dev(.false.,IO_Port(.true.,"consul_tty_i"),IO_Port(.true.,"consul_tty_o"))

  ! Разбираем аргументы командной строки

  argc = command_argument_count()

  ! Первый аргумент - имя файла образа ПЗУ

  if (argc > 0) then
    call get_command_argument(1, cmd_arg)
    rom_file = trim(cmd_arg)
  else
    rom_file = "ROM.csv"
  endif

  ! Подключаем устройства из командной строки

  if (argc > 1) then
    do i = 2, min(argc, max_arg_num)
      call get_command_argument(i, cmd_arg)
      read (cmd_arg, *, err = 10) gnum, unum, rd_flg, wr_flg, filename
      if (gnum >= -1 .and. gnum <= 1 .and. unum >= -4 .and. unum <= 4) then
        associate (io => io_unit(gnum, unum))
          if (rd_flg) then
            io%rd%flg = .true.
            io%rd%name = trim(filename)
          endif
          if (wr_flg) then
            io%wr%flg = .true.
            io%wr%name = trim(filename)
          endif
        end associate
      endif
10  enddo
  endif

  ! УВВ с номером 0 в каждой из 3х групп не используется (всегда отключено)

  do i = -1, 1
    io_unit(i,0) = IO_Dev(.false.,IO_Port(.false.,""),IO_Port(.false.,""))
  enddo

  ! Открываем файлы устройств

  do i = -1, 1
    do j = -4, 4
      associate (io => io_unit(i, j))
        if (io%rd%flg) then
          open(newunit = io%rd%unum, file = io%rd%name, action = "read", iostat = stat, iomsg = msg)
          if (stat /= 0) then
            io%rd%flg = .false.
            write (0,100) trim(msg)
            read (*,*)
          endif
        endif
        if (io%wr%flg) then
          open(newunit = io%wr%unum, file = io%wr%name, action = "write", iostat = stat, iomsg = msg)
          if (stat /= 0) then
            io%wr%flg = .false.
            write (0,100) trim(msg)
            read (*,*)
          endif
        endif
      end associate
    enddo
  enddo

  ! Инициализируем содержимое ПЗУ из образа в файле

  open(newunit = dfu, file = rom_file, action = "read", iostat = stat, iomsg = msg)
  if (stat /= 0) then
    write (0,100) trim(msg)
    read (*,*)
  endif

  call load_dump

  ! Чистим экран терминала, прячем курсор

  if (ansi_terminal) write (*,*) ESC, "[2J", ESC, "[H", ESC, "[?25l"

100 format(a,". Hit RETURN to continue...")

contains

  subroutine load_dump

    integer, dimension(:), allocatable :: page
    character(len=3), dimension(:), allocatable :: kod
    integer :: pg_cnt, row_cnt, i, j, offs

    ! Сначала читаем число страниц
    read (dfu,*) pg_cnt; rewind (dfu)

    ! Распределяем память под массивы
    allocate(page(1:pg_cnt), kod(0:pg_cnt))

    ! Читаем информационную строку полностью
    read (dfu,*) pg_cnt, kod(1:pg_cnt), row_cnt
    page(1:pg_cnt) = from_nonary(kod(1:pg_cnt))

    ! Читаем данные
    do j = 1, row_cnt
      kod = "000"
      read (dfu,*,end=10) kod(0:pg_cnt)
      offs = from_nonary(kod(0))
      do i = 1, pg_cnt
        m(page(i), offs, 1:6) = from_nonary(kod(i))
      enddo
    enddo

10  return

  end subroutine

end subroutine INIT_EMU

subroutine DUMP

  use Setun70
  use IO_module
  use DisAsm

  implicit none

  integer :: i
  type(Trit) :: adr(1:4)  ! Временная переменная для адресации в пределах одной страницы
  type(Trit), dimension(1:6) :: next_k, a1, a2, a3

  next_k = m(+ch, +ca, 1:6)
  adr = ca+1; a1 = m(+ch, +adr, 1:6)
  adr = ca+2; a2 = m(+ch, +adr, 1:6)
  adr = ca+3; a3 = m(+ch, +adr, 1:6)

  if (ansi_terminal) write (*,*) ESC, "[J", ESC, "[H"
  write (*,10) cycle_cnt

  if (register_dump) then
                 write (*,40) "h = ", character(h(-1:1,1:3)), character(hf)
                 write (*,70) "k'= ", character(c(1:8)), character(next_k), disassemble(next_k, a1, a2, a3)
                 write (*,80) "p = ", character(p), +pa
                 write (*,90) "t = ", character(tt), +tt
                 write (*,90) "e = ", character(e), +e
                 write (*,95) "T = ", character(T), +T
                 write (*,95) "S = ", character(S), +S
    if (verbose) write (*,95) "R = ", character(R), +R
    if (verbose) write (*,95) "Y = ", character(Y), +Y
    if (verbose) write (*,60) "v = ", character(v), character(w), +w
    if (verbose) write (*,20) "u = ", (character(u(i,1:4)),i=-1,1)
    if (verbose) write (*,30) "g = ", (character(g(i,1:7)),i=-1,1)
  endif

  if (single_step) start = 0

  cycle_cnt = cycle_cnt + 1
  return

10 format(51("-"),/,"cycle_cnt:",i0.8,/,51("-"))
20 format(a7,"u1:",4a,4x,"u2:",4a,4x,"u3:",4a,/,2x,49("-"))
30 format(a7,"g1:",7a,1x,"g2:",7a,1x,"g3:",7a,/,2x,49("-"))
40 format(a7,"h1:",3a,5x,"h2:",3a,5x,"h3:",3a,5x,"hf:",3a,/,2x,49("-"))
60 format(a7," v:",9a,3x,"w:",4a,6x,sp,"(",i15,")"/,2x,49("-"))
70 format(a7,"ca:",1a,":",3a,":",4a," => ",6a,4x,"(",a,")",/,2x,49("-"))
80 format(a7,"pa:",2a,":",3a,2x,"pb:",2a,":",3a,7x,sp,"(",i15,")"/,2x,49("-"))
90 format(a7,3x,6a,18x,sp,"(",i15,")",/,2x,49("-"))
95 format(a7,3x,3(6a,1x),3x,sp,"(",i15,")",/,2x,49("-"))

end subroutine

subroutine signal_handler(sig)

  use Setun70
  use IO_module

  implicit none

  integer, value, intent(in) :: sig

  select case (sig)
    case (SIGTSTP)
      start = 0
    case (SIGINT)
      v(8) = 1
  end select

end subroutine

subroutine us_sleep(time_us)

  use iso_c_binding
  implicit none

  interface
    subroutine usleep(useconds) bind(C)
      use iso_c_binding
      integer(c_int32_t), value :: useconds
    end subroutine
  end interface

  integer, intent(in) :: time_us

  call usleep(time_us)

end subroutine
