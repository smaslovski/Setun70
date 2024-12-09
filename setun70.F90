!-------------------------------------------------------------------------------
! Компилируемое алгоритмическое описание троичной машины Сетунь-70.
!
! (c) 2023 Станислав Масловский <stanislav.maslovski@gmail.com>
!
! Основные процедуры функционирования машины.
! Исходный текст на ЯП GNU Fortran 2008 с применением директив Си-препроцессора.
! Для реализации многопоточности использована библиотека OpenMP.
!-------------------------------------------------------------------------------

#include "setun70.h"

module Setun70

  use ternary_arith
  use IO_module

  implicit none

  ! В нижеследующем тексте программы порядок индексов многомерных массивов сохранен
  ! таким же, как и в оригинальном описании ЭВМ Сетунь-70 на Алгол-подобном языке.
  ! Однако, поскольку в Fortran принят другой порядок хранения многомерных массивов
  ! в памяти, для оптимизации операций с массивами, перед компиляцией порядок индексов
  ! автоматически заменяется на обратный с помощью макроподстановок. Сокращённые
  ! обозначения для обращения к элементам массивов (вроде T, S, ph, pa и т.д.) также
  ! определены через макроподстановки (см. файл "setun70_vars.h").

  type(Trit) :: f(-1:1, -3280:3280, -40:40, 1:6),&    ! внешнее ЗУ
                q(-1:1, 1:8),&                        ! регистр указателей страниц внешнего ЗУ
                m(-13:13, -40:40, 1:6),&              ! основное ЗУ как массив 6-тритных слогов
                mw(-13:13, -13:13, 1:18),&            ! основное ЗУ как массив 3-сложных слов
                h(-1:1, 1:3), hf(1:3),&               ! регистры выбора страниц ЗУ
                c(1:8),&                              ! регистр режима и указателя команд
                p(1:10),&                             ! регистр страницы и указателя стека
                k(1:6),&                              ! регистр кода команды
                e(1:6), R(1:18), Y(1:18),&            ! арифметические регистры
                g(-1:1, 1:7), u(-1:1, 1:4), a(-1:1),& ! регистры каналов УВВ
                v(1:9), w(1:4),&                      ! регистры управления прерываниями
                sr(-13:13, 1:12), psr(1:3),&          ! стек возвратов и указатель стека
                start                                 ! ключ старта

  declare_mem_equivalence   ! Размещает массивы m и mw по одному адресу (см. "setun70_vars.h")

contains

subroutine SWON(j)

  ! Процедура SWON(j) включает устройство
  ! ввода-вывода группы j c номером u(j, 3:4) и
  ! выключает все другие устройства ввода-вывода
  ! этой группы;

  integer, intent(in) :: j
  integer :: i

  do i = -4, 4
    if (i /= 0 .and. u(j, 3:4) == i) then
      io_unit(j,i)%on = .true.
    else
      io_unit(j,i)%on = .false.
    endif
  enddo

end subroutine SWON

subroutine SWOFF(j)

  ! Процедура SWOFF(j) выключает все
  ! устройства ввода-вывода группы j;

  integer, intent(in) :: j
  integer :: i

  do i = -4, 4
    io_unit(j,i)%on = .false.
  enddo

end subroutine SWOFF

subroutine PASS(j)

  ! Процедура PASS(j) передаёт значение
  ! g(j,:) устройству ввода-вывода c номером u(j, 3:4);

  integer, intent(in) :: j
  integer :: k

  associate (io => io_unit(j,+u(j,3:4)))
    if (io%on .and. io%wr%flg) then
      write (io%wr%unum,10) (g(j,k),k=7,1,-1) ! выводим, начиная с младшего разряда
    endif
  end associate

10 format(3i1,':',4i1)

end subroutine PASS

subroutine TAKE(j)

  ! Процедура TAKE(j) присваивает массиву
  ! g(j,:) значение, поступившее от устройства ввода-
  ! вывода c номером u(j,3:4);

  integer, intent(in) :: j
  character :: dummy
  integer :: k

  associate (io => io_unit(j,+u(j,3:4)))
    if (io%on .and. io%rd%flg) then
      read (io%rd%unum,10) (g(j,k),k=7,5,-1), dummy, (g(j,k),k=4,1,-1) ! вводим, начиная с младшего разряда
    endif
  end associate

10 format(3i1,a1,4i1)

end subroutine TAKE

subroutine INOUT(i)

  ! Процедура INOUT, будучи активизированной
  ! в результате обращения к ней, определяет процесс,
  ! выполняемый параллельно c процедурой SETUN_70,
  ! в которой сразу же начинают выполняться следующие
  ! за этим обращением операторы. Повторное обращение
  ! к процедуре INOUT определяет новый процесс, не
  ! прекращая выполнения старого, определяемого той
  ! же процедурой INOUT. В процедуре SETUN_70
  ! производится трижды обращение к этой процедуре
  ! INOUT c разными значениями параметра i = -1, 0, +1;

  integer, intent(in) :: i

  do while (.true.)
    call us_sleep(3333)  ! ограничение скорости обмена ~300 Бод
    if (a(i) == 1) then
      if (u(i,3:4) == 0) then
        call SWOFF(i)
      else
        call SWON(i)
        if (u(i,1) == 1) then ! вывод
          call PASS(i)
        elseif (u(i,1) == -1) then ! ввод
          call TAKE(i)
          if (g(i,1:7) == 0) cycle
        endif
        v(i+3) = 1
      endif
      a(i) = 0
    endif
  enddo

end subroutine INOUT

subroutine WATCH

  ! Процедура WATCH формирует заявки на прерывание v(1) = 1
  ! через определённые промежутки времени. Как и процедура INOUT,
  ! работает параллельно c процедурой SETUN_70;

  integer, parameter :: period = 100 ! Желаемый период таймера в мсек

  do while (.true.)
    call us_sleep(1000*period)
    if(.not. single_step .and. timer_interrupt) v(1) = 1
  enddo

end subroutine WATCH

subroutine SUIT(i)

  ! Процедура SUIT, будучи активизированной в результате
  ! обращения к ней, определяет процесс, выполняемый параллельно
  ! c процедурой SETUN_70. Сразу же после активизации процедуры
  ! SUIT выполняется оператор, следующий за обращением к этой
  ! процедуре. Процедура SUIT(i) осуществляет поиск страницы
  ! c номером q(i,:) в памяти второго уровня вида f(i,:,:,:) и
  ! завершает свою работу заявкой на прерывание v(i+6) = 1, указывающей
  ! на то, что поиск закончен и страницу можно использовать.
  ! Повторное обращение к процедуре SUIT определяет новый процесс,
  ! прекращая старый, определяемый этой процедурой c тем же
  ! значением параметра i, но не прекращая другие, ранее
  ! активизированные процессы, определяемые этой процедурой c
  ! другими значениями параметров;

  integer, intent(in) :: i

end subroutine

subroutine STOP

  ! Процедура STOP осуществляет выполнение оператора
  ! start = 0 в некоторый заранее не известный момент времени.
  ! Как и процедура INOUT, работает параллельно c процедурой SETUN_70;

  external :: signal_handler

  ! Настраиваем обработчик сигнала SIGTSTP
  if (signal(SIGTSTP, signal_handler) == SIG_ERR) then
    write (0,*) "Error while installing SIGTSTP signal handler."
  endif

end subroutine

subroutine ATTENT

  ! Процедура ATTENT формирует заявки на прерывание v(8) = 1
  ! при нажатии специальной кнопки на пульте управления машины.
  ! Как и процедура INOUT, работает параллельно c процедурой SETUN_70;

  external :: signal_handler

  ! Настраиваем обработчик сигнала SIGINT
  if (signal(SIGINT, signal_handler) == SIG_ERR) then
    write (0,*) "Error while installing SIGINT signal handler."
  endif

end subroutine

end module Setun70

!-------------------------------------------------------------------------------

subroutine SETUN_70

  use ternary_arith
  use Setun70

  implicit none

  type(Trit) :: x(1:36), z(1:18)
  integer :: i

! Логика работы с ключом start изменена для поддержки
! режима пошагового исполнения программы.
!
! Старт параллельно исполняемых процедур вынесен из
! процедуры SETUN_70 в тело основной программы.

START&
    if (start == 0) then  ! Холодный старт машины
      start = 1
      c1 = 1; ch = 12; h(0,1:3) = ch; ca = -40
      ph = 0; pa = 0; Y = 0
    endif

CYCLE&
    if (start == 0) goto FINISH

    call DUMP

    if (c1 /= 1) then
      do i = 1, 9
        if (v(i) /= 0) then
          w = i-41; v(i) = 0
          goto IR
        endif
      enddo
    endif

    k = m(+ch, +ca, 1:6)

    if (k1 == 0 .and. k2 == 0) then
      goto (MACRO, BASIC, SPEC), k3+2
    else
      if (c1 /= 1 .and. pa == 13) then
        w = -30
        goto IR
      endif
      pa = pa+1
      call REFSYL(*IR)
    endif
    goto CYCLE
! end CYCLE

MACRO&
    if (c1 == 1) goto FINISH
    if (c1 == 0) then
      w = -28
      goto IR
    endif
    if (pa == 13) then
      w = -30
      goto IR
    endif
    pa = pa+1
    z = 0; z(3:6) = c(1:4); z(9:12) = c(5:8)
    psr = psr+1; TSR = z(1:12)
    c1 = 0; ch = -13; h(0,1:3) = ch; ca = -13
    T = 0; tt = m(+ch, +ka, 1:6)
    goto CYCLE
! end MACRO

BASIC&
    if (c1 /= 1 .and. pa == -13) then
      w = -31
      goto IR
    endif
    goto (B1,  B2,  B3,  B4,  B5,  B6,  B7,  B8,  B9,&
          B10, B11, B12, B13, B14, B15, B16, B17, B18,&
          B19, B20, B21, B22, B23, B24, B25, B26, B27), ko+14

B1&
    x(1:18) = S; x(19:36) = Y
    x = eoshift(x, +tt, Trit(0))  ! Исходное выражение x = x*(3**tt) заменено, чтобы учесть случай tt < 0
    S = x(1:18); Y = x(19:36); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LST"

B2&
    x(1:18) = T
    if (x(1) /= 0) then
      ca = +tt
    else
      call INCRC
    endif
    pa = pa-1; goto CYCLE;   !--  Name: "COT"

B3&
    x(1:18) = T; x(19:36) = Y
    if (x(1) /= 0) then
      i = 1; x = x/3
    else
      do i = 0, -34, -1
        if (x(2) /= 0) then
          goto EX
        else
          x = x*3
        endif
      enddo
      i = 0
    endif
EX&
    e = e+i; T = x(1:18); Y = x(19:36); call INCRC
    goto CYCLE;   !--  Name: "XNN"

B4&
    if (T /= 0) then
      call SUBR(ca+1, +ca, *IR)
    else
      call INCRC; call INCRC
    endif
    goto CYCLE; !--  Name: "DOW"

B5&
    call INCRC
    if (T < 0) then
      call SUBR(+ca, ca+3, *IR)
    elseif (T == 0) then
      call SUBR(ca+1, ca+3, *IR)
    else
      call SUBR(ca+2, ca+3, *IR)
    endif
    pa = pa-1
    goto CYCLE; !--  Name: "BRT"

B6&
    z = T; pa = pa-1
    c(1:4) = z(3:6); h(0,1:3) = ch; c(5:8) = z(9:12);
    goto CYCLE; !--  Name: "JMP"

B7&
    T = T - e*(3**12); call INCRC
    goto CYCLE; !--  Name: "T-E"

B8&
    e = tt; pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "E=T"

B9&
    T = T + e*(3**12); call INCRC
    goto CYCLE; !--  Name: "T+E"

B10&
    if (S < 0) then
      ca = +tt; pa = pa-1
    else
      pa = pa-1; call INCRC
    endif
    goto CYCLE;  !--  Name: "CLT"

B11&
    if (S == 0) then
      ca = +tt; pa = pa-1
    else
      pa = pa-1; call INCRC
    endif
    goto CYCLE;  !--  Name: "CET"

B12&
    if (S > 0) then
      ca = +tt; pa = pa-1
    else
      pa = pa-1; call INCRC
    endif
    goto CYCLE;  !--  Name: "CGT"

B13&
    call INCRC; call SUBR(+ca, ca+1, *IR)
    goto CYCLE;  !--  Name: "JSR"

B14&
    R = T; pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "R=T"

B15&
    ca = +tt;  pa = pa-1;
    goto CYCLE;  !--  Name: "C=T"

B16&
    k = tt; call REFSYL(*IR)
    goto CYCLE;  !--  Name: "T=W"

B17&
    Y = 0; pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "YFT"

B18&
    k = tt
    if (c1 /= 1 .and. ka+k1+1 > 40) then
      w = -29; goto IR
    endif
    if (abs(+h(k2,1:3)) <= 4) then
      m(+h(k2, 1:3), ka:ka+k1+1, 1:6) = m(+ph, (pa-1)*3-1:(pa-1)*3+k1, 1:6)
    endif
    pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "W=S"

B19&
    x(1:18) = T; z = S
    do i = 1, 18
      z(i) = z(i)*x(i)
    enddo
    S = z; pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "SMT"

B20&
    Y = T; pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "Y=T"

B21&
    x(1:18) = T; z = S
    do i = 1, 18
      if (z(i) == 0) then
        z(i) = x(i)
      elseif (z(i) /= x(i) .and. x(i) /= 0) then
        z(i) = 0
      endif
    enddo
    S = z; pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "SAT"

B22&
    S = S - T; pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "S-T"

B23&
    T = -T; call INCRC
    goto CYCLE;  !--  Name: "TDN"

B24&
    S = S + T; pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "S+T"

B25&
    x(1:18) = S; x(19:36) = 0; x = x + T*R*(3**2)
    S = x(1:18); Y = x(19:36); pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "LBT"

B26&
    R = S; x = T*R*(3**2)
    S = x(1:18); Y = x(19:36)
    pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "L*T"

B27&
    x(1:18) = S; x(19:36) = Y; x = x + T*R*(3**2)
    S = x(1:18); Y = x(19:36); pa = pa-1; call INCRC
    goto CYCLE;  !--  Name: "LHT"
! end BASIC

SPEC&
    if (c1 == -1) then
      w = -28; goto IR
    endif
    if (c1 == 0 .and. pa == -13) then
      w = -31; goto IR
    endif
    goto (S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,&
          S10, S11, S12, S13, S14, S15, S16, S17, S18,&
          S19, S20, S21, S22, S23, S24, S25, S26, S27), ko+14

S1&
    call TRANSIN(-1); call INCRC
    goto CYCLE; !--  Name: "COPYG1"

S2&
    call TRANSIN(0); call INCRC
    goto CYCLE; !--  Name: "COPYG2"

S3&
    call TRANSIN(1); call INCRC
    goto CYCLE; !--  Name: "COPYG3"

S4&
    call COPY(-1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "COPYF1"

S5&
    call COPY(0); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "COPYF2"

S6&
    call COPY(1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "COPYF3"

S7&
    z = T; q(-1, 1:8) = z(5:12); call SUIT(-1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADQ1"

S8&
    z = T; q(0, 1:8) = z(5:12); call SUIT(0); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADQ2"

S9&
    z = T; q(1, 1:8) = z(5:12); call SUIT(1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADQ3"

S10&
    z = 0; z(5:6) = ph; z(9:11) = pa; T = z; call INCRC
    goto CYCLE; !--  Name: "COPYP"

S11&
    z(1:5) = p(1:5); p(1:5) = p(6:10); p(6:10) = z(1:5)
    call INCRC
    goto CYCLE; !--  Name: "EXCHP"

S12&
    z = T; ph = z(5:6); pa = z(9:11); call INCRC
    goto CYCLE; !--  Name: "LOADP"

S13&
    z = 0; z(1:12) = TSR; T = z; call INCRC
    goto CYCLE; !--  Name: "COPYMC"

S14&
    z(1:12) = TSR; psr = psr-1; c(1:4) = z(3:6); h(0,1:3) = ch; c(5:8) = z(9:12)
    goto CYCLE;  !--  Name: "RETNMC"

S15&
    z = T; psr = psr+1; TSR = z(1:12)
    pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADMC"

S16&
    z = T; h(-1, 1:3) = z(4:6); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADH1"

S17&
    z = T; h(0,1:3) = z(4:6); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADH2"

S18&
    z = T; h(1, 1:3) = z(4:6); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADH3"

S19&
    u(-1, 1:4) = +tt; a(-1) = 1; pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADU1"

S20&
    u(0, 1:4) = +tt;  a(0) = 1; pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADU2"

S21&
    u(1, 1:4) = +tt; a(1) = 1; pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADU3"

S22&
    call LOAD(-1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADF1"

S23&
    call LOAD(0); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADF2"

S24&
    call LOAD(1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADF3"

S25&
    call TRANSOUT(-1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADG1"

S26&
    call TRANSOUT(0); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADG2"

S27&
    call TRANSOUT(1); pa = pa-1; call INCRC
    goto CYCLE; !--  Name: "LOADG3"
! end SPEC

IR&
    z = 0; z(3:6) = c(1:4); z(9:12) = c(5:8)
    psr = psr+1; TSR = z(1:12)
    c1 = 1; ch = 13; h(0,1:3) = ch; ca = -13
    x(1:5) = p(6:10); p(6:10) = p(1:5); p(1:5) = x(1:5)
    pa = pa+1; T = 0; tt = m(+ch,+w,1:6)
    goto CYCLE
! end IR

FINISH&
    start = 0
  return

contains

subroutine INCRC

  ! Процедура INCRC осуществляет последовательное
  ! увеличение адреса очередного выполняемого слога (значения ca);

  if (c1 /= 1 .and. ca == 40) v(9) = 1
  ca = ca+1

end subroutine INCRC

subroutine REFSYL(*)

  ! Процедура REFSYL реализует слог-ссылку;

  if (c1 /= 1 .and. ka+k1+1 > 40) then
    w = -29; return 1 ! goto IR
  endif

  T = 0
  m(+ph, 3*pa-1:3*pa+k1, 1:6) = m(+h(k2,1:3), ka:ka+k1+1, 1:6)
  call INCRC

end subroutine REFSYL

subroutine SUBR(adr, ret, *)

  ! Процедура SUBR осуществляет переход на подпрограмму,
  ! адрес которой хранится в ячейке m[ch,adr], с запоминанием
  ! адреса возврата ret в стеке возвратов;

  integer, intent(in) :: adr, ret
  type(Trit) :: x(1:6), z(1:12)

  if (adr > 40 .or. ret > 40) then
    w = -29; return 1 ! goto IR
  endif

  x = m(+ch, adr, 1:6)
  z = 0; z(3) = c1; z(4:6) = ch; z(9:12) = ret
  psr = psr+1; TSR = z
  ch = +x(1:2); h(0,1:3) = ch; ca = x(3:6)

end subroutine SUBR

subroutine TRANSIN(l)

  ! Процедура TRANSIN(l) передаёт в T значение g(l,:).
  ! Если g(l,7) = 1, то в T передаётся g(l,1:6),
  ! если g(l,7) = 0, то в T передаётся -g(l,1:6);

  integer :: l

  T = 0
  if (g(l,7) == 1) then
    tt =  g(l,1:6)
  else
    tt = -g(l,1:6)
  endif
  a(l) = 1

end subroutine TRANSIN

subroutine TRANSOUT(l)

  ! Процедура TRANSOUT(l) передаёт в g(l,:) значение tt,
  ! преобразуя его из троичного кода в некоторый двоичный код;

  integer :: i, l

  x(1:6) = tt
  g(l,7) = 1
  do i = 1, 6
    if (x(i) /= 0) then
      g(l,i) = 1
    else
      g(l,i) = 0
    endif
    if (x(i) == -1) g(l,7) = 0
  enddo
  a(l) = 1

end subroutine TRANSOUT

subroutine COPY(l)

  ! Процедура COPY(l) присваивает странице
  ! оперативной памяти, номер которой определяет tt,
  ! значение страницы c номером q(l,:) памяти второго
  ! уровня вида f(l,:,:,:);

  integer :: i, l

  z = T
  hf = z(4:6)
  if (abs(+hf) <= 4) then
    do i = 0, 80
      m(+hf, 40-i, 1:6) = f(l, +q(l,1:8), 40-i, 1:6)
    enddo
  endif

end subroutine COPY

subroutine LOAD(l)

  ! Процедура LOAD(l) присваивает странице
  ! c номером q(l,:) памяти второго уровня вида f(l,:,:,:)
  ! значение страницы памяти первого уровня,
  ! номер которой определяет T;

  integer :: i, l

  z = T
  hf = z(4:6)
  do i = 0, 80
    f(l, +q(l,1:8), 40-i, 1:6) = m(+hf, 40-i, 1:6)
  enddo

end subroutine LOAD

end subroutine SETUN_70

!-------------------------------------------------------------------------------

program Setun_70_emulator

  use Setun70
  use IO_module
  implicit none
  character :: choice

  call INIT_EMU

!$omp parallel sections num_threads(5)

  call INOUT(-1)

!$omp section

  call INOUT(0)

!$omp section

  call INOUT(+1)

!$omp section

  call WATCH

!$omp section

  call ATTENT  ! Эта процедура устанавливает обработчик SIGINT  и выходит
  call STOP    ! Эта процедуре устанавливает обработчик SIGTSTP и выходит

  do while (.true.)
    call Setun_70
    write (*,"(/,a)") "Setun-70 CPU & periphery EMU. Copyright (c) 2023 Stanislav Maslovski"
    write (*,"(5(/,a),$)") "Options:", "  (0) Restart", "  (1) Run", "  (2) Step", "Your choice? (0-2) "
10  read (*,"(a1,$)") choice
    select case (choice)
      case ("0")
        cycle_cnt = 0
        start = 0
      case ("1")
        start = 1
        single_step = .false.
      case ("2")
        start = 1
        single_step = .true.
      case default
        goto 10
    end select
  enddo

!$omp end parallel sections

end program Setun_70_emulator

!-------------------------------------------------------------------------------
! Конец алгоритмического описания функционирования машины SETUN_70
!-------------------------------------------------------------------------------