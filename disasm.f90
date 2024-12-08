!-------------------------------------------------------------------------------
! Компилируемое алгоритмическое описание троичной машины Сетунь-70.
!
! (c) 2023 Станислав Масловский <stanislav.maslovski@gmail.com>
!
! Модуль дизассемблирования. Исходный текст на ЯП GNU Fortran 2008.
!-------------------------------------------------------------------------------

module DisAsm

  use ternary_arith
  use IO_module

  implicit none

  character(len=3), parameter :: mnemonic(-13:40) =&
  [&
    "LST", "COT", "XNN", "DOW", "BRT", "JMP", "T-E", "E=T", "T+E",&
    "CLT", "CET", "CGT", "JSR", "R=T", "C=T", "T=W", "YFT", "W=S",&
    "SMT", "Y=T", "SAT", "S-T", "TDN", "S+T", "LBT", "L*T", "LHT",&
    "CG1", "CG2", "CG3", "CF1", "CF2", "CF3", "LQ1", "LQ2", "LQ3",&
    "CP ", "EXP", "LP ", "CMC", "RMC", "LMC", "LH1", "LH2", "LH3",&
    "LU1", "LU2", "LU3", "LF1", "LF2", "LF3", "LG1", "LG2", "LG3"&
  ]

contains

  function disassemble(k, a1, a2, a3) result(res)

    Type(Trit), intent(in), dimension(1:6) :: k, a1, a2, a3
    character(len=3) :: ins
    character(len=15) :: res

    if (k(1:2) == 0) then
      if (k(3:6) >= -13) then
        ! Обычная или специальная инструкция
        ins = mnemonic(+k(3:6))
        select case (ins)
          case ("DOW", "JSR")
            write (res, "(a,1x,a)") ins, to_nonary(+a1)
          case ("BRT")
            write (res, "(a,1x,2(a,','),a)") ins, to_nonary(+a1), to_nonary(+a2), to_nonary(+a3)
          case default
            res = ins
        end select
      else
        ! Макрооперация
        write (res, "('MAC(',i0,')')") k(3:6)+40
      endif
    else
      ! Ссылка
      write (res, "('(',i1,',h',i1,',',a,')')") k(1)+2, k(2)+2, to_nonary(+k(3:6))
    endif

  end function

end module
