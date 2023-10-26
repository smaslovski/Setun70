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
    "LST", "COT", "XNN", "E-1", "E=0", "E+1", "T-E", "E=T", "T+E",&
    "CLT", "CET", "CGT", "T=C", "R=T", "C=T", "T=W", "YFT", "W=S",&
    "SMT", "Y=T", "SAT", "S-T", "TDN", "S+T", "LBT", "L*T", "LHT",&
    "CG1", "CG2", "CG3", "CF1", "CF2", "CF3", "LQ1", "LQ2", "LQ3",&
    "CP ", "EXP", "LP ", "CMC", "RMC", "LMC", "LH1", "LH2", "LH3",&
    "LU1", "LU2", "LU3", "LF1", "LF2", "LF3", "LG1", "LG2", "LG3"&
  ]

contains

  function disassemble(k) result(res)

    Type(Trit), intent(in) :: k(1:6)
    character(len=10) :: res

    if (k(1:2) == 0) then
      if (k(3:6) >= -13) then
        ! Обычная или специальная инструкция
        res = mnemonic(+k(3:6))
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
