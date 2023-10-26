!-------------------------------------------------------------------------------
! Компилируемое алгоритмическое описание троичной машины Сетунь-70.
!
! (c) 2023 Станислав Масловский <stanislav.maslovski@gmail.com>
!
! Модуль поддержки троичной арифметики на основе массивов тритов.
!-------------------------------------------------------------------------------

module ternary_arith

  implicit none
  integer, parameter :: tsz = selected_int_kind(1)

  ! Trit cell type

  type :: Trit
    sequence
    integer(tsz) :: val
  end type

  ! Constructor

  interface Trit
    module procedure modulo_tri
  end interface

  ! Assignment

  interface assignment (=)
    module procedure assign_i2t, assign_t2i, assign_i2ta, assign_ta2i
  end interface

  ! Explicit type conversion

  interface integer
    module procedure plus_t, plus_ta
  end interface

  interface character
    module procedure char_t
  end interface

  ! Arithmetic operations

  interface operator (+)
    module procedure plus_t, plus_ta, add_i_t, add_i_ta, add_ta_ta
  end interface

  interface operator (-)
    module procedure minus_t, minus_ta, sub_i_t, sub_i_ta, sub_ta_ta
  end interface

  interface operator (*)
    module procedure mul_t_t, mul_i_ta, mul_ta_i, mul_ta_ta
  end interface

  interface operator (/)
    module procedure div_ta_i
  end interface

  ! Logical operations

  interface operator (==)
    module procedure eq_t_t, eq_t_i, eq_ta_i, eq_ta_ta
  end interface

  interface operator (/=)
    module procedure neq_t_t, neq_t_i, neq_ta_i, neq_ta_ta
  end interface

  interface operator (>)
    module procedure gt_ta_i, gt_ta_ta
  end interface

  interface operator (<=)
    module procedure le_ta_i, le_ta_ta
  end interface

  interface operator (<)
    module procedure lt_ta_i, lt_ta_ta
  end interface

  interface operator (>=)
    module procedure ge_ta_i, ge_ta_ta
  end interface

contains

  pure function modulo_tri(i) result(t)
    integer, intent(in) :: i
    type(Trit) :: t
    integer :: r
    r = mod(i, 3)
    select case (r)
      case (-2)
        t%val = +1
      case (+2)
        t%val = -1
      case default
        t%val = r
    end select
  end function

  subroutine assign_i2t(t, i)
    type(Trit), intent(out) :: t
    integer, intent(in) :: i
    t = modulo_tri(i)
  end subroutine

  subroutine assign_i2ta(ta, i)
    type(Trit), dimension(:), intent(out) :: ta
    integer, intent(in) :: i
    integer :: d, j
    d = i
    do j = size(ta), 1, -1
      ta(j) = modulo_tri(d)
      d = (d - ta(j)%val) / 3
    enddo
  end subroutine

  subroutine assign_t2i(i, t)
    integer, intent(out) :: i
    type(Trit), intent(in) :: t
    i = t%val
  end subroutine

  subroutine assign_ta2i(i, ta)
    integer, intent(out) :: i
    type(Trit), dimension(:), intent(in) :: ta
    integer :: j
    i = 0
    do j = 1, size(ta)
      i = 3*i + ta(j)%val
    enddo
  end subroutine

  elemental function plus_t(t) result(res)
    type(Trit), intent(in) :: t
    integer :: res
    res = t%val
  end function

  function plus_ta(ta) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer :: res
    res = ta
  end function

  function add_i_t(t, i) result(res)
    integer, intent(in) :: i
    type(Trit), intent(in) :: t
    integer :: res
    res = t%val + i
  end function

  function add_i_ta(ta, i) result(res)
    integer, intent(in) :: i
    type(Trit), dimension(:), intent(in) :: ta
    integer :: res
    res = ta
    res = res + i
  end function

  function add_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    integer :: res
    res = tb
    res = ta + res
  end function

  function minus_t(t) result(res)
    type(Trit), intent(in) :: t
    integer :: res
    res = -t%val
  end function

  function minus_ta(ta) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer :: res
    res = ta
    res = -res
  end function

  function sub_i_t(t, i) result(res)
    integer, intent(in) :: i
    type(Trit), intent(in) :: t
    integer :: res
    res = t + (-i)
  end function

  function sub_i_ta(ta, i) result(res)
    integer, intent(in) :: i
    type(Trit), dimension(:), intent(in) :: ta
    integer :: res
    res = ta + (-i)
  end function

  function sub_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    integer :: res
    res = ta + (-tb)
  end function

  function mul_t_t(t1, t2) result(res)
    type(Trit), intent(in) :: t1, t2
    integer :: res
    res = t1%val * t2%val
  end function

  function mul_i_ta(ta, i) result(res)
    integer, intent(in) :: i
    type(Trit), dimension(:), intent(in) :: ta
    integer :: res
    res = ta
    res = res * i
  end function

  function mul_ta_i(i, ta) result(res)
    integer, intent(in) :: i
    type(Trit), dimension(:), intent(in) :: ta
    integer :: res
    res = ta * i
  end function

  function mul_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    integer :: res
    res = tb
    res = ta * res
  end function

  function div_ta_i(ta, i) result(res)
    integer, intent(in) :: i
    type(Trit), dimension(:), intent(in) :: ta
    integer :: res
    res = ta
    res = res / i
  end function

  function eq_t_t(t1, t2) result(res)
    type(Trit), intent(in) :: t1, t2
    logical :: res
    res = t1%val == t2%val
  end function

  function eq_t_i(t, i) result(res)
    type(Trit), intent(in) :: t
    integer, intent(in) :: i
    logical :: res
    res = t%val == i
  end function

  function eq_ta_i(ta, i) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer, intent(in) :: i
    logical :: res
    res = integer(ta) == i
  end function

  function eq_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    logical :: res
    res = integer(ta) == integer(tb)
  end function

  function neq_t_t(t1, t2) result(res)
    type(Trit), intent(in) :: t1, t2
    logical :: res
    res = .not. (t1 == t2)
  end function

  function neq_t_i(t, i) result(res)
    type(Trit), intent(in) :: t
    integer, intent(in) :: i
    logical :: res
    res = .not. (t == i)
  end function

  function neq_ta_i(ta, i) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer, intent(in) :: i
    logical :: res
    res = .not. (ta == i)
  end function

  function neq_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    logical :: res
    res = .not. (ta == tb)
  end function

  function gt_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    logical :: res
    res = integer(ta) > integer(tb)
  end function

  function le_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    logical :: res
    res = .not. (ta > tb)
  end function

  function gt_ta_i(ta, i) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer, intent(in) :: i
    logical :: res
    res = integer(ta) > i
  end function

  function le_ta_i(ta, i) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer, intent(in) :: i
    logical :: res
    res = .not. (ta > i)
  end function

  function lt_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    logical :: res
    res = integer(ta) < integer(tb)
  end function

  function ge_ta_ta(ta, tb) result(res)
    type(Trit), dimension(:), intent(in) :: ta, tb
    logical :: res
    res = .not. (ta < tb)
  end function

  function lt_ta_i(ta, i) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer, intent(in) :: i
    logical :: res
    res = integer(ta) < i
  end function

  function ge_ta_i(ta, i) result(res)
    type(Trit), dimension(:), intent(in) :: ta
    integer, intent(in) :: i
    logical :: res
    res = .not. (ta < i)
  end function

  elemental function char_t(t) result(res)
    type(Trit), intent(in) :: t
    character :: res
    select case(t%val)
      case(-1)
        res = "M"
      case(0)
        res = "0"
      case(+1)
        res = "P"
    end select
  end function

end module ternary_arith
