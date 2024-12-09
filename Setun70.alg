comment Алгоритмическое описание функционирования троичной ЭВМ "Сетунь-70"
  с учетом изменений внесенных после 1975 г. и описанных в данной статье:
  Брусенцов, Н.П., Рамиль Альварес, Х. Троичная ЭВМ "Сетунь 70". Труды SORUCOM-2011.
  Развитие вычислительной техники и ее программного обеспечения в России и странах
  бывшего СССР. Великий Новгород, 2011. C. 71-75;

begin
  ternary array f[-1:1, -3280:3280, -40:40, 1:6],
                m[-13:13, -40:40, 1:6], q[-1:1, 1:8],
                g[-1:1, 1:7], u[-1:1, 1:4], h[-1:1, 1:3],
                c[1:8], R[1:18], Y[1:18], p[1:10], v[1:9],
                e[1:6], k[1:6], w[1:4], hf[1:3], a[-1:1],
                sr[-13:13, 1:12], psr[1:3],
                start[1:1];

procedure INOUT(i);
comment Процедура INOUT, будучи активизированной
  в результате обращения к ней, определяет процесс,
  выполняемый параллельно с процедурой SETUN_70,
  в которой сразу же начинают выполняться следующие
  за этим обращением операторы.  Повторное обращение
  к процедуре INOUT определяет новый процесс, не
  прекращая выполнения старого, определяемого той
  же процедурой INOUT. В процедуре SETUN_70
  производится трижды обращение к этой процедуре
  INOUT с разными значениями параметра i = -1, 0, +1;

begin
  procedure SWON(j);
    comment Процедура SWON(j) включает устройство
      ввода-вывода группы j с номером u[j, 3:4] и
      выключает все другие устройства ввода-вывода
      этой группы;
  procedure SWOFF(j);
    comment Процедура SWOFF(j) выключает все
      устройства ввода-вывода группы j;
  procedure PASS(j);
    comment Процедура PASS(j) передаёт значение
      g[j] устройству ввода-вывода с номером u[j, 3:4];
  procedure TAKE(j);
    comment Процедура TAKE(j) присваивает массиву
      g[j] значение, поступившее от устройства ввода-
      вывода с номером u[j,3:4];
LO: if a[i] = 1 then
    begin
      if u[i,3:4] = 0 then SWOFF(i)
      else begin SWON(i);
        if u[i,1] = 1 then
	begin PASS(i); v[i+3] := 1 end
        else if u[i,1] = -1 then
          begin TAKE(i); if g[i] = 0 then goto LO;
            v[i+3] := 1
        end ELSE IF
    end ELSE;
    a[i] := 0
  end THEN;
  goto LO
end INOUT;

procedure WATCH;
  comment Процедура WATCH формирует заявки на прерывание v[1] := 1
    через определённые промежутки времени. Как и процедура INOUT,
    работает параллельно с процедурой SETUN_70;

procedure SUIT(i);
  comment Процедура SUIT, будучи активизированной в результате
    обращения к ней, определяет процесс, выполняемый параллельно
    с процедурой SETUN_70. Сразу же после активизации процедуры
    SUIT выполняется оператор, следующий за обращением к этой
    процедуре. Процедура SUIT(i) осуществляет поиск страницы
    с номером q[i] в памяти второго уровня вида f[i] и завершает
    свою работу заявкой на прерывание v[i+6] := 1, указывающей
    на то, что поиск закончен и страницу можно использовать.
    Повторное обращение к процедуре SUIT определяет новый процесс,
    прекращая старый, определяемый этой процедурой с тем же
    значением параметра i, но не прекращая другие, ранее
    активизированные процессы, определяемые этой процедурой с
    другими значениями параметров;

procedure STOP;
  comment Процедура STOP осуществляет выполнение оператора
    start := 0 в некоторый заранее не известный момент времени.
    Как и процедура INOUT, работает параллельно с процедурой SETUN_70;

procedure ATTENT;
  comment Процедура ATTENT формирует заявки на прерывание v[8]:= 1
    при нажатии специальной кнопки на пульте управления машины.
    Как и процедура INOUT, работает параллельно с процедурой SETUN_70;

procedure SETUN_70(T, S, t, k1, k2, k3, ka, ko, c1, ch, ca, ph, pa, TSR);
  comment Формальные параметры используются для введения
    сокращённых обозначений наиболее употребительных частей
    описанных выше массивов;
begin ternary array x[1:36], z[1:18];
  integer i;
    switch Operation := MACRO, BASIC, SPEC;

  procedure INCRC;
    comment Процедура INCRC осуществляет последовательное
      увеличение адреса очередного выполняемого слога (значения ca);
  begin if c1 != 1 & ca = 40 then v[9] := 1;
    ca := ca+1
  end INCRC;

  procedure REFSYL;
    comment Процедура REFSYL реализует слог-ссылку;
  begin
    if c1 != 1 & ka+k1+1 > 40 then
       begin w := -29; goto IR end;
    T := 0; m[ph, 3*pa-1:3*pa+k1] := m[h[k2], ka:ka+k1+1];
    INCRC
  end REFSYL;

  procedure SUBR(adr, ret);
    comment Процедура SUBR осуществляет переход на подпрограмму,
      адрес которой хранится в ячейке m[ch,adr], с запоминанием
      адреса возврата ret в стеке возвратов;
  begin
    if adr > 40 | ret > 40 then begin w := -29; goto IR end;
    x := m[ch, adr];
    z := 0; z[3] := c1; z[4:6] := ch; z[9:12] := ret;
    psr := psr+1; TSR := z[1:12];
    ch := h[0] := x[1:2]; ca := x[3:6];
  end SUBR;

START:
  if start = 0 then
  begin
    start := 1;
    c1 := 1; ch := h[0] := 12; ca := -40;
    ph := 0; pa := 0; Y := 0;
    psr := 0;
    INOUT(-1); INOUT(0); INOUT(+1);
    WATCH; ATTENT; STOP;

CYCLE:
    begin
      if start = 0  then goto FINISH;
      if c1 != 1 then
        for i := 1 step 1 until 9 do
          if v[i] != 0 then begin w := i-41; v[i]:= 0; goto IR end;
      k := m[ch, ca];
      if k1 = 0 & k2 = 0 then goto Operation[k3+2]
        else begin
          if c1 != 1 & pa = 13 then
            begin w := -30; goto IR end;
          pa := pa+1; REFSYL
        end else;
      goto CYCLE
    end CYCLE;

MACRO:
  begin
    if c1 = 1 then goto FINISH;
    if c1 = 0 then begin w := -28; goto IR end;
    if pa = 13 then begin w := -30; goto IR end;
    pa := pa+1;
    z := 0; z[3:6] := c[1:4]; z[9:12] := c[5:8];
    psr := psr+1; TSR := z[1:12];
    c1 := 0; ch := h[0] := ca := -13;
    T := 0; t := m[ch, ka];
    goto CYCLE
  end MACRO;

BASIC:
  begin switch Basop := B1,  B2,  B3,  B4,  B5,  B6,  B7,  B8,  B9,
                        B10, B11, B12, B13, B14, B15, B16, B17, B18,
                        B19, B20, B21, B22, B23, B24, B25, B26, B27;

    if c1 != 1 & pa = -13 then
      begin w := -31; goto IR end;
    goto Basop[ko+14];

B1: x[1:18] := S; x[19:36] := Y; x := x*(3^t);
      S:= x[1:18]; Y:= x[19:36]; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LST";

B2: х[1:18] := T;
      if x[1] != 0 then ca := t else INCRC; pa := pa-1;
      goto CYCLE; comment Name: "COT";

B3: x[1:18] := T; x[19:36] := Y;
      if x[1] != 0 then begin i := 1; x := x/3 end
      else begin
        for i := 0 step -1 until -34 do
          if x[2] != 0 then goto EX else x:= x*3;
        i:= 0
      end ELSE;
 EX:  e := e+i; T := x[1:18]; Y := x[19:36]; INCRC;
      goto CYCLE; comment Name: "XNN";

B4: if T != 0 then SUBR(ca+1, ca) else begin INCRC; INCRC end;
      goto CYCLE; comment Name: "DOW";

B5: INCRC;
      if T < 0 then SUBR(ca, ca+3)
      else if T = 0 then SUBR(ca+1, ca+3)
           else SUBR(ca+2, ca+3);
      pa := pa-1;
      goto CYCLE; comment Name: "BRT";

B6: z := T; pa := pa-1;
      c[1:4] := z[3:6]; h[0] := ch; c[5:8] := z[9:12];
      goto CYCLE; comment Name: "JMP";

B7: T := T - e*(3^12); INCRC;
      goto CYCLE; comment Name: "T-E";

B8: e := t; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "E=T";

B9: T := T + e*(3^12); INCRC;
      goto CYCLE; comment Name: "T+E";

B10: if S < 0 then ca := t else INCRC; pa := pa-1;
       goto CYCLE;  comment Name: "CLT";

B11: if S = 0 then ca := t else INCRC; pa := pa-1;
       goto CYCLE;  comment Name: "CET";

B12: if S > 0 then ca := t else INCRC; pa := pa-1;
       goto CYCLE;  comment Name: "CGT";

B13: INCRC; SUBR(ca, ca+1);
       goto CYCLE; comment Name: "JSR";

B14: R := T; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "R=T";

B15: ca := t;  pa := pa-1;
       goto CYCLE; comment Name: "C=T";

B16: k := t; REFSYL;
       goto CYCLE; comment Name: "T=W";

B17: Y := 0; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "YFT";

B18: k := t;
       if c1 != 1 & ka+k1+1 > 40 then
         begin w := -29; goto IR end;
       if abs(h[k2]) <= 4 then
         m[h[k2], ka:ka+k1+1] := m[ph, 3*(pa-1)-1:3*(pa-1)+k1];
       INCRC; pa := pa-1;
       goto CYCLE; comment Name: "W=S";

B19: x[1:18] := T; z := S;
       for i := 1 step 1 until 18 do
         z[i] := z[i]*x[i];
       S := z; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "SMT";

B20: Y := T; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "Y=T";

B21: x[1:18] := T; z := S;
       for i := 1 step 1 until 18 do
         begin
           if z[i] = 0 then z[i] := x[i]
           else if z[i] != x[i] & x[i] != 0 then z[i] := 0
         end FOR;
       S := z; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "SAT";

B22: S := S - T; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "S-T";

B23: T := -T; INCRC;
       goto CYCLE; comment Name: "TDN";

B24: S := S + T; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "S+T";

B25: x[1:18] := S; x[19:36] := 0; x := x + T*R*(3^2);
       S := x[1:18]; Y := x[19:36]; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "LBT";

B26: R := S; x := T*R*(3^2); S := x[1:18]; Y := x[19:36];
       INCRC; pa := pa-1;
       goto CYCLE; comment Name: "L*T";

B27: x[1:18] := S; x[19:36] := Y; x := x + T*R*(3^2);
       S := x[1:18]; Y:= x[19:36]; INCRC; pa := pa-1;
       goto CYCLE; comment Name: "LHT";
end BASIC;

SPEC:
  begin switch Specop := S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,
                         S10, S11, S12, S13, S14, S15, S16, S17, S18,
                         S19, S20, S21, S22, S23, S24, S25, S26, S27;
  procedure TRANSIN(l);
    comment Процедура TRANSIN(l) передаёт в T значение g[l].
            Если g[l,7] = 1, то в T передаётся g[l,1:6],
            если g[l,7] = 0, то в T передаётся -g[l,1:6];
  begin T := 0;
    if g[l,7] = 1 then t :=  g[l,1:6]
      else t := -g[l,1:6];
    a[l] := 1
  end TRANSIN;

  procedure TRANSOUT(l);
    comment Процедура TRANSOUT(l) передаёт в g[l] значение t,
      преобразуя его из троичного кода в некоторый двоичный код;
  begin x[1:6] := t; g[l,7] := 1; g[l,1:6] := 0;
    for i := 1 step 1 until 6 do
      begin
        if x[i] != 0 then begin
          g[l,i] := 1;
          if x[i] = -1 then g[l,7] := 0
        end;
      end FOR;
    a[l] := 1
  end TRANSOUT;

  procedure COPY(l);
    comment Процедура COPY(l) присваивает странице
      оперативной памяти, номер которой определяет t,
      значение страницы с номером q[l] памяти второго
      уровня вида f[l];
  begin z := T; hf := z[4:6];
    if abs(hf) <= 4 then
      for i := 0 step 1 until 80 do
        m[hf, 40-i] := f[l, q[l], 40-i]
  end COPY;

  procedure LOAD(l);
    comment Процедура LOAD(l) присваивает странице
      с номером q[l] памяти второго уровня вида f[l]
      значение страницы памяти первого уровня,
      номер которой определяет t;
  begin z := T; hf := z[4:6];
    for i := 0 step 1 until 80 do
      f[l, q[l], 40-i] := m[hf, 40-i]
  end LOAD;

  if c1 = -1 then
    begin w := -28; goto IR end;
  if c1 = 0 & pa = -13 then
    begin w := -31; goto IR end;
  goto Specop[ko+14];

S1: TRANSIN(-1); INCRC;
      goto CYCLE; comment Name: "CG1";

S2: TRANSIN(0); INCRC;
      goto CYCLE; comment Name: "CG2";

S3: TRANSIN(1); INCRC;
      goto CYCLE; comment Name: "CG3";

S4: COPY(-1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "CF1";

S5: COPY(0); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "CF2";

S6: COPY(1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "CF3";

S7: z := T; q[-1] := z[5:12]; SUIT(-1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LQ1";

S8: z := T; q[0] := z[5:12]; SUIT(0); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LQ2";

S9: z := T; q[1] := z[5:12]; SUIT(1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LQ3";

S10: z := 0; z[5:6] := ph; z[9:11] := pa; T:= z; INCRC;
      goto CYCLE; comment Name: "CP";

S11: z[1:5] := p[1:5]; p[1:5]:= p[6:10]; p[6:10] := z[1:5];
      INCRC;
      goto CYCLE; comment Name: "EXP";

S12: z := T; ph := z[5:6]; pa := z[9:11]; INCRC;
      goto CYCLE; comment Name: "LP";

S13: z := 0; z[1:12] := TSR; T:= z; INCRC;
      goto CYCLE; comment Name: "CMC";

S14: z[1:12] := TSR; psr := psr-1; c[1:4]:= z[3:6]; h[0] := ch; c[5:8]:= z[9:12];
      goto CYCLE;  comment Name: "RMC";

S15: z := T; psr := psr+1; TSR := z[1:12];
      INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LMC";

S16: z := T; h[-1] := z[4:6]; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LH1";

S17: z := T; h[0] := z[4:6]; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LH2";

S18: z := T; h[1] := z[4:6]; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LH3";

S19: u[-1, 1:4] := t; a[-1] := 1; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LU1";

S20: u[0, 1:4] := t;  a[0] := 1; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LU2";

S21: u[1, 1:4] := t; a[1] := 1; INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LU3";

S22: LOAD(-1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LF1";

S23: LOAD(0); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LF2";

S24: LOAD(1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LF3";

S25: TRANSOUT(-1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LG1";

S26: TRANSOUT(0); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LG2";

S27: TRANSOUT(1); INCRC; pa := pa-1;
      goto CYCLE; comment Name: "LG3";

end SPEC;

IR: begin
      z := 0; z[3:6] := c[1:4]; z[9:12] := c[5:8];
      psr := psr+1; TSR := z[1:12];
      c1 := 1; ch := h[0] := 13, ca := -13;
      x[1:5] := p[6:10]; p[6:10] := p[1:5]; p[1:5] := x[1:5];
      pa := pa+1; T := 0; t := m[ch,w];
      goto CYCLE
end IR;

FINISH:
    start:= 0;
  end START
end SETUN_70;

SETUN_70( m[p[1:2], 3*p[3:5]-1:3*p[3:5]+1],
          m[p[1:2], 3*(p[3:5]-1)-1:3*(p[3:5]-1)+1],
          m[p[1:2], 3*p[3:5]-1],
          k[1], k[2], k[3], k[3:6], k[4:6],
          c[1], c[2:4], c[5:8],
          p[1:2], p[3:5], sr[psr, 1:12] );

end     Алгоритмического описания функционирования
        машины SETUN_70;
