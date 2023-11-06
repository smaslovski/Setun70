## Emulator of "Setun-70" ("Сетунь-70") — A Soviet ternary computer with zero-address stack-based instruction set architecture

Copyright (c) 2023 Stanislav Maslovski. License: GPL-3.

The emulator is based on the logical description of "Setun-70" computer as published in [1] (check "docs" folder).
The algorithm described in this report can be found in the file Setun70.alg (OCRed from the report and edited).

The code in this repository has the following branches:

* original — This branch realizes the Setun-70 instruction set architecture (ISA) and operation as described in [1].
* structural — This branch is reserved for changes to the ISA that facilitate structural programming, which are described in [2].
* main — The main branch is for current development.

Startup ROM code with some IO and interrupt support is in the file ROM.ods. This file uses macros, and serves as a translator of Setun 70 assembler instructions. The first page of ROM.ods explains how to use it (in Russian). 

To build and run, do "make run". You will need a fresh GNU Fortran compiler with support of Fortran 2008 and OpenMP. You will also need xterm for the emulator windows. This will start four terminal emulator windows: one for Setun-70 CPU, one for Consul-254 teletype emulator, and two more to log the keyboard input (in punched tape form and as is). At the moment, the emulator runs a simple echo process that sends back the codes it reads from the console.

A running simulation can be interrupted by pressing Shift-S (i.e. entering a capital "S") in the Setun-70 window.

### References

1. Брусенцов, Е.П., Жоголев, Е.А., Алгоритмическое описание структуры и системы команд вычислительной машины Сетунь 70. Научный отчёт ВЦ МГУ № 27-ВТ (417). Москва, Ротапринт ВЦ МГУ, 1970.
2. Брусенцов, Е.П., Рамиль Альварес, Х., Троичная ЭВМ «Сетунь 70», Труды SORUCOM-2011. Развитие вычислительной техники и её программного обеспечения в России и странах бывшего СССР. Великий Новгород, 2011. С. 71-75.
