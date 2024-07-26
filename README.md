# cs467-6502-emulator

References (in no particular order):

1) Jacobs, Andrew. Instruction Reference. (accessed July 20, 2024). https://www.nesdev.org/obelisk-6502-guide/reference.html
2) 6502 Emulator. (accessed July 20, 2024). http://www.emulator101.com/6502-emulator.html
3) Wikipedia contributors. (2023, December 1). Interrupts in 65xx processors. In Wikipedia, The Free Encyclopedia. Retrieved 5:51pm PST, July 20, 2024, from https://en.wikipedia.org/w/index.php?title=Interrupts_in_65xx_processors&oldid=1187777565
4) Wilson, Garth. Investigating Interrupts. (accessed July 20, 2024). http://www.6502.org/tutorials/interrupts.html#2.2
5) Nesdev Wiki contributors. (accessed July 20, 2024). https://www.nesdev.org/wiki/Nesdev_Wiki
6) TODO: add citation for https://web.archive.org/web/20221112230813if_/http://archive.6502.org/books/mcs6500_family_programming_manual.pdf


Regarding the Disassembler

Since the disassembler will not be our main deliverable, the majority of the code was sourced from the
following URL: https://github.com/kpmiller/emulator101/blob/master/6502Disassembler/dis6502.c. We note that the license for this project (the Unlicense) is among those listed by the Open Source Initiative, and can be seen here: https://github.com/kpmiller/emulator101/blob/master/LICENSE.md.

The intention of building out the disassembler was to determine what opcodes to implement to run the 
Falling NES game. From my analysis of the disassembler output, 147 out of the total 148 instructions will need
to be implemented. 

The dissassembler output for the Falling NES game can be seen in the dis-output.txt file

The disassembler implementation is in the disa-vs2.c file. The original disassembler.c will need to be discarded. 

The hex-view.txt file is a hexdump view of the Falling.nes game

The opcodes-list.csv shows the occurrences of the opcodes in the Falling.nes game

The 6502ops.csv file is a listing of all the opcodes of the 6502 processor. This list was used to compare to all the 
opcode ocurrences in the Falling.nes game. A python script was run to manipulate opcode data in a dataframe to determine
what unique opcodes are used. The python file is called unique_opcode.py. 

As mentioned above, the disassembler was build only to aid in the building of the emulator and not as a main deliverable. 
