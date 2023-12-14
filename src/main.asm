org 0x7C00
bits 16

; setup stack
start:
    [bits 16]
    mov ax, 0
    mov ds, ax,
    mov es, ax
    mov ss, ax
    mov sp, 7C00h

    ; switch to protected mode

    ; 1. Disable interrupts
    cli
    ; Disabling NMI interrupts does not seem neccesary

    ; Enable a20 gate (mentioned on osdev)
    call enable_a20

    ; 2. Execute LGDT
    call load_GDT

    ; 3. Modify control register
    mov eax, cr0
    or al, 1
    mov cr0, eax

    ; 4. Jump into 32-bit protected mode
    jmp dword 08h:.pmode    ; 08h is the 2nd entry in the gdt (32-bit p code) (first entry is NULL)
    ; 5. The JMP or CALL instruction immediately after the MOV CR0 instruction changes the flow of execution and serializes the processor.

.pmode: ; Start of 32-bit  protected mode!
    [bits 32]

    ; 6. If paging is enabled, the code for the MOV CR0 instruction and the JMP or CALL instruction must come from a page that is identity mapped (that is, the linear address before the jump is the same as the physical address after paging and protected mode is enabled). The target instruction for the JMP or CALL instruction does not need to be identity mapped.

    ;   Paging is not enabled (yet)


    ; 7. If a local descriptor table is going to be used, execute the LLDT instruction to load the segment selector for the LDT in the LDTR register.

    ;   A local discriptor table is not being used

    ; 8. Execute the LTR instruction to load the task register with a segment selector to the initial protected-mode task or a writable area of memory that can be used to store TSS information on a tak switch.

    ;   Not using tasks

    ; 9. Setup the segment registers again
    mov ax, 0x10 ; (32-bit p data segment)
    mov ds, ax
    mov ss, ax

    ; 10. Execute the LIDT instructio nto load the IDTR register with the address and limit of the protected-mode IDT.

    ; Not using idt

    ; 11.Execute the STI instruction to enable maskable hardware interrupts and perform the necessary hardware operation to enable NMI interrupts

    ; not using nmi

    mov esi, g_Hello
    mov edi, vga_screen_buffer
    cld

    mov bl, 0

.loop:
    lodsb ; Loads char from char pointetr (esi) into al, increments esi to next char in string
    or al, al,
    jz .done
    mov [edi], al
    inc edi
    mov [edi], bl
    inc bl
    inc edi
    jmp .loop

.done:
    ; go back to real mode
    ; 1. Disable interrupts. A CLI instruction disables maskable hardware interrupts. NMI interrupts can be disabled with external circuitry.
    cli
    ; Not using NMI

    ; 2. If paging is enabled, perform the following operations:
    ;       - Transfer program control to linear addresses that are identity mapped to physical addresses (that is, linear addresses equal physical addresses).
    ;       - Ensure that the GDT and IDT are in identity mapped pages.
    ;       - Clear the PG bit in the CR0 register.
    ;       - Mov 0H into the CR3 register to flush the TLB.

    ; Not using paging

    ; 3. Transfer program control to a readable segment that has a limit of 64 KBytes (FFFFH). This operation loads the CS register with the segment limit equired in real-address mode.
    jmp word 0x18:.pmode16

.pmode16:
    [bits 16]

    ; 4. Load segment registers SS, DS, ES, FS, and GS with a selector for a descriptor containing the following values, which are appropriate for real-address mode:
    ;       - Limit = 64 KBytes (0FFFFH)
    ;       - Byte granular (G = 0)
    ;       - Expand up (E = 0)
    ;       - Writable (W = 1)
    ;       - Present (P = 1)
    ;       - Base = any value
    ;   The segment registers must be loaded with non-null segment selectors or the segment registers will be unusable in real-address mode. Note that if the segment registers are not reloaded, execution continues using the descriptor attributes loaded during protected mode..

    ; Not accessing memory yet, so skip no.4 for now..

    ; 5. Execute an LIDT instruction to point to a real-address mode interrupt table that is withint the 1MByte real-address mode address range.

    ; We haven't changed the IDT (yet)

    ; 6. Clear the PE flag in the CR0 register to switch to real-address mode.
    mov eax, cr0
    and eax, ~1
    mov cr0, eax

    ; 7. Execute a far JMP instruction to jump to a real-address mode program. This operation flushes the instruction queue and loads the appropriate base-address value in the CS register
    jmp word 00h:.rmode

.rmode:
    ; 8. Load the SS, DS, ES, FS, and GS registers as needed by the real-address mode code. If any of the registers are not going to be used in real-address mode, write 0s to them.
    mov ax, 0
    mov ds, ax
    mov ss, ax

    ; 9. Execute the STI instruciton to enable maskable hardware interrupts and perform the necessary hardware operation to enable NMI interrupts.
    sti
    ; Not using NMI


    ; print hello world using int 10h
    mov si, g_Hello_16

.rloop:
    lodsb
    or al, al
    jz .rdone
    mov ah, 0eh
    int 10h
    jmp .rloop

.rdone:


.halt:
    jmp .halt

enable_a20:
    [bits 16]
    ; disable keyboard
    call a20_wait_input
    mov al, kbd_ctrl_disable_keyboard
    out kbd_ctrl_command_port, al

    ; read control output port (flush ouput buffer)
    call a20_wait_input
    mov al, kbd_ctrl_read_ctrl_out_port
    out kbd_ctrl_command_port, al
    call a20_wait_output        ; wait for the data and read it
    in al, kbd_ctrl_data_port
    push eax

    ; write control output port
    call a20_wait_input
    mov al, kbd_ctrl_write_ctrl_out_port
    out kbd_ctrl_command_port, al
    call a20_wait_input
    pop eax
    or al, 2    ; bit 2 = a20 bit
    out kbd_ctrl_data_port, al

    ; re-enable keyboard
    call a20_wait_input
    mov al, kbd_ctrl_enable_keyboard
    out kbd_ctrl_command_port, al

    call a20_wait_input
    ret


a20_wait_input:
    [bits 16]
    ; wait until status bit 2 (input buffer) is 0
    in al, kbd_ctrl_command_port ; read status byte from command port
    test al, 2
    jnz a20_wait_input
    ret

a20_wait_output:
    [bits 16]
    ; wait until status bit 1 (output) buffer is 1
    in al, kbd_ctrl_command_port
    test al, 1
    jz a20_wait_output
    ret


load_GDT:
    [bits 16]
    lgdt [g_GDT_Decriptor]
    ret


kbd_ctrl_data_port              equ 0x60
kbd_ctrl_command_port           equ 0x64
kbd_ctrl_disable_keyboard       equ 0xAD
kbd_ctrl_enable_keyboard        equ 0xAE
kbd_ctrl_read_ctrl_out_port     equ 0xD0
kbd_ctrl_write_ctrl_out_port    equ 0xD1

vga_screen_buffer               equ 0xB8000

g_GDT:
    dq 0    ; NULL descriptor

    ; 32-bit code segment
    dw 0FFFFh       ; limit (bits 0-15) = 0xFFFFFFFF for full 32-bit range
    dw 0            ; base (bits 0-15) = 0x0
    db 0            ; base(bits 16-23)
    db 10011010b    ; access (present, ring 0, code segment, executable, direction 0, readable, accessed)
    db 11001111b    ; granularity (4k pages, 32-bit pmode) + limit (bits 16-19)
    db 0            ; base high

    ; 32-bit data segment
    dw 0FFFFh       ; limit (bits 0-15) = 0xFFFFFFFF for full 32-bit range
    dw 0            ; base (bits 0-15) = 0x0
    db 0            ; base(bits 16-23)
    db 10010010b    ; access (present, ring 0, data segment, executable, direction 0, writable, accessed)
    db 11001111b    ; granularity (4k pages, 32-bit pmode) + limit (bits 16-19)
    db 0            ; base high

    ; 16-bit code segment
    dw 0FFFFh       ; limit (bits 0-15) = 0xFFFF
    dw 0            ; base (bits 0-15) = 0x0
    db 0            ; base(bits 16-23)
    db 10011010b    ; access (present, ring 0, code segment, executable, direction 0, readable, accessed)
    db 00001111b    ; granularity (1b pages, 16-bit pmode) + limit (bits 16-19)
    db 0            ; base high

    ; 16-bit data segment
    dw 0FFFFh       ; limit (bits 0-15) = 0xFFFF
    dw 0            ; base (bits 0-15) = 0x0
    db 0            ; base(bits 16-23)
    db 10010010b    ; access (present, ring 0, data segment, executable, direction 0, writable, accessed)
    db 00001111b    ; granularity (1b pages, 16-bit pmode) + limit (bits 16-19)
    db 0            ; base high

g_GDT_Decriptor:    dw g_GDT_Decriptor - g_GDT - 1 ; size of gdt
                    dd g_GDT                       ; offset (address of gdt)

g_Hello:            db "Hello world from protected mode!", 0
g_Hello_16:         db "Hello world from real mode!", 0

times 510-($-$$) db 0
dw 0xAA55


