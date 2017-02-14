    .text
    .intel_syntax noprefix
    .globl _scheme_entry

    // scheme_entry(&ctx, stackBase, heapBase);
    // ctx: esp+4, stackBase: esp+8, heapBase: esp+12
    //typedef struct {
    //    void* eax;       /* 0  scratch  */
    //    void* ebx;       /* 4  preserve */
    //    void* ecx;       /* 8  scratch  */
    //    void* edx;       /* 12 scratch  */
    //    void* esi;       /* 16 preserve */
    //    void* edi;       /* 20 preserve */
    //    void* ebp;       /* 24 preserve */
    //    void* esp;       /* 28 preserve */
    //} context;
_scheme_entry:
    mov ecx, [esp + 4]            // ecx contains &ctx
    mov [ecx +  4], ebx
    mov [ecx + 16], esi
    mov [ecx + 20], edi
    mov [ecx + 24], ebp
    mov [ecx + 28], esp
    mov ebp, [esp + 12]         // heap  pointer
    mov esp, [esp +  8]         // stack pointer
    call R_scheme_entry
    // Restore registers
    mov ebx, [ecx +  4]
    mov esi, [ecx + 16]
    mov edi, [ecx + 20]
    mov ebp, [ecx + 24]
    mov esp, [ecx + 28]
    ret
