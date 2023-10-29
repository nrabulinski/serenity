/*
 * Copyright (c) 2023, Andreas Kling <kling@serenityos.org>
 * Copyright (c) 2023, Nikodem Rabulinski <1337-serenity@nrab.lol>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Platform.h>
#include <AK/StringView.h>
#include <AK/Vector.h>
#include <LibJIT/Assembler.h>

namespace JIT::X86_64 {

struct X86_64Reg final : public Assembler::Reg {
    static constexpr X86_64Reg rax() { return 0; }
    static constexpr X86_64Reg rcx() { return 1; }
    static constexpr X86_64Reg rdx() { return 2; }
    static constexpr X86_64Reg rbx() { return 3; }
    static constexpr X86_64Reg rsp() { return 4; }
    static constexpr X86_64Reg rbp() { return 5; }
    static constexpr X86_64Reg rsi() { return 6; }
    static constexpr X86_64Reg rdi() { return 7; }
    static constexpr X86_64Reg r8() { return 8; }
    static constexpr X86_64Reg r9() { return 9; }
    static constexpr X86_64Reg r10() { return 10; }
    static constexpr X86_64Reg r11() { return 11; }
    static constexpr X86_64Reg r12() { return 12; }
    static constexpr X86_64Reg r13() { return 13; }
    static constexpr X86_64Reg r14() { return 14; }
    static constexpr X86_64Reg r15() { return 15; }

private:
    constexpr X86_64Reg(u8 value)
        : Assembler::Reg(value)
    {
    }
};

struct X86_64Assembler final : public Assembler {
    X86_64Assembler(Vector<u8>& output)
        : Assembler(output, "x86_64"sv)
    {
    }

    void shift_right(Operand dst, Operand count) override
    {
        VERIFY(dst.type == Operand::Type::Reg);
        VERIFY(count.type == Operand::Type::Imm);
        VERIFY(count.fits_in_u8());
        emit_rex_for_slash(dst, REX_W::Yes);
        emit8(0xc1);
        emit_modrm_slash(5, dst);
        emit8(count.offset_or_immediate);
    }

    void mov(Operand dst, Operand src, Patchable patchable = Patchable::No) override
    {
        if (dst.is_register_or_memory() && src.type == Operand::Type::Reg) {
            if (src.type == Operand::Type::Reg && src.reg == dst.reg)
                return;
            emit_rex_for_mr(dst, src, REX_W::Yes);
            emit8(0x89);
            emit_modrm_mr(dst, src, patchable);
            return;
        }

        if (dst.type == Operand::Type::Reg && src.type == Operand::Type::Imm) {
            if (patchable == Patchable::No) {
                if (src.offset_or_immediate == 0) {
                    // xor dst, dst
                    // Note: Operand size does not matter here, as the result is 0-extended to 64 bit,
                    //       so we don't have to set the W flag in the REX prefix,
                    //       or use it at all in case we dont use REX addressed registers (elision is implemented in the helper)
                    emit_rex_for_mr(dst, dst, REX_W::No);
                    emit8(0x31);
                    emit_modrm_mr(dst, dst, patchable);
                    return;
                }
                if (src.fits_in_u32()) {
                    emit_rex_for_OI(dst, REX_W::No);
                    emit8(0xb8 | encode_reg(dst.reg));
                    emit32(src.offset_or_immediate);
                    return;
                }
            }
            emit_rex_for_OI(dst, REX_W::Yes);
            emit8(0xb8 | encode_reg(dst.reg));
            emit64(src.offset_or_immediate);
            return;
        }

        if (dst.type == Operand::Type::Reg && src.is_register_or_memory()) {
            emit_rex_for_rm(dst, src, REX_W::Yes);
            emit8(0x8b);
            emit_modrm_rm(dst, src, patchable);
            return;
        }

        VERIFY_NOT_REACHED();
    }

    [[nodiscard]] Label jump() override
    {
        // jmp target (RIP-relative 32-bit offset)
        emit8(0xe9);
        emit32(0xdeadbeef);
        Assembler::Label label {};
        label.add_jump(*this, m_output.size());
        return label;
    }

    void jump(Label& label) override
    {
        // jmp target (RIP-relative 32-bit offset)
        emit8(0xe9);
        emit32(0xdeadbeef);
        label.add_jump(*this, m_output.size());
    }

    void jump(Operand op) override
    {
        emit_rex_for_slash(op, REX_W::No);
        emit8(0xff);
        emit_modrm_slash(4, op);
    }

    void verify_not_reached() override
    {
        // ud2
        emit8(0x0f);
        emit8(0x0b);
    }

    void cmp(Operand lhs, Operand rhs) override
    {
        if (lhs.type == Operand::Type::Reg && rhs.type == Operand::Type::Imm && rhs.offset_or_immediate == 0) {
            test(lhs, lhs);
        } else if (lhs.is_register_or_memory() && rhs.type == Operand::Type::Reg) {
            emit_rex_for_mr(lhs, rhs, REX_W::Yes);
            emit8(0x39);
            emit_modrm_mr(lhs, rhs);
        } else if (lhs.is_register_or_memory() && rhs.type == Operand::Type::Imm && rhs.fits_in_i8()) {
            emit_rex_for_slash(lhs, REX_W::Yes);
            emit8(0x83);
            emit_modrm_slash(7, lhs);
            emit8(rhs.offset_or_immediate);
        } else if (lhs.is_register_or_memory() && rhs.type == Operand::Type::Imm && rhs.fits_in_i32()) {
            emit_rex_for_slash(lhs, REX_W::Yes);
            emit8(0x81);
            emit_modrm_slash(7, lhs);
            emit32(rhs.offset_or_immediate);
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void test(Operand lhs, Operand rhs) override
    {
        if (lhs.is_register_or_memory() && rhs.type == Operand::Type::Reg) {
            emit_rex_for_mr(lhs, rhs, REX_W::Yes);
            emit8(0x85);
            emit_modrm_mr(lhs, rhs);
        } else if (lhs.type != Operand::Type::Imm && rhs.type == Operand::Type::Imm) {
            VERIFY(rhs.fits_in_i32());
            emit_rex_for_slash(lhs, REX_W::Yes);
            emit8(0xf7);
            emit_modrm_slash(0, lhs);
            emit32(rhs.offset_or_immediate);
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void jump_if(Operand lhs, Condition condition, Operand rhs, Label& label) override
    {
        cmp(lhs, rhs);

        emit8(0x0F);
        emit8(0x80 | to_underlying(condition));
        emit32(0xdeadbeef);
        label.add_jump(*this, m_output.size());
    }

    void sign_extend_32_to_64_bits(Reg reg) override
    {
        // movsxd (reg as 64-bit), (reg as 32-bit)
        emit_rex_for_rm(Operand::Register(reg), Operand::Register(reg), REX_W::Yes);
        emit8(0x63);
        emit_modrm_rm(Operand::Register(reg), Operand::Register(reg));
    }

    void bitwise_and(Operand dst, Operand src) override
    {
        // and dst,src
        if (dst.is_register_or_memory() && src.type == Operand::Type::Reg) {
            emit_rex_for_mr(dst, src, REX_W::Yes);
            emit8(0x21);
            emit_modrm_mr(dst, src);
        } else if (dst.type == Operand::Type::Reg && src.type == Operand::Type::Imm && src.fits_in_i8()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x83);
            emit_modrm_slash(4, dst);
            emit8(src.offset_or_immediate);
        } else if (dst.type == Operand::Type::Reg && src.type == Operand::Type::Imm && src.fits_in_i32()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x81);
            emit_modrm_slash(4, dst);
            emit32(src.offset_or_immediate);
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void bitwise_or(Operand dst, Operand src) override
    {
        // or dst,src
        if (dst.is_register_or_memory() && src.type == Operand::Type::Reg) {
            emit_rex_for_mr(dst, src, REX_W::Yes);
            emit8(0x09);
            emit_modrm_mr(dst, src);
        } else if (dst.type == Operand::Type::Reg && src.type == Operand::Type::Imm && src.fits_in_i8()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x83);
            emit_modrm_slash(1, dst);
            emit8(src.offset_or_immediate);
        } else if (dst.type == Operand::Type::Reg && src.type == Operand::Type::Imm && src.fits_in_i32()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x81);
            emit_modrm_slash(1, dst);
            emit32(src.offset_or_immediate);
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void enter() override
    {
        push_callee_saved_registers();

        push(Operand::Register(X86_64Reg::rbp()));
        mov(Operand::Register(X86_64Reg::rbp()), Operand::Register(X86_64Reg::rsp()));
    }

    void exit() override
    {
        // leave
        emit8(0xc9);

        pop_callee_saved_registers();

        // ret
        emit8(0xc3);
    }

    void push_callee_saved_registers() override
    {
        // FIXME: Don't push RBX twice :^)
        push(Operand::Register(X86_64Reg::rbx()));
        push(Operand::Register(X86_64Reg::rbx()));
        push(Operand::Register(X86_64Reg::r12()));
        push(Operand::Register(X86_64Reg::r13()));
        push(Operand::Register(X86_64Reg::r14()));
        push(Operand::Register(X86_64Reg::r15()));
    }

    void pop_callee_saved_registers() override
    {
        pop(Operand::Register(X86_64Reg::r15()));
        pop(Operand::Register(X86_64Reg::r14()));
        pop(Operand::Register(X86_64Reg::r13()));
        pop(Operand::Register(X86_64Reg::r12()));

        // FIXME: Don't pop RBX twice :^)
        pop(Operand::Register(X86_64Reg::rbx()));
        pop(Operand::Register(X86_64Reg::rbx()));
    }

    void push(Operand op) override
    {
        if (op.type == Operand::Type::Reg) {
            emit_rex_for_OI(op, REX_W::No);
            emit8(0x50 | encode_reg(op.reg));
        } else if (op.type == Operand::Type::Imm) {
            if (op.fits_in_i8()) {
                emit8(0x6a);
                emit8(op.offset_or_immediate);
            } else if (op.fits_in_i32()) {
                emit8(0x68);
                emit32(op.offset_or_immediate);
            } else {
                VERIFY_NOT_REACHED();
            }
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void pop(Operand op) override
    {
        if (op.type == Operand::Type::Reg) {
            emit_rex_for_OI(op, REX_W::No);
            emit8(0x58 | encode_reg(op.reg));
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void add(Operand dst, Operand src) override
    {
        if (dst.is_register_or_memory() && src.type == Operand::Type::Reg) {
            emit_rex_for_mr(dst, src, REX_W::Yes);
            emit8(0x01);
            emit_modrm_mr(dst, src);
        } else if (dst.is_register_or_memory() && src.type == Operand::Type::Imm && src.fits_in_i8()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x83);
            emit_modrm_slash(0, dst);
            emit8(src.offset_or_immediate);
        } else if (dst.is_register_or_memory() && src.type == Operand::Type::Imm && src.fits_in_i32()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x81);
            emit_modrm_slash(0, dst);
            emit32(src.offset_or_immediate);
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void add32(Operand dst, Operand src, Optional<Label&> label) override
    {
        if (dst.is_register_or_memory() && src.type == Operand::Type::Reg) {
            emit_rex_for_mr(dst, src, REX_W::No);
            emit8(0x01);
            emit_modrm_mr(dst, src);
        } else if (dst.is_register_or_memory() && src.type == Operand::Type::Imm && src.fits_in_i8()) {
            emit_rex_for_slash(dst, REX_W::No);
            emit8(0x83);
            emit_modrm_slash(0, dst);
            emit8(src.offset_or_immediate);
        } else if (dst.is_register_or_memory() && src.type == Operand::Type::Imm && src.fits_in_i32()) {
            emit_rex_for_slash(dst, REX_W::No);
            emit8(0x81);
            emit_modrm_slash(0, dst);
            emit32(src.offset_or_immediate);
        } else {
            VERIFY_NOT_REACHED();
        }

        if (label.has_value()) {
            // jo label (RIP-relative 32-bit offset)
            emit8(0x0f);
            emit8(0x80);
            emit32(0xdeadbeef);
            label->add_jump(*this, m_output.size());
        }
    }

    void sub(Operand dst, Operand src) override
    {
        if (dst.is_register_or_memory() && src.type == Operand::Type::Reg) {
            emit_rex_for_mr(dst, src, REX_W::Yes);
            emit8(0x29);
            emit_modrm_mr(dst, src);
        } else if (dst.is_register_or_memory() && src.type == Operand::Type::Imm && src.fits_in_i8()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x83);
            emit_modrm_slash(5, dst);
            emit8(src.offset_or_immediate);
        } else if (dst.is_register_or_memory() && src.type == Operand::Type::Imm && src.fits_in_i32()) {
            emit_rex_for_slash(dst, REX_W::Yes);
            emit8(0x81);
            emit_modrm_slash(5, dst);
            emit32(src.offset_or_immediate);
        } else {
            VERIFY_NOT_REACHED();
        }
    }

    void native_call(
        void* callee,
        Vector<Operand> const& preserved_registers = {},
        Vector<Operand> const& stack_arguments = {})
        override
    {
        for (auto const& reg : preserved_registers.in_reverse())
            push(reg);

        // Preserve 16-byte stack alignment for non-even amount of stack-passed arguments
        auto const needs_aligning = ((stack_arguments.size() + preserved_registers.size()) % 2) == 1;
        if (needs_aligning)
            push(Operand::Imm(0));
        for (auto const& stack_argument : stack_arguments.in_reverse())
            push(stack_argument);

        // load callee into RAX
        mov(Operand::Register(X86_64Reg::rax()), Operand::Imm(bit_cast<u64>(callee)));

        // call RAX
        emit8(0xff);
        emit_modrm_slash(2, Operand::Register(X86_64Reg::rax()));

        if (!stack_arguments.is_empty() || needs_aligning)
            add(Operand::Register(X86_64Reg::rsp()), Operand::Imm((stack_arguments.size() + (needs_aligning ? 1 : 0)) * sizeof(void*)));

        for (auto const& reg : preserved_registers)
            pop(reg);
    }

    void trap() override
    {
        // int3
        emit8(0xcc);
    }

private:
    void link_jump(Label const& label, size_t offset_in_instruction_stream) override
    {
        auto offset = label.offset_of_label_in_instruction_stream.value() - offset_in_instruction_stream;
        auto jump_slot = offset_in_instruction_stream - 4;
        m_output[jump_slot + 0] = (offset >> 0) & 0xff;
        m_output[jump_slot + 1] = (offset >> 8) & 0xff;
        m_output[jump_slot + 2] = (offset >> 16) & 0xff;
        m_output[jump_slot + 3] = (offset >> 24) & 0xff;
    }

    static inline constexpr u8 encode_reg(Reg reg)
    {
        return reg.value() & 0x7;
    }

    union ModRM {
        static constexpr u8 Mem = 0b00;
        static constexpr u8 MemDisp8 = 0b01;
        static constexpr u8 MemDisp32 = 0b10;
        static constexpr u8 Reg = 0b11;
        struct {
            u8 rm : 3;
            u8 reg : 3;
            u8 mode : 2;
        };
        u8 raw;
    };

    void emit_modrm_slash(u8 slash, Operand rm, Patchable patchable = Patchable::No)
    {
        ModRM raw;
        raw.rm = encode_reg(rm.reg);
        raw.reg = slash;
        emit_modrm(raw, rm, patchable);
    }

    void emit_modrm_rm(Operand dst, Operand src, Patchable patchable = Patchable::No)
    {
        VERIFY(dst.type == Operand::Type::Reg);
        ModRM raw {};
        raw.reg = encode_reg(dst.reg);
        raw.rm = encode_reg(src.reg);
        emit_modrm(raw, src, patchable);
    }

    void emit_modrm_mr(Operand dst, Operand src, Patchable patchable = Patchable::No)
    {
        VERIFY(src.type == Operand::Type::Reg);
        ModRM raw {};
        raw.reg = encode_reg(src.reg);
        raw.rm = encode_reg(dst.reg);
        emit_modrm(raw, dst, patchable);
    }

    void emit_modrm(ModRM raw, Operand rm, Patchable patchable)
    {
        // FIXME: rm:100 (RSP) is reserved as the SIB marker
        VERIFY(rm.type != Operand::Type::Imm);

        switch (rm.type) {
        case Operand::Type::Reg:
            // FIXME: There is mod:00,rm:101(EBP?) -> disp32, that might be something else
            raw.mode = ModRM::Reg;
            emit8(raw.raw);
            break;
        case Operand::Type::Mem64BaseAndOffset: {
            auto disp = rm.offset_or_immediate;
            // We want to emit 32-bit displacement when it's meant to be patchable,
            // or when displacement is outside of [-128, 127] range.
            if (patchable == Patchable::Yes || static_cast<i64>(disp) < -128 || disp > 127) {
                raw.mode = ModRM::MemDisp32;
                emit8(raw.raw);
                emit32(disp);
            } else if (disp == 0) {
                raw.mode = ModRM::Mem;
                emit8(raw.raw);
            } else {
                raw.mode = ModRM::MemDisp8;
                emit8(raw.raw);
                emit8(disp & 0xff);
            }
            break;
        }
        case Operand::Type::Imm:
            VERIFY_NOT_REACHED();
        }
    }

    union REX {
        struct {
            u8 B : 1; // ModRM::RM
            u8 X : 1; // SIB::Index
            u8 R : 1; // ModRM::Reg
            u8 W : 1; // Operand size override
            u8 _ : 4 { 0b0100 };
        };
        u8 raw;
    };

    enum class REX_W : u8 {
        No = 0,
        Yes = 1
    };

    void emit_rex_for_OI(Operand arg, REX_W W)
    {
        emit_rex_for_slash(arg, W);
    }

    void emit_rex_for_slash(Operand arg, REX_W W)
    {
        VERIFY(arg.is_register_or_memory());

        if (W == REX_W::No && arg.reg.value() < 8)
            return;

        REX const rex {
            .B = static_cast<u8>(arg.reg.value() >= 8),
            .X = 0,
            .R = 0,
            .W = to_underlying(W)
        };
        emit8(rex.raw);
    }

    void emit_rex_for_mr(Operand dst, Operand src, REX_W W)
    {
        VERIFY(dst.is_register_or_memory());
        VERIFY(src.type == Operand::Type::Reg);
        if (W == REX_W::No && dst.reg.value() < 8 && src.reg.value() < 8)
            return;
        REX const rex {
            .B = static_cast<u8>(dst.reg.value() >= 8),
            .X = 0,
            .R = static_cast<u8>(src.reg.value() >= 8),
            .W = to_underlying(W)
        };
        emit8(rex.raw);
    }

    void emit_rex_for_rm(Operand dst, Operand src, REX_W W)
    {
        VERIFY(src.is_register_or_memory());
        VERIFY(dst.type == Operand::Type::Reg);
        if (W == REX_W::No && dst.reg.value() < 8 && src.reg.value() < 8)
            return;
        REX const rex {
            .B = static_cast<u8>(src.reg.value() >= 8),
            .X = 0,
            .R = static_cast<u8>(dst.reg.value() >= 8),
            .W = to_underlying(W)
        };
        emit8(rex.raw);
    }
};

}
