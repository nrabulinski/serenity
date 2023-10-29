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

namespace JIT {

struct Assembler {
    Assembler(Vector<u8>& output, StringView const arch)
        : arch(arch)
        , m_output(output)
    {
    }

    const StringView arch;

    virtual ~Assembler() = default;

    class Reg {
        u8 m_value;

    protected:
        constexpr Reg(u8 value)
            : m_value(value)
        {
        }

    public:
        static constexpr Reg empty()
        {
            return Reg(-1);
        }

        constexpr u8 value() const { return m_value; }

        constexpr int operator<=>(Reg const& other) const
        {
            return m_value > other.m_value
                ? 1
                : m_value < other.m_value
                ? -1
                : 0;
        }

        bool operator==(Reg const&) const = default;
        bool operator!=(Reg const&) const = default;
        bool operator<(Reg const&) const = default;
        bool operator<=(Reg const&) const = default;
        bool operator>(Reg const&) const = default;
        bool operator>=(Reg const&) const = default;
    };

    Vector<u8>& m_output;

    struct Operand {
        enum class Type {
            Reg,
            Imm,
            Mem64BaseAndOffset,
        };

        Type type {};

        Reg reg = Reg::empty();
        u64 offset_or_immediate { 0 };

        static Operand Register(Reg reg)
        {
            Operand operand;
            operand.type = Type::Reg;
            operand.reg = reg;
            return operand;
        }

        static Operand Imm(u64 imm)
        {
            Operand operand;
            operand.type = Type::Imm;
            operand.offset_or_immediate = imm;
            return operand;
        }

        static Operand Mem64BaseAndOffset(Reg base, u64 offset)
        {
            Operand operand;
            operand.type = Type::Mem64BaseAndOffset;
            operand.reg = base;
            operand.offset_or_immediate = offset;
            return operand;
        }

        bool is_register_or_memory() const
        {
            return type == Type::Reg || type == Type::Mem64BaseAndOffset;
        }

        bool fits_in_u8() const
        {
            VERIFY(type == Type::Imm);
            return offset_or_immediate <= NumericLimits<u8>::max();
        }
        bool fits_in_u32() const
        {
            VERIFY(type == Type::Imm);
            return offset_or_immediate <= NumericLimits<u32>::max();
        }
        bool fits_in_i8() const
        {
            VERIFY(type == Type::Imm);
            return (offset_or_immediate <= NumericLimits<i8>::max()) || (((~offset_or_immediate) & NumericLimits<i8>::min()) == 0);
        }
        bool fits_in_i32() const
        {
            VERIFY(type == Type::Imm);
            return (offset_or_immediate <= NumericLimits<i32>::max()) || (((~offset_or_immediate) & NumericLimits<i32>::min()) == 0);
        }
    };

    enum class Condition {
        EqualTo = 0x4,
        NotEqualTo = 0x5,
        UnsignedGreaterThan = 0x7,
        UnsignedGreaterThanOrEqualTo = 0x3,
        UnsignedLessThan = 0x2,
        UnsignedLessThanOrEqualTo = 0x6,
        SignedGreaterThan = 0xF,
        SignedGreaterThanOrEqualTo = 0xD,
        SignedLessThan = 0xC,
        SignedLessThanOrEqualTo = 0xE,
    };

    virtual void shift_right(Operand dst, Operand count) = 0;

    enum class Patchable {
        Yes,
        No,
    };

    virtual void mov(Operand dst, Operand src, Patchable patchable = Patchable::No) = 0;

    void emit8(u8 value)
    {
        m_output.append(value);
    }

    void emit32(u32 value)
    {
        m_output.append((value >> 0) & 0xff);
        m_output.append((value >> 8) & 0xff);
        m_output.append((value >> 16) & 0xff);
        m_output.append((value >> 24) & 0xff);
    }

    void emit64(u64 value)
    {
        m_output.append((value >> 0) & 0xff);
        m_output.append((value >> 8) & 0xff);
        m_output.append((value >> 16) & 0xff);
        m_output.append((value >> 24) & 0xff);
        m_output.append((value >> 32) & 0xff);
        m_output.append((value >> 40) & 0xff);
        m_output.append((value >> 48) & 0xff);
        m_output.append((value >> 56) & 0xff);
    }

    struct Label {
        Optional<size_t> offset_of_label_in_instruction_stream;
        Vector<size_t> jump_slot_offsets_in_instruction_stream;

        void add_jump(Assembler& assembler, size_t offset)
        {
            jump_slot_offsets_in_instruction_stream.append(offset);
            if (offset_of_label_in_instruction_stream.has_value())
                link_jump(assembler, offset);
        }

        void link(Assembler& assembler)
        {
            link_to(assembler, assembler.m_output.size());
        }

        void link_to(Assembler& assembler, size_t link_offset)
        {
            VERIFY(!offset_of_label_in_instruction_stream.has_value());
            offset_of_label_in_instruction_stream = link_offset;
            for (auto offset_in_instruction_stream : jump_slot_offsets_in_instruction_stream)
                link_jump(assembler, offset_in_instruction_stream);
        }

    private:
        void link_jump(Assembler& assembler, size_t offset_in_instruction_stream) const
        {
            assembler.link_jump(*this, offset_in_instruction_stream);
        }
    };

    [[nodiscard]] virtual Label jump() = 0;

    virtual void jump(Label& label) = 0;

    virtual void jump(Operand op) = 0;

    virtual void verify_not_reached() = 0;

    virtual void cmp(Operand lhs, Operand rhs) = 0;

    virtual void test(Operand lhs, Operand rhs) = 0;

    virtual void jump_if(Operand lhs, Condition condition, Operand rhs, Label& label) = 0;

    virtual void sign_extend_32_to_64_bits(Reg reg) = 0;

    virtual void bitwise_and(Operand dst, Operand src) = 0;

    virtual void bitwise_or(Operand dst, Operand src) = 0;

    virtual void enter() = 0;

    virtual void exit() = 0;

    virtual void push_callee_saved_registers() = 0;

    virtual void pop_callee_saved_registers() = 0;

    virtual void push(Operand op) = 0;

    virtual void pop(Operand op) = 0;

    virtual void add(Operand dst, Operand src) = 0;

    virtual void add32(Operand dst, Operand src, Optional<Label&> label) = 0;

    virtual void sub(Operand dst, Operand src) = 0;

    virtual void native_call(
        void* callee,
        Vector<Operand> const& preserved_registers = {},
        Vector<Operand> const& stack_arguments = {})
        = 0;

    virtual void trap() = 0;

private:
    virtual void link_jump(Label const& label, size_t offset_in_instruction_stream) = 0;
};

OwnPtr<Assembler> assembler_for_current_arch(Vector<u8>&);

}
