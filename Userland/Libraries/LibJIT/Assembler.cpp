/*
 * Copyright (c) 2023, Andreas Kling <kling@serenityos.org>
 * Copyright (c) 2023, Nikodem Rabulinski <1337-serenity@nrab.lol>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/OwnPtr.h>
#include <LibJIT/Assembler.h>
#include <LibJIT/X86_64/Assembler.h>

namespace JIT {

OwnPtr<Assembler> assembler_for_current_arch([[maybe_unused]] Vector<u8>& output)
{
#if ARCH(X86_64)
    return make<X86_64::X86_64Assembler>(output);
#else
    return {};
#endif
}

}
