#include "test.h"

static float data[] = { -1.0, 0.0, 0.5 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] =
    { JIT_ARG_ABI_INTMAX, JIT_ARG_ABI_FLOAT };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] =
    { { .gpr=JIT_R2 }, { .fpr=JIT_F0 } };

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);

  jit_stxi_f(j, (uintptr_t)data, JIT_R2, JIT_F0);
  jit_ret(j);

  void (*f)(intmax_t, float) = jit_end(j, NULL);

  ASSERT(data[0] == -1.0f);
  ASSERT(data[1] == 0.0f);
  ASSERT(data[2] == 0.5f);
  f(4, 42.5f);
  ASSERT(data[0] == -1.0f);
  ASSERT(data[1] == 42.5f);
  ASSERT(data[2] == 0.5f);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
