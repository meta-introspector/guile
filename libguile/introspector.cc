#include "config.h"
/*
  c++ meta template programming magic for
  * extracting of named entities
  * extracting of contexts of graphs of morphisms surrounding those identifiers
  * feeding to LLM
 */

#include <refl-cpp/refl.hpp>
#include <iostream>

template <
  class CPPCompiler,
  class SchemeCompiler,
  class MachineArchitecture,
  class OS,
  class ConfigurationVariables,
  class Guix,
  class SelfReferencialTokens,
  class LexicalIntrospection,
  class Introspection,
  class GuileIntrospectorSelf
  > class GuileConfig {
#include "config.h"

  //#include "strings.h"
  //#include "symbols.h"
  //#include "vports.h"

};

extern "C" {
#include "config.h"
#include "strings.h"
#include "symbols.h"
#include "vports.h"
#include "numbers.h"
#include "posix.h"
#include "print.h"
#include "scm.h"
#include "intrinsics.h"
#include "loader.h"

#include "chars.h"
#include "deprecation.h"
#include "error.h"
#include "generalized-vectors.h"
#include "gsubr.h"
#include "numbers.h"
#include "pairs.h"
#include "ports-internal.h"
#include "ports.h"
#include "private-options.h"
#include "striconveh.h"
#include "symbols.h"
#include "threads.h"

#include "strings.h"
#include "vm.h"
#include "atomic.h"
#include "bytevectors.h"
#include "filesys.h"
#include "fluids.h"
#include "foreign.h"
#include "frames.h"
#include "hashtab.h"
#include "numbers.h"
#include "ports.h"
#include "programs.h"
#include "scm.h"
#include "smob.h"
#include "srfi-14.h"
#include "strings.h"
#include "symbols.h"
#include "values.h"
#include "variable.h"
#include "vectors.h"
#include "vm.h"
#include "weak-vector.h"
}

struct SCMField : refl::attr::usage::field { };

//#include "vm-operationss.h"
typedef void* test16 [16];
REFL_TYPE(test16)
REFL_END

typedef int test2 [2];
REFL_TYPE(test2)
REFL_END

REFL_TYPE(SCM)
//REFL_FIELD(n)
void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_t_subr)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_t_dynamic_state)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_print_state)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_t_dynstack)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

//REFL_TYPE(scm_t_bits)
//REFL_END

REFL_TYPE(scm_vm_stack_element)
//  void spct2(const scm_vm_stack_element & t){
  //  std::cerr << "DEBUG" << t;
//}
// REFL_FIELD(as_uint)
// REFL_FIELD(as_vcode)
// REFL_FIELD(as_mcode)
// REFL_FIELD( as_scm)
// REFL_FIELD(as_f64)
// REFL_FIELD( as_u64)
// REFL_FIELD( as_s64)
// REFL_FIELD(as_ptr)
// REFL_FIELD( as_bits)

REFL_END

//REFL_TYPE(scm_vm_frame_kind)
//REFL_END

REFL_TYPE(jmp_buf)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_vm)
  void spct(){  std::cerr << "DEBUG";}
REFL_FIELD( ip)/* instruction pointer */
REFL_FIELD(sp); /* stack pointer */
REFL_FIELD(fp); /* frame pointer */
REFL_FIELD(stack_limit); /* stack limit address */
REFL_FIELD(compare_result);       /* flags register: a value from scm_compare */
REFL_FIELD(apply_hook_enabled);   /* if apply hook is enabled */
REFL_FIELD(return_hook_enabled);  /* if return hook is enabled */
REFL_FIELD(next_hook_enabled);    /* if next hook is enabled */
REFL_FIELD(abort_hook_enabled);   /* if abort hook is enabled */
REFL_FIELD(disable_mcode);        /* if mcode is disabled (because debugging) */
REFL_FIELD(engine);               /* which vm engine we're using */
//REFL_FIELD(unused;               /* padding */
REFL_FIELD( stack_size);		/* stack size */
REFL_FIELD(stack_bottom); /* lowest address in allocated stack */
REFL_FIELD( apply_hook);               /* apply hook */
REFL_FIELD(return_hook);              /* return hook */
REFL_FIELD(next_hook);                /* next hook */
REFL_FIELD( abort_hook);               /* abort hook */
REFL_FIELD( stack_top); /* highest address in allocated stack */
REFL_FIELD( overflow_handler_stack);   /* alist of max-stack-size -> thunk */
REFL_FIELD(registers);           /* registers captured at latest vm entry  */
REFL_FIELD(mra_after_abort)     /* mra to resume after nonlocal exit, or NULL */
REFL_FIELD(trace_level)              /* traces enabled if trace_level > 0 */

REFL_END

REFL_TYPE(scm_frame)
void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_jit_state)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_thread_wake_data)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_vm_cont)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

//REFL_TYPE(SCM_STACKITEM)
//REFL_END

//REFL_TYPE(pthread_t)
//REFL_END

REFL_TYPE(  pthread_cond_t)
  void spct(){  std::cerr << "DEBUG";}
REFL_END


REFL_TYPE(scm_thread)
  void spct(){  std::cerr << "DEBUG";}
REFL_FIELD( next_thread)
REFL_FIELD( vm)
REFL_FIELD( pending_asyncs)
REFL_FIELD( block_asyncs)
REFL_FIELD(freelists)
REFL_FIELD(pointerless_freelists)
REFL_FIELD( handle)
REFL_FIELD(pthread)
REFL_FIELD(result)
REFL_FIELD(exited)
REFL_FIELD(guile_mode);
REFL_FIELD(needs_unregister);
REFL_FIELD(wake)
REFL_FIELD(sleep_cond)
REFL_FIELD(sleep_pipe)
REFL_FIELD(dynamic_state)
REFL_FIELD(dynstack)
REFL_FIELD(continuation_root)
REFL_FIELD(continuation_base)
REFL_FIELD(base)
REFL_FIELD(jit_state)

REFL_END

REFL_TYPE(scm_t_c_hook_type)
void spct(){  std::cerr << "DEBUG";}
REFL_END

//REFL_TYPE(scm_t_subr) (int)
//REFL_END

REFL_TYPE(scm_t_c_hook_function)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

REFL_TYPE(scm_t_c_hook_entry)
  void spct(){  std::cerr << "DEBUG";}
REFL_END

template <
  class Ansible,
  class Terraform,
  class NeuralNet,
  class LLM,
  class Graph,
  class Storage,
  class Linux,
  //  class GNUCompiler,
  //  class LLVMCompiler,
  class CPPCompiler,
  class SchemeCompiler,
  class SchemeVirtualMachine,
  class MachineArchitecture,
  class VirtualMachine,
  class Guix,
  class GuixBuildSystem,
  class GuixBuildSystemPyProject,
  class GuixBuildSystemGNU,
  class Docker,
  class Containerization,
  class Proof,
  class MonteCarloTreeSearch,
  class TreeSearch,
  class PolicyGraph,
  class ValueGraph,
  class Identifiers,
  class Mathematics,
  class Topology,
  class AbstractAlgebra,
  class AbstractDataClasss,
  class AbstractSyntaxTrees,
  class ClassDeclarations,
  class ClassClassDeclarations,
  class TemplateClassClassDeclarations,
  class TemplateClassClassDeclarationParameters,
  class SelfReferencialTemplateClassClassDeclarationParameters,
  class SelfReferencialTokens,
  class LexicalIntrospection,
  class Introspection,
  class Observe,
  class Orient,
  class Decide,
  class Act,
  class Life,
  class LifeSimulation,
  class UniverseSimulation,
  class UniverseOfUniverses,
  class Unimath,
  class Researcher,
  class Agent,
  class Swarm,
  class GuileIntrospectorSelf
  > class GuileIntrospector {

};

template<class T>
void spct(const T& t);

#define DEBUG_TYPE(X) else if (X (c)) {std::cerr << "SWITCH " #X ":\n";}
template<>void spct<scm_thread>(scm_thread const&){}
template<>void spct<scm_unused_struct*>(scm_unused_struct* const& c){
  // this is the SCM
  if (scm_is_string (c)) {
    char * str = scm_to_locale_string((SCM)(void*)c);
    std::cerr << "DEBUG STR: " << str << "\n";
  }
  DEBUG_TYPE(scm_is_bytevector)
    DEBUG_TYPE(scm_is_vector)
    //    DEBUG_TYPE(scm_is_exact)
    //    DEBUG_TYPE(scm_is_inexact)
    DEBUG_TYPE(scm_is_integer)
    DEBUG_TYPE(scm_is_exact_integer)
    //    DEBUG_TYPE(scm_is_signed_integer)
    //    DEBUG_TYPE(scm_is_unsigned_integer)  uintmax_t, uintmax_t)’
    DEBUG_TYPE(scm_is_real)
    DEBUG_TYPE(scm_is_rational)
    DEBUG_TYPE(scm_is_complex)
    DEBUG_TYPE(scm_is_number)
    DEBUG_TYPE(scm_is_bool)
    DEBUG_TYPE(scm_is_pair  )
    DEBUG_TYPE(scm_is_mutable_pair   )
    DEBUG_TYPE(scm_is_array   )
    //    DEBUG_TYPE(scm_is_typed_array ) 
    DEBUG_TYPE(scm_is_bytevector)
    //    DEBUG_TYPE(SCM_DIR)
    //    DEBUG_TYPE(SCM_FLUID)
    //    DEBUG_TYPE(SCM_POINTER_P)
    //    DEBUG_TYPE(SCM_VM_FRAME_P)
    DEBUG_TYPE(SCM_HASHTABLE_P)
    DEBUG_TYPE(SCM_REALP)
    DEBUG_TYPE(SCM_COMPLEXP)
    DEBUG_TYPE(SCM_BIGP)
    DEBUG_TYPE(SCM_NUMP)
    DEBUG_TYPE(SCM_FRACTIONP)
    DEBUG_TYPE(SCM_PORTP)
    DEBUG_TYPE(SCM_PROGRAM_P)
    DEBUG_TYPE(SCM_CHARSETP)
    DEBUG_TYPE(scm_is_symbol)
    DEBUG_TYPE(SCM_VARIABLEP)
    DEBUG_TYPE(SCM_I_IS_VECTOR)
    DEBUG_TYPE(SCM_VM_CONT_P)
    DEBUG_TYPE(SCM_I_WVECTP)    
  else {
    std::cerr << "DEBUG OTHER Type: " << c;
      if (SCM_NIMP(c)){
	std::cerr << " type:"   << SCM_CELL_TYPE (c);
	std::cerr << " unpack:" << SCM_UNPACK (c) << "\n";
      }
  }
  //  typedef struct scm_unused_struct { char scm_unused_field; } *SCM;
}
template<>void spct<scm_vm_stack_element>(scm_vm_stack_element const&){}
template<>void spct<scm_vm>(scm_vm const&){}


template<typename T>
void print_fields(const T& t) {
  //runtime2::debug(std::cerr, t);
  constexpr auto type = refl::reflect<T>();
  spct<T>(t);;
  constexpr auto membertype = refl::member_list<T>();

  constexpr auto members = get_members(type);
  std::cerr << "NODE Type: " << type.name.c_str() << " VALUE=";
  refl::runtime::debug(std::cerr, t);;
  std::cerr  << "\n";
  // std::cerr << "\nDEBUG Type2: " << typeid(membertype).name() << "\n";
  // std::cerr << "DEBUG Type3: " << typeid(members).name() << "\n";


  
  // refl::util::for_each(members, [&](auto member, [[maybe_unused]] auto index) {

  //   //refl::descriptor::has_attribute<mylib::PrimaryKey>(member);
  //   //std::cout << member.name.str() << (is_pk ? "(Primary Key)\n" : "\n");


  //      //using member_t = decltype(member::value_type);
  //      //typename type3 = member::value_type;
  //      //typename trait::remove_qualifiers_t<member_t>::value_type>;
  //      //constexpr auto type2 = refl::reflect(type3);
  // 	 //std::cerr  << "Auto:" << foo <<"\n";
  //   std::cerr  << "Auto: " << member.name<< "\n";
  //   auto member_val = member(t);
  //   print_fields(member_val);
  //   //std::cerr << member_val
  //   //refl::runtime::debug(std::cerr, member_val);;
  //   std::cerr <<"\n";
  //      //std::cerr << "DEBUG Typea2: " << typeid(member_t).name() << "\n";
  //     //std::cerr << "DEBUG Type2: " << type2.name.c_str() << "\n";
  //    });
     std::cerr << "\n";
}

extern "C" void
spct_scm_call_n (
		 SCM proc,
		 SCM *argv,
		 size_t nargs,
		 SCM ret,
		 struct scm_vm *vp,
		 scm_thread * thread,
		 union scm_vm_stack_element *call_fp,
		 union scm_vm_stack_element *return_fp
		 )
{
  std::cerr << "(spct_scm_call_n proc=";  
  print_fields(proc);
  for (int i = 0; i < nargs; i++) {
    std::cerr << "arg_" << i << "=";
    print_fields(argv[i]);
  }
  std::cerr << "ret=";
  print_fields(ret);
  std::cerr << "vp=";
  print_fields(*vp);
  std::cerr << "thread=";
  print_fields(*thread);
  std::cerr << "call_fp=";
  print_fields(*call_fp);
  std::cerr << "return_fp=";
  print_fields(*return_fp);
  std::cerr << "spct_scm_call_n_DONE )";
}
