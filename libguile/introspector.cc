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
};

extern "C" {
  //#include "stacks.h"
#include "strings.h"
#include "symbols.h"
#include "vports.h"
#include "numbers.h"
#include "posix.h"
#include "print.h"
#include "scm.h"
#include "intrinsics.h"
#include "loader.h"
}
//#include "exceptions.h"
//#include "vm-operationss.h"

union scm_vm_stack_element
{
  uintptr_t as_uint;
  uint32_t *as_vcode;
  uint8_t *as_mcode;
  SCM as_scm;
  double as_f64;
  uint64_t as_u64;
  int64_t as_s64;

  /* For GC purposes.  */
  void *as_ptr;
  scm_t_bits as_bits;
};


REFL_TYPE( scm_vm_stack_element)
//REFL_FIELD( as_uint) //: usize,
// REFL_FIELD( as_vcode)//: *mut u32,
// REFL_FIELD(as_mcode)//: *mut u8,
// REFL_FIELD(as_scm) //: SCM,
// REFL_FIELD( as_f64)//: f64,
// REFL_FIELD( as_u64)//: u64,
// REFL_FIELD(as_s64) //: i64,
// REFL_FIELD(as_ptr)//: *mut ::std::os::raw::c_void,
// REFL_FIELD(as_bits)//: scm_t_bits,
//REFL_FIELD(_bindgen_union_align)//: u64,

REFL_END

REFL_TYPE( scm_vm)
//REFL_FIELD( ip) // *mut u32,
//REFL_FIELD( sp) // *mut scm_vm_stack_element,
//REFL_FIELD( fp) // *mut scm_vm_stack_element,
//REFL_FIELD( stack_limit) // *mut scm_vm_stack_element,
//REFL_FIELD( compare_result) // u8,
//REFL_FIELD( apply_hook_enabled) // u8,
//REFL_FIELD( return_hook_enabled) // u8,
//REFL_FIELD( next_hook_enabled) // u8,
//REFL_FIELD( abort_hook_enabled) // u8,
//REFL_FIELD( disable_mcode) // u8,
//REFL_FIELD( engine) // u8,
//REFL_FIELD( unused) // u8,
REFL_FIELD( stack_size) // size_t,
//REFL_FIELD( stack_bottom) // *mut scm_vm_stack_element,
// REFL_FIELD( apply_hook) // SCM,
// REFL_FIELD( return_hook) // SCM,
// REFL_FIELD( next_hook) // SCM,
// REFL_FIELD( abort_hook) // SCM,
//REFL_FIELD( stack_top) // *mut scm_vm_stack_element,
REFL_FIELD( overflow_handler_stack) // SCM,
//REFL_FIELD( registers) // *mut jmp_buf, // all different
//REFL_FIELD( mra_after_abort) // *mut u8,
//REFL_FIELD( trace_level ) //) // ::std::os::raw::c_int
REFL_END


//pub type scm_t_dynstack = scm_dynstack;
REFL_TYPE(scm_t_dynstack)

// pub type scm_t_bits = usize;

REFL_FIELD( base ) //: *mut scm_t_bits,
//REFL_FIELD(top) // *mut scm_t_bits, //0 
//REFL_FIELD(limit)// *mut scm_t_bits,

REFL_END



REFL_TYPE( __pthread_cond_s)
//REFL_FIELD( __bindgen_anon_1) //: __pthread_cond_s__bindgen_ty_1,
//REFL_FIELD( __bindgen_anon_2)//: __pthread_cond_s__bindgen_ty_2,
REFL_FIELD(__g_refs) // [::std::os::raw::c_uint; 2usize],
REFL_FIELD(__g_size) // [::std::os::raw::c_uint; 2usize],
REFL_FIELD( __g1_orig_size) //: ::std::os::raw::c_uint,
REFL_FIELD( __wrefs ) //: ::std::os::raw::c_uint,
REFL_FIELD( __g_signals) //: [::std::os::raw::c_uint; 2usize],
REFL_END

REFL_TYPE(scm_thread )
//REFL_FIELD(next_thread) //: *mut scm_thread,
REFL_FIELD( vm) // : scm_vm,
REFL_FIELD( pending_asyncs) // : SCM,
//REFL_FIELD( block_asyncs) // : ::std::os::raw::c_uint,
//REFL_FIELD( freelists) // : [*mut ::std::os::raw::c_void; 16usize],
REFL_FIELD( pointerless_freelists) // : [*mut ::std::os::raw::c_void; 16usize]
//REFL_FIELD( handle) // : SCM,
//REFL_FIELD( pthread) //: pthread_t,
//REFL_FIELD( result) // : SCM, 0x4
//REFL_FIELD( exited ) // ::std::os::raw::c_int,
//REFL_FIELD( guile_mode) // : ::std::os::raw::c_int,
REFL_FIELD( needs_unregister) // : ::std::os::raw::c_int,
//REFL_FIELD( wake) // : *mut scm_thread_wake_data, always 0
//REFL_FIELD( sleep_cond)// pthread_cond_t, not printable
//REFL_FIELD( sleep_pipe)// [::std::os::raw::c_int; 2usize],
//REFL_FIELD( dynamic_state)// *mut scm_t_dynamic_state,
//REFL_FIELD( dynstack)// scm_t_dynstack,
REFL_FIELD( continuation_root)// SCM,
REFL_FIELD( continuation_base)// *mut SCM_STACKITEM,
REFL_FIELD( base) //: *mut SCM_STACKITEM,
//REFL_FIELD( jit_state) // : *mut scm_jit_state,

REFL_END

template <
  class AnsibleLikeSSH,
  class TerraformInfrastructureCreation,
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
  class DockerLikeContainerization,
  class ProofInMetaCoqLikeSystem,
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


extern "C" {
  #include "ports.h"
#include "backtrace.h"

#include "introspector.h"
}

// void   scm_backtrace(){
//   scm_puts (";;; TRACE ", scm_current_warning_port ());
//   scm_print_exception (scm_current_warning_port (), SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F);
// }


void
spct_invoke_generic_hook (const char * name, scm_thread * thread) {
  std::cout << name << std::endl;
  
  refl::runtime::debug(std::cout,*thread);

  struct scm_vm *vp;
  vp = &thread->vm;

  auto port = scm_current_output_port ();

  ///  scm_backtrace();


  auto stack_depth = vp->stack_top - vp->sp;
  std::cout << "depth: " << stack_depth <<  std::endl;
  for (int i = 0; i < stack_depth; i++) {

    std::cout << " Item: " <<  i << ":";
    auto s  = vp->sp[i].as_scm;
    //std::cout << "unpack16:";
    //    scm_uintprint (SCM_UNPACK (s), 16, port);

    //std::cout << "DISP:";
    //scm_display (s, port);

    std::cout << "DEBUG:";
    refl::runtime::debug(std::cout,s);
    
    std::cout <<  std::endl;
  }
}

void
spct_invoke_apply_hook (scm_thread * thread) {
  spct_invoke_generic_hook ("apply", thread);
}

void
spct_invoke_return_hook (scm_thread * thread) {
  spct_invoke_generic_hook ("return", thread);
}

void
spct_invoke_next_hook (scm_thread * thread) {
  spct_invoke_generic_hook ("next", thread);
}

void
spct_invoke_abort_hook (scm_thread * thread) {
  spct_invoke_generic_hook ("abort", thread);
}
