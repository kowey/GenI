#!/bin/bash
chmod u+x $(dirname $0)/clean-mac.sh
$(dirname $0)/clean-mac.sh

gcc test-c.c -o test-c.s -I .\
  -march=i686 -m32 -fno-stack-protector -DDONT_WANT_WIN32_DLL_SUPPORT\
  -mdynamic-no-pic -S\
  -Wimplicit -O -D__GLASGOW_HASKELL__=700 -DTABLES_NEXT_TO_CODE

gcc      -c test-c.s -o test-c.o\
  -march=i686 -m32 -fno-stack-protector -DDONT_WANT_WIN32_DLL_SUPPORT

gcc -o test-c2 test-c.o MinimalGenI.dylib\
 -march=i686 -m32 -fno-stack-protector -DDONT_WANT_WIN32_DLL_SUPPORT\
 -lm -ldl -liconv\
 -L.\
 -lHSrts -lHSrtsmain -lHSffi\
 -lHSbase-4.3.1.0 -lHSinteger-gmp-0.2.0.3 -lHSghc-prim-0.2.0.0\
 -u _ghczmprim_GHCziTypes_Izh_static_info \
 -u _ghczmprim_GHCziTypes_Czh_static_info \
 -u _ghczmprim_GHCziTypes_Fzh_static_info \
 -u _ghczmprim_GHCziTypes_Dzh_static_info \
 -u _base_GHCziPtr_Ptr_static_info \
 -u _base_GHCziWord_Wzh_static_info \
 -u _base_GHCziInt_I8zh_static_info \
 -u _base_GHCziInt_I16zh_static_info \
 -u _base_GHCziInt_I32zh_static_info \
 -u _base_GHCziInt_I64zh_static_info \
 -u _base_GHCziWord_W8zh_static_info \
 -u _base_GHCziWord_W16zh_static_info \
 -u _base_GHCziWord_W32zh_static_info \
 -u _base_GHCziWord_W64zh_static_info \
 -u _base_GHCziStable_StablePtr_static_info \
 -u _ghczmprim_GHCziTypes_Izh_con_info \
 -u _ghczmprim_GHCziTypes_Czh_con_info \
 -u _ghczmprim_GHCziTypes_Fzh_con_info \
 -u _ghczmprim_GHCziTypes_Dzh_con_info \
 -u _base_GHCziPtr_Ptr_con_info \
 -u _base_GHCziPtr_FunPtr_con_info \
 -u _base_GHCziStable_StablePtr_con_info \
 -u _ghczmprim_GHCziBool_False_closure \
 -u _ghczmprim_GHCziBool_True_closure \
 -u _base_GHCziPack_unpackCString_closure \
 -u _base_GHCziIOziException_stackOverflow_closure \
 -u _base_GHCziIOziException_heapOverflow_closure \
 -u _base_ControlziExceptionziBase_nonTermination_closure \
 -u _base_GHCziIOziException_blockedIndefinitelyOnMVar_closure \
 -u _base_GHCziIOziException_blockedIndefinitelyOnSTM_closure \
 -u _base_ControlziExceptionziBase_nestedAtomically_closure \
 -u _base_GHCziWeak_runFinalizzerBatch_closure \
 -u _base_GHCziTopHandler_runIO_closure \
 -u _base_GHCziTopHandler_runNonIO_closure \
 -u _base_GHCziConcziIO_ensureIOManagerIsRunning_closure \
 -u _base_GHCziConcziSync_runSparks_closure \
 -u _base_GHCziConcziSignal_runHandlers_closure -Wl,-search_paths_first -read_only_relocs warning

export DYLD_LIBRARY_PATH=$(pwd)
./test-c2 ej/macros ej/lexicon ej/sem
