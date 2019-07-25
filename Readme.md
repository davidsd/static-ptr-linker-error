If one attempts to compile this executable with `stack build`, the
result is a linker error:

    .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/linker-error-exec/linker-error-exec-tmp/Main.o:ghc_5.c:function hs_spt_init_Main: error: undefined reference to 'rcs4_closure'
collect2: error: ld returned 1 exit status
    `gcc' failed in phase `Linker'. (Exit code: 1)

    --  While building custom Setup.hs for package static-ptr-linker-error-0.1.0.0 using:
          /central/home/dssimmon/.stack/setup-exe-cache/x86_64-linux/Cabal-simple_mPHDZzAJ_2.0.1.0_ghc-8.2.2 --builddir=.stack-work/dist/x86_64-linux/Cabal-2.0.1.0 build exe:linker-error-exec --ghc-options " -ddump-hi -ddump-to-file -fdiagnostics-color=always"
        Process exited with code: ExitFailure 1

The error goes away if one performs simple code transformations, for
example, in line 94, if one replaces

    (\(_,_) -> undefined)

with

    (\_ -> undefined)

Then the code suddenly compiles.

I initially observed this error in a codebase of about 50 modules. I
tried to simplify the code that exhibits the error as much as
possible. Unfortunately, I have not been able to simplify it further
-- most code transformations, or substitutions with `undefined` cause
the linker error to go away. The code is nonsensical because I have
substituted several expressions with `undefined` in an attempt to
simplify it.

 The error also goes away if one turns on -O2, but that does not help
in general. In the codebase where I originally observed this error, I
had -O2 turned on.
