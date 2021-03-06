eightbit-kansas-lava
====================

Kansas Lava implementation of various 8-bit home computers from the early 80's.
The target hardware is the [Papilio Pro](http://papilio.cc/index.php?n=Papilio.PapilioPro),
but anything supported by Kansas Lava that has the right IO ports should work without
too much hassle.

Implemented machines
--------------------

 * Commodore PET (WIP)


Software Dependencies
---------------------

 * Kansas Lava 0.2.4.1, available on Hackage
 * [Shake build rules for Kansas Lava](http://github.com/gergoerdi/kansas-lava-shake)
 * [MOS6502-kansas-lava](http://github.com/gergoerdi/mos6502-kansas-lava)

Setup
-----

Create a `build.mk` file and set either `XILINX_ROOT` or `XILINX_WRAPPER`
variables:

 * Use `XILINX_ROOT` to run Xilinx tools directly from the given
   directory
 * Use `XILINX_WRAPPER` to run Xilinx tools via a wrapper script that
   is passed the tool name as the first argument. This is useful for
   example if you need to run the toolchain in a Docker container.
