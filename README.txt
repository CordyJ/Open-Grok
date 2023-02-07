Grok for Turing Plus
Jim Cordy, October 2000

Revised for Turing+ 6.0, Jim Cordy, March 2019

Limitations
-----------
1. The Turing Plus translation was done ignoring warnings.  
	Lots of warnings will be generated when you compile 
	this source if you do not specify the suppress warnings 
	(-w) command line option.

2. The DRI/Legasys version of the T+ compiler, that treats 
	unimported symbols as a warning, is required.  
	Unimported symbols are a fatal error in the original 
	Turing Plus compiler.

3. Ropes (long strings) are not implemented, but are irrelevant
	in Turing+ 6.0 since strings can be up to 4095 characters.

JRC 15.3.19
