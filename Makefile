all: grok.x

grok.x:	grok.t globals.i nametab.i list.i pigeon.i filemgr.i \
		ta2rsf/taparse.i ta2rsf/prefixes.i ta2rsf/taoutput.i ta2rsf/graxout.i \
		relcomp.i relmerge.i closur2.i asgnop.i help.i
	tpc -w grok.t 

clean:
	/bin/rm -f grok.x
