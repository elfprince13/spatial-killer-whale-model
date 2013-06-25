#!/usr/bin/env python

#NetLogo Call-Graph Analysis

import re,sys

data = ""
with open(sys.argv[1],'r') as mainfile:
	data = mainfile.read()
	data = data.split("@#$#@#$#@")[0]


ident = r"[\w\.\?=\*!<>:#\+/\$\^'&-]+"
procre = re.compile(r"to(-report)?\s+(?P<proc_name>%s).*?$(?P<proc_body>.*?)^\s*end" % ident,flags=re.UNICODE|re.MULTILINE|re.DOTALL)
identusedre = re.compile(r"[\s\[(]*(?P<ident_name>%s)[\s\])]*" % ident,flags=re.UNICODE)

incre = re.compile(r"^(?P<include_line>\s*__includes\s+\[(?P<include_files>.+?)\]\s*)$",flags=re.MULTILINE)
wre = re.compile(r"\s+")
commentre = re.compile(r"\s*;.*?$",flags=re.MULTILINE)
includes = incre.search(data)

#if len(includes) > 1:
#		print "Invalid nlogo file with multiple includes"
#else:
#		includes, = includes

if includes:
	istart,istop = includes.span('include_line')
	file_list = wre.split(includes.groupdict()['include_files'].strip())
	ifdata = []
	for fname in file_list:
		#print fname[1:-1]
		with open(fname[1:-1],'r') as infile:
			ifdata.append(infile.read())
	data =  "%s\n%s\n%s" %(data[:istart],"\n\n\n".join(ifdata),data[istop:])
	
data = commentre.sub("",data).lower()

procs = {}
found = procre.findall(data)
#print found
for kind,name,code in found:
	procs[name] = []
#print procs

for kind,name,code in found:
	potential_calls = identusedre.findall(code)
	#print name,potential_calls
	procs[name] = list(set([call for call in potential_calls if call in procs]))

#print procs

with open(sys.argv[2],'w') as outgraph:
	outgraph.write("""digraph G {
rankdir=LR
%s
	}
""" % "\n".join(['"%s" -> "%s";' % (caller,callee) for caller,callees in procs.iteritems() for callee in callees ]))
	