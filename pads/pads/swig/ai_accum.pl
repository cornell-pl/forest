#!/usr/bin/perl -w 

use accumulator;

my %cfg = (
	PADS_TY =>  "http_clf_t",
	PADS_TY_MODULE => "ai",
	IO_DISC_MK => 'pads::P_ctrec_make(10, 0)'
);

accumulator::main(%cfg);
