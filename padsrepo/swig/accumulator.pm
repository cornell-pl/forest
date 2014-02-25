package accumulator;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw( );

use pads;
#
#  This PERL module is a reimplementation of the Accumulator template 
#  program.  It is used by a short driver program (example below) and
#  in conjunction with an interface module corresponding to the data
#  type the user wishes to run an accumulator for.  (See ai_accum.pl
#  for an example of that).
#
#  Example driver program:
#
#	#!/usr/bin/perl -w 
#
#	use accumulator;
#
# 	# For full list of config parameters, see list below
#	my %cfg = (		
#		PADS_TY =>  "http_clf_t",	
#		PADS_TY_MODULE => "ai",
#		IO_DISC_MK => "pads::P_ctrec_make(10, 0)"
#	);
#
#	accumulator::main(%cfg);
#	__END__
#
# Pass a hash to accumulator::main with the following keys to configure
# template.
#
# Key				Default		Notes
# ----------------------------------------------------------------------------
# PADS_TY			-		Mandatory: Name of type to accumulate
# PADS_HDR_TY			-
# PADS_TY_MODULE		-		Mandatory: Name of SWIG I/F
# READ_MASK			P_CheckAndSet	
# EXTRA_READ_ARGS		-		precede with comma, escape $
# EXTRA_HDR_READ_ARGS		-		precede with comma, escape $
# DEF_INPUT_FILE		/dev/stdin
# MAX_RECS			0
# EXTRA_DECLS			-
# PRE_LIT_LWS			-
# WSPACE_OK			-
# IN_TIME_ZONE			-
# OUT_TIME_ZONE			-
# TIMESTAMP_IN_FMT		-
# DATE_IN_FMT			-
# TIME_IN_FMT			-
# TIMESTAMP_EXPLICIT_OUT_FMT	-
# TIMESTAMP_OUT_FMT		-
# DATE_EXPLICIT_OUT_FMT		-
# DATE_OUT_FMT			-
# TIME_EXPLICIT_OUT_FMT		-
# TIME_OUT_FMT			-
# IO_DISC_MK			-
# IO_DISC_DESCR			-
# EXTRA_BEGIN_CODE		-
# EXTRA_BAD_READ_CODE		-
# EXTRA_HDR_READ_ARGS		-		precede with comma.

sub main(\%) {
	my %cfg = %{$_[0]};
	#print $cfg; print "\n";

	my $PADS_TY = $cfg{PADS_TY};
	die "PADS_TY not provided" if !defined($PADS_TY);

	my $pkg = $cfg{PADS_TY_MODULE};
	die "PADS_TY_MODULE not provided" if !defined($pkg);
	
	eval "use $pkg;";
	#$PADS_TY = "SomaCPEStat_t" unless defined $PADS_TY; # TODO: remove
	# compute function names

	my $pads_type 	  = $PADS_TY;
	my $pads_type_pd  = "${PADS_TY}_pd";
	my $pads_type_m	  = "${PADS_TY}_m";
	my $pads_type_acc = "${PADS_TY}_acc";

	my $pads_type_init     = "${pkg}::${pads_type}_init";
	my $pads_type_pd_init  = "${pkg}::${pads_type_pd}_init";
	my $pads_type_m_init   = "${pkg}::${pads_type_m}_init";
	my $pads_type_acc_init = "${pkg}::${pads_type_acc}_init";

	my $void_to_type = "${pkg}::voidTo${PADS_TY}";
	my $void_to_type_pd = "${pkg}::voidTo" . $PADS_TY . "_pd";
	my $void_to_type_m = "${pkg}::voidTo" . $PADS_TY . "_m";
	my $void_to_type_acc = "${pkg}::voidTo" . $PADS_TY . "_acc";

	my $sizeof_type     = "${pkg}::sizeof_${pads_type}";
	my $sizeof_type_pd  = "${pkg}::sizeof_${pads_type_pd}";
	my $sizeof_type_m   = "${pkg}::sizeof_${pads_type_m}";
	my $sizeof_type_acc = "${pkg}::sizeof_${pads_type_acc}";

	my $pads_type_cleanup     = "${pkg}::${pads_type}_cleanup";
	my $pads_type_pd_cleanup  = "${pkg}::${pads_type_pd}_cleanup";
	my $pads_type_acc_cleanup = "${pkg}::${pads_type_acc}_cleanup";

	# back to regularly scheduled code....
	my $num_recs = 0;
	#my $rmm_zero;	#  RMM_t            *rmm_zero;
	#my $pads;	#  P_t              *pads;
	#my $my_disc = new_Pdisc_t;	#  Pdisc_t           my_disc = Pdefault_disc;
	# or
	my $my_disc = $pads::Pdefault_disc;	#  Pdisc_t           my_disc = Pdefault_disc;
	my $io_disc;	#  Pio_disc_t       *io_disc = 0;
	#my ($bpos, $epos);#  Ppos_t            bpos, epos;
	#
  	#my $rep;	#  PADS_TY( )       *rep;
	#my $pd;		#  PADS_TY(_pd)     *pd;
	#my $m;		#  PADS_TY(_m)      *m;
	#my $acc;	#  PADS_TY(_acc)    *acc;
	#
	#ifdef PADS_HDR_TY
	my $hdr_rep;	#  PADS_HDR_TY( )    hdr_rep;
	my $hdr_pd;	#  PADS_HDR_TY(_pd)  hdr_pd;
	my $hdr_m;	#  PADS_HDR_TY(_m)   hdr_m;
	#endif /* PADS_HDR_TY */
  	#
	my $fileName;	#  char             *fileName = 0;
	#
	my $EXTRA_DECLS = $cfg{EXTRA_DECLS};
	eval $EXTRA_DECLS if defined($EXTRA_DECLS);
	warn $@ if $@;

	my $DEF_INPUT_FILE = $cfg{DEF_INPUT_FILE};
	$DEF_INPUT_FILE = "/dev/stdin" unless defined $DEF_INPUT_FILE;

	my $PRE_LIT_LWS = $cfg{PRE_LIT_LWS};
	pads::Pdisc_s::swig_pre_list_lws_set($my_disc, $PRE_LIT_LWS) if defined($PRE_LIT_LWS);

	my $WSPACE_OK = $cfg{WSPACE_OK};
	pads::Pdisc_s::swig_flags_set($my_disc,  pads::Pdisc_s::swig_flags_get($my_disc) | $pads::P_WSPACE_OK) if defined($WSPACE_OK);

	pads::Pdisc_s::swig_copy_strings_set($my_disc, 1);  # strings should almost always be copied for accumulator programs

	my $IN_TIME_ZONE = $cfg{IN_TIME_ZONE};
	if (defined $IN_TIME_ZONE) { 
		pads::Pdisc_s::swig_in_time_zone_set($my_disc, $IN_TIME_ZONE);
		pads::error(0, "Note: set my_disc.in_time_zone to \"%s\"\n", $IN_TIME_ZONE);
	}
	my $OUT_TIME_ZONE = $cfg{OUT_TIME_ZONE};
	if (defined $OUT_TIME_ZONE) {
		pads::Pdisc_s::swig_out_time_zone_set($my_disc, $OUT_TIME_ZONE);
		pads::error(0, "Note: set my_disc.out_time_zone to \"%s\"\n", $OUT_TIME_ZONE);
	} 

	my $TIMESTAMP_IN_FMT = $cfg{TIMESTAMP_IN_FMT};
	pads::Pdisc_s::swig_in_formats::timestamp_set($my_disc, $TIMESTAMP_IN_FMT) if defined $TIMESTAMP_IN_FMT;

	my $DATE_IN_FMT = $cfg{DATE_IN_FMT};
	pads::Pdisc_s::swig_in_formats::timestamp_set($my_disc, $DATE_IN_FMT) if defined $DATE_IN_FMT;

	my $TIME_IN_FMT = $cfg{TIME_IN_FMT};
	pads::Pdisc_s::swig_in_formats::timestamp_set($my_disc, $TIME_IN_FMT) if defined $TIME_IN_FMT;

	my $TIMESTAMP_EXPLICIT_OUT_FMT = $cfg{TIMESTAMP_EXPLICIT_OUT_FMT};
	pads::Pdisc_s::swig_out_formats::timestamp_set($my_disc, $TIMESTAMP_EXPLICIT_OUT_FMT) if defined $TIMESTAMP_EXPLICIT_OUT_FMT;

	my $TIMESTAMP_OUT_FMT = $cfg{TIMESTAMP_OUT_FMT};
	pads::Pdisc_s::swig_out_formats::timestamp_set($my_disc, $TIMESTAMP_OUT_FMT) if defined $TIMESTAMP_OUT_FMT;

	my $DATE_EXPLICIT_OUT_FMT = $cfg{DATE_EXPLICIT_OUT_FMT};
	pads::Pdisc_s::swig_out_formats::timestamp_set($my_disc, $DATE_EXPLICIT_OUT_FMT) if defined $DATE_EXPLICIT_OUT_FMT;

	my $DATE_OUT_FMT = $cfg{DATE_OUT_FMT};
	pads::Pdisc_s::swig_out_formats::timestamp_set($my_disc, $DATE_OUT_FMT) if defined $DATE_OUT_FMT;

	my $TIME_EXPLICIT_OUT_FMT = $cfg{TIME_EXPLICIT_OUT_FMT};
	pads::Pdisc_s::swig_out_formats::timestamp_set($my_disc, $TIME_EXPLICIT_OUT_FMT) if defined $TIME_EXPLICIT_OUT_FMT;

	my $TIME_OUT_FMT = $cfg{TIME_OUT_FMT};
	pads::Pdisc_s::swig_out_formats::timestamp_set($my_disc, $TIME_OUT_FMT) if defined $TIME_OUT_FMT;

	my $IO_DISC_MK = $cfg{IO_DISC_MK};
	if (defined $IO_DISC_MK) {
		eval '$io_disc = ' . "$IO_DISC_MK";
		warn $@ if $@;
		#print "io_disc=$io_disc\n";
		#print "ref io_disc = "; print ref $io_disc; print "\n";
		if ($@) {
			pads::error(${pads::ERROR_FATAL}, "IO discipline make call [ $IO_DISC_MK ] failed");
		} else {
			my $IO_DISC_DESCR = $cfg{IO_DSIC_DESCR};
			if (defined $IO_DISC_DESCR) {
				pads::error(0, "Installed $IO_DISC_DESCR");
			} 
		}
	} 

	$argv = @ARGV;
	$fileName = ($argv == 1) ? $ARGV[0] : $DEF_INPUT_FILE;
  	pads::error(0, "Data file = %s\n", $fileName);

	my ($rc, $pads) = pads::P_open($my_disc, $io_disc);
	pads::error($pads::ERROR_FATAL, "*** P_open failed ***") if $rc == $pads::P_ERR;

	print "fileName=$fileName\n";
	$rc = pads::P_io_fopen($pads, $fileName);
	pads::error($pads::ERROR_FATAL, "*** P_io_fopen failed ***") if $rc == $pads::P_ERR;

	# allocate the main rep, pd, m, and acc in the heap
	my $rmm_zero = pads::P_rmm_zero($pads);

	my $PADS_HDR_TY = $cfg{PADS_HDR_TY};
	#$PADS_HDR_TY = "" unless defined $PADS_HDR_TY; # TODO: remove

  	my $rep  = &$void_to_type(pads::RMM_alloc_unmanaged_buf($rmm_zero, &$sizeof_type()));
  	my $pd   = &$void_to_type_pd(pads::RMM_alloc_unmanaged_buf($rmm_zero, &$sizeof_type_pd()));
	my $m    = &$void_to_type_m(pads::RMM_alloc_unmanaged_buf($rmm_zero, &$sizeof_type_m()));
	my $acc  = &$void_to_type_acc(pads::RMM_alloc_unmanaged_buf($rmm_zero, &$sizeof_type_acc()));

	$rc = &$pads_type_init($pads, $rep);
	pads::error($pads::ERROR_FATAL, "*** representation initialization failed ***") if $rc == $pads::P_ERR;
	$rc = &$pads_type_pd_init($pads, $pd);
	pads::error($pads::ERROR_FATAL, "*** parse description initialization failed ***") if $rc == $pads::P_ERR;
	$rc = &$pads_type_acc_init($pads, $acc);
	pads::error($pads::ERROR_FATAL, "*** accumulator initialization failed ***") if $rc == $pads::P_ERR;

	# init mask -- must do this!
	my $READ_MASK = $cfg{READ_MASK};
	$READ_MASK = 7 unless defined $READ_MASK;
	&$pads_type_m_init($pads, $m, $READ_MASK);

	my $EXTRA_BEGIN_CODE = $cfg{EXTRA_BEGIN_CODE};
	eval $EXTRA_BEGIN_CODE if defined($EXTRA_BEGIN_CODE);
	warn $@ if $@;

	if (defined $PADS_HDR_TY) {
		my $pads_hdr_type_init    = "${PADS_HDR_TY}_init";
		my $pads_hdr_type_pd_init = "${PADS_HDR_TY}_pd_init";
		my $pads_hdr_type_m_init  = "${PADS_HDR_TY}_m_init";

		$rc = &$pads_hdr_type_init($pads, $hdr_rep);
		pads::error($pads::ERROR_FATAL, "*** header representation initialization failed ***") if $rc == $pads::P_ERR;
		$rc = &$pads_hdr_type_pd_init($pads, $hdr_pd);
		pads::error($pads::ERROR_FATAL, "*** header parse description initialization failed ***") if $rc == $pads::P_ERR;

		# init mask -- must do this!
		$rc = &$pads_hdr_type_m_init($pads, $hdr_m, $pads::P_CheckAndSet);
		
		# Try to read header
		if (!pads::P_io_at_eof($pads)) {
			my $pads_hdr_type_read = "${PADS_HDR_TY}_read";
			my $EXTRA_HDR_READ_ARGS = $cfg{EXTRA_HDR_READ_ARGS};
			eval '$rc = &$pads_hdr_type_read($pads, $hdr_m, $hdr_pd, $hdr_rep ' . " $EXTRA_HDR_READ_ARGS)";
			warn $@ if $@;
			pads::error($pads::ERROR_FATAL, "Note: header read returned error") if $rc == $pads::P_ERR;
		} else {
			pads::error(2, "Note: header read returned OK");
  		}
	} 

	#
	# Try to read each line of data
	#
	my ($bpos, $epos) = (pads::Ppos_s->new(), pads::Ppos_s->new());
	my $MAX_RECS = $cfg{MAX_RECS};
	$MAX_RECS = 0 if !defined $MAX_RECS;
	my $EXTRA_READ_ARGS = $cfg{EXTRA_READ_ARGS};
	$EXTRA_READ_ARGS = "" unless defined $EXTRA_READ_ARGS;
	my $EXTRA_BAD_READ_CODE = $cfg{EXTRA_BAD_READ_CODE};
	my $EXTRA_GOOD_READ_CODE = $cfg{EXTRA_GOOD_READ_CODE};
	my $pads_type_read = "${pkg}::${PADS_TY}_read";
	my $pads_type_acc_add = "${pkg}::${PADS_TY}_acc_add";
	while (!pads::P_io_at_eof($pads) && ($MAX_RECS == 0 || $num_recs++ < $MAX_RECS)) {
		$rc = pads::P_io_getPos($pads, $bpos, 0);
		eval '$rc = &$pads_type_read($pads, $m, $pd, $rep' . "$EXTRA_READ_ARGS )";
		warn $@ if $@;
		if ($rc != $pads::P_OK) {
			if (defined $EXTRA_BAD_READ_CODE) {
				eval $EXTRA_BAD_READ_CODE;
				warn $@ if $@;
			} else {
				pads::error(2, "read returned error");
			}
		} else {
			if (defined $EXTRA_GOOD_READ_CODE) {
				eval '$rc = &$pads_type_verify($rep ' . "$EXTRA_READ_ARGS)";
				warn $@ if $@;
				if ($rc == $pads::P_OK) {
					pads::error(2, "read reported no errors and passed predicate test.");  
				} else {
					pads::error(2, "read reported no errors but failed predicate test.");
				}
				eval $EXTRA_GOOD_READ_CODE;
				warn $@ if $@;
			}
		}

		$rc = pads::P_io_getPos($pads, $epos, 0);
		if (pads::P_POS_EQ($bpos, $epos)) {
			pads::error($pads::ERROR_FATAL, "*** read loop stuck: read call did not advance IO cursor");
		}

		# accum both good and bad vals 
		$rc = &$pads_type_acc_add($pads, $acc, $pd, $rep);
		pads::error($pads::ERROR_FATAL, "*** accumulator add failed ***") if $rc == $pads::P_ERR;
	} 
	
	my $pads_type_acc_report = "${pkg}::${PADS_TY}_acc_report";
  	$rc = &$pads_type_acc_report($pads, "", 0, 0, $acc);
	pads::error($pads::ERROR_FATAL, "** accum_report failed **") if $rc == $pads::P_ERR;

	my $EXTRA_DONE_CODE = $cfg{EXTRA_DONE_CODE};
	eval $EXTRA_DONE_CODE if defined($EXTRA_DONE_CODE);
	warn $@ if $@;

	pads::P_io_close($pads) == $pads::P_ERR and pads::error($pads::ERROR_FATAL, "*** P_io_close failed ***");

  	$rc = &$pads_type_cleanup($pads, $rep);
	pads::error($pads::ERROR_FATAL, "** representation cleanup failed **") if $rc == $pads::P_ERR;

  	$rc = &$pads_type_pd_cleanup($pads, $pd);
	pads::error($pads::ERROR_FATAL, "** parse descriptor cleanup failed **") if $rc == $pads::P_ERR;

  	$rc = &$pads_type_acc_cleanup($pads, $acc);
	pads::error($pads::ERROR_FATAL, "** accumulator cleanup failed **") if $rc == $pads::P_ERR;

	# free the heap-allocated rep, pd, m, acc
	pads::RMM_free_buf($rmm_zero, $rep);
	pads::RMM_free_buf($rmm_zero, $pd);
	pads::RMM_free_buf($rmm_zero, $m);
	pads::RMM_free_buf($rmm_zero, $acc);

	$rc = pads::P_close($pads);
	pads::error($pads::ERROR_FATAL, "*** P_close failed ***") if $rc == $pads::P_ERR;

	return 0;
}
1;
